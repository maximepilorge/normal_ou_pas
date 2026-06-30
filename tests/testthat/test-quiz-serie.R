# Tests des fonctions pures du quiz « Série de 10 » (utils/helpers.R)
source(here::here("utils", "helpers.R"))

test_that("mois_saison mappe chaque saison (et NULL pour Toutes)", {
  expect_equal(mois_saison("Hiver"), c(12L, 1L, 2L))
  expect_equal(mois_saison("Été"), c(6L, 7L, 8L))
  expect_equal(mois_saison("Automne"), c(9L, 10L, 11L))
  expect_null(mois_saison("Toutes les saisons"))
  expect_null(mois_saison("inconnu"))
})

test_that("repartition_cibles somme à n et reste ~équilibrée", {
  cibles <- repartition_cibles(10, CATEGORIES_QUIZ)
  expect_equal(sum(cibles), 10)
  expect_equal(names(cibles), CATEGORIES_QUIZ)
  # 10 sur 3 catégories -> deux à 3 et une à 4
  expect_true(all(cibles >= 3) && max(cibles) == 4)
  # n divisible : strictement équilibré
  expect_equal(unname(repartition_cibles(9, CATEGORIES_QUIZ)), c(3L, 3L, 3L))
})

test_that("materialiser_question tire dans [min,max] et reconstruit la date", {
  set.seed(1)
  for (i in 1:50) {
    q <- materialiser_question(data.frame(
      ville = "Lyon", mois = 8L, jour_mois = 14L,
      categorie = "Au-dessus des normales",
      min_temp = 30.04, max_temp = 34.96, normale_moy = 28.97))
    expect_gte(q$temp, 30.0)
    expect_lte(q$temp, 35.0)
  }
  expect_equal(q$city, "Lyon")
  expect_equal(q$correct_answer, "Au-dessus des normales")
  expect_equal(format(q$date, "%m-%d"), "08-14")
  expect_equal(q$normale_moy, 29.0)
})

test_that("materialiser_question : min == max renvoie cette valeur unique", {
  q <- materialiser_question(data.frame(
    ville = "Brest", mois = 1L, jour_mois = 1L,
    categorie = "Dans les normales de saison",
    min_temp = 8.0, max_temp = 8.0, normale_moy = 8.0))
  expect_equal(q$temp, 8.0)
})

# Pool riche : 3 villes x 20 jours x 3 catégories
faire_candidats <- function(villes = c("Lyon", "Paris", "Lille"), jours = 1:20) {
  rows <- expand.grid(ville = villes, jour_mois = jours,
                      categorie = CATEGORIES_QUIZ, stringsAsFactors = FALSE)
  rows$mois <- 7L
  rows$min_temp <- 20; rows$max_temp <- 30; rows$normale_moy <- 25
  rows
}

test_that("echantillonner_serie respecte n, l'unicité (ville,jour) et les bornes", {
  set.seed(42)
  serie <- echantillonner_serie(faire_candidats(), n = 10)
  expect_length(serie, 10)
  # toutes les questions valides
  cats <- vapply(serie, function(q) q$correct_answer, character(1))
  expect_true(all(cats %in% CATEGORIES_QUIZ))
  temps <- vapply(serie, function(q) q$temp, numeric(1))
  expect_true(all(temps >= 20 & temps <= 30))
  # pas deux fois le même couple (ville, jour)
  cles <- vapply(serie, function(q) paste(q$city, format(q$date, "%m-%d")), character(1))
  expect_equal(length(unique(cles)), 10)
})

test_that("echantillonner_serie équilibre les catégories quand le pool est riche", {
  set.seed(7)
  serie <- echantillonner_serie(faire_candidats(), n = 9)
  cats <- vapply(serie, function(q) q$correct_answer, character(1))
  # pool abondant -> exactement 3 par catégorie
  comptes <- as.integer(table(factor(cats, levels = CATEGORIES_QUIZ)))
  expect_equal(sort(comptes), c(3L, 3L, 3L))
})

test_that("echantillonner_serie plafonne au nombre de couples (ville,jour) distincts", {
  # 1 ville x 4 jours x 3 catégories = 12 lignes mais 4 couples distincts
  petit <- faire_candidats(villes = "Nice", jours = 1:4)
  serie <- echantillonner_serie(petit, n = 10)
  expect_length(serie, 4)
  cles <- vapply(serie, function(q) paste(q$city, format(q$date, "%m-%d")), character(1))
  expect_equal(length(unique(cles)), 4)
})

test_that("echantillonner_serie gère un pool vide", {
  expect_equal(echantillonner_serie(NULL), list())
  expect_equal(echantillonner_serie(faire_candidats()[0, ]), list())
})

test_that("commentaire_serie : un texte par palier, bascule taquin/poli", {
  for (s in 0:10) {
    expect_type(commentaire_serie(s, 10, poli = FALSE), "character")
    expect_type(commentaire_serie(s, 10, poli = TRUE), "character")
  }
  # le ton poli porte le mot « Score » ; le taquin du 0 est distinct
  expect_match(commentaire_serie(2, 10, poli = TRUE), "^Score :")
  expect_match(commentaire_serie(0, 10, poli = FALSE), "Z[ée]ro", perl = TRUE)
  expect_false(identical(commentaire_serie(10, 10), commentaire_serie(0, 10)))
})

test_that("commentaire_serie est calibré sur le hasard d'un quiz à 3 choix", {
  # 4/10 ne doit plus évoquer « pile ou face » / « pile poil le hasard »
  expect_false(grepl("pile", commentaire_serie(4, 10), ignore.case = TRUE))
  # autour du niveau du hasard (3-4/10), on cadre explicitement sur « le hasard »
  expect_match(commentaire_serie(3, 10), "hasard|chance", ignore.case = TRUE)
  expect_match(commentaire_serie(4, 10), "hasard|chance", ignore.case = TRUE)
  # paliers hauts : pas de référence au hasard
  expect_false(grepl("hasard", commentaire_serie(10, 10), ignore.case = TRUE))
})

test_that("commentaire_manche : taquin escaladant, neutre si poli, repli stable", {
  expect_type(commentaire_manche(TRUE, 1, FALSE), "character")
  expect_type(commentaire_manche(FALSE, 1, FALSE), "character")
  # poli -> messages neutres courts
  expect_match(commentaire_manche(TRUE, 1, TRUE), "[Bb]onne")
  expect_match(commentaire_manche(FALSE, 1, TRUE), "[Mm]auvaise")
  # escalade : rangs successifs -> messages différents tant qu'on est dans le pool
  expect_false(identical(commentaire_manche(TRUE, 1, FALSE), commentaire_manche(TRUE, 2, FALSE)))
  expect_false(identical(commentaire_manche(FALSE, 1, FALSE), commentaire_manche(FALSE, 2, FALSE)))
  # au-delà du pool -> repli sur un message stable et non vide
  expect_true(nzchar(commentaire_manche(TRUE, 99, FALSE)))
  expect_true(nzchar(commentaire_manche(FALSE, 99, FALSE)))
})

test_that("phrase_projection_futur ne dit pas « passerait » d'une temp déjà en-dessous", {
  # Zone normale projetée 2100 = [10, 20] °C.
  # Déjà en-dessous aujourd'hui ET sous la zone future -> « encore plus en-dessous ».
  r <- phrase_projection_futur(5, "En-dessous des normales", 10, 20, "2100")
  expect_match(r$txt, "encore plus en-dessous")
  expect_false(grepl("passerait", r$txt))
  # Normale aujourd'hui mais sous la zone future (qui a monté) -> « passerait en-dessous ».
  r2 <- phrase_projection_futur(9, "Dans les normales de saison", 10, 20, "2100")
  expect_match(r2$txt, "passerait en-dessous")
  # Au-dessus de la zone future -> « resterait au-dessus ».
  expect_match(phrase_projection_futur(25, "Au-dessus des normales", 10, 20, "2100")$txt,
               "resterait au-dessus")
  # Dans la zone future -> « dans les normales ».
  expect_match(phrase_projection_futur(15, "Au-dessus des normales", 10, 20, "2100")$txt,
               "dans les normales")
  # Bornes non finies -> NULL (projections indisponibles).
  expect_null(phrase_projection_futur(5, "En-dessous des normales", NA, 20, "2100"))
})

test_that("couleur_score : bandes rouge (0-2) / ambre (3-6) / vert (7-8) / or (9-10)", {
  expect_equal(couleur_score(0, 10), "#C0392B")
  expect_equal(couleur_score(2, 10), "#C0392B")
  expect_equal(couleur_score(3, 10), "#E8A33D")
  expect_equal(couleur_score(6, 10), "#E8A33D")
  expect_equal(couleur_score(7, 10), "#2E8B57")
  expect_equal(couleur_score(8, 10), "#2E8B57")
  expect_equal(couleur_score(9, 10), "#B8860B")
  expect_equal(couleur_score(10, 10), "#B8860B")
})
