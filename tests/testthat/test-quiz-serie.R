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

test_that("construire_projections assemble présent + niveaux (normale + deltas)", {
  base <- data.frame(t_moy = 20, seuil_bas_p10 = 15, seuil_haut_p90 = 25)
  deltas <- data.frame(
    niveau_rechauffement = c("2050_+2.7", "2100_+4.0"),
    delta_moy = c(1, 2), delta_p10 = c(0.5, 1), delta_p90 = c(1.5, 3),
    delta_moy_bas = c(0.8, 1.6), delta_moy_haut = c(1.2, 2.4))
  proj <- construire_projections(base, deltas)
  # Présent = bornes 1991-2020 telles quelles.
  expect_equal(proj$present, list(p10 = 15, p90 = 25, moy = 20))
  expect_length(proj$niveaux, 2)
  # Niveau 2100 = normale + delta correspondant.
  n2100 <- Filter(function(x) x$niveau == "2100_+4.0", proj$niveaux)[[1]]
  expect_equal(n2100$moy, 22); expect_equal(n2100$p10, 16); expect_equal(n2100$p90, 28)
  expect_equal(n2100$label, "2100 (+4 °C)")
})

test_that("construire_projections : entrées vides / niveau manquant -> repli", {
  base <- data.frame(t_moy = 20, seuil_bas_p10 = 15, seuil_haut_p90 = 25)
  # Aucune ligne de delta -> NULL.
  expect_null(construire_projections(base, base[0, , drop = FALSE]))
  expect_null(construire_projections(base, NULL))
  expect_null(construire_projections(NULL, data.frame(niveau_rechauffement = "2050_+2.7")))
  # Un seul niveau présent -> une seule entrée dans niveaux.
  deltas1 <- data.frame(
    niveau_rechauffement = "2050_+2.7",
    delta_moy = 1, delta_p10 = 0.5, delta_p90 = 1.5,
    delta_moy_bas = 0.8, delta_moy_haut = 1.2)
  proj <- construire_projections(base, deltas1)
  expect_length(proj$niveaux, 1)
  expect_equal(proj$niveaux[[1]]$niveau, "2050_+2.7")
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

# --- Animation pédagogique de révélation (fonctions pures) --------------------

test_that("jour_calendaire_2023 projette sur une année non bissextile fixe", {
  expect_equal(jour_calendaire_2023(as.Date("2024-01-01")), 1L)
  expect_equal(jour_calendaire_2023(as.Date("2024-08-12")), 224L)
  expect_equal(jour_calendaire_2023(as.Date("2024-12-31")), 365L)
  # deux 12 août de deux années -> MÊME jour calendaire (condition de l'empilement)
  expect_equal(jour_calendaire_2023(as.Date("1975-08-12")),
               jour_calendaire_2023(as.Date("2021-08-12")))
  # 29 février ramené au 28 (jour 59) -> pas de trou dans l'axe
  expect_equal(jour_calendaire_2023(as.Date("2020-02-29")),
               jour_calendaire_2023(as.Date("2023-02-28")))
})

test_that("annee_reference_fenetre choisit l'année de moyenne médiane", {
  cloud <- rbind(
    data.frame(date = as.Date("2018-08-05") + 0:14, tmax = 20),   # froide
    data.frame(date = as.Date("2019-08-05") + 0:14, tmax = 25),   # médiane
    data.frame(date = as.Date("2020-08-05") + 0:14, tmax = 30))   # chaude
  expect_equal(annee_reference_fenetre(cloud), 2019L)
})

# Nuage synthétique : 3 années × 15 jours autour du 12 août ; moyennes 24/25/26.
faire_nuage_anim <- function(annees = 2018:2020, m = 8L, j = 12L) {
  do.call(rbind, lapply(annees, function(an) {
    c0 <- as.Date(sprintf("%d-%02d-%02d", an, m, j))
    data.frame(date = c0 + (-7:7), tmax = 25 + (an - 2019) + seq(-2, 2, length.out = 15))
  }))
}
faire_courbe_anim <- function(annee = 2019L) {
  d <- seq(as.Date(sprintf("%d-01-01", annee)), as.Date(sprintf("%d-12-31", annee)), by = "day")
  data.frame(date = d, tmax = 15 + 10 * sin(2 * pi * (as.integer(format(d, "%j")) - 114) / 365))
}

test_that("preparer_payload_anim assemble un payload cohérent avec le boxplot", {
  cloud <- faire_nuage_anim()
  courbe <- faire_courbe_anim(2019L)
  p <- preparer_payload_anim(
    cloud = cloud, annee_curve = courbe, annee_ref = 2019L,
    ville = "Toulouse", periode = "1991-2020", date_quiz = as.Date("2024-08-12"),
    quiz_temp = 33, categorie = "Au-dessus des normales",
    zone = list(p10 = 21, p90 = 29), moy = 25)

  # Corps du boxplot = quantiles type 7 du nuage (ordre indifférent).
  qs <- as.numeric(stats::quantile(cloud$tmax, c(.25, .5, .75), type = 7))
  expect_equal(unname(c(p$box$q1, p$box$q2, p$box$q3)), qs)
  # Passe-plats.
  expect_equal(p$quizTemp, 33); expect_equal(p$moy, 25)
  expect_equal(p$zone, list(p10 = 21, p90 = 29))
  expect_equal(p$centerX, 224L)               # 12 août
  expect_equal(p$nAnnees, 3L)
  expect_equal(length(p$tickVals), 12L); expect_equal(length(p$tickText), 12L)

  # Groupes d'empilement : année type d'abord, indices 0-based couvrant tout le nuage.
  expect_length(p$groups, 3L)
  expect_equal(p$groups[[1]]$year, 2019L)
  tous_idx <- sort(unlist(lapply(p$groups, function(g) as.integer(g$idx))))
  expect_equal(tous_idx, 0:(nrow(cloud) - 1L))

  # Courbe triée par jour calendaire croissant ; 6 légendes ; verdict cohérent.
  expect_false(is.unsorted(p$yearX))
  expect_length(p$caps, 6L)
  expect_match(p$caps[6], "au-dessus")
  expect_match(p$caps[6], "Au-dessus des normales")
})

test_that("preparer_payload_anim : jitter reproductible et RNG global non pollué", {
  cloud <- faire_nuage_anim(); courbe <- faire_courbe_anim()
  args <- list(cloud = cloud, annee_curve = courbe, annee_ref = 2019L,
               ville = "Toulouse", periode = "1991-2020", date_quiz = as.Date("2024-08-12"),
               quiz_temp = 33, categorie = "Dans les normales de saison",
               zone = list(p10 = 21, p90 = 29), moy = 25)
  p1 <- do.call(preparer_payload_anim, args)
  p2 <- do.call(preparer_payload_anim, args)
  expect_equal(p1$cloudCalX, p2$cloudCalX)   # jitter figé (seed local)

  # avec_seed_local restaure .Random.seed : un runif() encadrant l'appel est intact.
  set.seed(123); attendu <- runif(1)
  set.seed(123); invisible(do.call(preparer_payload_anim, args)); obtenu <- runif(1)
  expect_equal(obtenu, attendu)
})
