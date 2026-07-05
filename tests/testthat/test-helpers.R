# Tests de utils/helpers.R
source(here::here("utils", "helpers.R"))

test_that("get_season_info retourne la bonne saison", {
  expect_equal(get_season_info(as.Date("2020-01-15"))$nom, "en hiver")
  expect_equal(get_season_info(as.Date("2020-04-15"))$nom, "au printemps")
  expect_equal(get_season_info(as.Date("2020-07-15"))$nom, "en été")
  expect_equal(get_season_info(as.Date("2020-10-15"))$nom, "en automne")
})

test_that("get_season_info expose les mois de la saison", {
  expect_equal(get_season_info(as.Date("2020-12-31"))$mois, c(12, 1, 2))
  expect_equal(get_season_info(as.Date("2020-08-01"))$mois, c(6, 7, 8))
})

test_that("log_debug est silencieux par défaut et actif sur option", {
  old <- options(normaloupas.debug = FALSE)
  on.exit(options(old), add = TRUE)
  expect_silent(log_debug("rien"))
  options(normaloupas.debug = TRUE)
  expect_message(log_debug("coucou"), "coucou")
})

test_that("classer_jour_extreme classe une valeur dans une distribution", {
  v <- c(20, 22, 25, 28, 30, 35)
  # 35 est la valeur la plus chaude -> rang_haut 1 ; rang_bas 6
  r_chaud <- classer_jour_extreme(v, 35)
  expect_equal(r_chaud$rang_haut, 1)
  expect_equal(r_chaud$rang_bas, 6)
  expect_equal(r_chaud$n, 6)
  # 20 est la plus froide -> rang_haut 6 ; rang_bas 1
  r_froid <- classer_jour_extreme(v, 20)
  expect_equal(r_froid$rang_haut, 6)
  expect_equal(r_froid$rang_bas, 1)
})

test_that("classer_jour_extreme ignore les NA et gère les ex æquo", {
  v <- c(10, NA, 30, 30, 30)
  r <- classer_jour_extreme(v, 30)
  expect_equal(r$n, 4)            # NA exclu
  expect_equal(r$rang_haut, 1)    # aucune valeur strictement supérieure
})

test_that("fmt_temp arrondit, met une virgule décimale et gère les décimales", {
  expect_equal(fmt_temp(23.44), "23,4")
  expect_equal(fmt_temp(23), "23,0")            # nsmall force la décimale
  expect_equal(fmt_temp(-1.25, 2), "-1,25")     # dec = 2
  expect_equal(fmt_temp(c(1.04, 2.46)), c("1,0", "2,5"))  # vectorisé
})

test_that("classer_normale applique la règle p10/p90 et gère l'absence de bornes", {
  expect_equal(classer_normale(30, 10, 20), "Au-dessus des normales")
  expect_equal(classer_normale(5, 10, 20), "En-dessous des normales")
  expect_equal(classer_normale(15, 10, 20), "Dans les normales de saison")
  # bornes incluses -> « dans » (comparaisons strictes)
  expect_equal(classer_normale(20, 10, 20), "Dans les normales de saison")
  expect_equal(classer_normale(10, 10, 20), "Dans les normales de saison")
  # bornes indisponibles -> « dans »
  expect_equal(classer_normale(30, NA, NA), "Dans les normales de saison")
  expect_equal(classer_normale(30, 10, NA), "Dans les normales de saison")
})

test_that("couleur_categorie mappe chaque catégorie à sa couleur", {
  expect_equal(couleur_categorie("Au-dessus des normales"), "#E41A1C")
  expect_equal(couleur_categorie("En-dessous des normales"), "#1f77b4")
  expect_equal(couleur_categorie("Dans les normales de saison"), "#2E8B57")
})

test_that(".periode_bornes extrait les bornes d'un libellé AAAA-AAAA", {
  expect_equal(.periode_bornes("1991-2020"), c(1991, 2020))
  expect_equal(.periode_bornes("1951-1980")[1], 1951)
})

test_that("construire_query_string encode onglet et état du module", {
  expect_equal(construire_query_string("quiz"), "?onglet=quiz")
  expect_equal(construire_query_string("methodo", NULL), "?onglet=methodo")
  # ville + date (Une journée), accents et espaces encodés
  qs <- construire_query_string("jour", list(ville = "Orléans", date = as.Date("2024-08-15")))
  expect_equal(qs, "?onglet=jour&ville=Orl%C3%A9ans&date=2024-08-15")
  qs2 <- construire_query_string("comparer", list(ville = "Le Havre", annee = 2003L))
  expect_equal(qs2, "?onglet=comparer&ville=Le%20Havre&annee=2003")
  # champs NULL ou vides omis
  expect_equal(construire_query_string("evolution", list(ville = NULL)), "?onglet=evolution")
  expect_equal(construire_query_string("evolution", list(ville = "")), "?onglet=evolution")
})

test_that("ONGLETS_APP couvre les cinq onglets de l'app", {
  expect_setequal(ONGLETS_APP, c("quiz", "comparer", "jour", "evolution", "methodo"))
})
