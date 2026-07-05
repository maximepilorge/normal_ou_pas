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

test_that("serialiser/deserialiser_defi font un aller-retour fidèle", {
  serie <- list(
    list(city = "Orléans", date = as.Date("2024-08-15"), temp = 34.5,
         correct_answer = "Au-dessus des normales", normale_moy = 27.3),
    list(city = "Le Havre", date = as.Date("2024-01-02"), temp = -1,
         correct_answer = "En-dessous des normales", normale_moy = 7))
  payload <- serialiser_defi(serie, "1991-2020", score = 2L)
  out <- deserialiser_defi(payload, c("Orléans", "Le Havre"), "1991-2020")
  expect_equal(out$periode, "1991-2020")
  expect_equal(out$score, 2L)
  expect_length(out$serie, 2)
  expect_equal(out$serie[[1]]$city, "Orléans")
  expect_equal(out$serie[[1]]$date, as.Date("2024-08-15"))
  expect_equal(out$serie[[1]]$temp, 34.5)
  expect_equal(out$serie[[1]]$correct_answer, "Au-dessus des normales")
  expect_equal(out$serie[[2]]$temp, -1)          # sérialisé "-1.0"
  expect_equal(out$serie[[2]]$normale_moy, 7)
})

test_that("serialiser_defi sans score laisse le champ vide (score NA au retour)", {
  serie <- list(list(city = "Paris", date = as.Date("2024-06-01"), temp = 25,
                     correct_answer = "Dans les normales de saison", normale_moy = 22))
  out <- deserialiser_defi(serialiser_defi(serie, "1951-1980"), "Paris", "1951-1980")
  expect_true(is.na(out$score))
  expect_length(out$serie, 1)
})

test_that("deserialiser_defi rejette tout payload douteux", {
  villes <- "Paris"; periodes <- "1991-2020"
  ok <- serialiser_defi(list(list(city = "Paris", date = as.Date("2024-06-01"),
    temp = 25, correct_answer = "Dans les normales de saison", normale_moy = 22)),
    "1991-2020", 5L)
  expect_null(deserialiser_defi(NULL, villes, periodes))
  expect_null(deserialiser_defi("n'importe quoi", villes, periodes))
  expect_null(deserialiser_defi(sub("^v1", "v9", ok), villes, periodes))                    # version inconnue
  expect_null(deserialiser_defi(gsub("Paris", "Gotham", ok, fixed = TRUE), villes, periodes))    # ville inconnue
  expect_null(deserialiser_defi(gsub("1991-2020", "1901-1930", ok, fixed = TRUE), villes, periodes)) # période inconnue
  expect_null(deserialiser_defi(gsub("~25.0~", "~99.0~", ok, fixed = TRUE), villes, periodes))   # température invraisemblable
  expect_null(deserialiser_defi(gsub("~2~", "~8~", ok, fixed = TRUE), villes, periodes))    # catégorie hors bornes
  # score incohérent (> nb de manches) : neutralisé, la série reste jouable
  louche <- deserialiser_defi(sub(";5;", ";11;", ok, fixed = TRUE), villes, periodes)
  expect_true(is.na(louche$score))
  expect_length(louche$serie, 1)
})

test_that("libelles_periodes traduit les périodes en repères d'époque", {
  periodes <- c("1951-1980", "1961-1990", "1971-2000", "1981-2010", "1991-2020")
  lib <- libelles_periodes(periodes, annee = 2026)
  expect_equal(unname(lib), periodes)          # les valeurs restent les périodes
  expect_equal(names(lib), c(
    "L'époque de vos grands-parents (1951-1980)",
    "Il y a un demi-siècle (1961-1990)",
    "L'époque de vos parents (1971-2000)",
    "La fin du XXe siècle (1981-2010)",
    "Le climat actuel (1991-2020)"))
})

test_that("libelles_periodes vieillit avec l'année et gère les cas limites", {
  # Dans 20 ans, 1971-2000 devient l'époque des grands-parents.
  lib <- libelles_periodes(c("1971-2000", "1991-2020"), annee = 2046)
  expect_equal(names(lib), c("L'époque de vos grands-parents (1971-2000)",
                             "Le climat actuel (1991-2020)"))
  expect_length(libelles_periodes(character(0)), 0)
})

test_that("rechauffement_depuis compare l'époque d'origine aux années récentes", {
  # Réchauffement linéaire de 0,03 °C/an sur 1950-2025.
  annees <- 1950:2025
  anoms <- data.frame(annee = annees, anomalie = (annees - 1950) * 0.03)
  # Né en 1990 : fenêtre 1983-1997 (centre 1990) vs 2011-2025 (centre 2018).
  expect_equal(rechauffement_depuis(anoms, 1990), 28 * 0.03, tolerance = 1e-9)
  # Trop peu de valeurs autour de l'origine (série commençant en 1988) -> NA.
  expect_true(is.na(rechauffement_depuis(anoms[anoms$annee >= 1988, ], 1950)))
  expect_true(is.na(rechauffement_depuis(NULL, 1990)))
  expect_true(is.na(rechauffement_depuis(anoms, NA)))
  # Naissance récente : fenêtres qui se chevauchent, écart proche de zéro.
  expect_lt(abs(rechauffement_depuis(anoms, 2020)), 0.25)
})
