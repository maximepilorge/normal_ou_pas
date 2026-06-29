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
