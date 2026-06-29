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
  withr_ok <- requireNamespace("withr", quietly = TRUE)
  old <- options(normaloupas.debug = FALSE)
  on.exit(options(old), add = TRUE)
  expect_silent(log_debug("rien"))
  options(normaloupas.debug = TRUE)
  expect_message(log_debug("coucou"), "coucou")
})
