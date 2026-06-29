# Tests de utils/calculer_indicateurs.R
source(here::here("utils", "calculer_indicateurs.R"))
library(dplyr)

test_that("calculer_seuil_forte_chaleur renvoie le 90e pct estival par ville", {
  d <- tibble(
    ville = "TestVille",
    date = seq(as.Date("1973-01-01"), as.Date("2003-12-31"), by = "day")
  ) %>%
    mutate(temperature_max = 20 + (lubridate::yday(date) %% 15))
  s <- calculer_seuil_forte_chaleur(d)
  expect_equal(nrow(s), 1)
  expect_equal(s$ville, "TestVille")
  attendu <- d %>%
    filter(lubridate::month(date) %in% 6:8) %>%
    summarise(q = quantile(temperature_max, 0.9, na.rm = TRUE)) %>%
    pull(q)
  expect_equal(s$seuil, as.numeric(attendu))
})

test_that("calculer_indicateurs_annuels compte gel, chaleur et nuits tropicales", {
  d <- tibble(
    ville = "V",
    annee = 2000L,
    mois = c(1L, 1L, 7L, 7L),
    jour_mois = c(1L, 2L, 1L, 2L),
    date = as.Date(c("2000-01-01", "2000-01-02", "2000-07-01", "2000-07-02")),
    temperature_max = c(2, 3, 36, 31),
    temperature_min = c(-5, -1, 22, 18)
  )
  r <- calculer_indicateurs_annuels(d)
  expect_equal(nrow(r), 1)
  expect_equal(r$jours_gel, 2)               # tmin <= 0 : -5 et -1
  expect_equal(r$nuits_tropicales, 1)        # tmin >= 20 : 22
  expect_equal(r$jours_chaleur_30, 2)        # tmax >= 30 : 36 et 31
  expect_equal(r$jours_forte_chaleur_35, 1)  # tmax >= 35 : 36
  expect_equal(r$nb_jours, 4)
})
