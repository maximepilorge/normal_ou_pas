# Tests de utils/render_partage.R
source(here::here("utils", "render_partage.R"))

test_that("autour_de gère l'élision et les cas particuliers", {
  expect_equal(autour_de("Paris"), "autour de Paris")
  expect_equal(autour_de("Bordeaux"), "autour de Bordeaux")
  expect_equal(autour_de("Amiens"), "autour d'Amiens")
  expect_equal(autour_de("Avignon"), "autour d'Avignon")
  expect_equal(autour_de("Le Havre"), "autour du Havre")
})

test_that("%||% retombe sur la valeur de repli", {
  expect_equal(NULL %||% "x", "x")
  expect_equal(NA %||% "x", "x")
  expect_equal("y" %||% "x", "y")
})

test_that("dessiner_carte_partage retourne un objet ggplot", {
  p <- dessiner_carte_partage(list(
    ville = "Paris", date = as.Date("2020-08-15"), temp = 35,
    normale_moy = 26, periode_ref = "1991-2020",
    categorie = "Au-dessus des normales", juste = TRUE))
  expect_s3_class(p, "ggplot")
})

test_that("dessiner_carte_partage fonctionne sans verdict (juste = NA)", {
  p <- dessiner_carte_partage(list(
    ville = "Lyon", date = as.Date("2021-01-10"), temp = 4,
    normale_moy = 6, periode_ref = "1991-2020",
    categorie = "Dans les normales de saison", juste = NA))
  expect_s3_class(p, "ggplot")
})
