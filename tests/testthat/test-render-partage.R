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
  expect_equal(character(0) %||% "x", "x")          # vecteur vide
  # Un vecteur de longueur > 1 est renvoyé tel quel (pas de plantage du ||).
  expect_equal(c("a", "b") %||% "x", c("a", "b"))
  expect_equal(c(1, NA) %||% "x", c(1, NA))
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

test_that("dessiner_carte_partage rend la phrase de projection (futur)", {
  p <- dessiner_carte_partage(list(
    ville = "Lyon", date = as.Date("2024-08-15"), temp = 36,
    normale_moy = 29, periode_ref = "1991-2020",
    categorie = "Au-dessus des normales", juste = TRUE, p10 = 25, p90 = 33,
    projection_txt = "En 2100, une telle température sera dans les normales.",
    projection_couleur = "#2E8B57"))
  expect_s3_class(p, "ggplot")
})

test_that("dessiner_carte_jour retourne un ggplot (onglet « Une journée »)", {
  p <- dessiner_carte_jour(list(
    ville = "Amiens", date = as.Date("2026-06-23"), temp = 39.3,
    normale_moy = 28.1, periode_ref = "1991-2020",
    categorie = "Au-dessus des normales",
    rang_txt = "Jour le plus chaud autour du 23 juin depuis 1950"))
  expect_s3_class(p, "ggplot")
})

test_that("sauver_carte_jour produit un PNG non vide", {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  sauver_carte_jour(list(
    ville = "Paris", date = as.Date("2026-06-23"), temp = 39.3,
    normale_moy = 28.1, periode_ref = "1991-2020",
    categorie = "Au-dessus des normales",
    rang_txt = "Jour le plus chaud autour du 23 juin depuis 1950"), f)
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 1000)
})

test_that("dessiner_carte_stripes retourne un ggplot et tolère les données lacunaires", {
  p <- dessiner_carte_stripes(list(
    ville = "Nantes", annees = 1950:2025,
    anomalies = seq(-0.5, 1.6, length.out = 76),
    periode_ref = "1951-1980", rechauffement = 1.8))
  expect_s3_class(p, "ggplot")
  # NA épars et réchauffement absent : la carte se dessine quand même.
  anoms <- c(NA, seq(-0.3, 1.2, length.out = 30), NA)
  p2 <- dessiner_carte_stripes(list(
    ville = "Avignon", annees = 1994:2025, anomalies = anoms,
    periode_ref = "1951-1980"))
  expect_s3_class(p2, "ggplot")
  # Anomalies toutes nulles : bornes de palette dégénérées gérées.
  p3 <- dessiner_carte_stripes(list(
    ville = "Le Havre", annees = 2000:2020, anomalies = rep(0, 21),
    periode_ref = "1951-1980"))
  expect_s3_class(p3, "ggplot")
})

test_that("sauver_carte_stripes produit un PNG non vide", {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  sauver_carte_stripes(list(
    ville = "Paris", annees = 1950:2025,
    anomalies = (0:75) * 0.025, periode_ref = "1951-1980",
    rechauffement = 1.6), f)
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 1000)
})
