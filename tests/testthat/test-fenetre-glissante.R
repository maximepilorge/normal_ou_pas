# Tests de utils/fenetre_glissante.R
source(here::here("utils", "fenetre_glissante.R"))

test_that("construire_ref_jours couvre les 366 jours calendaires", {
  rj <- construire_ref_jours()
  expect_equal(nrow(rj), 366)
  expect_setequal(rj$jour_ref, 1:366)
  # Le 29 février est présent (indexation sur une année bissextile fixe).
  expect_true(any(rj$mois == 2 & rj$jour_mois == 29))
})

test_that("construire_fenetre_map a la bonne taille et 2N+1 sources par jour", {
  fm <- construire_fenetre_map(7)
  expect_equal(nrow(fm), 366 * 15)            # 2*7+1 sources par jour cible
  expect_true(all(table(fm$jour_cible) == 15))
})

test_that("construire_fenetre_map est circulaire (le 1er janvier emprunte à décembre)", {
  fm <- construire_fenetre_map(7)
  src_j1 <- sort(fm$jour_ref[fm$jour_cible == 1])
  expect_equal(src_j1, sort(c(360:366, 1:8)))
})

test_that("la largeur de fenêtre est paramétrable", {
  expect_equal(nrow(construire_fenetre_map(0)), 366)       # uniquement le jour lui-même
  expect_equal(nrow(construire_fenetre_map(3)), 366 * 7)   # 2*3+1
})
