#!/usr/bin/env Rscript
# Lance la suite de tests unitaires des fonctions PURES de l'application
# (aucune dépendance à la base de données).
#
#   Rscript tests/run_tests.R
#
# Les tests vivent dans tests/testthat/ (fichiers test-*.R) et sourcent
# directement les fichiers utils/ qu'ils vérifient.

library(testthat)
library(here)

test_dir(here::here("tests", "testthat"),
         reporter = "summary",
         stop_on_failure = TRUE)
