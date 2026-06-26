# deployer_projections.R
# -----------------------------------------------------------------------------
# Écrit les tables de PROJECTION TRACC dans la base LOCALE (schéma public), à
# partir des sorties de calculer_projections.R (data/projections_*.rds).
#
# Script SÉPARÉ du pipeline ERA5 (preparer_data.R) : les projections dérivent des
# fichiers DRIAS (volumineux, locaux) et ne se recalculent que rarement.
# Chaîne projections : telecharger_drias.R -> calculer_projections.R -> CE script.
# La promotion vers la production se fait ensuite via transferer_data.R (les deux
# tables y sont ajoutées) puis deployer_production.R.
#
# Tables écrites :
#   stats_normales_projetees(ville, mois, jour_mois, niveau_rechauffement,
#       delta_moy, delta_moy_bas, delta_moy_haut, delta_p10, delta_p90)
#   canicules_projetees(ville, niveau_rechauffement,
#       jours_an_central, jours_an_bas, jours_an_haut, episodes_an_central)
# -----------------------------------------------------------------------------

library(DBI)
library(RPostgres)
library(dotenv)
library(here)

load_dot_env(file = here::here(".Renviron"))
stopifnot(Sys.getenv("DB_HOST") != "")

chemin_deltas <- here::here("data", "projections_deltas.rds")
chemin_ext    <- here::here("data", "projections_extremes.rds")
if (!file.exists(chemin_deltas) || !file.exists(chemin_ext))
  stop("Sorties projections absentes — lancer d'abord calculer_projections.R.")

deltas <- readRDS(chemin_deltas)
ext    <- readRDS(chemin_ext)
cat(sprintf("Lu : %d lignes deltas, %d lignes extrêmes.\n", nrow(deltas), nrow(ext)))

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"), host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASS")
)

tryCatch({
  cat("Écriture 'stats_normales_projetees'...\n")
  dbWriteTable(con, "stats_normales_projetees", as.data.frame(deltas), overwrite = TRUE)
  dbExecute(con, "ALTER TABLE public.stats_normales_projetees ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, paste("ALTER TABLE public.stats_normales_projetees ADD CONSTRAINT",
                       "uc_proj_ville_jour_niveau UNIQUE (ville, mois, jour_mois, niveau_rechauffement);"))
  dbExecute(con, "CREATE INDEX idx_proj_ville_niveau ON public.stats_normales_projetees (ville, niveau_rechauffement);")
  dbExecute(con, "VACUUM ANALYZE public.stats_normales_projetees;")

  cat("Écriture 'extremes_projetes'...\n")
  dbWriteTable(con, "extremes_projetes", as.data.frame(ext), overwrite = TRUE)
  dbExecute(con, "ALTER TABLE public.extremes_projetes ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, paste("ALTER TABLE public.extremes_projetes ADD CONSTRAINT",
                       "uc_ext_proj_ville_niveau UNIQUE (ville, niveau_rechauffement);"))
  dbExecute(con, "VACUUM ANALYZE public.extremes_projetes;")

  cat("✅ Tables de projection écrites dans la base locale (public).\n")
  cat("→ Promotion prod : transferer_data.R puis deployer_production.R.\n")
}, finally = {
  dbDisconnect(con)
})
