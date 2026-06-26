# deployer_production.R
# Objectif : Mettre en production les tables préparées dans le schéma 'preparation'.
#
# Stratégie (par table, dans une seule transaction) :
#   1. On vérifie d'ABORD que toutes les tables existent dans 'preparation'.
#   2. On supprime l'ancienne version de production (public.<table>).
#   3. On promeut la nouvelle version (preparation.<table> -> public.<table>).
#
# Note : ALTER TABLE ... SET SCHEMA *déplace* la table. Après un déploiement
# réussi, 'preparation' est donc vidé : il faut relancer transferer_data.R
# avant de redéployer. La vérification de l'étape 1 transforme ce cas en
# message d'erreur explicite plutôt qu'en échec au milieu de la transaction.

# --- Chargement des librairies ---
library(DBI)
library(RPostgres)
library(dotenv)
library(here)

# --- Configuration ---
load_dot_env(file = here::here(".Renviron"))

# Tables à promouvoir preparation -> public.
# /!\ Doit rester synchronisé avec `tables_a_transferer` de transferer_data.R
tables_a_deployer <- c(
  "temperatures_max",
  "stats_normales",
  "quiz_data_precalculee",
  "indicateurs_annuels",
  "stats_normales_projetees",   # projections TRACC (quiz)
  "extremes_projetes"           # projections TRACC (forte chaleur + gel)
)

# --- Connexion et exécution ---
cat("Connexion à la base de données de production...\n")
con_prod <- NULL

tryCatch({

  con_prod <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST_PROD"),
    port = as.integer(Sys.getenv("DB_PORT_PROD")),
    dbname = Sys.getenv("DB_NAME_PROD"),
    user = Sys.getenv("DB_USER_PROD"),
    password = Sys.getenv("DB_PASS_PROD")
  )

  # 1. Vérifier que TOUTES les tables existent dans 'preparation'
  #    avant de toucher au schéma 'public'.
  cat("Vérification des tables dans le schéma 'preparation'...\n")
  manquantes <- character(0)
  for (nom_table in tables_a_deployer) {
    reg <- dbGetQuery(
      con_prod,
      sprintf("SELECT to_regclass('preparation.%s') AS oid", nom_table)
    )$oid
    if (is.na(reg)) manquantes <- c(manquantes, nom_table)
  }
  if (length(manquantes) > 0) {
    stop(
      "Tables absentes du schéma 'preparation' : ",
      paste(manquantes, collapse = ", "), ".\n",
      "       Lancez d'abord utils/transferer_data.R pour (re)peupler 'preparation'."
    )
  }
  cat("  -> Les ", length(tables_a_deployer), " tables sont présentes.\n", sep = "")

  # 2. Déploiement dans une transaction (tout ou rien)
  dbBegin(con_prod)
  cat("Déploiement dans une transaction...\n")

  for (nom_table in tables_a_deployer) {
    cat(sprintf("  -> %s : remplacement de public.%s\n", nom_table, nom_table))
    # Supprimer l'ancienne version de production
    dbExecute(con_prod, sprintf("DROP TABLE IF EXISTS public.%s CASCADE;", nom_table))
    # Promouvoir la nouvelle version (déplacement, pas de copie)
    dbExecute(con_prod, sprintf("ALTER TABLE preparation.%s SET SCHEMA public;", nom_table))
  }

  # 3. Valider la transaction
  dbCommit(con_prod)

  cat("\n✅ Déploiement terminé et transaction validée avec succès !\n")

}, error = function(e) {
  cat("\nERREUR : Le déploiement a échoué. Annulation de la transaction (ROLLBACK).\n")
  print(e$message)

  if (!is.null(con_prod) && dbIsValid(con_prod)) {
    dbRollback(con_prod)
  }

}, finally = {
  if (!is.null(con_prod) && dbIsValid(con_prod)) {
    cat("Fermeture de la connexion.\n")
    dbDisconnect(con_prod)
  }
})
