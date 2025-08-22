# transferer_data.R
# Objectif : Transférer les tables de la base locale vers le schéma "preparation" de la base de production.

# --- Chargement des librairies ---
library(DBI)
library(RPostgres)
library(dplyr)
library(dotenv)
library(here)

# --- Configuration ---
# Charger les variables d'environnement
load_dot_env(file = here::here(".Renviron"))

# Noms des tables à transférer
tables_a_transferer <- c("temperatures_max", "stats_normales", "quiz_data_precalculee")

# --- Connexions aux bases de données ---
cat("Connexion aux bases de données...\n")

# Connexion à la base LOCALE (source)
con_local <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# Connexion à la base de PRODUCTION (destination)
con_prod <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST_PROD"),
  port = as.integer(Sys.getenv("DB_PORT_PROD")),
  dbname = Sys.getenv("DB_NAME_PROD"),
  user = Sys.getenv("DB_USER_PROD"),
  password = Sys.getenv("DB_PASS_PROD")
)

# --- Boucle de transfert ---
# On utilise tryFinally pour s'assurer que les connexions sont bien fermées, même en cas d'erreur.
tryCatch({
  
  cat("Début du transfert des tables.\n")
  
  for (nom_table in tables_a_transferer) {
    
    # 1. Lire la table depuis la base locale
    cat(paste0("  -> Lecture de la table '", nom_table, "' depuis la source locale...\n"))
    
    table_locale_id <- Id(schema = "public", table = nom_table)
    donnees <- dbReadTable(con_local, table_locale_id)
    donnees$id <- NULL
    cat(paste0("     ", nrow(donnees), " lignes lues.\n"))
    
    # 2. Écrire la table dans le schéma 'preparation' de la base de production
    cat(paste0("  -> Écriture de la table '", nom_table, "' vers la destination production (schéma preparation)...\n"))
    table_prod_id <- Id(schema = "preparation", table = nom_table)
    dbWriteTable(con_prod, table_prod_id, donnees, overwrite = TRUE)
    
    cat("     Écriture terminée.\n")
  }
  
  cat("\n✅ Transfert de toutes les tables terminé avec succès !\n")
  
  cat("\nApplication de la structure (Clés, Contraintes, Index) sur le schéma 'preparation'...\n")
  
  # -- Table: temperatures_max --
  dbExecute(con_prod, "ALTER TABLE preparation.temperatures_max ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con_prod, "ALTER TABLE preparation.temperatures_max ADD CONSTRAINT uc_temperatures_max_ville_date UNIQUE (ville, date);")
  
  # -- Table: stats_normales --
  dbExecute(con_prod, "ALTER TABLE preparation.stats_normales ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con_prod, "ALTER TABLE preparation.stats_normales ADD CONSTRAINT uc_stats_normales_ville_jour_periode UNIQUE (ville, mois, jour_mois, periode_ref);")
  dbExecute(con_prod, "CREATE INDEX IF NOT EXISTS idx_stats_ville_periode ON preparation.stats_normales (ville, periode_ref);")
  
  # -- Table: quiz_data_precalculee --
  dbExecute(con_prod, "ALTER TABLE preparation.quiz_data_precalculee ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con_prod, "ALTER TABLE preparation.quiz_data_precalculee ADD CONSTRAINT uc_quiz_data_ville_date_periode UNIQUE (ville, date, periode_ref);")
  dbExecute(con_prod, "CREATE INDEX IF NOT EXISTS idx_quiz_main ON preparation.quiz_data_precalculee (periode_ref, categorie, mois);")
  
  cat("✅ Structure appliquée avec succès.\n")
  
}, finally = {
  # 3. Fermer les connexions
  cat("Fermeture des connexions aux bases de données.\n")
  dbDisconnect(con_local)
  dbDisconnect(con_prod)
})