# Titre: Script de vérification de la couverture des statistiques climatiques

# --- 1. CHARGEMENT DES LIBRAIRIES ---
library(DBI)
library(RPostgres)
library(dplyr)
library(dotenv)
library(here)

# --- 2. CONNEXION À LA BASE DE DONNÉES ---
load_dot_env(file = here::here(".Renviron"))
stopifnot(Sys.getenv("DB_HOST") != "")
nom_table <- "stats_normales"

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# --- 3. REQUÊTE ADAPTÉE À 'stats_normales' ---
# On groupe par ville et par période de référence
summary_stats <- tbl(con, nom_table) %>%
  group_by(ville, periode_ref) %>%
  summarise(
    # On compte simplement le nombre de jours pour lesquels une stat a été calculée
    nombre_jours_calculés = n(),
    .groups = "drop" # On retire le groupement après le summarise
  ) %>%
  collect()

# On peut maintenant fermer la connexion
dbDisconnect(con)

# --- 4. CALCULS FINAUX EN R ---
summary_final <- summary_stats %>%
  mutate(
    # Le nombre théorique de jours dans une année est 366 (pour couvrir les années bissextiles)
    nombre_jours_theorique = 366,
    # On calcule le nombre de jours pour lesquels il manque une statistique
    jours_manquants = nombre_jours_theorique - nombre_jours_calculés
  ) %>%
  # On trie pour la lisibilité
  arrange(ville, periode_ref)

# --- 5. AFFICHAGE DU RÉSULTAT ---
print("Résumé de la couverture des statistiques par ville et par période :")
print(n = 50, summary_final) # Augmentation de n pour tout voir