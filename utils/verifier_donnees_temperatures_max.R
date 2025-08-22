# Titre: Script de vérification de la couverture des données de température

# --- 1. CHARGEMENT DES LIBRAIRIES ---
# Assurez-vous que ces packages sont installés : install.packages(c("DBI", "RSQLite", "dplyr"))
library(DBI)
library(RSQLite)
library(dplyr)
library(here)
library(dotenv)

# --- 2. CONNEXION À LA BASE DE DONNÉES ---
# Modifiez ce chemin si votre script n'est pas à la racine du projet
load_dot_env(file = here::here(".Renviron"))
stopifnot(Sys.getenv("DB_HOST") != "")
nom_table <- "temperatures_max"

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# --- 3. PRÉPARATION ET EXÉCUTION DE LA REQUÊTE ---
# On utilise dplyr pour construire la requête qui sera envoyée à la BDD
# Cette première étape est très rapide car elle est exécutée par SQLite.
summary_db <- tbl(con, nom_table) %>%
  group_by(ville) %>%
  summarise(
    # On récupère les dates min/max (au format numérique) et le compte des lignes
    date_debut_num = min(date, na.rm = TRUE),
    date_fin_num = max(date, na.rm = TRUE),
    nombre_donnees_presentes = n()
  ) %>%
  # On rapatrie ce petit résumé en mémoire dans R
  collect()

# On peut maintenant fermer la connexion à la base de données
dbDisconnect(con)

# --- 4. CALCULS FINAUX ET FORMATAGE EN R ---
# On travaille maintenant sur le dataframe R 'summary_db'
summary_final <- summary_db %>%
  mutate(
    # Conversion des dates numériques en vraies dates R
    date_debut = as.Date(date_debut_num, origin = "1970-01-01"),
    date_fin = as.Date(date_fin_num, origin = "1970-01-01"),
    
    # Calcul du nombre total de jours qui devraient exister entre le début et la fin
    nombre_jours_theorique = as.integer(date_fin - date_debut) + 1,
    
    # Calcul simple des données manquantes
    nombre_donnees_manquantes = nombre_jours_theorique - nombre_donnees_presentes
  ) %>%
  # On sélectionne et réorganise les colonnes pour un affichage clair
  select(
    ville,
    date_debut,
    date_fin,
    nombre_donnees_presentes,
    nombre_donnees_manquantes,
    nombre_jours_theorique
  ) %>%
  # On trie par ordre alphabétique pour la lisibilité
  arrange(ville)

# --- 5. AFFICHAGE DU RÉSULTAT ---
print("Résumé de la couverture des données par ville :")
print(n=30, summary_final)