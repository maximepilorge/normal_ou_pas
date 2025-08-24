# Améliorations:
# - Ajouter la possibilité de comparer la distribution de deux années complètes sur une ville et éventuellement une saison spécifique (onglet "Comparer les années" ?)
# Une idée peut etre d'ajouter une seconde courbe optionnelle au graphique existant mais ça peut polluer visuellement. Sinon, à la suite, on ajoute un boxplot mettant les deux années cote à cote
# Ca signifie ajouter un input pour choisir l'autre année
# - Ajouter le nombre de records de chaleur dans l'onglet d'évolution globale avec l'analyse du réchauffement (graphique par année ?). Nécessiterait
# de construire une table avec le nombre de records enregistrés par an (début historique = 1950 ici cependant)

# Librairies
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(lubridate)
library(dplyr) 
library(tidyr) 
library(lubridate) 
library(here)
library(plotly)
library(bslib)
library(DBI)
library(RPostgres)
library(pool)
library(dbplyr)
library(glue)

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# --- CHARGEMENT DES DONNÉES ---

# Établir une "promesse" de connexion à la BDD
db_pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# S'assurer que le pool se ferme quand l'app s'arrête
onStop(function() {
  pool::poolClose(db_pool)
})

# --- FONCTIONS ---
get_season_info <- function(date_input) {
  mois <- month(date_input)
  if (mois %in% c(12, 1, 2)) {
    return(list(nom = "en hiver", mois = c(12, 1, 2)))
  } else if (mois %in% c(3, 4, 5)) {
    return(list(nom = "au printemps", mois = c(3, 4, 5)))
  } else if (mois %in% c(6, 7, 8)) {
    return(list(nom = "en été", mois = c(6, 7, 8)))
  } else { # mois %in% c(9, 10, 11)
    return(list(nom = "en automne", mois = c(9, 10, 11)))
  }
}

calculer_frequence <- function(ville_sel, date_sel, temp_sel, periode_ref_str, data_brutes) {
  
  # Extraire les années de la période de référence
  annees_periode <- as.numeric(unlist(strsplit(periode_ref_str, "-")))
  annee_debut <- annees_periode[1]
  annee_fin <- annees_periode[2]
  nombre_annees_periode <- annee_fin - annee_debut + 1
  
  # Déterminer si on cherche un événement chaud ou froid (par rapport à la moyenne)
  moyenne_jour <- tbl(db_pool, "stats_normales") %>%
    filter(
      ville == !!ville_sel, 
      jour_annee == !!yday(date_sel), 
      periode_ref == !!periode_ref_str
    ) %>%
    pull(t_moy)
  
  # Si on n'a pas de moyenne pour ce jour, on ne peut pas continuer
  if (length(moyenne_jour) == 0) return(NULL)
  
  direction <- if (temp_sel >= moyenne_jour) "supérieure ou égale" else "inférieure ou égale"
  comparaison_jour <- if (direction == "supérieure ou égale") `>=` else `<=`
  
  # --- Calcul sur le jour précis ---
  donnees_historiques_jour <- data_brutes %>%
    filter(city == ville_sel, jour_annee == yday(date_sel), year(date) >= annee_debut, year(date) <= annee_fin)
  
  nombre_occurrences_jour <- sum(comparaison_jour(donnees_historiques_jour$tmax_celsius, temp_sel), na.rm = TRUE)
  
  texte_jour <- paste0("Pour ce jour précis (le ", paste(format(date_sel, "%d"), mois_fr[as.numeric(format(date_sel, "%m"))]), "), une température ", direction, " ou égale à ", temp_sel, "°C s'est produite <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
  
  # --- Calcul sur la saison ---
  saison <- get_season_info(date_sel)
  donnees_historiques_saison <- data_brutes %>%
    filter(city == ville_sel, month(date) %in% saison$mois, year(date) >= annee_debut, year(date) <= annee_fin)
  
  nombre_occurrences_saison <- sum(comparaison_jour(donnees_historiques_saison$tmax_celsius, temp_sel), na.rm = TRUE)
  
  texte_saison <- paste0("À l'échelle de la saison (", saison$nom, "), une température ", direction, " ou égale à ", temp_sel, "°C s'est produite <b>", nombre_occurrences_saison, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
  
  return(list(jour = texte_jour, saison = texte_saison))
}

# --- AUTRES VARIABLES ---
mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
             "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")

villes <- tibble::tribble(
  ~ville, ~latitude, ~longitude,
  "Paris", 48.8566, 2.3522,
  "Marseille", 43.2965, 5.3698,
  "Lyon", 45.7640, 4.8357,
  "Toulouse", 43.6047, 1.4442,
  "Nice", 43.7102, 7.2620,
  "Nantes", 47.2184, -1.5536,
  "Strasbourg", 48.5833, 7.7458,
  "Montpellier", 43.6108, 3.8767,
  "Bordeaux", 44.8378, -0.5792,
  "Lille", 50.6292, 3.0573,
  "Rennes", 48.1173, -1.6778,
  "Reims", 49.2583, 4.0317,
  "Le Havre", 49.4944, 0.1079,
  "Saint-Étienne", 45.4397, 4.3872,
  "Toulon", 43.1242, 5.9280,
  "Angers", 47.4784, -0.5632,
  "Dijon", 47.3220, 5.0415,
  "Brest", 48.3904, -4.4869,
  "Clermont-Ferrand", 45.7772, 3.0870,
  "Limoges", 45.8336, 1.2611,
  "Tours", 47.3941, 0.6849,
  "Amiens", 49.8941, 2.2958,
  "Metz", 49.1193, 6.1757,
  "Besançon", 47.2378, 6.0240,
  "Perpignan", 42.6887, 2.8948,
  "La Rochelle", 46.1603, -1.1511,
  "Avignon", 43.9493, 4.8068,
  "Carcassonne", 43.2105, 2.3486,
  "Poitiers", 46.5802, 0.3405,
  "Ajaccio", 41.9207, 8.7397
)

periodes_disponibles <- tbl(db_pool, "stats_normales") %>%
  distinct(periode_ref) %>%
  arrange(periode_ref) %>%
  pull()

villes_triees <- tbl(db_pool, "temperatures_max") %>%
  distinct(ville) %>%
  arrange(ville) %>%
  pull(ville)

an_min_data <- 1950
an_max_data <- 2024