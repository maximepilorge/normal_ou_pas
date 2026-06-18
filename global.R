# Améliorations:
# - Corriger bug mod_visualisation : sur smartphone quand on change d'année, la courbe rouge apparait alors qu'elle devrait etre masquée sur smartphone
# - Supprimer titre des graph dans mod_visu et mod_analyse et ajouter des compléments dans le header de la card des graphiques (ville, année)
# - Forcer la barre d'onglet en haut de la page meme quand on scrolle
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

periodes_disponibles <- tbl(db_pool, "stats_normales") %>%
  distinct(periode_ref) %>%
  arrange(periode_ref) %>%
  pull()

villes_triees <- tbl(db_pool, "temperatures_max") %>%
  distinct(ville) %>%
  arrange(ville) %>%
  pull(ville)

# Bornes d'années dérivées dynamiquement de la BDD pour rester synchronisées
# avec les données après chaque mise à jour (cf. pipeline utils/)
plage_annees <- tbl(db_pool, "temperatures_max") %>%
  summarise(
    min = min(annee, na.rm = TRUE),
    max = max(annee, na.rm = TRUE)
  ) %>%
  collect()

an_min_data <- plage_annees$min
an_max_data <- plage_annees$max