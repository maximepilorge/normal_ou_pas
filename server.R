# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(shinyjs)

# On charge la logique serveur de chaque module
source("modules/mod_quiz.R")
source("modules/mod_visualisation.R")
source("modules/mod_analyse.R")

server <- function(input, output, session) {
  
  # --- CHARGEMENT DES DONNÉES ---
  stats_normales <- readRDS("data/stats_normales_precalculees.rds")
  tmax_annuelles <- readRDS("data/era5_temperatures_france.rds") %>%
    rename(city = ville, tmax_celsius = temperature_max) %>%
    mutate(jour_annee = yday(date))
  
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
    moyenne_jour <- stats_normales %>%
      filter(city == ville_sel, jour_annee == yday(date_sel), periode_ref == periode_ref_str) %>%
      pull(t_moy)
    
    # Si on n'a pas de moyenne pour ce jour, on ne peut pas continuer
    if (length(moyenne_jour) == 0) return(NULL)
    
    direction <- if (temp_sel >= moyenne_jour) "supérieure ou égale" else "inférieure ou égale"
    comparaison_jour <- if (direction == "supérieure ou égale") `>=` else `<=`
    
    # --- Calcul sur le jour précis ---
    donnees_historiques_jour <- data_brutes %>%
      filter(city == ville_sel, jour_annee == yday(date_sel), year(date) >= annee_debut, year(date) <= annee_fin)
    
    nombre_occurrences_jour <- sum(comparaison_jour(donnees_historiques_jour$tmax_celsius, temp_sel), na.rm = TRUE)
    
    texte_jour <- paste0("Pour ce jour précis (le ", format(date_sel, "%d %B"), "), une température ", direction, " ou égale à ", temp_sel, "°C s'est produite <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
    
    # --- Calcul sur la saison ---
    saison <- get_season_info(date_sel)
    donnees_historiques_saison <- data_brutes %>%
      filter(city == ville_sel, month(date) %in% saison$mois, year(date) >= annee_debut, year(date) <= annee_fin)
    
    nombre_occurrences_saison <- sum(comparaison_jour(donnees_historiques_saison$tmax_celsius, temp_sel), na.rm = TRUE)
    
    texte_saison <- paste0("À l'échelle de la saison (", saison$nom, "), une température ", direction, " ou égale à ", temp_sel, "°C s'est produite <b>", nombre_occurrences_saison, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
    
    return(list(jour = texte_jour, saison = texte_saison))
  }
  
  # --- MISE A JOUR DES INPUTS GLOBAUX ---
  observe({
    periodes_disponibles <- unique(stats_normales$periode_ref)
    updateSelectInput(session, "periode_normale", choices = periodes_disponibles)
    updateSelectInput(session, "periode_select", choices = periodes_disponibles)
    updateSelectInput(session, "periode_analyse", choices = periodes_disponibles)
    
    annees_disponibles <- unique(year(tmax_annuelles$date))
    updateSliderInput(session, "annee_select", 
                      min = min(annees_disponibles), 
                      max = max(annees_disponibles),
                      value = max(annees_disponibles) -1)
    
    villes_triees <- tmax_annuelles %>%
      filter(is.finite(tmax_celsius)) %>%
      distinct(city) %>%
      pull(city) %>%
      sort()
    
    updateSelectInput(session, "ville_select", choices = villes_triees, selected = villes_triees[1])
    updateSelectInput(session, "ville_analyse", choices = villes_triees, selected = villes_triees[1])
  })
  
  # --- APPELS AUX SERVEURS DES MODULES ---
  
  # Module Quiz
  mod_quiz_server("quiz_1", 
                  periode_globale = reactive(input$periode_normale),
                  data_stats = stats_normales, 
                  data_tmax = tmax_annuelles,
                  get_season_info_func = get_season_info)
  
  # Module Visualisation
  mod_visualisation_server("visu_1", 
                           data_stats = stats_normales, 
                           data_tmax = tmax_annuelles,
                           ville = reactive(input$ville_select), 
                           periode = reactive(input$periode_select),
                           annee = reactive(input$annee_select))
  
  # Module Analyse
  mod_analyse_server("analyse_1", 
                     ville = reactive(input$ville_analyse),
                     periode = reactive(input$periode_analyse),
                     data_tmax = tmax_annuelles,
                     calculer_frequence_func = calculer_frequence)
  
}