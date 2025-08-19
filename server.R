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