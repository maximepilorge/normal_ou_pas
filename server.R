# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(shinyjs)
library(dbplyr)

# On charge la logique serveur de chaque module
source("modules/mod_quiz.R")
source("modules/mod_visualisation.R")
source("modules/mod_analyse.R")

server <- function(input, output, session) {
  
  # Cette fonction s'exécutera automatiquement quand un utilisateur fermera son navigateur
  session$onSessionEnded(function() {
    gc()
    print("Session terminée et mémoire nettoyée.")
  })
  
  # --- APPELS AUX SERVEURS DES MODULES ---
  
  # Module Quiz
  mod_quiz_server("quiz_1", 
                  db_pool = db_pool)
  
  # Module Visualisation
  mod_visualisation_server("visu_1", 
                           db_pool = db_pool,
                           ville = reactive(input$ville_select), 
                           periode = reactive(input$periode_select),
                           annee = reactive(input$annee_select))
  
  # Module Analyse
  mod_analyse_server("analyse_1", 
                     db_pool = db_pool)
  
}