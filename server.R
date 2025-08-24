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
  
  # On stocke l'heure de début et l'ID du visiteur
  session_start_time <- Sys.time()

  # On crée un environnement R standard pour stocker les dernières valeurs
  session_final_data <- new.env()
  session_final_data$visitor_id <- NULL
  session_final_data$successes <- 0
  session_final_data$failures <- 0
  session_final_data$device_type <- NULL
  
  # On observe les valeurs réactives et on met à jour notre environnement simple
  visitor_id_reactive <- reactiveVal(NULL)
  observeEvent(input$visitor_id, {
    req(input$visitor_id)
    visitor_id_reactive(input$visitor_id)
    session_final_data$visitor_id <- input$visitor_id # Mise à jour directe
  }, once = TRUE)
  
  observeEvent(input$device_type, {
    req(input$device_type)
    session_final_data$device_type <- input$device_type
  }, once = TRUE)
  
  # On appelle les modules
  quiz_scores <- mod_quiz_server("quiz_1", db_pool = db_pool)
  
  # Cet observe s'exécutera chaque fois que les scores changent
  observe({
    session_final_data$successes <- quiz_scores$successes()
    session_final_data$failures <- quiz_scores$failures()
  })
  
  # Cette fonction s'exécutera automatiquement quand un utilisateur fermera son navigateur
  session$onSessionEnded(function() {
    
    # On utilise isolate() pour obtenir la dernière valeur connue sans contexte réactif
    visitor_id_val <- session_final_data$visitor_id
    final_successes <- session_final_data$successes
    final_failures <- session_final_data$failures
    device_type_val <- session_final_data$device_type
    
    # On s'assure d'avoir bien reçu l'ID avant d'écrire en base.
    if (is.null(visitor_id_val)) {
      cat("Avis : L'ID du visiteur n'a pas été reçu avant la fin de la session. Rien n'a été enregistré.\n")
      return() # On arrête l'exécution si on n'a pas l'ID
    }
    
    session_end_time <- Sys.time()
    session_duration <- as.integer(round(difftime(session_end_time, session_start_time, units = "secs")))
    
    # On prépare la ligne de données à insérer avec les valeurs "isolées"
    visit_data <- data.frame(
      visitor_id = visitor_id_val,
      session_start_ts = session_start_time,
      session_end_ts = session_end_time,
      session_duration_seconds = session_duration,
      device_type = device_type_val,
      quiz_successes = final_successes,
      quiz_failures = final_failures
    )
    
    # On utilise le pool de connexion pour écrire dans la BDD
    pool::dbAppendTable(db_pool, "analytics_visits", visit_data)
    
    gc()
    print("Session terminée et mémoire nettoyée.")
    
  })
  
  # --- APPELS AUX SERVEURS DES MODULES (HORS QUIZ) ---
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