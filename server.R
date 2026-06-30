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
source("modules/mod_analyse.R")
# Onglet « Comparer » : fusion des anciens Comparaison + Carte (mod_comparer).
source("modules/mod_comparer.R")
# Onglet « Une journée » : analyse d'un jour précis (rang, fréquence, partage).
source("modules/mod_jour.R")

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
  quiz_scores <- mod_quiz_server("quiz_1", db_pool = db_pool,
                                 visitor_id = visitor_id_reactive)

  # Cet observe s'exécutera chaque fois que les scores changent
  observe({
    session_final_data$successes <- quiz_scores$successes()
    session_final_data$failures <- quiz_scores$failures()
  })

  # Persistance d'un score par SÉRIE terminée (table runtime quiz_series_scores),
  # distincte de l'écriture analytics_visits de fin de session. Dégrade proprement
  # si la table n'existe pas encore (quiz_scores_disponibles), si l'ID visiteur
  # n'est pas encore connu, ou si la BDD refuse (tryCatch).
  observeEvent(quiz_scores$serie_terminee(), {
    ev <- quiz_scores$serie_terminee()
    req(ev)
    if (!isTRUE(quiz_scores_disponibles)) return()
    vid <- session_final_data$visitor_id
    if (is.null(vid)) return()
    dtype <- session_final_data$device_type
    if (is.null(dtype)) dtype <- NA_character_
    duree <- ev$duree_seconds
    if (is.null(duree) || is.na(duree)) duree <- NA_integer_
    ligne <- data.frame(
      visitor_id    = vid,
      played_ts     = ev$stamp,
      score         = ev$score,
      nb_questions  = ev$nb_questions,
      periode_ref   = ev$periode_ref,
      ville_filtre  = ev$ville_filtre,
      saison_filtre = ev$saison_filtre,
      device_type   = dtype,
      duree_seconds = duree,
      stringsAsFactors = FALSE
    )
    tryCatch(
      pool::dbAppendTable(db_pool, "quiz_series_scores", ligne),
      error = function(e) cat("Avis : écriture quiz_series_scores échouée :",
                              conditionMessage(e), "\n"))
  }, ignoreInit = TRUE)
  
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
  # Module Comparer (fusion Comparaison + Carte) : sidebar et contrôles internes.
  mod_comparer_server("comparer_1", db_pool = db_pool)

  # Module « Une journée » : analyse d'un jour précis + partage.
  mod_jour_server("jour_1", db_pool = db_pool)

  # Module Analyse
  mod_analyse_server("analyse_1",
                     db_pool = db_pool)

}