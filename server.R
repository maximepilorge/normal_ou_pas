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

  # --- Permaliens & navigation inter-onglets --------------------------------
  # Pré-remplissages à destination des modules (URL entrante ou lien interne).
  # Chaque demande porte un champ `stamp` : deux demandes successives identiques
  # (même ville) déclenchent quand même les observateurs des modules.
  prefill <- reactiveValues(quiz = NULL, jour = NULL, comparer = NULL,
                            analyse = NULL, quiz_defi = NULL)

  # Navigation programmée d'un module vers un onglet, avec pré-remplissage de la
  # ville cible (boucle de circulation : bilan du quiz -> Évolution, Une journée
  # -> Quiz…). Passée aux modules émetteurs.
  naviguer_vers <- function(onglet, ville = NULL) {
    if (!is.null(ville)) {
      cible <- switch(onglet, evolution = "analyse", quiz = "quiz",
                      jour = "jour", comparer = "comparer")
      if (!is.null(cible)) prefill[[cible]] <- list(ville = ville, stamp = Sys.time())
    }
    bslib::nav_select("nav_principal", onglet, session = session)
  }

  # URL entrante : applique UNE FOIS, au démarrage, l'état demandé par le
  # permalien (?onglet=...&ville=...&date=...&annee=...). Les valeurs sont
  # validées contre les données (ville connue, bornes d'années) sinon ignorées.
  observeEvent(session$clientData$url_search, {
    params <- parseQueryString(session$clientData$url_search)

    ville <- params$ville
    if (!is.null(ville) && !ville %in% villes_triees) ville <- NULL
    date_p <- if (!is.null(params$date))
      suppressWarnings(as.Date(params$date)) else NULL
    if (!is.null(date_p) && (length(date_p) != 1 || is.na(date_p))) date_p <- NULL
    annee_p <- if (!is.null(params$annee))
      suppressWarnings(as.integer(params$annee)) else NULL
    if (!is.null(annee_p) &&
        (is.na(annee_p) || annee_p < an_min_data || annee_p > an_max_data)) annee_p <- NULL

    onglet <- params$onglet
    if (is.null(onglet) || !onglet %in% ONGLETS_APP) return()
    if (onglet == "jour")
      prefill$jour <- list(ville = ville, date = date_p, stamp = Sys.time())
    if (onglet == "comparer")
      prefill$comparer <- list(ville = ville, annee = annee_p, stamp = Sys.time())
    if (onglet == "evolution")
      prefill$analyse <- list(ville = ville, stamp = Sys.time())
    bslib::nav_select("nav_principal", onglet, session = session)
  }, once = TRUE)

  # On appelle les modules
  quiz_scores <- mod_quiz_server("quiz_1", db_pool = db_pool,
                                 visitor_id = visitor_id_reactive,
                                 prefill = reactive(prefill$quiz),
                                 naviguer = naviguer_vers)

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
  comparer_mod <- mod_comparer_server("comparer_1", db_pool = db_pool,
                                      prefill = reactive(prefill$comparer))

  # Module « Une journée » : analyse d'un jour précis + partage.
  jour_mod <- mod_jour_server("jour_1", db_pool = db_pool,
                              prefill = reactive(prefill$jour),
                              naviguer = naviguer_vers)

  # Module Analyse
  analyse_mod <- mod_analyse_server("analyse_1",
                                    db_pool = db_pool,
                                    prefill = reactive(prefill$analyse))

  # Permalien continu : l'URL reflète l'onglet actif et l'état du module affiché
  # (ville/date/année) — copiable-partageable à tout moment, sans historique
  # parasite (mode replace).
  observe({
    onglet <- input$nav_principal
    req(onglet)
    etat <- switch(onglet,
                   jour = jour_mod$etat_url(),
                   comparer = comparer_mod$etat_url(),
                   evolution = analyse_mod$etat_url(),
                   NULL)
    updateQueryString(construire_query_string(onglet, etat),
                      mode = "replace", session = session)
  })

}