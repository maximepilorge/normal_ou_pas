# modules/mod_quiz.R

mod_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Testez votre intuition climatique",
      
      sidebar = sidebar(
        width = "350px",
        # Carte pour les paramètres généraux
        card(
          card_header("Paramètres"),
          pickerInput(ns("periode_normale"), 
                      "Période de référence climatique", 
                      choices = periodes_disponibles, 
                      options = list('live-search' = FALSE)),
          pickerInput(ns("ville_select_quiz"),
                      "Filtrer par ville (optionnel) :",
                      choices = c("Toutes les villes", villes_triees),
                      selected = "Toutes les villes",
                      options = list('live-search' = FALSE)),
          pickerInput(ns("saison_select"), 
                           "Filtrer par saison (optionnel) :",
                           choices = c("Toutes les saisons", "Hiver", "Printemps", "Été", "Automne"),
                           selected = "Toutes les saisons",
                           options = list('live-search' = FALSE))
        ),
        # Le bouton de nouvelle question reste dans la barre latérale pour ne pas encombrer le quiz
        actionButton(ns("new_question_btn"), "Tirer une température au hasard !", icon = icon("dice"), class = "btn-primary w-100 mb-3"),
        checkboxInput(ns("trash_talk_mode"), "Me forcer à vous répondre poliment", value = FALSE)
      ),
      
      # Contenu principal de la page
      card(
        card_header(h3(textOutput(ns("question_text")))),
        #hr(),
        # Déplacer les réponses et le bouton de validation dans le corps principal
        h4("Votre Réponse"),
        radioButtons(ns("user_answer"), "Cette température est :",
                     choices = c("En-dessous des normales", "Dans les normales de saison", "Au-dessus des normales"),
                     selected = character(0)),
        actionButton(ns("submit_answer_btn"), "Valider", icon = icon("check"), class = "btn-success w-100"),
        hr(),
        uiOutput(ns("feedback_ui"))
      )
    )
  )
}

mod_quiz_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    quiz_data <- reactiveVal(NULL)
    boxplot_data <- reactiveVal(NULL)
    score_succes <- reactiveVal(0)
    score_echecs <- reactiveVal(0)
    
    observeEvent(req(input$periode_normale), {
      shinyjs::click("new_question_btn")
    }, once = TRUE)
    
    # --- NOUVELLE QUESTION ---
    observeEvent(input$new_question_btn, {
      
      req(input$periode_normale)
      
      message("Génération d'une nouvelle question !")
      
      shinyjs::disable("new_question_btn")
      boxplot_data(NULL)
      
      # Boucle de robustesse : on essaie jusqu'à 10 fois de trouver un combo valide
      # pour éviter une boucle infinie si les filtres sont trop stricts.
      question_valide <- NULL
      
      # On prépare la liste des jours possibles en amont de la boucle
      # 1. On crée une table de tous les jours d'une année bissextile
      tous_les_jours <- tibble(date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")) %>%
        mutate(mois = month(date))
      
      # 2. On filtre cette table si une saison est sélectionnée
      jours_possibles <- if (input$saison_select != "Toutes les saisons") {
        saison_mois <- switch(input$saison_select,
                              "Hiver"   = c(12, 1, 2), "Printemps" = c(3, 4, 5),
                              "Été"     = c(6, 7, 8), "Automne"   = c(9, 10, 11))
        tous_les_jours %>% filter(mois %in% saison_mois)
      } else {
        tous_les_jours
      }
      
      for (i in 1:10) {
        
        # --- ÉTAPE 1 : Tirer une réponse, un jour et une ville ---
        categorie_choisie <- sample(
          c("Au-dessus des normales", "En-dessous des normales", "Dans les normales de saison"),
          size = 1,
          prob = c(0.3, 0.2, 0.5)
        )
        
        # On tire un jour et un mois au hasard
        date_aleatoire <- jours_possibles %>% sample_n(1) %>% pull(date)
        jour_aleatoire <- day(date_aleatoire)
        mois_aleatoire <- month(date_aleatoire)
        
        ville_choisie <- if (input$ville_select_quiz == "Toutes les villes") {
          sample(villes_triees, 1)
        } else {
          input$ville_select_quiz
        }
        
        # --- ÉTAPE 2 : Filtrer les données ---
        requete_base <- tbl(db_pool, "quiz_data_precalculee") %>%
          filter(
            periode_ref == !!input$periode_normale,
            categorie == !!categorie_choisie,
            ville == !!ville_choisie,
            mois == !!mois_aleatoire,
            jour_mois == !!jour_aleatoire
          )
        
        # --- ÉTAPE 3 : Récupérer min, max et moyenne en une seule requête ---
        bornes_et_moyenne <- requete_base %>%
          summarise(
            min_temp = min(tmax_celsius, na.rm = TRUE),
            max_temp = max(tmax_celsius, na.rm = TRUE),
            normale_moy = min(t_moy, na.rm = TRUE)
          ) %>%
          collect()
        
        # On vérifie si on a un résultat valide
        if (nrow(bornes_et_moyenne) > 0 && is.finite(bornes_et_moyenne$min_temp)) {
          question_valide <- list(
            bornes = bornes_et_moyenne,
            categorie = categorie_choisie,
            ville = ville_choisie,
            date = date_aleatoire
          )
          break # On a trouvé, on sort de la boucle
        }
      }
      
      req(question_valide, cancelOutput = TRUE)
      
      # --- ÉTAPE 4 : Tirer une température au hasard entre les bornes ---
      min_val <- round(question_valide$bornes$min_temp, 1)
      max_val <- round(question_valide$bornes$max_temp, 1)
      
      temp_selectionnee <- if (min_val == max_val) {
        min_val
      } else {
        # On crée une séquence de valeurs possibles avec un pas de 0.1
        valeurs_possibles <- seq(min_val, max_val, by = 0.1)
        sample(valeurs_possibles, 1)
      }

      # --- ÉTAPE 5 : Alimenter quiz_data avec ces informations ---
      quiz_data(list(
        city = question_valide$ville,
        date = question_valide$date,
        temp = temp_selectionnee,
        correct_answer = question_valide$categorie,
        normale_moy = round(question_valide$bornes$normale_moy, 1)
      ))
      
      message(paste0("Ville : ", quiz_data()$city))
      message(paste0("Date (jour/mois) : ", format(quiz_data()$date, "%d/%m")))
      message(paste0("Température générée : ", quiz_data()$temp))
      
      # On met à jour l'UI
      updateRadioButtons(session, "user_answer", selected = character(0))
      output$feedback_ui <- renderUI(NULL)
      shinyjs::enable("user_answer")
      shinyjs::enable("submit_answer_btn")
      shinyjs::enable("new_question_btn")
      js$enablePicker(ns("periode_normale"))
      js$enablePicker(ns("ville_select_quiz"))
      js$enablePicker(ns("saison_select"))
    })
    
    # --- AFFICHAGE QUESTION ---
    output$question_text <- renderText({
      req(quiz_data())
      data <- quiz_data()
      formatted_date <- paste(format(data$date, "%d"), mois_fr[as.numeric(format(data$date, "%m"))])
      paste0('Le ', formatted_date, ', à ', data$city, ', la température maximale observée est de ', data$temp, '°C. Normal ou pas ?')
    })
    
    # --- ENVOI RÉPONSE ---
    observeEvent(input$submit_answer_btn, {
      
      req(quiz_data(), input$user_answer, input$periode_normale)
      
      shinyjs::disable("submit_answer_btn")
      js$disablePicker(ns("periode_normale"))
      js$disablePicker(ns("ville_select_quiz"))
      js$disablePicker(ns("saison_select"))
      
      data <- quiz_data()
      is_correct <- (input$user_answer == data$correct_answer)

      if (is_correct && !input$trash_talk_mode) {
        nouveau_score_succes <- score_succes() + 1
        score_succes(nouveau_score_succes)
      } else if (!is_correct && !input$trash_talk_mode) {
        nouveau_score_echecs <- score_echecs() + 1
        score_echecs(nouveau_score_echecs)
      }
      
      messages_succes <- c("C’est la chance du débutant, j’imagine.", 
                           "Tu es vraiment obligé de montrer que tu sais tout mieux que tout le monde.", 
                           "Je pourrais presque commencer à t’apprécier, à force.", 
                           "Promis, j’arrête d’être désagréable à partir de maintenant car tu l’as bien mérité.")
      message_succes_classique <- "Tu es trop fort !"
      messages_echecs <- c("Tu feras mieux la prochaine fois, ne t’en fais pas. À vrai dire, tu peux difficilement faire pire.", 
                           "Tu ne pouvais pas mieux te tromper, félicitations !", 
                           "Ta détermination à échouer force l’admiration.", 
                           "Je pourrais être extrêmement désagréable à ce stade mais je m’en voudrais de ruiner ta confiance en toi.")
      message_echec_classique <- "Dommage, tu feras mieux la prochaine fois !"
      
      intro_message <- ""
      if (!input$trash_talk_mode) {
        if (is_correct) {
          intro_message <- if (score_succes() <= length(messages_succes)) messages_succes[score_succes()] else message_succes_classique
        } else {
          intro_message <- if (score_echecs() <= length(messages_echecs)) messages_echecs[score_echecs()] else message_echec_classique
        }
      } else {
        intro_message <- if (is_correct) message_succes_classique else message_echec_classique
      }
      
      # On prépare les éléments du filtre
      annees_periode <- as.numeric(unlist(strsplit(input$periode_normale, "-")))
      annee_debut <- annees_periode[1]
      annee_fin <- annees_periode[2]
      annees_a_filtrer <- seq(annee_debut, annee_fin)
      jour_quiz <- lubridate::day(data$date)
      mois_quiz <- lubridate::month(data$date)
      
      donnees_historiques_jour <- tbl(db_pool, "temperatures_max") %>%
        filter(
          ville == !!data$city,
          annee %in% !!annees_a_filtrer,
          mois == !!mois_quiz,
          jour_mois == !!jour_quiz
        ) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max)
      
      # On stocke immédiatement les données pour le graphique
      boxplot_data(donnees_historiques_jour)
      moyenne_reelle <- data$normale_moy
      diff <- round(abs(data$temp - moyenne_reelle), 1)
      direction <- if (data$temp > moyenne_reelle) "supérieure" else "inférieure"
      
      if (data$correct_answer == "Dans les normales de saison") {
        explication_text <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", round(moyenne_reelle, 1), "°C) et est considérée comme normale pour un ", paste(format(data$date, "%d"), mois_fr[as.numeric(format(data$date, "%m"))]), " à ", data$city, ".")
      } else {
        
        explication_principale <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", round(moyenne_reelle, 1), "°C) pour la période ", input$periode_normale, ".")
        
        nombre_occurrences_jour <- if (direction == "supérieure") sum(donnees_historiques_jour$tmax_celsius >= data$temp, na.rm = TRUE) else sum(donnees_historiques_jour$tmax_celsius <= data$temp, na.rm = TRUE)
        frequence_jour_text <- if (nombre_occurrences_jour == 0) paste0("Pour ce jour précis, un événement de cette intensité ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".") else paste0("Pour ce jour précis, une température égale ou ", direction, " est arrivée <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
        
        saison <- get_season_info(data$date)

        donnees_historiques_saison <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!data$city,
            annee %in% !!annees_a_filtrer,
            mois %in% !!saison$mois
          ) %>%
          select(date, temperature_max) %>%
          collect() %>%
          rename(tmax_celsius = temperature_max)
        
        nombre_occurrences_saison <- if (direction == "supérieure") sum(donnees_historiques_saison$tmax_celsius >= data$temp, na.rm = TRUE) else sum(donnees_historiques_saison$tmax_celsius <= data$temp, na.rm = TRUE)
        message_occurrence_saison <- if (round(nombre_occurrences_saison/(annee_fin-annee_debut+1) >= 1)) {
           paste0(round(nombre_occurrences_saison/(annee_fin-annee_debut+1), 0))
        } else if (round(nombre_occurrences_saison/(annee_fin-annee_debut+1) > 0)) {
          "moins d'une"
        } else {
          "0"
        }
        frequence_saison_text <- if (nombre_occurrences_saison == 0) paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ", if (direction == "supérieure") "élevée" else "basse", " ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".") else paste0("À l'échelle de la saison (", saison$nom, "), une température égale ou ", direction, " est arrivée en moyenne <b>", message_occurrence_saison, " fois</b> par an entre ", annee_debut, " et ", annee_fin, ".")
        
        explication_text <- paste(explication_principale, frequence_jour_text, frequence_saison_text, sep = "<br><br>")
      }
      
      feedback_body <- paste0("<b>", intro_message, "</b><br><br>", explication_text)
      
      # --- BOXPLOT ---
      output$feedback_boxplot <- renderPlotly({
        
        req(boxplot_data())
        
        data_quiz <- quiz_data()
        donnees_historiques_jour_plot <- boxplot_data()

        main_title <- paste("Distribution historique pour un", paste(format(data_quiz$date, "%d"), mois_fr[as.numeric(format(data_quiz$date, "%m"))]), "à", data_quiz$city)
        subtitle_text <- paste("Période", input$periode_normale)
        full_title <- paste0(main_title, "<br><sup>", subtitle_text, "</sup><br>")
        
        points_specifiques <- data.frame(
          normale_moy = data_quiz$normale_moy,
          temp_quiz = data_quiz$temp
        )
        
        # Création du graphique
        p <- ggplot(donnees_historiques_jour_plot, aes(x = "", y = tmax_celsius)) +
          # Boxplot basé sur les données historiques
          geom_boxplot(width = 0.3, fill = "skyblue", alpha = 0.7) +
          # Afficher tous les points historiques avec un peu de jitter
          geom_jitter(
            aes(text = paste(
              "Date :", format(date, "%d %b %Y"),
              "<br>Température :", round(tmax_celsius, 1), "°C"
            )),
            width = 0.1, alpha = 0.4, color = "darkblue"
          ) +
          # On ajoute la moyenne avec une forme et une couleur distinctes
          geom_point(
            data = points_specifiques,
            aes(x = "", y = normale_moy, text = paste(
              "Moyenne historique :", normale_moy, "°C"
            )),
            shape = 4,
            size = 4, 
            color = "black"
          ) +
          # Ajouter le point de la température du quiz en rouge
          geom_point(
            data = points_specifiques,
            aes(x = "", y = temp_quiz, text = paste(
              "Température du quiz :", temp_quiz, "°C"
            )),
            color = "red", size = 4, shape = 4, stroke = 1.5
          ) +
          scale_y_continuous(labels = ~paste(.x, "°C")) +
          labs(
            title = main_title,
            x = "",
            y = "Température Maximale"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"))
        
        ggplotly(p, tooltip = "text") %>%
          # On verrouille les axes pour désactiver le zoom et le déplacement
          layout(
            title = list(text = full_title, x = 0.5),
            xaxis = list(fixedrange = TRUE),
            yaxis = list(fixedrange = TRUE)
          ) %>%
          config(displayModeBar = FALSE)
        
      })
      
      # --- Affichage du texte et du graphique dans l'UI ---
      output$feedback_ui <- renderUI({
        tagList(
          HTML(feedback_body),
          hr(),               
          h4("Visualisation de la distribution"),
          plotlyOutput(session$ns("feedback_boxplot"))
        )
      })
      
      shinyjs::disable("user_answer")
      shinyjs::disable("submit_answer_btn")
      
    })
    
    return(list(
      successes = score_succes,
      failures = score_echecs
    ))
    
  })
}