# modules/mod_quiz.R

mod_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Testez votre intuition climatique",
      
      sidebar = sidebar(
        width = "350px",
        # Première carte pour les paramètres généraux
        card(
          card_header("Paramètres"),
          selectInput("periode_normale", "Période de référence climatique", choices = NULL),
          selectInput(ns("saison_select"), "Filtrer par saison (optionnel) :",
                      choices = c("Toutes les saisons", "Hiver", "Printemps", "Été", "Automne"),
                      selected = "Toutes les saisons")
        ),
        # Deuxième carte pour l'interaction avec le quiz
        card(
          card_header("Action"),
          actionButton(ns("new_question_btn"), "Tirer une température au hasard !", icon = icon("dice"), class = "btn-primary w-100 mb-3"),
          checkboxInput(ns("trash_talk_mode"), "Me forcer à vous répondre poliment", value = FALSE),
          hr(),
          h4("Votre Réponse"),
          radioButtons(ns("user_answer"), "Cette température est :",
                       choices = c("En-dessous des normales", "Dans les normales de saison", "Au-dessus des normales"),
                       selected = character(0)),
          actionButton(ns("submit_answer_btn"), "Valider", icon = icon("check"), class = "btn-success w-100")
        )
      ),
      
      # La carte principale pour afficher la question et les résultats
      card(
        card_header(h3(textOutput(ns("question_text")))),
        hr(),
        uiOutput(ns("feedback_ui"))
      )
    )
  )
}

mod_quiz_server <- function(id, periode_globale, db_pool) {
  moduleServer(id, function(input, output, session) {
    
    quiz_data <- reactiveVal(NULL)
    score_succes <- reactiveVal(0)
    score_echecs <- reactiveVal(0)
    
    # --- NOUVELLE QUESTION ---
    observeEvent(input$new_question_btn, {
      
      # 1. On choisit une catégorie en R (instantané)
      categories_possibles <- c("Au-dessus des normales", "En-dessous des normales", "Dans les normales de saison")
      prob_vector <- c(0.3, 0.2, 0.5)
      categorie_choisie <- sample(categories_possibles, size = 1, prob = prob_vector)
      
      # 2. On construit une requête de base SIMPLE sur la nouvelle table
      requete_base <- tbl(db_pool, "quiz_data_precalculee") %>%
        filter(
          periode_ref == !!periode_globale(),
          categorie == !!categorie_choisie
        )
      
      # On ajoute le filtre saisonnier s'il y a lieu
      if (input$saison_select != "Toutes les saisons") {
        saison_mois <- switch(input$saison_select,
                              "Hiver"     = c(12, 1, 2), "Printemps" = c(3, 4, 5),
                              "Été"       = c(6, 7, 8), "Automne"   = c(9, 10, 11))
        requete_base <- requete_base %>% filter(mois %in% !!saison_mois)
      }
      
      # 3. On tire UNE ligne au hasard. Cette requête est maintenant ultra-rapide
      question_selectionnee <- requete_base %>%
        arrange(dbplyr::sql("RANDOM()")) %>%
        head(1) %>%
        collect()
      
      req(nrow(question_selectionnee) > 0)
      
      # 4. On utilise les données
      quiz_data(list(
        city = question_selectionnee$ville, 
        date = as.Date(question_selectionnee$date, origin = "1970-01-01"), 
        temp = round(question_selectionnee$tmax_celsius, 1),
        correct_answer = question_selectionnee$categorie,
        normale_moy = round(question_selectionnee$t_moy, 1)
      ))
      
      updateRadioButtons(session, "user_answer", selected = character(0))
      output$feedback_ui <- renderUI(NULL)
      shinyjs::enable("user_answer")
      shinyjs::enable("submit_answer_btn")
    })
    
    # --- AFFICHAGE QUESTION ---
    output$question_text <- renderText({
      req(quiz_data())
      data <- quiz_data()
      formatted_date <- paste(format(data$date, "%d"), mois_fr[as.numeric(format(data$date, "%m"))])
      paste0('Le ', formatted_date, ', à ', data$city, ', la température maximale observée est de ', data$temp, '°C. Est-ce normal ?')
    })
    
    # --- ENVOI RÉPONSE ---
    observeEvent(input$submit_answer_btn, {
      
      req(quiz_data(), input$user_answer, periode_globale())
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
      
      diff <- round(abs(data$temp - data$normale_moy), 1)
      direction <- if (data$temp > data$normale_moy) "supérieure" else "inférieure"
      
      if (data$correct_answer == "Dans les normales de saison") {
        explication_text <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) et est considérée comme normale pour un ", paste(format(data$date, "%d"), mois_fr[as.numeric(format(data$date, "%m"))]), " à ", data$city, ".")
      } else {
        annees_periode <- as.numeric(unlist(strsplit(periode_globale(), "-")))
        annee_debut <- annees_periode[1]; annee_fin <- annees_periode[2]
        
        explication_principale <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) pour la période ", periode_globale(), ".")
        
        # On prépare les éléments du filtre en R
        jour_quiz_str <- sprintf("%02d", lubridate::day(data$date))
        mois_quiz_str <- sprintf("%02d", lubridate::month(data$date))
        annee_debut_str <- as.character(annee_debut)
        annee_fin_str <- as.character(annee_fin)
        
        # Ce code demande à la BDD de faire TOUT le filtrage avant le transfert
        donnees_historiques_jour <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!data$city,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") >= !!annee_debut_str,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") <= !!annee_fin_str,
            dbplyr::sql("STRFTIME('%m', date * 86400, 'unixepoch')") == !!mois_quiz_str,
            dbplyr::sql("STRFTIME('%d', date * 86400, 'unixepoch')") == !!jour_quiz_str
          ) %>%
          collect() %>% # <-- Transfert de ~30 lignes seulement !
          mutate(date = as.Date(date, origin = "1970-01-01")) %>% # On convertit la date après coup
          rename(tmax_celsius = temperature_max)

        nombre_occurrences_jour <- if (direction == "supérieure") sum(donnees_historiques_jour$tmax_celsius >= data$temp, na.rm = TRUE) else sum(donnees_historiques_jour$tmax_celsius <= data$temp, na.rm = TRUE)
        frequence_jour_text <- if (nombre_occurrences_jour == 0) paste0("Pour ce jour précis, un événement de cette intensité ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".") else paste0("Pour ce jour précis, une température égale ou ", direction, " est arrivée <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
        
        saison <- get_season_info(data$date)
        mois_saison_str <- sprintf("%02d", saison$mois)
        
        donnees_historiques_saison <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!data$city,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") >= !!annee_debut_str,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") <= !!annee_fin_str,
            dbplyr::sql("STRFTIME('%m', date * 86400, 'unixepoch')") %in% !!mois_saison_str
          ) %>%
          collect() %>%
          mutate(date = as.Date(date, origin = "1970-01-01")) %>%
          rename(tmax_celsius = temperature_max)
        
        nombre_occurrences_saison <- if (direction == "supérieure") sum(donnees_historiques_saison$tmax_celsius >= data$temp, na.rm = TRUE) else sum(donnees_historiques_saison$tmax_celsius <= data$temp, na.rm = TRUE)
        frequence_saison_text <- if (nombre_occurrences_saison == 0) paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ", if (direction == "supérieure") "élevée" else "basse", " ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".") else paste0("À l'échelle de la saison (", saison$nom, "), une température égale ou ", direction, " est arrivée en moyenne <b>", round(nombre_occurrences_saison/(annee_fin-annee_debut+1), 0), " fois</b> par an entre ", annee_debut, " et ", annee_fin, ".")
        
        explication_text <- paste(explication_principale, frequence_jour_text, frequence_saison_text, sep = "<br><br>")
      }
      
      feedback_body <- paste0("<b>", intro_message, "</b><br><br>", explication_text)
      
      # --- BOXPLOT ---
      output$feedback_boxplot <- renderPlot({
        
        data_quiz <- quiz_data()
        
        # Filtrer les données historiques pour le jour et la ville du quiz
        annees_periode <- as.numeric(unlist(strsplit(periode_globale(), "-")))
        annee_debut <- annees_periode[1]
        annee_fin <- annees_periode[2]
        jour_quiz_str <- sprintf("%02d", lubridate::day(data_quiz$date))
        mois_quiz_str <- sprintf("%02d", lubridate::month(data_quiz$date))
        annee_debut_str <- as.character(annee_debut)
        annee_fin_str <- as.character(annee_fin)
        
        donnees_historiques_jour_plot <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!data_quiz$city,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") >= !!annee_debut_str,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") <= !!annee_fin_str,
            dbplyr::sql("STRFTIME('%m', date * 86400, 'unixepoch')") == !!mois_quiz_str,
            dbplyr::sql("STRFTIME('%d', date * 86400, 'unixepoch')") == !!jour_quiz_str
          ) %>%
          collect() %>% # <-- Transfert de ~30 lignes seulement !
          rename(tmax_celsius = temperature_max) %>%
          mutate(date = as.Date(date, origin = "1970-01-01")) # On convertit la date après coup
        
        # Création du graphique
        ggplot(donnees_historiques_jour_plot, aes(x = "", y = tmax_celsius)) +
          # Boxplot basé sur les données historiques
          geom_boxplot(width = 0.3, fill = "skyblue", alpha = 0.7) +
          # Afficher tous les points historiques avec un peu de jitter
          geom_jitter(width = 0.1, alpha = 0.4, color = "darkblue") +
          # Ajouter le point de la température du quiz en rouge
          annotate("point", x = "", y = data_quiz$temp, color = "red", size = 5, shape = 18) +
          scale_y_continuous(labels = ~paste(.x, "°C")) +
          labs(
            title = paste("Distribution historique pour un", paste(format(data_quiz$date, "%d"), mois_fr[as.numeric(format(data_quiz$date, "%m"))]), "à", data_quiz$city),
            subtitle = paste("Période", periode_globale(), "- Le point rouge est la température du quiz."),
            x = "",
            y = "Température Maximale"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"))
      })
      
      # --- Affichage du texte et du graphique dans l'UI ---
      output$feedback_ui <- renderUI({
        tagList(
          HTML(feedback_body),
          hr(),               
          h4("Visualisation de la distribution"),
          plotOutput(session$ns("feedback_boxplot"))
        )
      })
      
      shinyjs::disable("user_answer")
      shinyjs::disable("submit_answer_btn")
      
    })
  })
}