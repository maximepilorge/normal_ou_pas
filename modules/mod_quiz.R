# modules/mod_quiz.R

mod_quiz_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Testez votre intuition climatique"),
    sidebarLayout(
      sidebarPanel(
        # Note : ce selectInput est global et mis à jour dans le serveur principal
        selectInput("periode_normale", "Période de référence climatique", choices = NULL),
        h4("Nouvelle Question"),
        actionButton(ns("new_question_btn"), "Tirer une température au hasard !", icon = icon("dice")),
        checkboxInput(ns("trash_talk_mode"), "Me forcer à vous répondre poliment", value = FALSE),
        hr(),
        h4("Votre Réponse"),
        radioButtons(ns("user_answer"), "Cette température est :",
                     choices = c("En-dessous des normales", "Dans les normales de saison", "Au-dessus des normales"),
                     selected = character(0)),
        actionButton(ns("submit_answer_btn"), "Valider", icon = icon("check")),
        width = 3
      ),
      mainPanel(
        h3(textOutput(ns("question_text"))),
        hr(),
        uiOutput(ns("feedback_ui")),
        width = 9
      )
    )
  )
}

mod_quiz_server <- function(id, periode_globale, data_stats, data_tmax, get_season_info_func) {
  moduleServer(id, function(input, output, session) {
    
    quiz_data <- reactiveVal(NULL)
    score_succes <- reactiveVal(0)
    score_echecs <- reactiveVal(0)
    
    # NOUVEAU : On utilise un reactiveVal pour stocker les données classées et les probabilités
    # Cela évite de les recalculer à chaque nouvelle question si la période de référence ne change pas.
    donnees_et_probabilites <- reactive({
      req(periode_globale())
      
      # 1. Préparation des seuils de normalité pour la période choisie
      normales_periode_selectionnee <- data_stats %>%
        filter(periode_ref == periode_globale()) %>%
        mutate(
          iqr = t_q3 - t_q1,
          seuil_haut = t_q3 + 1.5 * iqr,
          seuil_bas = t_q1 - 1.5 * iqr
        ) %>%
        select(city, jour_annee, seuil_bas, seuil_haut, normale_moy = t_moy)
      
      # 2. Classification de TOUT l'historique
      donnees_classees <- data_tmax %>%
        left_join(normales_periode_selectionnee, by = c("city", "jour_annee")) %>%
        filter(!is.na(seuil_haut)) %>%
        mutate(
          categorie = case_when(
            tmax_celsius > seuil_haut ~ "Au-dessus des normales",
            tmax_celsius < seuil_bas ~ "En-dessous des normales",
            TRUE ~ "Dans les normales de saison"
          )
        )
      
      # 3. Calcul des probabilités basées sur la répartition réelle des données
      probabilites_reelles <- donnees_classees %>%
        count(categorie) %>%
        mutate(prob = n / sum(n))
      
      # On retourne une liste contenant les données classées et le tableau des probabilités
      list(
        donnees = donnees_classees,
        probabilites = probabilites_reelles
      )
    })
    
    observeEvent(input$new_question_btn, {
      
      # On récupère les données et probabilités calculées
      calc <- donnees_et_probabilites()
      donnees_classees <- calc$donnees
      probabilites_df <- calc$probabilites
      
      # On s'assure que l'ordre des catégories est le même pour le tirage au sort
      categories_possibles <- c("Au-dessus des normales", "En-dessous des normales", "Dans les normales de saison")
      
      # On réorganise le dataframe de probabilités pour correspondre à l'ordre de `categories_possibles`
      prob_vector <- probabilites_df %>%
        mutate(categorie = factor(categorie, levels = categories_possibles)) %>%
        arrange(categorie) %>%
        pull(prob)
      
      # 4. Tirage au sort de la catégorie en utilisant les probabilités réelles
      categorie_choisie <- sample(categories_possibles, size = 1, prob = prob_vector)
      
      # 5. Tirage d'une question au hasard DANS la catégorie choisie
      question_selectionnee <- donnees_classees %>%
        filter(categorie == categorie_choisie) %>%
        sample_n(1)
      
      quiz_data(list(
        city = question_selectionnee$city, 
        date = question_selectionnee$date, 
        temp = round(question_selectionnee$tmax_celsius, 1),
        correct_answer = question_selectionnee$categorie,
        normale_moy = round(question_selectionnee$normale_moy, 1)
      ))
      
      updateRadioButtons(session, "user_answer", selected = character(0))
      output$feedback_ui <- renderUI(NULL)
      shinyjs::enable("user_answer")
      shinyjs::enable("submit_answer_btn")
    })
    
    output$question_text <- renderText({
      req(quiz_data())
      data <- quiz_data()
      formatted_date <- format(data$date, "%d %B") 
      paste0('Le ', formatted_date, ', à ', data$city, ', la température maximale observée est de ', data$temp, '°C. Est-ce normal ?')
    })
    
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
      
      messages_succes <- c("C’est la chance du débutant, j’imagine.", "Tu es vraiment obligé de montrer que tu sais tout mieux que tout le monde.", "Je pourrais presque commencer à t’apprécier, à force.", "Promis, j’arrête d’être désagréable à partir de maintenant car tu l’as bien mérité.")
      message_succes_classique <- "Tu es trop fort !"
      messages_echecs <- c("Tu feras mieux la prochaine fois, ne t’en fais pas. À vrai dire, tu peux difficilement faire pire.", "Tu ne pouvais pas mieux te tromper, félicitations !", "Ta détermination à échouer force l’admiration.", "Je pourrais être extrêmement désagréable mais je m’en voudrais de ruiner ta confiance en toi.")
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
        explication_text <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) et est considérée comme normale pour un ", format(data$date, "%d %B"), " à ", data$city, ".")
      } else {
        annees_periode <- as.numeric(unlist(strsplit(periode_globale(), "-")))
        annee_debut <- annees_periode[1]; annee_fin <- annees_periode[2]
        
        explication_principale <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) pour la période ", periode_globale(), ".")
        
        donnees_historiques_jour <- data_tmax %>%
          filter(city == data$city, jour_annee == yday(data$date), year(date) >= annee_debut, year(date) <= annee_fin)
        
        nombre_occurrences_jour <- if (direction == "supérieure") sum(donnees_historiques_jour$tmax_celsius >= data$temp, na.rm = TRUE) else sum(donnees_historiques_jour$tmax_celsius <= data$temp, na.rm = TRUE)
        frequence_jour_text <- if (nombre_occurrences_jour == 0) paste0("Pour ce jour précis, un événement de cette intensité ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".") else paste0("Pour ce jour précis, une température égale ou ", direction, " est arrivée <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
        
        saison <- get_season_info_func(data$date)
        donnees_historiques_saison <- data_tmax %>%
          filter(city == data$city, month(date) %in% saison$mois, year(date) >= annee_debut, year(date) <= annee_fin)
        
        nombre_occurrences_saison <- if (direction == "supérieure") sum(donnees_historiques_saison$tmax_celsius >= data$temp, na.rm = TRUE) else sum(donnees_historiques_saison$tmax_celsius <= data$temp, na.rm = TRUE)
        frequence_saison_text <- if (nombre_occurrences_saison == 0) paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ", if (direction == "supérieure") "élevée" else "basse", " ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".") else paste0("À l'échelle de la saison (", saison$nom, "), une température égale ou ", direction, " est arrivée <b>", nombre_occurrences_saison, " fois</b> au total entre ", annee_debut, " et ", annee_fin, ".")
        
        explication_text <- paste(explication_principale, frequence_jour_text, frequence_saison_text, sep = "<br><br>")
      }
      
      feedback_body <- paste0("<b>", intro_message, "</b><br><br>", explication_text)
      
      # --- Création du rendu pour le boxplot ---
      output$feedback_boxplot <- renderPlot({
        
        data_quiz <- quiz_data()
        
        # Filtrer les données historiques pour le jour et la ville du quiz
        annees_periode <- as.numeric(unlist(strsplit(periode_globale(), "-")))
        annee_debut <- annees_periode[1]
        annee_fin <- annees_periode[2]
        
        donnees_historiques_jour <- data_tmax %>%
          filter(
            city == data_quiz$city, 
            jour_annee == yday(data_quiz$date), 
            year(date) >= annee_debut, 
            year(date) <= annee_fin
          )
        
        # Création du graphique
        ggplot(donnees_historiques_jour, aes(x = "", y = tmax_celsius)) +
          # Boxplot basé sur les données historiques
          geom_boxplot(width = 0.3, fill = "skyblue", alpha = 0.7) +
          # Afficher tous les points historiques avec un peu de jitter
          geom_jitter(width = 0.1, alpha = 0.4, color = "darkblue") +
          # Ajouter le point de la température du quiz en rouge
          geom_point(aes(y = data_quiz$temp), color = "red", size = 5, shape = 18) +
          scale_y_continuous(labels = ~paste(.x, "°C")) +
          labs(
            title = paste("Distribution historique pour un", format(data_quiz$date, "%d %B"), "à", data_quiz$city),
            subtitle = paste("Période", periode_globale(), "- Le point rouge est la température du quiz."),
            x = "",
            y = "Température Maximale"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold"))
      })
      
      # --- MODIFIÉ : Affichage du texte ET du graphique dans l'UI ---
      output$feedback_ui <- renderUI({
        tagList(
          HTML(feedback_body), # Le texte de feedback comme avant
          hr(),               # Une ligne de séparation
          h4("Visualisation de la distribution"),
          plotOutput(session$ns("feedback_boxplot")) # L'appel au nouveau graphique
        )
      })
      
      shinyjs::disable("user_answer")
      shinyjs::disable("submit_answer_btn")
    })
  })
}