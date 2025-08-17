# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)

# --- CHARGEMENT DES DONNÉES ---

# Charger les statistiques pré-calculées
stats_normales <- readRDS("data/stats_normales_precalculees.rds")

# Charger les données journalières complètes pour la comparaison
tmax_annuelles <- readRDS("data/era5_temperatures_france.rds") %>%
  rename(city = ville, tmax_celsius = temperature_max) %>%
  mutate(jour_annee = yday(date))

# ---- Server (Logique de l'application) ----
server <- function(input, output, session) {
  
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
  
  # Mise à jour dynamique des sliders/sélecteurs en fonction des données chargées
  observe({
    # Met à jour les périodes de référence disponibles dans le sélecteur
    periodes_disponibles <- unique(stats_normales$periode_ref)
    updateSelectInput(session, "periode_select", choices = periodes_disponibles)
    
    # Met à jour le slider des années
    annees_disponibles <- unique(year(tmax_annuelles$date))
    updateSliderInput(session, "annee_select", 
                      min = min(annees_disponibles), 
                      max = max(annees_disponibles),
                      value = max(annees_disponibles) -1)
    
    # --- Mise à jour de la liste des villes ---
    
    # On filtre les données pour ne garder que les lignes avec une température valide,
    # puis on extrait la liste des villes uniques.
    villes_avec_donnees_valides <- tmax_annuelles %>%
      # La fonction is.finite() exclut les NA, NaN, Inf et -Inf en une seule fois.
      filter(is.finite(tmax_celsius)) %>%
      distinct(city) %>%
      pull(city)
    
    # On trie cette liste par ordre alphabétique
    villes_triees <- sort(villes_avec_donnees_valides)
    
    # On met à jour le selectInput avec la liste
    updateSelectInput(session, "ville_select",
                      choices = villes_triees,
                      selected = villes_triees[1])
    
  })
  
  # -- Logique du Quiz --
  quiz_data <- reactiveVal(NULL)
  score_succes <- reactiveVal(0)
  score_echecs <- reactiveVal(0)
  
  observeEvent(input$new_question_btn, {
    
    # On ajoute la colonne 'jour_annee' si elle n'existe pas déjà
    tmax_pour_quiz <- tmax_annuelles %>% mutate(jour_annee = yday(date))
    
    repeat {
      random_sample <- tmax_pour_quiz %>% sample_n(1)
      random_city <- random_sample$city
      random_day_of_year <- random_sample$jour_annee
      
      normale_stats <- stats_normales %>%
        filter(city == random_city, jour_annee == random_day_of_year, periode_ref == input$periode_normale)
      
      # Condition de sortie :
      # 1. On a bien trouvé une ligne de stats
      # 2. ET les valeurs nécessaires au calcul ne sont pas NA
      if (nrow(normale_stats) > 0 && !is.na(normale_stats$t_moy) && !is.na(normale_stats$t_q1) && !is.na(normale_stats$t_q3)) {
        break
      }
    }
    
    random_date <- random_sample$date
    
    temp_range <- tmax_pour_quiz %>%
      filter(city == random_city, jour_annee == random_day_of_year) %>%
      summarise(min_t = floor(min(tmax_celsius, na.rm = TRUE)), 
                max_t = ceiling(max(tmax_celsius, na.rm = TRUE)))
    
    random_temp <- sample(temp_range$min_t:temp_range$max_t, 1)
    
    normale_stats <- stats_normales %>%
      filter(city == random_city, jour_annee == random_day_of_year, periode_ref == input$periode_normale)
    
    iqr <- normale_stats$t_q3 - normale_stats$t_q1
    upper_bound <- normale_stats$t_q3 + 1.5 * iqr
    lower_bound <- normale_stats$t_q1 - 1.5 * iqr
    
    correct_answer <- case_when(
      random_temp > upper_bound ~ "Au-dessus des normales",
      random_temp < lower_bound ~ "En-dessous des normales",
      TRUE ~ "Dans les normales de saison"
    )
    
    quiz_data(list(
      city = random_city, date = random_date, temp = random_temp,
      correct_answer = correct_answer, normale_moy = round(normale_stats$t_moy, 1)
    ))
    
    updateRadioButtons(session, "user_answer", selected = character(0))
    output$feedback_ui <- renderUI(NULL)
    
    # On réactive les boutons pour la nouvelle question
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
    
    req(quiz_data(), input$user_answer)
    data <- quiz_data()
    is_correct <- (input$user_answer == data$correct_answer)
    
    if (is_correct && !input$trash_talk_mode) {
      nouveau_score_succes <- score_succes() + 1
      score_succes(nouveau_score_succes)
    } else if (!is_correct && !input$trash_talk_mode) {
      nouveau_score_echecs <- score_echecs() + 1
      score_echecs(nouveau_score_echecs)
    }
    
    # --- Construction du message de feedback "trash talk" ou classique ---
    
    # Définition des messages
    messages_succes <- c(
      "C’est la chance du débutant, j’imagine.",
      "Tu es vraiment obligé de montrer que tu sais tout mieux que tout le monde.",
      "Je pourrais presque commencer à t’apprécier, à force.",
      "Promis, j’arrête d’être désagréable à partir de maintenant car tu l’as bien mérité."
    )
    message_succes_classique <- "Tu es trop fort !"
    
    messages_echecs <- c(
      "Tu feras mieux la prochaine fois, ne t’en fais pas. À vrai dire, tu peux difficilement faire pire.",
      "Tu ne pouvais pas mieux te tromper, félicitations !",
      "Ta détermination à échouer force l’admiration.",
      "Je pourrais être extrêmement désagréable mais je m’en voudrais de ruiner ta confiance en toi."
    )
    message_echec_classique <- "Dommage, tu feras mieux la prochaine fois !"
    
    # Sélection du message d'introduction
    intro_message <- ""
    if (!input$trash_talk_mode) { # Si le mode "trash talk" est activé
      if (is_correct) {
        if (nouveau_score_succes <= length(messages_succes)) {
          intro_message <- messages_succes[nouveau_score_succes]
        } else {
          intro_message <- message_succes_classique
        }
      } else { # Si c'est un échec
        if (nouveau_score_echecs <= length(messages_echecs)) {
          intro_message <- messages_echecs[nouveau_score_echecs]
        } else {
          intro_message <- message_echec_classique
        }
      }
    } else { # Si le mode est désactivé
      intro_message <- if (is_correct) message_succes_classique else message_echec_classique
    }
    
    # Construction du corps du feedback (l'explication factuelle reste la même)
    
    # --- Préparation des informations communes ---
    diff <- round(abs(data$temp - data$normale_moy), 1)
    direction <- if (data$temp > data$normale_moy) "supérieure" else "inférieure"
    direction_bis <- if (data$temp > data$normale_moy) "élevée" else "basse"
    explication_principale <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) pour la période ", input$periode_normale, ".")
    
    if (data$correct_answer == "Dans les normales de saison") {
      explication_text <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) et est considérée comme normale pour un ", format(data$date, "%d %B"), " à ", data$city, ".")
    } else {
      annees_periode <- as.numeric(unlist(strsplit(input$periode_normale, "-")))
      annee_debut <- annees_periode[1]
      annee_fin <- annees_periode[2]
      nombre_annees_periode <- annee_fin - annee_debut + 1
      
      # --- Analyse sur le jour précis ---
      donnees_historiques_jour <- tmax_annuelles %>%
        filter(city == data$city, jour_annee == yday(data$date), year(date) >= annee_debut, year(date) <= annee_fin)
      
      nombre_occurrences_jour <- if (direction == "supérieure") {
        sum(donnees_historiques_jour$tmax_celsius >= data$temp, na.rm = TRUE)
      } else {
        sum(donnees_historiques_jour$tmax_celsius <= data$temp, na.rm = TRUE)
      }
      
      frequence_jour_text <- "" # Initialisation
      if (nombre_occurrences_jour == 0) {
        frequence_jour_text <- "Pour ce jour précis, un événement de cette intensité ne s'est <b>jamais produit</b> durant la période de référence."
      } else {
        frequence_jour_text <- paste0("Pour ce jour précis, cela s'est produit <b>", nombre_occurrences_jour, " fois</b> en ", nombre_annees_periode, " ans.")
      }
      
      # --- Analyse sur la saison complète ---
      saison <- get_season_info(data$date)
      
      donnees_historiques_saison <- tmax_annuelles %>%
        filter(city == data$city, month(date) %in% saison$mois, year(date) >= annee_debut, year(date) <= annee_fin)
      
      nombre_occurrences_saison <- if (direction == "supérieure") {
        sum(donnees_historiques_saison$tmax_celsius >= data$temp, na.rm = TRUE)
      } else {
        sum(donnees_historiques_saison$tmax_celsius <= data$temp, na.rm = TRUE)
      }
      
      if (nombre_occurrences_saison == 0) {
        frequence_saison_text <- paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ", direction_bis, " ne s'est <b>jamais produit</b> durant la période de référence.")
      } else {
        frequence_saison_text <- paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ", direction_bis, " est arrivée <b>", nombre_occurrences_saison, " fois</b> au total sur cette période.")
      }
        
      # --- 3. Assemblage final de toutes les explications ---
      explication_text <- paste(explication_principale, frequence_jour_text, frequence_saison_text, sep = "<br><br>")
      
    }
    
    # On assemble le tout
    feedback_body <- paste0("<b>", intro_message, "</b><br><br>", explication_text)
    
    # Affichage final
    output$feedback_ui <- renderUI({ HTML(feedback_body) })
    
    # On désactive les boutons pour empêcher une nouvelle soumission
    shinyjs::disable("user_answer")
    shinyjs::disable("submit_answer_btn")
    
  })
  
  # -- Logique de l'Explorateur --
  output$climate_plot <- renderPlotly({
    
    req(input$ville_select, input$periode_select, input$annee_select)
    
    annee_origine <- paste0(input$annee_select, "-01-01")
    
    plot_data_normale <- stats_normales %>%
      filter(city == input$ville_select, periode_ref == input$periode_select) %>%
      mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
    
    plot_data_annee <- tmax_annuelles %>%
      filter(city == input$ville_select, year(date) == input$annee_select) %>%
      mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
    
    p <- ggplot() +
      geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_min, ymax = t_max), fill = "lightblue", alpha = 0.5) +
      geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_q1, ymax = t_q3), fill = "skyblue", alpha = 0.6) +
      geom_line(data = plot_data_normale, aes(x = date, y = t_moy, color = "Moyenne normale"), linetype = "dashed") +
      geom_line(data = plot_data_annee, aes(x = date, y = tmax_celsius, color = "Année sélectionnée"), size = 1) +
      scale_color_manual(values = c("Moyenne normale" = "darkblue", "Année sélectionnée" = "red")) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(
        title = paste("Températures maximales journalières à", input$ville_select, "en", input$annee_select),
        subtitle = paste("Comparaison avec la normale climatique", input$periode_select),
        y = "Température maximale (°C)", x = "Jour de l'année", color = "Légende"
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y"))
    
  })
}