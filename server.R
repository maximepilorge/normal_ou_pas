# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(shinyjs)

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
    
    # --- Calcul sur le JOUR PRÉCIS ---
    donnees_historiques_jour <- data_brutes %>%
      filter(city == ville_sel, jour_annee == yday(date_sel), year(date) >= annee_debut, year(date) <= annee_fin)
    
    nombre_occurrences_jour <- sum(comparaison_jour(donnees_historiques_jour$tmax_celsius, temp_sel), na.rm = TRUE)
    
    texte_jour <- paste0("Pour ce jour précis (le ", format(date_sel, "%d %B"), "), une température ", direction, " ou égale à ", temp_sel, "°C s'est produite <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
    
    # --- Calcul sur la SAISON ---
    saison <- get_season_info(date_sel)
    donnees_historiques_saison <- data_brutes %>%
      filter(city == ville_sel, month(date) %in% saison$mois, year(date) >= annee_debut, year(date) <= annee_fin)
    
    nombre_occurrences_saison <- sum(comparaison_jour(donnees_historiques_saison$tmax_celsius, temp_sel), na.rm = TRUE)
    
    texte_saison <- paste0("À l'échelle de la saison (", saison$nom, "), une température ", direction, " ou égale à ", temp_sel, "°C s'est produite <b>", nombre_occurrences_saison, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
    
    return(list(jour = texte_jour, saison = texte_saison))
  }
  
  # Mise à jour dynamique des sliders/sélecteurs en fonction des données chargées
  observe({
    # Met à jour les périodes de référence disponibles dans le sélecteur
    periodes_disponibles <- unique(stats_normales$periode_ref)
    
    updateSelectInput(session, "periode_normale", choices = periodes_disponibles)
    updateSelectInput(session, "periode_select", choices = periodes_disponibles)
    updateSelectInput(session, "periode_analyse", choices = periodes_disponibles)
    
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
    
    # On met à jour le selectInput de l'onglet d'analyse
    updateSelectInput(session, "ville_analyse",
                      choices = villes_triees,
                      selected = villes_triees[1])
    
  })
  
  # -- Logique du Quiz --
  quiz_data <- reactiveVal(NULL)
  score_succes <- reactiveVal(0)
  score_echecs <- reactiveVal(0)
  
  observeEvent(input$new_question_btn, {
    
    # 1. Préparer les données de normales pour la période de référence choisie par l'utilisateur
    normales_periode_selectionnee <- stats_normales %>%
      filter(periode_ref == input$periode_normale) %>%
      # Calculer les seuils pour la définition de "normal" (basé sur l'écart interquartile)
      mutate(
        iqr = t_q3 - t_q1,
        seuil_haut = t_q3 + 1.5 * iqr,
        seuil_bas = t_q1 - 1.5 * iqr
      ) %>%
      select(city, jour_annee, seuil_bas, seuil_haut, normale_moy = t_moy)
    
    # 2. Classifier chaque jour de l'historique par rapport à sa normale
    # On joint les données journalières avec les seuils calculés précédemment
    donnees_classees <- tmax_annuelles %>%
      left_join(normales_periode_selectionnee, by = c("city", "jour_annee")) %>%
      # On supprime les jours pour lesquels on n'a pas de normale (ex: 29 fév)
      filter(!is.na(seuil_haut)) %>%
      mutate(
        categorie = case_when(
          tmax_celsius > seuil_haut ~ "Au-dessus des normales",
          tmax_celsius < seuil_bas ~ "En-dessous des normales",
          TRUE ~ "Dans les normales de saison"
        )
      )
    
    # 3. Définir les catégories et leurs probabilités de tirage
    categories_possibles <- c("Au-dessus des normales", "En-dessous des normales", "Dans les normales de saison")
    probabilites <- c(0.33, 0.2, 0.47) # IL FAUDRAIT ADAPTER LA PROBABILITE A LA REPARTITION ENTRE LES CATEGORIES DE 1950 A AUJOURD'HUI NON ?
    
    # 4. Choisir une catégorie au hasard (mais avec pondération)
    categorie_choisie <- sample(categories_possibles, size = 1, prob = probabilites)
    
    # 5. Tirer une question au hasard DANS la catégorie choisie
    question_selectionnee <- donnees_classees %>%
      filter(categorie == categorie_choisie) %>%
      sample_n(1)
    
    # On stocke les données de la question pour les utiliser dans l'UI
    quiz_data(list(
      city = question_selectionnee$city, 
      date = question_selectionnee$date, 
      temp = round(question_selectionnee$tmax_celsius, 1), # On utilise la vraie température observée
      correct_answer = question_selectionnee$categorie, # La bonne réponse est la catégorie qu'on a tirée
      normale_moy = round(question_selectionnee$normale_moy, 1)
    ))
    
    # Réinitialiser l'interface pour la nouvelle question
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
    
    if (data$correct_answer == "Dans les normales de saison") {
      explication_text <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) et est considérée comme normale pour un ", format(data$date, "%d %B"), " à ", data$city, ".")
    } else {
      annees_periode <- as.numeric(unlist(strsplit(input$periode_normale, "-")))
      annee_debut <- annees_periode[1]
      annee_fin <- annees_periode[2]
      nombre_annees_periode <- annee_fin - annee_debut + 1
      
      explication_principale <- paste0("Cette température est <b>", diff, "°C</b> ", direction, " à la moyenne de saison (", data$normale_moy, "°C) pour la période ", input$periode_normale, ".")
      
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
        frequence_jour_text <- paste0("Pour ce jour précis, un événement de cette intensité ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".")
      } else {
        frequence_jour_text <- paste0("Pour ce jour précis, une température égale ou ", direction, " est arrivée <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
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
        frequence_saison_text <- paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ", direction_bis, " ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".")
      } else {
        frequence_saison_text <- paste0("À l'échelle de la saison (", saison$nom, "), une température égale ou ", direction, " est arrivée <b>", nombre_occurrences_saison, " fois</b> au total entre ", annee_debut, " et ", annee_fin, ".")
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
  
  # On utilise un reactiveVal pour stocker le résultat
  resultat <- reactiveVal(NULL)
  
  observeEvent(input$toute_annee_analyse, {
    toggleState("selecteurs_date", condition = !input$toute_annee_analyse)
  })
  
  # Mettre à jour le nombre de jours disponibles en fonction du mois choisi
  observeEvent(input$mois_analyse, {
    req(input$mois_analyse)
    jours_dans_le_mois <- days_in_month(as.Date(paste("2001", input$mois_analyse, "01", sep="-")))
    updateSelectInput(session, "jour_analyse", choices = 1:jours_dans_le_mois)
  })
  
  # Le calcul est déclenché par le clic sur le bouton
  observeEvent(input$calculer_frequence_btn, {
    req(input$ville_analyse, input$temp_analyse, input$periode_analyse)
    
    # Si on analyse l'année entière
    if (input$toute_annee_analyse) {
      
      annees_periode <- as.numeric(unlist(strsplit(input$periode_analyse, "-")))
      annee_debut <- annees_periode[1]; annee_fin <- annees_periode[2]
      
      donnees_historiques_annee <- tmax_annuelles %>%
        filter(city == input$ville_analyse, year(date) >= annee_debut, year(date) <= annee_fin)

      occurrences_sup <- sum(donnees_historiques_annee$tmax_celsius >= input$temp_analyse, na.rm = TRUE)

      res <- list(
        jour = paste0("Sur l'ensemble de l'année, une température supérieure ou égale à <b>", input$temp_analyse, "°C</b> est arrivée <b>", occurrences_sup, " fois</b> entre ", annee_debut, " et ", annee_fin, "."),
        saison = ""
      )
      
      # Si on analyse un jour spécifique
    } else {
      req(input$jour_analyse, input$mois_analyse)
      
      # On reconstruit une date (l'année n'a pas d'importance, on prend une non-bissextile)
      date_selectionnee <- as.Date(paste("2001", input$mois_analyse, input$jour_analyse, sep="-"), format="%Y-%m-%d")
      
      # On vérifie que la date est valide
      if (is.na(date_selectionnee)) {
        res <- list(jour = "Date invalide.", saison = "")
      } else {
        # Appel de notre fonction réutilisable
        res <- calculer_frequence(
          ville_sel = input$ville_analyse,
          date_sel = date_selectionnee,
          temp_sel = input$temp_analyse,
          periode_ref_str = input$periode_analyse,
          data_brutes = tmax_annuelles
        )
      }
    }
    resultat(res) # On stocke le résultat
  })
  
  # On affiche le résultat dans l'UI
  output$resultat_frequence_ui <- renderUI({
    res <- resultat()
    if (is.null(res)) {
      return(tags$p("Veuillez renseigner tous les champs et cliquer sur 'Calculer' pour voir un résultat."))
    }
    
    # Mise en forme du résultat en HTML
    tagList(
      h3("Résultats de l'analyse"),
      tags$p(HTML(res$jour)),
      tags$hr(),
      tags$p(HTML(res$saison))
    )
  })
  
}