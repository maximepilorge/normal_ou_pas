# modules/mod_analyse.R

# --- UI MISE À JOUR ---
mod_analyse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Analyser l'évolution des températures",
      
      sidebar = sidebar(
        width = "350px",
        card(
          card_header("Paramètres de la série temporelle"),
          pickerInput(ns("ville_analyse"),
                      "Choisissez une ville :", 
                      choices = villes_triees,
                      selected = villes_triees[1],
                      options = list('live-search' = FALSE)),
          sliderInput(ns("annee_range_analyse"), 
                      "Période d'analyse :", 
                      min = an_min_data, 
                      max = an_max_data, 
                      value = c(an_min_data, an_max_data), 
                      sep = ""),
          checkboxInput(ns("lissage_toggle"), "Lisser la courbe (moyenne mobile 365 jours)", value = TRUE)
        ),
        card(
          card_header("Analyser un seuil de température"),
          p("Définissez un seuil pour identifier les jours de chaleur et analyser leur fréquence."),
          numericInput(ns("temp_seuil_analyse"), "Température maximale à analyser (°C) :",
                       value = 30, min = -20, max = 50),
          pickerInput(ns("saison_analyse"), 
                      "Filtrer par saison (optionnel) :",
                      choices = c("Toutes les saisons" = "all", 
                                  "Hiver" = "hiver", 
                                  "Printemps" = "printemps", 
                                  "Été" = "ete", 
                                  "Automne" = "automne"),
                      selected = "all",
                      options = list('live-search' = FALSE)),
          actionButton(ns("lancer_analyse_btn"), "Analyser le seuil", icon = icon("magnifying-glass-chart"), class = "btn-primary w-100 mt-3"),
          actionButton(ns("reset_analyse_btn"), "Réinitialiser l'analyse", icon = icon("refresh"), class = "btn-light w-100 mt-2")
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Évolution des températures maximales journalières"),
        plotlyOutput(ns("evolution_plot"), height = "500px")
      ),
      
      uiOutput(ns("resultats_defaut_ui")),
      uiOutput(ns("resultats_seuil_ui")) 
    )
  )
}


# --- SERVER MIS À JOUR ---
# Remplacez l'intégralité de l'ancienne fonction mod_analyse_server par celle-ci

mod_analyse_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    resultats_defaut <- reactiveVal(NULL)
    resultats_seuil <- reactiveVal(NULL) 
    
    donnees_long_terme <- reactive({
      req(input$ville_analyse, input$annee_range_analyse)
      
      annee_debut <- input$annee_range_analyse[1]
      annee_fin <- input$annee_range_analyse[2]
      start_date <- as.Date(paste0(input$annee_range_analyse[1], "-01-01"))
      end_date <- as.Date(paste0(input$annee_range_analyse[2] + 1, "-01-01"))
      
      tbl(db_pool, "temperatures_max") %>%
        filter(
          ville == !!input$ville_analyse,
          date >= !!start_date,
          date < !!end_date
        ) %>%
        select(date, temperature_max, tmax_lisse_365j) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max) %>%
        mutate(
          annee = year(date),
          mois = month(date)
        ) %>%
        arrange(date)
      
    }) %>% bindCache(input$ville_analyse, input$annee_range_analyse)
    
    observeEvent(donnees_long_terme(), {
      
      df_analyse <- donnees_long_terme()
      req(nrow(df_analyse) > 0) # S'assurer qu'on a des données
      
      annee_debut <- min(df_analyse$annee)
      annee_fin <- max(df_analyse$annee)
      
      # 1. Calcul de l'analyse du réchauffement (identique à avant)
      stats_30ans <- NULL
      if ((annee_fin - annee_debut) >= 29) {
        moyenne_debut_periode <- df_analyse %>%
          filter(annee >= annee_debut, annee <= annee_debut + 29) %>%
          summarise(moyenne = mean(tmax_celsius, na.rm = TRUE)) %>% pull(moyenne)
        
        moyenne_fin_periode <- df_analyse %>%
          filter(annee >= annee_fin - 29, annee <= annee_fin) %>%
          summarise(moyenne = mean(tmax_celsius, na.rm = TRUE)) %>% pull(moyenne)
        
        stats_30ans <- list(
          periode1_str = paste0(annee_debut, "-", annee_debut + 29), moyenne1 = moyenne_debut_periode,
          periode2_str = paste0(annee_fin - 29, "-", annee_fin), moyenne2 = moyenne_fin_periode
        )
      }
      
      # 2. Calcul du top 5 des événements
      top_5_events <- df_analyse %>% arrange(desc(tmax_celsius)) %>% head(5)
      
      # 3. On stocke ces résultats dans notre nouveau reactiveVal
      resultats_defaut(list(
        ville = input$ville_analyse, 
        annee_range = input$annee_range_analyse,
        stats_30ans = stats_30ans, 
        top_5 = top_5_events
      ))
      
      # 4. On réinitialise l'analyse de seuil quand les données de base changent
      resultats_seuil(NULL)
      
    }, ignoreNULL = TRUE)
    
    output$evolution_plot <- renderPlotly({
      df <- donnees_long_terme()
      req(df)
      
      # --- DÉBUT DE LA MODIFICATION ---
      # On construit un graphique entièrement séparé pour chaque cas de figure
      
      if (input$lissage_toggle) {
        # GRAPHIQUE POUR LA VUE LISSÉE
        p <- ggplot(df, aes(x = date)) +
          geom_line(aes(y = tmax_lisse_365j), color = "#1f77b4", linewidth = 0.6, na.rm = TRUE) +
          geom_point(aes(y = tmax_lisse_365j, text = paste("Date:", format(date, "%d %b %Y"), "<br>Temp. lissée:", round(tmax_lisse_365j, 1), "°C")),
                     alpha = 0, na.rm = TRUE) +
          geom_smooth(aes(y = tmax_lisse_365j, color = "Tendance"), method = "loess", span = 0.2, se = FALSE, linewidth = 1.2, na.rm = TRUE, linetype = "dashed") +
          scale_color_manual(values = c("Tendance" = "#E41A1C")) +
          labs(
            title = paste("Températures maximales \nà", input$ville_analyse),
            y = "Température (°C)", x = "Année", color = ""
          )
        
      } else {
        # GRAPHIQUE POUR LA VUE BRUTE
        p <- ggplot(df, aes(x = date, y = tmax_celsius)) +
          geom_line(color = "grey60", linewidth = 0.3) +
          geom_point(aes(text = paste("Date:", format(date, "%d %b %Y"), "<br>Température:", round(tmax_celsius, 1), "°C")),
                     alpha = 0) +
          geom_smooth(aes(color = "Tendance"), method = "loess", span = 0.2, se = FALSE, linewidth = 1.2, na.rm = TRUE, linetype = "dashed") +
          scale_color_manual(values = c("Tendance" = "#E41A1C")) +
          labs(
            title = paste("Températures maximales \nà", input$ville_analyse),
            y = "Température (°C)", x = "Année", color = ""
          )
      }
      
      # On ajoute les éléments de thème communs aux deux graphiques
      p <- p +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
          )
      # --- FIN DE LA MODIFICATION ---
      
      if (!is.null(resultats_seuil()) && !input$lissage_toggle) {
        seuil_val <- resultats_seuil()$seuil
        p <- p + geom_hline(yintercept = seuil_val, linetype = "dotdash", color = "#343a40", linewidth = 0.8)
      }
      
      ggplotly(p, tooltip = "text") %>%
        # On verrouille les axes pour désactiver le zoom et le déplacement
        layout(
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
        ) %>%
        config(displayModeBar = FALSE)
    }) %>%
      bindCache(
        input$ville_analyse,
        input$annee_range_analyse,
        input$lissage_toggle,
        if (is.null(resultats_seuil())) "aucun" else resultats_seuil()$seuil
      )
    
    observeEvent(list(input$ville_analyse, input$annee_range_analyse, input$saison_analyse), {
      shinyjs::click("reset_analyse_btn")
    }, ignoreInit = TRUE)
    
    observeEvent(input$lancer_analyse_btn, {
      req(donnees_long_terme(), input$temp_seuil_analyse)
      
      df_analyse <- donnees_long_terme()
      
      # Filtrage saisonnier (identique à avant)
      df_pour_stats <- df_analyse
      lookup_saison <- c("all" = "Toutes les saisons", "hiver" = "en hiver", "printemps" = "au printemps", "ete" = "en été", "automne" = "en automne")
      saison_choisie_nom <- lookup_saison[input$saison_analyse]
      
      if (input$saison_analyse != "all") {
        mois_saison <- switch(input$saison_analyse, "hiver" = c(12, 1, 2), "printemps" = c(3, 4, 5), "ete" = c(6, 7, 8), "automne" = c(9, 10, 11))
        df_pour_stats <- df_analyse %>% filter(mois %in% mois_saison)
      }
      
      # Calcul du nombre de jours au-dessus du seuil
      donnees_filtrees <- df_pour_stats %>% filter(tmax_celsius >= input$temp_seuil_analyse)
      
      # On stocke les résultats dans le reactiveVal dédié au seuil
      resultats_seuil(list(
        data = donnees_filtrees, seuil = input$temp_seuil_analyse,
        annee_range = input$annee_range_analyse, saison_nom = saison_choisie_nom
      ))
    })
    
    observeEvent(input$reset_analyse_btn, {
      resultats_seuil(NULL)
    })
    
    # --- AJOUT ---
    # UI pour les résultats par défaut (analyse réchauffement + top 5)
    output$resultats_defaut_ui <- renderUI({
      req(resultats_defaut())
      res <- resultats_defaut()
      
      texte_stats_ui <- NULL
      if (!is.null(res$stats_30ans)) {
        stats <- res$stats_30ans
        diff <- round(stats$moyenne2 - stats$moyenne1, 2)
        texte_stats_ui <- p(HTML(paste0(
          "<b>Analyse du réchauffement :</b> ",
          "La température maximale moyenne sur la période <b>", stats$periode1_str, "</b> était de <b>", round(stats$moyenne1, 2), "°C</b>. ",
          "Pour la période <b>", stats$periode2_str, "</b>, elle a atteint <b>", round(stats$moyenne2, 2), "°C</b>, ",
          "soit une augmentation de <b>", diff, "°C</b>."
        )))
      }
      
      card(
        card_header(paste0("Analyse pour ", res$ville, " (", res$annee_range[1], "-", res$annee_range[2], ")")),
        texte_stats_ui,
        hr(),
        card(card_header("Top 5 des événements les plus chauds"), if(nrow(res$top_5) > 0) tableOutput(ns("top_events_table")) else p("Aucune donnée disponible."))
      )
    })
    
    # --- MODIFICATION ---
    # L'ancienne UI est renommée et ne génère plus que la partie "seuil"
    output$resultats_seuil_ui <- renderUI({
      req(resultats_seuil())
      res <- resultats_seuil()
      
      saison_texte <- if (res$saison_nom != "Toutes les saisons") paste0(" ", res$saison_nom) else ""
      texte_synthese_seuil <- p(HTML(paste0(
        "Il y a eu <b>", nrow(res$data), " jours</b>", saison_texte, 
        " où <b>", res$seuil, "°C</b> a été atteint ou dépassé."
      )))
      
      card(
        texte_synthese_seuil,
        card(card_header("Nombre de jours au-dessus du seuil par décennie"), plotOutput(ns("decennie_plot"), height = "300px"))
      )
    })
    
    # --- MODIFICATION ---
    # La table utilise maintenant le reactiveVal "resultats_defaut"
    output$top_events_table <- renderTable({
      req(resultats_defaut())
      resultats_defaut()$top_5 %>%
        select(Date = date, `Température (°C)` = tmax_celsius) %>%
        mutate(Date = format(Date, "%d/%m/%Y"), `Température (°C)` = sprintf("%.1f", `Température (°C)`))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%")
    
    output$decennie_plot <- renderPlot({
      req(resultats_seuil())
      resultats_figes <- resultats_seuil()
      annee_debut <- resultats_figes$annee_range[1]; annee_fin <- resultats_figes$annee_range[2]
      decennie_debut <- floor(annee_debut / 10) * 10; decennie_fin <- floor(annee_fin / 10) * 10
      toutes_les_decennies <- tibble(decennie = seq(decennie_debut, decennie_fin, by = 10))
      
      if (nrow(resultats_figes$data) > 0) {
        comptes_par_decennie <- resultats_figes$data %>% mutate(decennie = floor(annee / 10) * 10) %>% count(decennie, name = "nb_jours")
        donnees_plot <- toutes_les_decennies %>% left_join(comptes_par_decennie, by = "decennie") %>% mutate(nb_jours = ifelse(is.na(nb_jours), 0, nb_jours))
      } else {
        donnees_plot <- toutes_les_decennies %>% mutate(nb_jours = 0)
      }
      
      donnees_plot <- donnees_plot %>%
        mutate(statut = case_when(
          decennie == max(decennie) & annee_fin %% 10 != 9 ~ "Partielle",
          decennie == min(decennie) & annee_debut %% 10 != 0 ~ "Partielle",
          TRUE ~ "Complète"
        ))
      
      ggplot(donnees_plot, aes(x = as.factor(decennie), y = nb_jours, fill = statut)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = nb_jours), vjust = -0.5, size = 4.5) +
        scale_fill_manual(name = "Statut", 
                          values = c("Complète" = "steelblue", "Partielle" = "grey70")) +
        labs(x = "Décennie", y = "Nombre de jours") + 
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) 
    })
    
  })
}