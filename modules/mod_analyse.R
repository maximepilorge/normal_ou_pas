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
                      choices = NULL,
                      options = list('live-search' = TRUE)),
          sliderInput(ns("annee_range_analyse"), "Période d'analyse :", 
                      min = an_min_data, max = an_max_data, 
                      value = c(1950, an_max_data), sep = ""),
          # --- AJOUT : Case à cocher pour le lissage ---
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
                                  "Hiver (Déc, Jan, Fév)" = "hiver", 
                                  "Printemps (Mar, Avr, Mai)" = "printemps", 
                                  "Été (Juin, Juil, Aoû)" = "ete", 
                                  "Automne (Sep, Oct, Nov)" = "automne"),
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
      
      uiOutput(ns("resultats_analyse_ui"))
    )
  )
}


# --- SERVER MIS À JOUR ---
# Remplacez l'intégralité de l'ancienne fonction mod_analyse_server par celle-ci

mod_analyse_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observe({
      updatePickerInput(session, "ville_analyse", choices = villes_triees, selected = "Paris")
    })
    
    analyse_trigger <- reactiveVal(NULL) 
    
    donnees_long_terme <- reactive({
      req(input$ville_analyse, input$annee_range_analyse)
      
      annee_debut <- input$annee_range_analyse[1]
      annee_fin <- input$annee_range_analyse[2]
      
      tbl(db_pool, "temperatures_max") %>%
        filter(
          ville == !!input$ville_analyse,
          dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") >= !!annee_debut,
          dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") <= !!annee_fin
        ) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max) %>%
        mutate(
          date = as.Date(date, origin = "1970-01-01"),
          annee = year(date),
          mois = month(date)
        ) %>%
        arrange(date) %>%
        mutate(
          tmax_lisse_365j = zoo::rollmean(tmax_celsius, k = 365, fill = NA, align = "center")
        )
      
    }) %>% bindCache(input$ville_analyse, input$annee_range_analyse)
    
    
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
          geom_smooth(aes(y = tmax_lisse_365j, color = "Tendance"), method = "loess", span = 0.2, se = FALSE, linewidth = 1.2, na.rm = TRUE) +
          scale_color_manual(values = c("Tendance" = "#E41A1C")) +
          labs(
            title = paste("Températures maximales à", input$ville_analyse),
            y = "Température (°C)", x = "Année", color = ""
          )
        
      } else {
        # GRAPHIQUE POUR LA VUE BRUTE
        p <- ggplot(df, aes(x = date, y = tmax_celsius)) +
          geom_line(color = "grey60", linewidth = 0.3) +
          geom_point(aes(text = paste("Date:", format(date, "%d %b %Y"), "<br>Température:", round(tmax_celsius, 1), "°C")),
                     alpha = 0) +
          geom_smooth(aes(color = "Tendance"), method = "loess", span = 0.2, se = FALSE, linewidth = 1.2, na.rm = TRUE) +
          scale_color_manual(values = c("Tendance" = "#E41A1C")) +
          labs(
            title = paste("Températures maximales à", input$ville_analyse),
            y = "Température (°C)", x = "Année", color = ""
          )
      }
      
      # On ajoute les éléments de thème communs aux deux graphiques
      p <- p +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      # --- FIN DE LA MODIFICATION ---
      
      if (!is.null(analyse_trigger()) && !input$lissage_toggle) {
        seuil_val <- analyse_trigger()$seuil
        p <- p + geom_hline(yintercept = seuil_val, linetype = "dotdash", color = "#343a40", linewidth = 0.8)
      }
      
      ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = FALSE)
    })
    
    observeEvent(input$lancer_analyse_btn, {
      req(donnees_long_terme(), input$temp_seuil_analyse)
      
      df_analyse <- donnees_long_terme()
      annee_debut <- min(df_analyse$annee)
      annee_fin <- max(df_analyse$annee)
      
      df_pour_stats <- df_analyse
      lookup_saison <- c("all" = "Toutes les saisons", "hiver" = "en hiver", "printemps" = "au printemps", "ete" = "en été", "automne" = "en automne")
      saison_choisie_nom <- lookup_saison[input$saison_analyse]
      
      if (input$saison_analyse != "all") {
        mois_saison <- switch(input$saison_analyse, "hiver" = c(12, 1, 2), "printemps" = c(3, 4, 5), "ete" = c(6, 7, 8), "automne" = c(9, 10, 11))
        df_pour_stats <- df_analyse %>% filter(mois %in% mois_saison)
      }
      
      stats_30ans <- NULL
      if ((annee_fin - annee_debut) >= 29) {
        moyenne_debut_periode <- df_pour_stats %>%
          filter(annee >= annee_debut, annee <= annee_debut + 29) %>%
          summarise(moyenne = mean(tmax_celsius, na.rm = TRUE)) %>%
          pull(moyenne)
        
        moyenne_fin_periode <- df_pour_stats %>%
          filter(annee >= annee_fin - 29, annee <= annee_fin) %>%
          summarise(moyenne = mean(tmax_celsius, na.rm = TRUE)) %>%
          pull(moyenne)
        
        stats_30ans <- list(
          periode1_str = paste0(annee_debut, "-", annee_debut + 29),
          moyenne1 = moyenne_debut_periode,
          periode2_str = paste0(annee_fin - 29, "-", annee_fin),
          moyenne2 = moyenne_fin_periode)
      }
      
      donnees_filtrees <- df_pour_stats %>% filter(tmax_celsius >= input$temp_seuil_analyse)
      
      analyse_trigger(list(
        data = donnees_filtrees, seuil = input$temp_seuil_analyse,
        ville = input$ville_analyse, annee_range = input$annee_range_analyse,
        saison_nom = saison_choisie_nom, stats_30ans = stats_30ans
      ))
    })
    
    observeEvent(input$reset_analyse_btn, {
      analyse_trigger(NULL)
    })
    
    output$resultats_analyse_ui <- renderUI({
      req(analyse_trigger())
      resultats <- analyse_trigger()
      
      saison_texte <- if (resultats$saison_nom != "Toutes les saisons") paste0(" ", resultats$saison_nom) else ""
      texte_synthese_seuil <- p(HTML(paste0(
        "Pour le seuil de <b>", resultats$seuil, "°C</b> : Il y a eu <b>", nrow(resultats$data), " jours</b>", saison_texte, 
        " où cette température a été atteinte ou dépassée."
      )))
      
      texte_stats_ui <- NULL
      if (!is.null(resultats$stats_30ans)) {
        stats <- resultats$stats_30ans
        diff <- round(stats$moyenne2 - stats$moyenne1, 2)
        
        prefixe_saison <- ""
        if (resultats$saison_nom != "Toutes les saisons") {
          saison_jolie <- paste0(toupper(substring(resultats$saison_nom, 1, 1)), substring(resultats$saison_nom, 2))
          prefixe_saison <- paste0(saison_jolie, ", ")
        }
        
        texte_stats_ui <- tagList(
          p(HTML(paste0(
            "<b>Analyse du réchauffement :</b> ",
            prefixe_saison,
            "la température maximale moyenne sur la période <b>", stats$periode1_str, "</b> était de <b>", round(stats$moyenne1, 2), "°C</b>. ",
            "Pour la période <b>", stats$periode2_str, "</b>, elle a atteint <b>", round(stats$moyenne2, 2), "°C</b>, ",
            "soit une augmentation de <b>", diff, "°C</b>.",
            "<br><br><i>Note : cette analyse compare la moyenne des températures maximales de chaque jour et non les températures moyennes qui sont plus couramment utilisées pour ce genre d'indicateurs.</i>"
          )))
        )
      }
      
      card(
        card_header(paste0("Résultats pour ", resultats$ville, " (", resultats$annee_range[1], "-", resultats$annee_range[2], ")")),
        texte_stats_ui,
        hr(),
        texte_synthese_seuil,
        layout_columns(
          col_widths = c(12, 12, 6, 6),
          card(card_header("Top 5 des événements les plus chauds"), if(nrow(resultats$data) > 0) tableOutput(ns("top_events_table")) else p("Aucun événement n'a dépassé ce seuil.")),
          card(card_header("Nombre de jours au-dessus du seuil par décennie"), plotOutput(ns("decennie_plot"), height = "300px"))
        )
      )
    })
    
    output$top_events_table <- renderTable({
      req(analyse_trigger())
      analyse_trigger()$data %>% arrange(desc(tmax_celsius)) %>% head(5) %>%
        select(Date = date, `Température (°C)` = tmax_celsius) %>%
        mutate(Date = format(Date, "%d/%m/%Y"), `Température (°C)` = round(`Température (°C)`, 1))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%")
    
    output$decennie_plot <- renderPlot({
      req(analyse_trigger())
      resultats_figes <- analyse_trigger()
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