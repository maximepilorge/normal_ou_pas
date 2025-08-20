# modules/mod_analyse.R

mod_analyse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Quelle est la rareté d'un événement météo chaud ?",
      
      sidebar = sidebar(
        width = "350px",
        card(
          card_header("Paramètres d'analyse"),
          p("Testez une température pour un jour ou pour l'année entière et découvrez sa fréquence d'occurrence."),
          pickerInput("ville_analyse", 
                      "Choisissez une ville :", 
                      choices = NULL,
                      options = list('live-search' = FALSE)),
          pickerInput("periode_analyse", 
                      "Choisissez la période de référence :", 
                      choices = periodes_disponibles,
                      options = list('live-search' = FALSE)),
          numericInput(ns("temp_analyse"), "Température maximale à tester (°C) :",
                       value = 35, min = -50, max = 50),
          hr(),
          checkboxInput(ns("toute_annee_analyse"), "Analyser sur l'année entière", value = FALSE),
          div(id = ns("selecteurs_date"),
              pickerInput(ns("mois_analyse"), 
                          "Mois :",
                          choices = setNames(1:12, mois_fr), 
                          selected = 1,
                          options = list('live-search' = FALSE)),
              pickerInput(ns("jour_analyse"), 
                          "Jour :", 
                          choices = 1:31, 
                          selected = 1,
                          options = list('live-search' = FALSE))
          ),
          actionButton(ns("calculer_frequence_btn"), "Calculer la fréquence", icon = icon("calculator"), class = "btn-primary w-100 mt-3")
        )
      ),
      
      # La carte principale pour les résultats
      card(
        uiOutput(ns("resultat_frequence_ui"))
      )
    )
  )
}

mod_analyse_server <- function(id, ville, periode, db_pool) {
  moduleServer(id, function(input, output, session) {
    
    resultat <- reactiveVal(NULL)
    
    observeEvent(input$toute_annee_analyse, {
      toggleState(id = "selecteurs_date", condition = !input$toute_annee_analyse)
    })
    
    observeEvent(input$mois_analyse, {
      req(input$mois_analyse)
      jours_dans_le_mois <- days_in_month(as.Date(paste("2001", input$mois_analyse, "01", sep="-")))
      updatePickerInput(session, "jour_analyse", choices = 1:jours_dans_le_mois)
    })
    
    observeEvent(input$calculer_frequence_btn, {
      req(ville(), input$temp_analyse, periode())
      
      annees_periode <- as.numeric(unlist(strsplit(periode(), "-")))
      annee_debut <- annees_periode[1]
      annee_fin <- annees_periode[2]
      
      res <- NULL
      
      if (input$toute_annee_analyse) {
        
        donnees_historiques <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!ville(),
            dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") >= !!annee_debut,
            dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") <= !!annee_fin
          ) %>%
          collect() %>%
          rename(tmax_celsius = temperature_max)
          
        
        occurrences_sup <- sum(donnees_historiques$tmax_celsius >= input$temp_analyse, na.rm = TRUE)
        
        res <- list(
          jour = paste0("Sur l'ensemble de l'année, une température supérieure ou égale à <b>", input$temp_analyse, "°C</b> est arrivée <b>", occurrences_sup, " fois</b> entre ", annee_debut, " et ", annee_fin, "."),
          saison = ""
        )
      } else {
        
        req(input$jour_analyse, input$mois_analyse)
        
        jour_str <- sprintf("%02d", as.numeric(input$jour_analyse))
        mois_str <- sprintf("%02d", as.numeric(input$mois_analyse))
        
        # Calcul pour le jour précis
        donnees_jour <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!ville(),
            dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") >= !!annee_debut,
            dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") <= !!annee_fin,
            dbplyr::sql("TO_CHAR(TO_TIMESTAMP(date * 86400), 'MM')") == !!mois_str,
            dbplyr::sql("TO_CHAR(TO_TIMESTAMP(date * 86400), 'DD')") == !!jour_str
          ) %>%
          collect() %>%
          rename(tmax_celsius = temperature_max)
        
        occurrences_jour <- sum(donnees_jour$tmax_celsius >= input$temp_analyse, na.rm = TRUE)
        texte_jour <- paste0("Pour le <b>", input$jour_analyse, " ", mois_fr[as.numeric(input$mois_analyse)], "</b>, une température supérieure ou égale à <b>", input$temp_analyse, "°C</b> est arrivée <b>", occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
        
        # Calcul pour la saison
        date_selectionnee <- as.Date(paste("2001", input$mois_analyse, input$jour_analyse, sep="-"))
        saison <- get_season_info(date_selectionnee)
        mois_saison_str <- sprintf("%02d", saison$mois)
        mois_clause_sql <- paste0("TO_CHAR(TO_TIMESTAMP(date * 86400), 'MM') IN (", paste0("'", mois_saison_str, "'", collapse = ", "), ")")
        
        donnees_saison <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!ville(),
            dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") >= !!annee_debut,
            dbplyr::sql("EXTRACT(YEAR FROM TO_TIMESTAMP(date * 86400))") <= !!annee_fin,
            dbplyr::sql(mois_clause_sql)
          ) %>%
          collect() %>%
          rename(tmax_celsius = temperature_max)
        
        occurrences_saison <- sum(donnees_saison$tmax_celsius >= input$temp_analyse, na.rm = TRUE)
        texte_saison <- paste0("À l'échelle de la saison (", saison$nom, "), une température supérieure ou égale à <b>", input$temp_analyse, "°C</b> est arrivée <b>", occurrences_saison, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
        
        res <- list(jour = texte_jour, saison = texte_saison)

      }
      resultat(res)
    })
    
    output$resultat_frequence_ui <- renderUI({
      res <- resultat()
      if (is.null(res)) {
        return(p("Veuillez renseigner tous les champs et cliquer sur 'Calculer' pour voir un résultat."))
      }
      tagList(
        h3("Résultats de l'analyse"),
        p(HTML(res$jour)),
        hr(),
        p(HTML(res$saison))
      )
    })
  })
}