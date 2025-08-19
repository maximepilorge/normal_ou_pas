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
          selectInput("ville_analyse", "Choisissez une ville :", choices = NULL),
          selectInput("periode_analyse", "Choisissez la période de référence :", choices = NULL),
          numericInput(ns("temp_analyse"), "Température maximale à tester (°C) :",
                       value = 35, min = -20, max = 50),
          hr(),
          checkboxInput(ns("toute_annee_analyse"), "Analyser sur l'année entière", value = FALSE),
          div(id = ns("selecteurs_date"),
              selectInput(ns("mois_analyse"), "Mois :",
                          choices = setNames(1:12, mois_fr), selected = 1),
              selectInput(ns("jour_analyse"), "Jour :", choices = 1:31, selected = 1)
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
      updateSelectInput(session, "jour_analyse", choices = 1:jours_dans_le_mois)
    })
    
    observeEvent(input$calculer_frequence_btn, {
      req(ville(), input$temp_analyse, periode())
      
      annees_periode <- as.numeric(unlist(strsplit(periode(), "-")))
      annee_debut <- annees_periode[1]
      annee_fin <- annees_periode[2]
      annee_debut_str <- as.character(annee_debut)
      annee_fin_str <- as.character(annee_fin)
      
      res <- NULL
      
      if (input$toute_annee_analyse) {
        
        donnees_historiques <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!ville(),
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") >= !!annee_debut_str,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") <= !!annee_fin_str
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
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") >= !!annee_debut_str,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") <= !!annee_fin_str,
            dbplyr::sql("STRFTIME('%m', date * 86400, 'unixepoch')") == !!mois_str,
            dbplyr::sql("STRFTIME('%d', date * 86400, 'unixepoch')") == !!jour_str
          ) %>%
          collect() %>%
          rename(tmax_celsius = temperature_max)
        
        occurrences_jour <- sum(donnees_jour$tmax_celsius >= input$temp_analyse, na.rm = TRUE)
        texte_jour <- paste0("Pour le <b>", input$jour_analyse, " ", mois_fr[as.numeric(input$mois_analyse)], "</b>, une température supérieure ou égale à <b>", input$temp_analyse, "°C</b> est arrivée <b>", occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")
        
        # Calcul pour la saison
        date_selectionnee <- as.Date(paste("2001", input$mois_analyse, input$jour_analyse, sep="-"))
        saison <- get_season_info(date_selectionnee) # get_season_info() vient de global.R
        mois_saison_str <- sprintf("%02d", saison$mois)
        
        donnees_saison <- tbl(db_pool, "temperatures_max") %>%
          filter(
            ville == !!ville(),
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") >= !!annee_debut_str,
            dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") <= !!annee_fin_str,
            dbplyr::sql("STRFTIME('%m', date * 86400, 'unixepoch')") %in% !!mois_saison_str
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