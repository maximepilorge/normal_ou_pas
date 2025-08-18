# modules/mod_analyse.R

mod_analyse_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(), # Activer shinyjs pour cet onglet
    titlePanel("Quelle est la rareté d'un événement météo ?"),
    sidebarLayout(
      sidebarPanel(
        p("Testez une température pour un jour ou pour l'année entière et découvrez sa fréquence historique."),
        # Ces contrôles sont globaux
        selectInput("ville_analyse", "Choisissez une ville :", choices = NULL),
        selectInput("periode_analyse", "Choisissez la période de référence :", choices = NULL),
        
        numericInput(ns("temp_analyse"), "Température maximale à tester (°C) :", 
                     value = 35, min = -20, max = 50),
        hr(),
        checkboxInput(ns("toute_annee_analyse"), "Analyser sur l'année entière", value = FALSE),
        
        div(id = ns("selecteurs_date"),
            selectInput(ns("mois_analyse"), "Mois :", 
                        choices = setNames(1:12, month.name), selected = 6), 
            selectInput(ns("jour_analyse"), "Jour :", choices = 1:31, selected = 26)
        ),
        
        actionButton(ns("calculer_frequence_btn"), "Calculer la fréquence", icon = icon("calculator")),
        width = 3
      ),
      mainPanel(
        uiOutput(ns("resultat_frequence_ui")),
        width = 9
      )
    )
  )
}

mod_analyse_server <- function(id, ville, periode, data_tmax, calculer_frequence_func) {
  moduleServer(id, function(input, output, session) {
    
    resultat <- reactiveVal(NULL)
    
    observeEvent(input$toute_annee_analyse, {
      # On utilise ns() pour cibler le div spécifique à ce module
      toggleState(id = "selecteurs_date", condition = !input$toute_annee_analyse)
    })
    
    observeEvent(input$mois_analyse, {
      req(input$mois_analyse)
      jours_dans_le_mois <- days_in_month(as.Date(paste("2001", input$mois_analyse, "01", sep="-")))
      updateSelectInput(session, "jour_analyse", choices = 1:jours_dans_le_mois)
    })
    
    observeEvent(input$calculer_frequence_btn, {
      req(ville(), input$temp_analyse, periode())
      
      if (input$toute_annee_analyse) {
        annees_periode <- as.numeric(unlist(strsplit(periode(), "-")))
        annee_debut <- annees_periode[1]; annee_fin <- annees_periode[2]
        
        donnees_historiques_annee <- data_tmax %>%
          filter(city == ville(), year(date) >= annee_debut, year(date) <= annee_fin)
        
        occurrences_sup <- sum(donnees_historiques_annee$tmax_celsius >= input$temp_analyse, na.rm = TRUE)
        
        res <- list(
          jour = paste0("Sur l'ensemble de l'année, une température supérieure ou égale à <b>", input$temp_analyse, "°C</b> est arrivée <b>", occurrences_sup, " fois</b> entre ", annee_debut, " et ", annee_fin, "."),
          saison = ""
        )
      } else {
        req(input$jour_analyse, input$mois_analyse)
        date_selectionnee <- as.Date(paste("2001", input$mois_analyse, input$jour_analyse, sep="-"), format="%Y-%m-%d")
        
        if (is.na(date_selectionnee)) {
          res <- list(jour = "Date invalide.", saison = "")
        } else {
          res <- calculer_frequence_func(
            ville_sel = ville(),
            date_sel = date_selectionnee,
            temp_sel = input$temp_analyse,
            periode_ref_str = periode(),
            data_brutes = data_tmax
          )
        }
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