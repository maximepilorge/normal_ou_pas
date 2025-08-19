# modules/mod_visualisation.R

mod_visualisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Visualiser le changement climatique",
      
      sidebar = sidebar(
        width = "350px",
        # On encapsule les contrôles dans une "card" pour un meilleur rendu visuel
        card(
          card_header("Paramètres"),
          selectInput("ville_select", "Choisissez une ville :", choices = NULL),
          selectInput("periode_select", "Choisissez la période de référence :", choices = NULL),
          sliderInput("annee_select", "Choisissez l'année à comparer :", 
                      min = 1950, max = 2024, value = 2024, sep = "")
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Évolution des températures maximales"),
        plotlyOutput(ns("climate_plot"), height = "600px")
      )
    )
  )
}

mod_visualisation_server <- function(id, db_pool, ville, periode, annee) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    screen_width <- reactiveVal()
    
    observe({
      # On injecte l'ID complet et "namespacé" dans le code JS
      js_code <- sprintf(
        "
        $(window).on('resize', function(){
          Shiny.setInputValue('%s', window.innerWidth);
        });
        Shiny.setInputValue('%s', window.innerWidth);
        ",
        ns("screen_width"), ns("screen_width")
      )
      shinyjs::runjs(js_code)
    })
    
    observeEvent(input$screen_width, {
      req(input$screen_width) # On attend d'avoir la largeur
      
      # 768px est un seuil commun pour les tablettes/smartphones
      is_mobile <- input$screen_width < 768 
      
      # On utilise plotlyProxy pour modifier le graphique existant sans le redessiner
      plotlyProxy("climate_plot", session) %>%
        # On met à jour la visibilité des traces 6 et 7 (indices 5 et 6)
        plotlyProxyInvoke("restyle", list(visible = !is_mobile), as.list(5:6))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    plot_data_annee <- reactive({
      req(ville(), annee())

      # On prépare l'année en chaîne de caractères pour la requête SQL
      annee_str <- as.character(annee())
      
      donnees_preparees <- tbl(db_pool, "temperatures_max") %>%
        filter(
          ville == !!ville(),
          dbplyr::sql("STRFTIME('%Y', date * 86400, 'unixepoch')") == !!annee_str
        ) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max) %>%
        mutate(
          date = as.Date(date, origin = "1970-01-01"),
        )
      
      # On retourne le dataframe
      donnees_preparees
    })
    
    # 1. On dessine le graphique complet une seule fois au démarrage
    output$climate_plot <- renderPlotly({

      req(ville(), periode(), annee())
      
      # Données pour les normales (ne change pas avec l'année)
      annee_origine <- paste0(annee(), "-01-01")
      plot_data_normale <- tbl(db_pool, "stats_normales") %>%
        filter(
          ville == !!ville(), 
          periode_ref == !!periode()
          ) %>%
        collect() %>%
        mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
      
      # Construction du graphique complet
      p <- ggplot() +
        # Les rubans ne changent pas
        geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_min, ymax = t_max), fill = "lightblue", alpha = 0.5) +
        geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_q1, ymax = t_q3), fill = "skyblue", alpha = 0.6) +
        
        # --- Couches pour la moyenne normale ---
        # 1. On dessine la ligne (simple, sans 'text')
        geom_line(data = plot_data_normale, aes(x = date, y = t_moy, color = "Moyenne normale"),
                  linetype = "dashed", linewidth = 0.6) +
        # 2. On ajoute les points invisibles (alpha=0) juste pour l'infobulle
        geom_point(data = plot_data_normale, aes(x = date, y = t_moy, 
                                                 text = paste("Date:", format(date, "%d %b"), "<br>Moyenne normale:", round(t_moy, 1), "°C")),
                   alpha = 0) +
        # La courbe de lissage geom_smooth
        geom_smooth(data = plot_data_annee(), 
                    aes(x = date, y = tmax_celsius, color = "Tendance de l'année"), # <--- On déplace "color" ici
                    linetype = "dashed", linewidth = 0.6, method = "loess", span = 0.3, se = FALSE) +
        
        # --- Couches pour l'année sélectionnée ---
        # 1. On dessine la ligne (simple, sans 'text')
        geom_line(data = plot_data_annee(), aes(x = date, y = tmax_celsius, color = "Année sélectionnée"), 
                  linewidth = 0.8) +
        # 2. On ajoute les points invisibles (alpha=0) juste pour l'infobulle
        geom_point(data = plot_data_annee(), aes(x = date, y = tmax_celsius, 
                                                 text = paste("Date:", format(date, "%d %b"), "<br>Température:", round(tmax_celsius, 1), "°C")),
                   alpha = 0) +
        
        scale_color_manual(values = c("Moyenne normale" = "darkblue", 
                                      "Année sélectionnée" = "#E41A1C",
                                      "Tendance de l'année" = "#8B0000")) +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        labs(
          title = paste("Températures maximales journalières à", ville(), "en", annee()),
          subtitle = paste("Comparaison avec la normale climatique", periode()),
          y = "Température maximale (°C)", x = "Jour de l'année", color = "Légende"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = FALSE)
    })
    
    # 2. On observe les changements de l'année pour mettre à jour le graphique (après le premier affichage)
    observeEvent(annee(), {
      
      # On récupère les données via le reactive
      data_annee_maj <- plot_data_annee()
      
      # On met à jour les données des traces (lignes) via le proxy
      plotlyProxy("climate_plot", session) %>%
        # Met à jour les traces pour le smooth et la ligne de l'année.
        # Les index sont corrects (2 et 3) car plotly commence à compter à 0
        # et les couches de l'année sont les 3ème et 4ème dans le ggplot.
        plotlyProxyInvoke("restyle", list(
          x = list(data_annee_maj$date, data_annee_maj$date),
          y = list(data_annee_maj$tmax_celsius, data_annee_maj$tmax_celsius)
        ), as.list(2:3)) %>%
        # On met à jour le titre
        plotlyProxyInvoke("relayout", list(
          title = paste("Températures maximales journalières à", ville(), "en", annee())
        ))
      
    }, ignoreInit = TRUE)
    
  })
}