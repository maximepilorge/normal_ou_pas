# modules/mod_visualisation.R

mod_visualisation_ui <- function(id) {
  ns <- NS(id)
  # On remplace fluidPage par une simple liste de tags, 
  # car page_sidebar gère la structure de la page.
  tagList(
    # page_sidebar est le remplaçant moderne de sidebarLayout
    page_sidebar(
      title = "Visualiser le changement climatique",
      
      # Le contenu de l'ancienne sidebarPanel va ici
      sidebar = sidebar(
        # On encapsule les contrôles dans une "card" pour un meilleur rendu visuel
        card(
          card_header("Paramètres"),
          selectInput("ville_select", "Choisissez une ville :", choices = NULL),
          selectInput("periode_select", "Choisissez la période de référence :", choices = NULL),
          sliderInput("annee_select", "Choisissez l'année à comparer :", 
                      min = 1950, max = 2024, value = 2024, sep = "")
        )
      ),
      
      # Le contenu de l'ancien mainPanel va ici, encapsulé dans une card
      card(
        full_screen = TRUE, # Permet à l'utilisateur d'agrandir le graphique
        card_header("Évolution des températures maximales"),
        plotlyOutput(ns("climate_plot"), height = "600px")
      )
    )
  )
}

mod_visualisation_server <- function(id, data_stats, data_tmax, ville, periode, annee) {
  moduleServer(id, function(input, output, session) {
    
    plot_data_annee <- reactive({
      req(ville(), annee())
      annee_origine <- paste0(annee(), "-01-01")
      data_tmax %>%
        filter(city == ville(), year(date) == annee()) %>%
        mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
    })
    
    # 1. On dessine le graphique complet une seule fois au démarrage
    output$climate_plot <- renderPlotly({
      # req() s'assure que les valeurs initiales sont bien chargées
      req(ville(), periode(), annee())
      
      # Données pour les normales (ne change pas avec l'année)
      annee_origine <- paste0(annee(), "-01-01")
      plot_data_normale <- data_stats %>%
        filter(city == ville(), periode_ref == periode()) %>%
        mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
      
      # Construction du graphique complet
      p <- ggplot() +
        geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_min, ymax = t_max), fill = "lightblue", alpha = 0.5) +
        geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_q1, ymax = t_q3), fill = "skyblue", alpha = 0.6) +
        geom_line(data = plot_data_normale, aes(x = date, y = t_moy, color = "Moyenne normale"), linetype = "dashed", linewidth = 0.6) +
        geom_smooth(data = plot_data_annee(), aes(x = date, y = tmax_celsius), color = "#8B0000", linetype = "dashed", linewidth = 0.6, method = "loess", span = 0.3, se = FALSE) +
        geom_line(data = plot_data_annee(), aes(x = date, y = tmax_celsius, color = "Année sélectionnée"), linewidth = 1) +
        scale_color_manual(values = c("Moyenne normale" = "darkblue", "Année sélectionnée" = "#E41A1C")) +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        labs(
          title = paste("Températures maximales journalières à", ville(), "en", annee()),
          subtitle = paste("Comparaison avec la normale climatique", periode()),
          y = "Température maximale (°C)", x = "Jour de l'année", color = "Légende"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("x", "y"))
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