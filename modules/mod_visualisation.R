# modules/mod_visualisation.R

mod_visualisation_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Visualiser le changement climatique"),
    sidebarLayout(
      sidebarPanel(
        # Ces contrôles sont globaux et définis dans ui.R
        selectInput("ville_select", "Choisissez une ville :", choices = NULL),
        selectInput("periode_select", "Choisissez la période de référence :", choices = NULL),
        sliderInput("annee_select", "Choisissez l'année à comparer :", 
                    min = 1950, max = 2024, value = 2023, sep = "",
                    animate = animationOptions(interval = 800, loop = TRUE)),
        width = 3
      ),
      mainPanel(
        plotlyOutput(ns("climate_plot"), height = "600px"),
        width = 9
      )
    )
  )
}

mod_visualisation_server <- function(id, data_stats, data_tmax, ville, periode, annee) {
  moduleServer(id, function(input, output, session) {
    
    output$climate_plot <- renderPlotly({
      req(ville(), periode(), annee())
      
      annee_origine <- paste0(annee(), "-01-01")
      
      plot_data_normale <- data_stats %>%
        filter(city == ville(), periode_ref == periode()) %>%
        mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
      
      plot_data_annee <- data_tmax %>%
        filter(city == ville(), year(date) == annee()) %>%
        mutate(date = as.Date(jour_annee - 1, origin = annee_origine))
      
      p <- ggplot() +
        geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_min, ymax = t_max), fill = "lightblue", alpha = 0.5) +
        geom_ribbon(data = plot_data_normale, aes(x = date, ymin = t_q1, ymax = t_q3), fill = "skyblue", alpha = 0.6) +
        # Ligne de la moyenne normale
        geom_line(data = plot_data_normale, aes(x = date, y = t_moy, color = "Moyenne normale"), 
                  linetype = "dashed", 
                  size = 0.6) +
        
        # Courbe de tendance (en arrière-plan)
        geom_smooth(data = plot_data_annee, aes(x = date, y = tmax_celsius), 
                    color = "#8B0000", linetype = "dashed",
                    size = 0.6,
                    method = "loess", span = 0.3, se = FALSE) +
        
        # Courbe journalière (au premier plan)
        geom_line(data = plot_data_annee, aes(x = date, y = tmax_celsius, color = "Année sélectionnée"), 
                  size = 1) +
        
        # Légende
        scale_color_manual(values = c("Moyenne normale" = "darkblue", 
                                      "Année sélectionnée" = "#E41A1C")) +
        
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        labs(
          title = paste("Températures maximales journalières à", ville(), "en", annee()),
          subtitle = paste("Comparaison avec la normale climatique", periode()),
          y = "Température maximale (°C)", x = "Jour de l'année", color = "Légende"
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("x", "y"))
      
    })
    
  })
}