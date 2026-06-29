# modules/mod_visualisation.R

mod_visualisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Hauteur du graphe réduite en portrait pour limiter le scroll (la police et
    # les ticks sont, eux, adaptés côté serveur via clientData).
    tags$head(tags$style(HTML("
      .comparaison-plot-wrap { height: 600px; }
      @media (max-width: 575.98px) { .comparaison-plot-wrap { height: 430px; } }
    "))),
    page_sidebar(
      title = "Visualiser le changement climatique",
      fillable = FALSE,

      # Sidebar adaptée à l'orientation (breakpoint bslib 576px) :
      #   - mobile = "always"  -> en portrait, filtres empilés sous le graphe, visibles.
      #   - desktop = "closed" -> en paysage/desktop, repliée : graphe pleine largeur.
      # NB : les inputs restent NON-namespacés (lus tels quels par server.R).
      sidebar = sidebar(
        width = "350px",
        open = list(mobile = "always", desktop = "closed"),
        card(
          card_header("Paramètres"),
          pickerInput("ville_select",
                      "Choisissez une ville :",
                      choices = villes_triees,
                      select = villes_triees[1],
                      options = list('live-search' = FALSE)),
          pickerInput("periode_select",
                      "Choisissez la période de référence :",
                      choices = periodes_disponibles,
                      options = list('live-search' = FALSE)),
          sliderInput("annee_select", "Choisissez l'année à comparer :",
                      min = an_min_data,
                      max = an_max_data,
                      value = an_max_data,
                      sep = "")
        )
      ),

      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_comparaison"))),
        div(class = "comparaison-plot-wrap",
            plotlyOutput(ns("climate_plot"), height = "100%"))
      )
    )
  )
}

mod_visualisation_server <- function(id, db_pool, ville, periode, annee) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Largeur réelle (px) du graphe, transmise par le client. Booléen (et non
    # largeur brute) pour ne ré-invalider le rendu qu'au franchissement du seuil
    # ~500px (≈ smartphone en portrait), pas à chaque pixel.
    est_mobile <- reactive({
      w <- session$clientData[[paste0("output_", ns("climate_plot"), "_width")]]
      !is.null(w) && w > 0 && w < 500
    })

    # Titre dynamique de la carte : rappelle la ville et l'année (s'enroule en
    # HTML, contrairement à un titre plotly qui se tronque sur écran étroit).
    output$titre_comparaison <- renderUI({
      req(ville(), annee(), periode())
      tagList(
        HTML(paste0("Températures maximales à <strong>", ville(),
                    "</strong> en <strong>", annee(), "</strong>")),
        tags$div(class = "small text-muted fw-normal",
                 paste("Comparaison à la normale climatique", periode()))
      )
    })

    # Données de l'année sélectionnée (tmax journalières).
    plot_data_annee <- reactive({
      req(ville(), annee())
      start_date <- as.Date(paste0(annee(), "-01-01"))
      end_date   <- as.Date(paste0(annee() + 1, "-01-01"))
      tbl(db_pool, "temperatures_max") %>%
        filter(ville == !!ville(), date >= !!start_date, date < !!end_date) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max)
    }) %>% bindCache(ville(), annee())

    # Normales climatiques (rubans + moyenne) pour la ville et la période, datées
    # sur l'année affichée (le 29/02 est retiré hors année bissextile).
    plot_data_normale <- reactive({
      req(ville(), periode(), annee())
      annee_origine <- paste0(annee(), "-01-01")
      tbl(db_pool, "stats_normales") %>%
        filter(ville == !!ville(), periode_ref == !!periode()) %>%
        collect() %>%
        filter(!(mois == 2 & jour_mois == 29 & !leap_year(as.Date(annee_origine)))) %>%
        mutate(date = as.Date(paste(year(as.Date(annee_origine)), mois, jour_mois, sep = "-")))
    }) %>% bindCache(ville(), periode(), annee())

    # Graphe re-rendu à chaque changement (ville / période / année / largeur).
    # bindCache garde le tout rapide ; plus de plotlyProxy ni de JS custom.
    output$climate_plot <- renderPlotly({
      req(ville(), periode(), annee())
      d_norm  <- plot_data_normale()
      d_annee <- plot_data_annee()
      req(nrow(d_norm) > 0, nrow(d_annee) > 0)

      mob <- est_mobile()
      taille_police  <- if (mob) 11 else 14
      taille_legende <- if (mob) 10 else 12

      p <- ggplot() +
        # Rubans des normales (min–max puis quartiles).
        geom_ribbon(data = d_norm, aes(x = date, ymin = t_min, ymax = t_max),
                    fill = "lightblue", alpha = 0.5) +
        geom_ribbon(data = d_norm, aes(x = date, ymin = t_q1, ymax = t_q3),
                    fill = "skyblue", alpha = 0.6) +
        # Moyenne normale : ligne + points invisibles porteurs de l'infobulle.
        geom_line(data = d_norm, aes(x = date, y = t_moy, color = "Moyenne normale"),
                  linetype = "dashed", linewidth = 0.6) +
        geom_point(data = d_norm, aes(x = date, y = t_moy,
                   text = paste("Date:", format(date, "%d %b"),
                                "<br>Moyenne normale:", round(t_moy, 1), "°C")),
                   alpha = 0)

      # La « Tendance de l'année » (lissage loess) n'est tracée que sur grand
      # écran : sur mobile elle surcharge un graphe déjà étroit.
      if (!mob)
        p <- p + geom_smooth(data = d_annee,
                    aes(x = date, y = tmax_celsius, color = "Tendance de l'année"),
                    linetype = "dashed", linewidth = 0.6,
                    method = "loess", span = 0.3, se = FALSE)

      p <- p +
        # Année sélectionnée : ligne + points invisibles porteurs de l'infobulle.
        geom_line(data = d_annee, aes(x = date, y = tmax_celsius, color = "Année sélectionnée"),
                  linewidth = 0.8) +
        geom_point(data = d_annee, aes(x = date, y = tmax_celsius,
                   text = paste("Date:", format(date, "%d %b"),
                                "<br>Température:", round(tmax_celsius, 1), "°C")),
                   alpha = 0) +
        scale_color_manual(values = c("Moyenne normale"    = "darkblue",
                                      "Année sélectionnée" = "#E41A1C",
                                      "Tendance de l'année" = "#8B0000")) +
        labs(y = "Température maximale (°C)", x = NULL, color = NULL) +
        theme_minimal(base_size = taille_police) +
        theme(legend.position = "bottom")

      # Ticks de mois maîtrisés côté plotly : 1 sur 2 en portrait (sinon les 12
      # mois se chevauchent), les 12 en grand écran. tickformat %b = mois abrégé.
      dtick_mois <- if (mob) "M2" else "M1"

      ggplotly(p, tooltip = "text") %>%
        layout(
          xaxis = list(fixedrange = TRUE, title = NULL, dtick = dtick_mois,
                       tickformat = "%b", tickangle = if (mob) -45 else 0),
          yaxis = list(fixedrange = TRUE),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = -0.15, yanchor = "top",
                        font = list(size = taille_legende)),
          margin = list(b = if (mob) 70 else 80, t = 10)
        ) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    }) %>%
      bindCache(ville(), periode(), annee(), est_mobile())

  })
}
