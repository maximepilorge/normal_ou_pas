# modules/mod_carte.R
#
# Onglet « Carte » — réchauffement comparé des villes, EXPLORABLE dans le temps.
# Un curseur d'année recolore les pastilles selon l'écart de l'année choisie à la
# normale d'une période ancienne : en faisant glisser le curseur, on voit le
# réchauffement se diffuser sur le territoire.
# Clic sur une ville = affichage, SOUS la carte, de sa trajectoire d'anomalies
# année par année, comparée à l'ensemble des villes (situe la ville dans le temps
# et par rapport au reste du territoire).
#
# S'appuie sur villes_coords + anomalies_villes_annee (pré-calculés dans global.R).

mod_carte_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Le réchauffement, ville par ville et année par année",
      fillable = FALSE,

      sidebar = sidebar(
        width = "320px",
        card(
          card_header("Réglages"),
          sliderInput(ns("annee_carte"), "Année :",
                      min = an_min_data, max = an_max_data,
                      value = an_max_data, step = 1, sep = ""),
          pickerInput(ns("villes_carte"),
                      "Villes affichées :",
                      choices = villes_coords$ville,
                      selected = villes_coords$ville,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 3",
                        `count-selected-text` = "{0} villes sélectionnées",
                        `live-search` = TRUE,
                        `none-selected-text` = "Aucune ville"
                      )),
          helpText(paste0(
            "Couleur = écart de la température maximale moyenne de l'année à la ",
            "normale ", periode_ref_carte, ". Faites glisser l'année pour voir le ",
            "réchauffement progresser ; cliquez une ville pour sa trajectoire détaillée."))
        )
      ),

      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_carte"))),
        leafletOutput(ns("carte"), height = "520px")
      ),

      card(
        card_header(uiOutput(ns("titre_graphe_ville"))),
        plotlyOutput(ns("graphe_ville"), height = "340px")
      )
    )
  )
}

mod_carte_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ville_selectionnee <- reactiveVal(NULL)

    # Échelle de couleur FIXE (comparable d'une année à l'autre), divergente
    # centrée sur 0 : bleu = sous la normale ancienne, rouge = au-dessus.
    dom <- c(-4, 4)
    pal <- colorNumeric("RdBu", domain = dom, reverse = TRUE, na.color = "#9aa0a6")
    clamp <- function(x) pmax(dom[1], pmin(dom[2], x))

    # Anomalies de l'année choisie (lecture du pré-calcul, aucune requête).
    anomalies_annee <- reactive({
      req(input$annee_carte)
      if (is.null(anomalies_villes_annee)) return(NULL)
      anomalies_villes_annee %>%
        filter(annee == input$annee_carte) %>%
        right_join(villes_coords, by = "ville")
    })

    construire_popups <- function(df) {
      vapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        a <- if (is.finite(r$anomalie)) sprintf("%+.1f °C", r$anomalie) else "n/d"
        m <- if (is.finite(r$moy_annuelle))
          paste0(format(round(r$moy_annuelle, 1), nsmall = 1, decimal.mark = ","), " °C") else "n/d"
        partiel <- if (!is.na(r$n_jours) && r$n_jours > 0 && r$n_jours < 350)
          " <i>(année incomplète)</i>" else ""
        paste0("<b>", r$ville, "</b> — ", input$annee_carte, partiel,
               "<br>Écart à la normale ", periode_ref_carte, " : <b>", a, "</b>",
               "<br>Tmax moyenne : ", m,
               "<br><i>Cliquez pour la trajectoire détaillée</i>")
      }, character(1))
    }

    # Ajoute les pastilles à une carte (leaflet) ou à un proxy (leafletProxy).
    ajouter_marqueurs <- function(carte, df) {
      if (nrow(df) == 0) return(carte)
      addCircleMarkers(
        carte, data = df, lng = ~longitude, lat = ~latitude,
        group = "villes", layerId = ~ville,
        radius = 11, stroke = TRUE, color = "#444444", weight = 1,
        fillColor = ~pal(clamp(anomalie)), fillOpacity = 0.9,
        label = ~ville, popup = construire_popups(df)
      )
    }

    # Sous-ensemble selon le filtre de villes.
    selection <- function(df) {
      sel <- input$villes_carte
      if (is.null(sel)) df else df %>% filter(ville %in% sel)
    }

    # Carte de base + pastilles initiales. On dessine les marqueurs ICI (en
    # isolate) pour éviter la race condition leafletProxy/onglet inactif ; les
    # mises à jour (année, filtre) passent ensuite par le proxy.
    output$carte <- renderLeaflet({
      df0 <- isolate(anomalies_annee())
      carte <- leaflet(options = leafletOptions(minZoom = 4)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(-5.5, 41.0, 9.8, 51.5) %>%
        addLegend("bottomright", pal = pal, values = dom,
                  title = paste0("Écart à la<br>normale ", periode_ref_carte, " (°C)"),
                  opacity = 0.9)
      if (!is.null(df0)) carte <- ajouter_marqueurs(carte, isolate(selection(df0)))
      carte
    })

    output$titre_carte <- renderUI({
      req(input$annee_carte)
      paste0("Écart à la normale ", periode_ref_carte, " — année ", input$annee_carte)
    })

    # Mise à jour des pastilles quand l'année OU le filtre changent.
    observe({
      df <- anomalies_annee()
      req(!is.null(df))
      leafletProxy("carte", session) %>%
        clearGroup("villes") %>%
        ajouter_marqueurs(selection(df))
    })

    # Clic sur une ville : on mémorise la ville et on la met en surbrillance.
    observeEvent(input$carte_marker_click, {
      clk <- input$carte_marker_click
      req(clk$id)
      ville_selectionnee(clk$id)
      leafletProxy("carte", session) %>%
        clearGroup("surbrillance") %>%
        addCircleMarkers(
          lng = clk$lng, lat = clk$lat, group = "surbrillance",
          radius = 16, stroke = TRUE, color = "#212529", weight = 3,
          fill = FALSE, opacity = 1
        )
    })

    output$titre_graphe_ville <- renderUI({
      v <- ville_selectionnee()
      if (is.null(v)) "Trajectoire d'une ville — cliquez une pastille sur la carte"
      else paste0("Trajectoire de ", v, " vs l'ensemble des villes (écart à la normale ",
                  periode_ref_carte, ")")
    })

    # Graphique : trajectoire annuelle des anomalies de la ville cliquée,
    # comparée à la moyenne et à l'étendue (min–max) de l'ensemble des villes.
    output$graphe_ville <- renderPlotly({
      v <- ville_selectionnee()
      if (is.null(v) || is.null(anomalies_villes_annee)) {
        return(plotly_empty(type = "scatter", mode = "lines") %>%
                 layout(annotations = list(list(
                   text = "Cliquez une ville sur la carte pour afficher sa trajectoire.",
                   showarrow = FALSE, font = list(color = "#6c757d", size = 15)))) %>%
                 config(displayModeBar = FALSE))
      }

      ensemble <- anomalies_villes_annee %>%
        group_by(annee) %>%
        summarise(moy = mean(anomalie, na.rm = TRUE),
                  bas = min(anomalie, na.rm = TRUE),
                  haut = max(anomalie, na.rm = TRUE), .groups = "drop")
      df_ville <- anomalies_villes_annee %>% filter(ville == v) %>% arrange(annee)

      p <- ggplot() +
        # Étendue (min–max) des villes : situe la ville dans le « peloton ».
        geom_ribbon(data = ensemble, aes(x = annee, ymin = bas, ymax = haut),
                    fill = "grey80", alpha = 0.5) +
        geom_line(data = ensemble, aes(x = annee, y = moy,
                                       color = "Moyenne des villes"),
                  linewidth = 0.7, linetype = "dashed") +
        geom_line(data = df_ville, aes(x = annee, y = anomalie, color = v),
                  linewidth = 1) +
        geom_hline(yintercept = 0, color = "#343a40", linewidth = 0.4) +
        # Repère de l'année active sur la carte.
        geom_vline(xintercept = input$annee_carte, color = "#6c757d",
                   linetype = "dotted", linewidth = 0.5) +
        scale_color_manual(values = setNames(c("#E41A1C", "#6c757d"),
                                             c(v, "Moyenne des villes"))) +
        scale_x_continuous(breaks = scales::breaks_width(10)) +
        labs(x = NULL, y = "Écart à la normale (°C)", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    })

  })
}
