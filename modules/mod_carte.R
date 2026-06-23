# modules/mod_carte.R
#
# Onglet « Carte » — réchauffement comparé des villes, EXPLORABLE dans le temps.
# Un curseur d'année recolore les pastilles selon l'écart de l'année choisie à la
# normale d'une période ancienne (la plus ancienne disponible) : en faisant
# glisser le curseur, on voit le réchauffement se diffuser sur le territoire.
# Clic sur une ville = zoom (recentrage). Filtre multi-villes pour décharger.
#
# S'appuie sur villes_coords + normales_jour_ref (pré-calculés dans global.R) et
# requête temperatures_max pour l'année sélectionnée.

mod_carte_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Le réchauffement, ville par ville et année par année",
      fillable = TRUE,

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
            "réchauffement progresser ; cliquez une ville pour zoomer."))
        )
      ),

      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_carte"))),
        leafletOutput(ns("carte"), height = "650px")
      )
    )
  )
}

mod_carte_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Échelle de couleur FIXE (comparable d'une année à l'autre), divergente
    # centrée sur 0 : bleu = sous la normale ancienne, rouge = au-dessus.
    dom <- c(-4, 4)
    pal <- colorNumeric("RdBu", domain = dom, reverse = TRUE, na.color = "#9aa0a6")
    clamp <- function(x) pmax(dom[1], pmin(dom[2], x))

    # Anomalie de chaque ville pour l'année choisie = moyenne, sur les jours
    # observés, de (tmax - normale du jour) pour la période de référence.
    anomalies_annee <- reactive({
      req(input$annee_carte)
      if (is.null(normales_jour_ref)) return(NULL)
      an <- input$annee_carte

      obs <- tbl(db_pool, "temperatures_max") %>%
        filter(annee == !!an) %>%
        select(ville, mois, jour_mois, temperature_max) %>%
        collect()

      if (nrow(obs) == 0) {
        return(villes_coords %>%
                 mutate(anomalie = NA_real_, moy_annuelle = NA_real_, n_jours = 0L))
      }

      obs %>%
        inner_join(normales_jour_ref, by = c("ville", "mois", "jour_mois")) %>%
        group_by(ville) %>%
        summarise(
          anomalie = mean(temperature_max - t_moy, na.rm = TRUE),
          moy_annuelle = mean(temperature_max, na.rm = TRUE),
          n_jours = n(),
          .groups = "drop"
        ) %>%
        right_join(villes_coords, by = "ville")
    }) %>% bindCache(input$annee_carte)

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
               "<br>Tmax moyenne : ", m)
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

    # Zoom sur une ville au clic d'une pastille.
    observeEvent(input$carte_marker_click, {
      clk <- input$carte_marker_click
      req(clk)
      leafletProxy("carte", session) %>%
        flyTo(lng = clk$lng, lat = clk$lat, zoom = 8)
    })

  })
}
