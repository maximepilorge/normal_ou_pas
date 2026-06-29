# modules/mod_comparer.R
#
# Onglet « Comparer » — fusion des anciens onglets Comparaison + Carte.
# Deux lentilles sur la même donnée (écart à une normale de référence), pilotées
# par une VILLE FOCUS et une ANNÉE communes :
#   - « Dans l'année »   : courbe jour-vs-normale de l'année choisie (réf. au choix).
#   - « Entre les villes »: carte des écarts par ville (réf. ancienne figée) +
#     trajectoire annuelle de la ville focus vs l'ensemble.
# La ville focus est partagée et BIDIRECTIONNELLE : cliquer une pastille de la
# carte met à jour le sélecteur (donc la courbe), et inversement.

mod_comparer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .comparer-courbe-wrap { height: 560px; }
        @media (max-width: 575.98px) { .comparer-courbe-wrap { height: 430px; } }
        /* Légende Leaflet compactée en portrait (cf. ancien onglet Carte). */
        @media (max-width: 575.98px) {
          .leaflet-control.legend {
            font-size: 0.7rem !important; line-height: 1.1 !important;
            padding: 4px 7px !important;
            transform: scale(0.92); transform-origin: bottom right;
          }
        }
      "))
    ),
    div(
      class = "p-2",
      h5(class = "mb-2", "Comparer : dans le temps et entre les villes"),

      # Barre de réglages EN LIGNE (remplace la sidebar) : ville + période visibles
      # d'emblée, au-dessus des vues. Les menus s'affichent hors flux (container =
      # "body") pour ne pas créer de double défilement.
      card(
        class = "mb-2",
        card_body(
          class = "py-2",
          layout_columns(
            col_widths = breakpoints(sm = 6),
            pickerInput(ns("ville_focus"), "Ville :",
                        choices = villes_triees, selected = villes_triees[1],
                        options = list(container = "body", `live-search` = TRUE, size = 8)),
            pickerInput(ns("periode"), "Période de référence (normale) :",
                        choices = periodes_disponibles,
                        options = list(container = "body", `live-search` = FALSE))
          ),
          helpText(HTML(paste0(
            "La <b>période de référence</b> s'applique à la vue « Dans l'année ». ",
            "La carte « Entre les villes » compare à une normale ancienne figée (",
            periode_ref_carte, ") pour visualiser le réchauffement progresser.")))
        )
      ),

      # Année : scrubber COMMUN aux deux vues (on le fait glisser pour animer la carte).
      div(class = "px-1 pb-2",
          sliderInput(ns("annee"), "Année :",
                      min = an_min_data, max = an_max_data,
                      value = an_max_data, step = 1, sep = "", width = "100%")),

      navset_card_tab(
        id = ns("sousvue"),
        nav_panel(
          "Dans l'année", value = "courbe",
          uiOutput(ns("titre_courbe")),
          div(class = "comparer-courbe-wrap",
              plotlyOutput(ns("climate_plot"), height = "100%"))
        ),
        nav_panel(
          "Entre les villes", value = "carte",
          uiOutput(ns("titre_carte")),
          leafletOutput(ns("carte"), height = "500px"),
          hr(),
          uiOutput(ns("titre_trajectoire")),
          plotlyOutput(ns("graphe_ville"), height = "320px")
        )
      )
    )
  )
}

mod_comparer_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- État partagé : la ville focus est pilotée par le sélecteur ET par les
    #     clics sur la carte. Le sélecteur reste la source de vérité ; un clic
    #     écrit dedans via updatePickerInput. -------------------------------------
    observeEvent(input$carte_marker_click, {
      clk <- input$carte_marker_click
      req(clk$id)
      if (!identical(clk$id, input$ville_focus))
        updatePickerInput(session, "ville_focus", selected = clk$id)
    })

    # Leaflet (et plotly) dans un sous-onglet initialement masqué s'affichent à
    # taille nulle (carte grise). À chaque bascule, on force le recalcul des
    # dimensions : invalidateSize() sur la carte + un 'resize' pour la trajectoire.
    # Petit délai pour laisser la transition d'onglet poser les dimensions finales.
    observeEvent(input$sousvue, {
      shinyjs::runjs(sprintf(
        "setTimeout(function() {
           if (window.HTMLWidgets) {
             var w = HTMLWidgets.find('#%s');
             if (w && typeof w.getMap === 'function') {
               try { w.getMap().invalidateSize(); } catch (e) {}
             }
           }
           window.dispatchEvent(new Event('resize'));
         }, 200);",
        ns("carte")))
    }, ignoreInit = TRUE)

    # ================== VUE « DANS L'ANNÉE » : courbe jour-vs-normale =============

    est_mobile_courbe <- reactive({
      largeur_sous_seuil(session, ns("climate_plot"))
    })

    output$titre_courbe <- renderUI({
      req(input$ville_focus, input$annee, input$periode)
      tagList(
        HTML(paste0("Températures maximales à <strong>", input$ville_focus,
                    "</strong> en <strong>", input$annee, "</strong>")),
        tags$div(class = "small text-muted fw-normal",
                 paste("Comparaison à la normale climatique", input$periode))
      )
    })

    plot_data_annee <- reactive({
      req(input$ville_focus, input$annee)
      start_date <- as.Date(paste0(input$annee, "-01-01"))
      end_date   <- as.Date(paste0(input$annee + 1, "-01-01"))
      tbl(db_pool, "temperatures_max") %>%
        filter(ville == !!input$ville_focus, date >= !!start_date, date < !!end_date) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max)
    }) %>% bindCache(input$ville_focus, input$annee)

    plot_data_normale <- reactive({
      req(input$ville_focus, input$periode, input$annee)
      annee_origine <- paste0(input$annee, "-01-01")
      tbl(db_pool, "stats_normales") %>%
        filter(ville == !!input$ville_focus, periode_ref == !!input$periode) %>%
        collect() %>%
        filter(!(mois == 2 & jour_mois == 29 & !leap_year(as.Date(annee_origine)))) %>%
        mutate(date = as.Date(paste(year(as.Date(annee_origine)), mois, jour_mois, sep = "-")))
    }) %>% bindCache(input$ville_focus, input$periode, input$annee)

    output$climate_plot <- renderPlotly({
      req(input$ville_focus, input$periode, input$annee)
      d_norm  <- plot_data_normale()
      d_annee <- plot_data_annee()
      req(nrow(d_norm) > 0, nrow(d_annee) > 0)

      mob <- est_mobile_courbe()
      taille_police  <- if (mob) 11 else 14
      taille_legende <- if (mob) 10 else 12

      p <- ggplot() +
        geom_ribbon(data = d_norm, aes(x = date, ymin = t_min, ymax = t_max),
                    fill = "lightblue", alpha = 0.5) +
        geom_ribbon(data = d_norm, aes(x = date, ymin = t_q1, ymax = t_q3),
                    fill = "skyblue", alpha = 0.6) +
        geom_line(data = d_norm, aes(x = date, y = t_moy, color = "Moyenne normale"),
                  linetype = "dashed", linewidth = 0.6) +
        geom_point(data = d_norm, aes(x = date, y = t_moy,
                   text = paste("Date:", format(date, "%d %b"),
                                "<br>Moyenne normale:", round(t_moy, 1), "°C")),
                   alpha = 0)

      # « Tendance de l'année » seulement sur grand écran (surcharge en mobile).
      if (!mob)
        p <- p + geom_smooth(data = d_annee,
                    aes(x = date, y = tmax_celsius, color = "Tendance de l'année"),
                    linetype = "dashed", linewidth = 0.6,
                    method = "loess", span = 0.3, se = FALSE)

      p <- p +
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
      bindCache(input$ville_focus, input$periode, input$annee, est_mobile_courbe())

    # ================ VUE « ENTRE LES VILLES » : carte + trajectoire =============

    # Échelle de couleur FIXE divergente centrée sur 0 (comparable d'une année à
    # l'autre) : bleu = sous la normale ancienne, rouge = au-dessus.
    dom <- c(-4, 4)
    pal <- colorNumeric("RdBu", domain = dom, reverse = TRUE, na.color = "#9aa0a6")
    clamp <- function(x) pmax(dom[1], pmin(dom[2], x))

    anomalies_annee <- reactive({
      req(input$annee)
      if (is.null(anomalies_villes_annee)) return(NULL)
      anomalies_villes_annee %>%
        filter(annee == input$annee) %>%
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
        paste0("<b>", r$ville, "</b> — ", input$annee, partiel,
               "<br>Écart à la normale ", periode_ref_carte, " : <b>", a, "</b>",
               "<br>Tmax moyenne : ", m,
               "<br><i>Cliquez pour basculer la ville focus</i>")
      }, character(1))
    }

    # Toutes les villes sont affichées (pas de filtre) : la ville focus est
    # simplement surlignée. layerId = ville -> remonte dans carte_marker_click$id.
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

    surligner <- function(carte, v) {
      co <- villes_coords %>% filter(ville == v)
      if (nrow(co) != 1) return(carte)
      addCircleMarkers(carte, lng = co$longitude, lat = co$latitude,
                       group = "surbrillance", radius = 16, stroke = TRUE,
                       color = "#212529", weight = 3, fill = FALSE, opacity = 1)
    }

    output$titre_carte <- renderUI({
      req(input$annee)
      tagList(
        HTML(paste0("Écart à la normale ", periode_ref_carte,
                    " — année <strong>", input$annee, "</strong>")),
        tags$div(class = "small text-muted fw-normal",
                 "Cliquez une pastille pour basculer la ville focus.")
      )
    })

    # Carte de base + pastilles + surbrillance initiales dessinées ICI (isolate)
    # pour éviter la race condition leafletProxy/onglet masqué au démarrage.
    output$carte <- renderLeaflet({
      df0 <- isolate(anomalies_annee())
      carte <- leaflet(options = leafletOptions(minZoom = 4)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(-5.5, 41.0, 9.8, 51.5) %>%
        addLegend("bottomright", pal = pal, values = dom, bins = 4,
                  title = paste0("Écart ", periode_ref_carte, " (°C)"),
                  opacity = 0.9)
      if (!is.null(df0)) carte <- ajouter_marqueurs(carte, df0)
      carte <- surligner(carte, isolate(input$ville_focus))
      carte
    })

    # Pastilles + surbrillance synchronisées via proxy, UNIQUEMENT quand l'onglet
    # carte est visible : un leafletProxy sur une carte masquée (taille nulle) la
    # laisse grise au ré-affichage. L'observe dépend aussi de input$sousvue → il se
    # relance à l'affichage de l'onglet et resynchronise l'année/la ville qui ont
    # pu changer pendant que la carte était masquée.
    observe({
      req(identical(input$sousvue, "carte"))
      df <- anomalies_annee()
      req(!is.null(df))
      leafletProxy("carte", session) %>%
        clearGroup("villes")       %>% ajouter_marqueurs(df) %>%
        clearGroup("surbrillance") %>% surligner(input$ville_focus)
    })

    # ----- Trajectoire annuelle de la ville focus vs l'ensemble des villes -------

    est_mobile_traj <- reactive({
      largeur_sous_seuil(session, ns("graphe_ville"))
    })

    output$titre_trajectoire <- renderUI({
      req(input$ville_focus)
      HTML(paste0("Trajectoire de <strong>", input$ville_focus,
                  "</strong> vs l'ensemble des villes (écart à la normale ",
                  periode_ref_carte, ")"))
    })

    output$graphe_ville <- renderPlotly({
      v <- input$ville_focus
      req(v)
      if (is.null(anomalies_villes_annee))
        return(plotly_empty(type = "scatter", mode = "lines") %>%
                 layout(annotations = list(list(
                   text = "Trajectoire indisponible.",
                   showarrow = FALSE, font = list(color = "#6c757d", size = 15)))) %>%
                 config(displayModeBar = FALSE))

      mob <- est_mobile_traj()
      fmt2 <- function(x) format(round(x, 2), nsmall = 2, decimal.mark = ",")

      ensemble <- anomalies_villes_annee %>%
        group_by(annee) %>%
        summarise(moy = mean(anomalie, na.rm = TRUE),
                  bas = min(anomalie, na.rm = TRUE),
                  haut = max(anomalie, na.rm = TRUE), .groups = "drop") %>%
        mutate(text_moy = paste0("Année ", annee,
                                 "<br>Moyenne des villes : ", fmt2(moy), " °C"))
      df_ville <- anomalies_villes_annee %>% filter(ville == v) %>% arrange(annee) %>%
        mutate(text = paste0("Année ", annee, "<br>", v, " : ", fmt2(anomalie), " °C"))

      p <- ggplot() +
        geom_ribbon(data = ensemble, aes(x = annee, ymin = bas, ymax = haut),
                    fill = "grey80", alpha = 0.5) +
        geom_line(data = ensemble, aes(x = annee, y = moy, color = "Moyenne des villes"),
                  linewidth = 0.7, linetype = "dashed") +
        geom_point(data = ensemble, aes(x = annee, y = moy, text = text_moy), alpha = 0) +
        geom_line(data = df_ville, aes(x = annee, y = anomalie, color = v), linewidth = 1) +
        geom_point(data = df_ville, aes(x = annee, y = anomalie, text = text), alpha = 0) +
        geom_hline(yintercept = 0, color = "#343a40", linewidth = 0.4) +
        geom_vline(xintercept = input$annee, color = "#6c757d",
                   linetype = "dotted", linewidth = 0.5) +
        scale_color_manual(values = setNames(c("#E41A1C", "#6c757d"),
                                             c(v, "Moyenne des villes"))) +
        labs(x = NULL, y = "Écart à la normale (°C)", color = NULL) +
        theme_minimal(base_size = if (mob) 10 else 13) +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text") %>%
        layout(
          xaxis = list(fixedrange = TRUE, dtick = if (mob) 20 else 10, tickangle = -45),
          yaxis = list(fixedrange = TRUE),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2,
                        font = list(size = if (mob) 10 else 12))
        ) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    })

    # On laisse suspendWhenHidden au défaut (TRUE) : la carte se rend ainsi À
    # L'AFFICHAGE de son onglet — donc à taille correcte — plutôt qu'au démarrage,
    # masquée et à taille nulle (ce qui la laissait grise). Les mises à jour
    # passent ensuite par l'observe ci-dessus, gardé visible-only.

  })
}
