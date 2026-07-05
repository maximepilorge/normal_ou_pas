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
        /* La courbe de l'année est cliquable (bascule vers « Une journée »). */
        .comparer-courbe-wrap .nsewdrag { cursor: pointer !important; }
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
          # Ville · année dans une même rangée (l'année est le scrubber COMMUN aux
          # deux vues : on le fait glisser pour animer la carte). L'époque de
          # référence, réglage d'expert, est repliée dans « Réglages avancés ».
          layout_columns(
            col_widths = breakpoints(sm = c(6, 6)),
            pickerInput(ns("ville_focus"), "Ville :",
                        choices = villes_triees, selected = villes_triees[1],
                        options = list(container = "body", `live-search` = TRUE, size = 8)),
            sliderInput(ns("annee"), "Année :",
                        min = an_min_data, max = an_max_data,
                        value = an_max_data, step = 1, sep = "", width = "100%")
          ),
          # Le défaut (l'époque la plus ancienne) maximise le contraste de
          # réchauffement affiché — c'est le bon réglage pour presque tout le monde.
          tags$details(
            class = "small mt-1",
            tags$summary(class = "text-muted", style = "cursor: pointer;",
                         icon("sliders"), " Réglages avancés"),
            div(class = "mt-2", style = "max-width: 420px;",
                pickerInput(ns("periode"), "Époque de référence (normale) :",
                            choices = periodes_disponibles,
                            width = "100%",
                            choicesOpt = list(subtext = soustitres_periodes(periodes_disponibles)),
                            options = list(container = "body", `live-search` = FALSE)),
                helpText(HTML(paste0(
                  "L'époque de référence s'applique à la courbe « Dans l'année » et à ",
                  "la carte « Entre les villes » (réchauffement entre cette époque et ",
                  "la plus récente). La trajectoire du bas, elle, compare toujours à ",
                  "la normale la plus ancienne (", periode_ref_carte, ")."))))
          )
        )
      ),

      navset_card_tab(
        id = ns("sousvue"),
        nav_panel(
          "Dans l'année", value = "courbe",
          uiOutput(ns("titre_courbe")),
          div(class = "comparer-courbe-wrap",
              plotlyOutput(ns("climate_plot"), height = "100%")),
          # Boucle de circulation : zoom sur le jour le plus marquant de l'année.
          div(class = "text-center mt-2 mb-1", uiOutput(ns("lien_jour_chaud")))
        ),
        nav_panel(
          "Entre les villes", value = "carte",
          uiOutput(ns("titre_carte")),
          # Sélecteur d'indicateur cartographié (réchauffement par défaut ; jours de
          # forte chaleur / gel si la table d'indicateurs est disponible).
          div(class = "mb-2",
              radioGroupButtons(
                ns("carte_indic"), label = NULL,
                choices = c(c("Réchauffement (°C)" = "rechauffement"),
                            if (indicateurs_disponibles)
                              c("Jours de forte chaleur" = "forte_chaleur",
                                "Jours de gel" = "gel")),
                selected = "rechauffement", size = "sm", status = "primary")),
          leafletOutput(ns("carte"), height = "500px"),
          hr(),
          uiOutput(ns("titre_trajectoire")),
          plotlyOutput(ns("graphe_ville"), height = "320px"),
          # Boucle de circulation : de la trajectoire vers l'analyse complète.
          div(class = "text-center mt-2 mb-1", uiOutput(ns("lien_evolution")))
        )
      )
    )
  )
}

mod_comparer_server <- function(id, db_pool, prefill = reactive(NULL), naviguer = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Drapeau : une ville a été choisie pendant que la carte était masquée -> on
    # recentrera à la réouverture de l'onglet carte.
    centrage_en_attente <- reactiveVal(FALSE)

    # Pré-remplissage (permalien ?onglet=comparer&ville=...&annee=... ou lien
    # interne, ex. année cliquée dans Évolution). `vue` cible la sous-vue
    # (« courbe » / « carte ») ; le recentrage carte est déjà géré par
    # l'observeEvent(ville_focus) existant.
    observeEvent(prefill(), {
      pf <- prefill()
      req(pf)
      if (!is.null(pf$ville)) updatePickerInput(session, "ville_focus", selected = pf$ville)
      if (!is.null(pf$annee)) updateSliderInput(session, "annee", value = pf$annee)
      if (!is.null(pf$vue) && pf$vue %in% c("courbe", "carte"))
        bslib::nav_select("sousvue", pf$vue, session = session)
    })

    # Le curseur « Année » est fait pour être GLISSÉ (scrubber commun aux deux vues).
    # Chaque cran émettait une valeur -> requêtes + reconstruction ggplotly par cran.
    # On débounce : les données/graphes ne se rafraîchissent qu'à la fin du geste
    # (250 ms sans mouvement). Le titre, lui, reste sur input$annee (retour immédiat).
    annee_deb <- debounce(reactive(input$annee), 250)

    # --- État partagé : la ville focus est pilotée par le sélecteur ET par les
    #     clics sur la carte. Le sélecteur reste la source de vérité ; un clic
    #     écrit dedans via updatePickerInput. -------------------------------------
    observeEvent(input$carte_marker_click, {
      clk <- input$carte_marker_click
      req(clk$id)
      if (!identical(clk$id, input$ville_focus))
        updatePickerInput(session, "ville_focus", selected = clk$id)
    })

    # Recentre la carte sur la ville (lng, lat) en préservant le zoom courant
    # (isolate → pas de relance à chaque +/-). Repli zoom 5 si non connu.
    recentrer_sur_ville <- function(ville) {
      co <- villes_coords %>% filter(ville == !!ville)
      if (nrow(co) != 1) return(invisible())
      z <- isolate(input$carte_zoom)
      leafletProxy("carte", session) %>%
        flyTo(lng = co$longitude, lat = co$latitude, zoom = if (is.null(z)) 5 else z)
    }

    # Recentrer la carte sur la ville sélectionnée (liste OU clic d'une pastille) :
    # sur petit écran on ne voit pas toutes les villes à la fois. Si l'onglet carte
    # est visible, on recentre tout de suite ; sinon on note le recentrage en attente
    # (réalisé à la réouverture de l'onglet). ignoreInit : on garde la vue « France
    # entière » au démarrage (aucune ville encore choisie).
    observeEvent(input$ville_focus, {
      if (identical(input$sousvue, "carte")) {
        recentrer_sur_ville(input$ville_focus)
      } else {
        centrage_en_attente(TRUE)
      }
    }, ignoreInit = TRUE)

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
    # Garde anti-course : vrai quand le conteneur de la courbe a une taille réelle
    # (l'arrivée par navigation programmée — clic d'une année dans Évolution —
    # peut déclencher le rendu en pleine transition d'onglet, à taille nulle).
    dims_courbe_ok <- reactive({
      dimensions_valides(session, ns("climate_plot"))
    })

    output$titre_courbe <- renderUI({
      req(input$ville_focus, input$annee, input$periode)
      tagList(
        HTML(paste0("Températures maximales à <strong>", input$ville_focus,
                    "</strong> en <strong>", input$annee, "</strong>")),
        tags$div(class = "small text-muted fw-normal",
                 paste0("Comparaison à la normale climatique ", input$periode,
                        " · cliquez un jour pour l'analyser"))
      )
    })

    plot_data_annee <- reactive({
      req(input$ville_focus, annee_deb())
      annee <- annee_deb()
      start_date <- as.Date(paste0(annee, "-01-01"))
      end_date   <- as.Date(paste0(annee + 1, "-01-01"))
      tbl(db_pool, "temperatures_max") %>%
        filter(ville == !!input$ville_focus, date >= !!start_date, date < !!end_date) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max)
    }) %>% bindCache(input$ville_focus, annee_deb())

    plot_data_normale <- reactive({
      req(input$ville_focus, input$periode, annee_deb())
      annee_origine <- paste0(annee_deb(), "-01-01")
      tbl(db_pool, "stats_normales") %>%
        filter(ville == !!input$ville_focus, periode_ref == !!input$periode) %>%
        collect() %>%
        filter(!(mois == 2 & jour_mois == 29 & !leap_year(as.Date(annee_origine)))) %>%
        mutate(date = as.Date(paste(year(as.Date(annee_origine)), mois, jour_mois, sep = "-")))
    }) %>% bindCache(input$ville_focus, input$periode, annee_deb())

    output$climate_plot <- renderPlotly({
      req(dims_courbe_ok(), input$ville_focus, input$periode, annee_deb())
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

      gp <- ggplotly(p, tooltip = "text", source = ns("courbe_clic")) %>%
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
      # Clic sur un jour -> « Une journée » (cf. observeEvent plus bas).
      plotly::event_register(gp, "plotly_click")
    }) %>%
      bindCache(input$ville_focus, input$periode, annee_deb(), est_mobile_courbe(),
                dims_courbe_ok())

    # Clic sur un jour de la courbe -> onglet « Une journée » sur (ville, date).
    # Le x du clic arrive dans un format qui dépend de la conversion ggplotly
    # (chaîne ISO ou nombre) : date_click_plotly le normalise. On ne navigue que
    # si la date tombe dans l'année affichée. suppressWarnings : avant le premier
    # rendu, event_data avertit que la source n'est pas encore enregistrée.
    observeEvent(suppressWarnings(
      plotly::event_data("plotly_click", source = ns("courbe_clic"),
                         priority = "event")), {
      d <- suppressWarnings(
        plotly::event_data("plotly_click", source = ns("courbe_clic")))
      dt <- date_click_plotly(d$x)
      req(length(dt) == 1, !is.na(dt),
          lubridate::year(dt) == annee_deb(), input$ville_focus)
      if (is.function(naviguer)) naviguer("jour", ville = input$ville_focus, date = dt)
    })

    # Lien « jour le plus chaud de l'année » : la date du max vient des données
    # déjà chargées pour la courbe (aucune requête supplémentaire).
    output$lien_jour_chaud <- renderUI({
      d <- tryCatch(plot_data_annee(), error = function(e) NULL)
      req(!is.null(d), nrow(d) > 0)
      actionLink(ns("jour_chaud_btn"),
                 label = tagList(icon("temperature-high"),
                                 sprintf(" Analyser le jour le plus chaud de %d",
                                         annee_deb())))
    })

    observeEvent(input$jour_chaud_btn, {
      d <- tryCatch(plot_data_annee(), error = function(e) NULL)
      req(!is.null(d), nrow(d) > 0, input$ville_focus)
      date_max <- as.Date(d$date[which.max(d$tmax_celsius)])
      if (is.function(naviguer)) naviguer("jour", ville = input$ville_focus, date = date_max)
    })

    # ================ VUE « ENTRE LES VILLES » : carte + trajectoire =============

    # Période « récente » = la plus récente disponible (référent du réchauffement).
    periode_recente <- periodes_disponibles[which.max(
      vapply(periodes_disponibles, function(p) .periode_bornes(p)[1], numeric(1)))]

    # Moyenne annuelle de la normale (tmax) d'une période, par ville.
    moy_normale_periode <- function(per) {
      tbl(db_pool, "stats_normales") %>%
        filter(periode_ref == !!per) %>%
        group_by(ville) %>%
        summarise(moy = mean(t_moy, na.rm = TRUE), .groups = "drop") %>%
        collect()
    }
    # Moyennes annuelles des indicateurs (forte chaleur, gel) sur une plage d'années
    # COMPLÈTES, par ville.
    moy_indicateurs_periode <- function(bornes) {
      tbl(db_pool, "indicateurs_annuels") %>%
        filter(annee >= !!bornes[1], annee <= !!bornes[2], nb_jours >= 360L) %>%
        group_by(ville) %>%
        summarise(chaleur = mean(jours_forte_chaleur, na.rm = TRUE),
                  gel = mean(jours_gel, na.rm = TRUE), .groups = "drop") %>%
        collect()
    }

    # Valeur cartographiée par ville = ÉCART entre la période la plus récente et la
    # période de référence SÉLECTIONNÉE (input$periode), pour l'indicateur choisi.
    # STATIQUE (ne dépend pas de l'année). Jointe aux coordonnées des villes.
    carte_data <- reactive({
      req(input$periode)
      indic <- if (is.null(input$carte_indic)) "rechauffement" else input$carte_indic
      P <- input$periode
      d <- if (indic == "rechauffement") {
        mr <- moy_normale_periode(periode_recente)
        mp <- moy_normale_periode(P)
        mr %>% rename(r = moy) %>%
          inner_join(mp %>% rename(p = moy), by = "ville") %>%
          transmute(ville, valeur = r - p)
      } else {
        if (!indicateurs_disponibles) return(NULL)
        ir <- moy_indicateurs_periode(.periode_bornes(periode_recente))
        ip <- moy_indicateurs_periode(.periode_bornes(P))
        col <- if (indic == "forte_chaleur") "chaleur" else "gel"
        ir %>% inner_join(ip, by = "ville", suffix = c("_r", "_p")) %>%
          transmute(ville, valeur = .data[[paste0(col, "_r")]] - .data[[paste0(col, "_p")]])
      }
      if (is.null(d)) return(NULL)
      d %>% right_join(villes_coords, by = "ville")
    }) %>% bindCache(input$periode, input$carte_indic)

    # Config d'affichage selon l'indicateur : palette divergente (rouge = hausse),
    # domaine symétrique calé sur les données, libellés et unité.
    config_carte <- function(indic, vals) {
      vals <- vals[is.finite(vals)]
      plancher <- if (indic == "rechauffement") 1 else 5
      D <- max(plancher, if (length(vals)) ceiling(max(abs(vals))) else plancher)
      dom <- c(-D, D)
      list(
        dom = dom,
        pal = colorNumeric("RdBu", domain = dom, reverse = TRUE, na.color = "#9aa0a6"),
        unite = if (indic == "rechauffement") "°C" else "j/an",
        titre_leg = switch(indic, rechauffement = "Réchauffement (°C)",
                           forte_chaleur = "Forte chaleur (Δ j/an)", gel = "Gel (Δ j/an)"),
        libelle = switch(indic, rechauffement = "Réchauffement",
                         forte_chaleur = "Jours de forte chaleur (écart)",
                         gel = "Jours de gel (écart)")
      )
    }
    fmt_val <- function(x, unite) if (!is.finite(x)) "n/d" else paste0(sprintf("%+.1f", x), " ", unite)

    construire_popups <- function(df, cfg) {
      vapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        paste0("<b>", r$ville, "</b>",
               "<br>", cfg$libelle, " : <b>", fmt_val(r$valeur, cfg$unite), "</b>",
               "<br><span style='color:#6c757d'>", input$periode, " → ", periode_recente, "</span>",
               "<br><i>Cliquez pour basculer la ville focus</i>")
      }, character(1))
    }

    # Toutes les villes sont affichées (pas de filtre) : la ville focus est
    # simplement surlignée. layerId = ville -> remonte dans carte_marker_click$id.
    ajouter_marqueurs <- function(carte, df, cfg) {
      if (nrow(df) == 0) return(carte)
      clamp <- function(x) pmax(cfg$dom[1], pmin(cfg$dom[2], x))
      addCircleMarkers(
        carte, data = df, lng = ~longitude, lat = ~latitude,
        group = "villes", layerId = ~ville,
        radius = 11, stroke = TRUE, color = "#444444", weight = 1,
        fillColor = ~cfg$pal(clamp(valeur)), fillOpacity = 0.9,
        label = ~ville, popup = construire_popups(df, cfg)
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
      req(input$periode)
      indic <- if (is.null(input$carte_indic)) "rechauffement" else input$carte_indic
      lib <- switch(indic, rechauffement = "Réchauffement",
                    forte_chaleur = "Évolution des jours de forte chaleur",
                    gel = "Évolution des jours de gel")
      tagList(
        HTML(paste0("<strong>", lib, "</strong> par ville : de la normale <strong>",
                    input$periode, "</strong> à <strong>", periode_recente, "</strong>")),
        tags$div(class = "small text-muted fw-normal",
                 "Cliquez une pastille pour basculer la ville focus.")
      )
    })

    # Carte de base + pastilles + surbrillance initiales dessinées ICI (isolate)
    # pour éviter la race condition leafletProxy/onglet masqué au démarrage.
    output$carte <- renderLeaflet({
      df0 <- isolate(carte_data())
      indic0 <- isolate(if (is.null(input$carte_indic)) "rechauffement" else input$carte_indic)
      cfg0 <- config_carte(indic0, if (is.null(df0)) numeric(0) else df0$valeur)
      # Carte volontairement « figée » : seul le zoom par les boutons +/- est
      # autorisé. On désactive la molette, le double-clic, le pincé tactile, le
      # zoom-cadre, le clavier ET le déplacement (dragging) — pour éviter les zooms
      # accidentels et, surtout sur smartphone, pour que le geste de défilement
      # fasse défiler la PAGE (et non la carte) jusqu'au graphique du bas.
      carte <- leaflet(options = leafletOptions(
        minZoom = 4,
        scrollWheelZoom = FALSE, doubleClickZoom = FALSE, touchZoom = FALSE,
        boxZoom = FALSE, dragging = FALSE, keyboard = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(-5.5, 41.0, 9.8, 51.5) %>%
        addLegend("bottomright", pal = cfg0$pal, values = cfg0$dom, bins = 4,
                  title = cfg0$titre_leg, opacity = 0.9, layerId = "legende")
      if (!is.null(df0)) carte <- ajouter_marqueurs(carte, df0, cfg0)
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
      df <- carte_data()
      req(!is.null(df))
      indic <- if (is.null(input$carte_indic)) "rechauffement" else input$carte_indic
      cfg <- config_carte(indic, df$valeur)
      leafletProxy("carte", session) %>%
        clearGroup("villes")       %>% ajouter_marqueurs(df, cfg) %>%
        clearGroup("surbrillance") %>% surligner(input$ville_focus) %>%
        removeControl("legende") %>%
        addLegend("bottomright", pal = cfg$pal, values = cfg$dom, bins = 4,
                  title = cfg$titre_leg, opacity = 0.9, layerId = "legende")
      # Recentrage différé : si la ville a changé pendant que la carte était masquée,
      # on recentre maintenant qu'elle est (ré)affichée, puis on lève le drapeau.
      if (isolate(centrage_en_attente())) {
        recentrer_sur_ville(input$ville_focus)
        centrage_en_attente(FALSE)
      }
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
      # Même garde anti-course que la courbe : ce graphe vit dans le sous-onglet
      # carte, initialement masqué (cf. erreurs « width/height incorrecte »).
      req(dimensions_valides(session, ns("graphe_ville")), v)
      # Agrégat chargé PARESSEUSEMENT au premier affichage de cette trajectoire
      # (cf. global.R : obtenir_anomalies_villes_annee), plus au démarrage de l'app.
      anomalies <- obtenir_anomalies_villes_annee()
      if (is.null(anomalies))
        return(plotly_empty(type = "scatter", mode = "lines") %>%
                 layout(annotations = list(list(
                   text = "Trajectoire indisponible.",
                   showarrow = FALSE, font = list(color = "#6c757d", size = 15)))) %>%
                 config(displayModeBar = FALSE))

      mob <- est_mobile_traj()

      ensemble <- anomalies %>%
        group_by(annee) %>%
        summarise(moy = mean(anomalie, na.rm = TRUE),
                  bas = min(anomalie, na.rm = TRUE),
                  haut = max(anomalie, na.rm = TRUE), .groups = "drop") %>%
        mutate(text_moy = paste0("Année ", annee,
                                 "<br>Moyenne des villes : ", fmt_temp(moy, 2), " °C"))
      df_ville <- anomalies %>% filter(ville == v) %>% arrange(annee) %>%
        mutate(text = paste0("Année ", annee, "<br>", v, " : ", fmt_temp(anomalie, 2), " °C"))

      p <- ggplot() +
        geom_ribbon(data = ensemble, aes(x = annee, ymin = bas, ymax = haut),
                    fill = "grey80", alpha = 0.5) +
        geom_line(data = ensemble, aes(x = annee, y = moy, color = "Moyenne des villes"),
                  linewidth = 0.7, linetype = "dashed") +
        geom_point(data = ensemble, aes(x = annee, y = moy, text = text_moy), alpha = 0) +
        geom_line(data = df_ville, aes(x = annee, y = anomalie, color = v), linewidth = 1) +
        geom_point(data = df_ville, aes(x = annee, y = anomalie, text = text), alpha = 0) +
        geom_hline(yintercept = 0, color = "#343a40", linewidth = 0.4) +
        geom_vline(xintercept = annee_deb(), color = "#6c757d",
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

    # Lien de la trajectoire vers l'analyse longue durée de la ville focus.
    output$lien_evolution <- renderUI({
      req(input$ville_focus)
      actionLink(ns("evolution_btn"),
                 label = tagList(icon("chart-line"),
                                 paste0(" Voir ", an_max_data - an_min_data,
                                        " ans d'évolution ", autour_de(input$ville_focus))))
    })

    observeEvent(input$evolution_btn, {
      req(input$ville_focus)
      if (is.function(naviguer)) naviguer("evolution", ville = input$ville_focus)
    })

    # On laisse suspendWhenHidden au défaut (TRUE) : la carte se rend ainsi À
    # L'AFFICHAGE de son onglet — donc à taille correcte — plutôt qu'au démarrage,
    # masquée et à taille nulle (ce qui la laissait grise). Les mises à jour
    # passent ensuite par l'observe ci-dessus, gardé visible-only.

    # État exposé à server.R pour le permalien (?onglet=comparer&ville=...&annee=...).
    return(list(
      etat_url = reactive(list(ville = input$ville_focus, annee = input$annee))
    ))

  })
}
