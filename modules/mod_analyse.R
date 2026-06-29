# modules/mod_analyse.R

# --- UI MISE À JOUR ---
mod_analyse_ui <- function(id) {
  ns <- NS(id)

  # Note d'information repliée par défaut (<details> natif, sans JS) : allège la
  # page, le détail s'ouvre au clic sur le résumé.
  info_repliable <- function(...) {
    tags$details(
      class = "small text-muted",
      tags$summary(style = "cursor: pointer;",
                   icon("circle-info"), " À propos de ces chiffres"),
      div(class = "mt-2", ...)
    )
  }

  tagList(
    # Hauteur du graphe d'évolution réduite en portrait pour limiter le scroll
    # (la police/les ticks sont, eux, adaptés côté serveur via clientData).
    tags$head(tags$style(HTML("
      .evolution-plot-wrap { height: 500px; }
      @media (max-width: 575.98px) { .evolution-plot-wrap { height: 420px; } }
    "))),
    div(
      class = "p-2",
      h5(class = "mb-2", "Analyser l'évolution des températures"),

      # Barre de réglages EN LIGNE (remplace la sidebar) : ville, période d'analyse
      # et normale de référence, visibles d'emblée au-dessus des graphes. Menus hors
      # flux (container = "body") pour éviter tout double défilement.
      card(
        class = "mb-2",
        card_body(
          class = "py-2",
          layout_columns(
            col_widths = breakpoints(sm = c(4, 4, 4)),
            pickerInput(ns("ville_analyse"), "Ville :",
                        choices = villes_triees, selected = villes_triees[1],
                        options = list(container = "body", `live-search` = TRUE, size = 8)),
            sliderInput(ns("annee_range_analyse"), "Période d'analyse :",
                        min = an_min_data, max = an_max_data,
                        value = c(an_min_data, an_max_data), sep = ""),
            pickerInput(ns("periode_ref_analyse"), "Période de référence (normale) :",
                        choices = periodes_disponibles, selected = periodes_disponibles[1],
                        options = list(container = "body", `live-search` = FALSE))
          ),
          helpText("Chaque barre indique l'écart de la moyenne annuelle des températures maximales par rapport à la normale de la période de référence choisie.")
        )
      ),

      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_evolution"))),
        uiOutput(ns("analyse_rechauffement_ui")),
        div(class = "evolution-plot-wrap",
            plotlyOutput(ns("evolution_plot"), height = "100%")),
        card_footer(
          info_repliable(HTML(paste(
            "Ces indicateurs reposent sur les températures <b>maximales</b> journalières",
            "(réanalyse ERA5-Land), choisies pour leur lisibilité.",
            "Les références climatiques officielles (Météo-France, GIEC) s'appuient sur la",
            "température <b>moyenne</b> et sur des séries de stations homogénéisées.",
            "Ces valeurs ne sont donc pas directement comparables aux chiffres officiels :",
            "elles illustrent des tendances, et non des références absolues."
          )))
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_forte_chaleur"))),
        plotOutput(ns("forte_chaleur_plot"), height = "380px"),
        card_footer(
          info_repliable(HTML(paste(
            "Nombre moyen de jours par an. Vers le <b>haut</b>, les <b>jours de forte",
            "chaleur</b> : tmax au-dessus du <b>seuil local</b> (90ᵉ percentile des",
            "tmax estivales 1973-2003 — une journée parmi les 10 % d'étés les plus chauds).",
            "Vers le <b>bas</b>, les <b>jours de gel</b> (tmin ≤ 0 °C).",
            "Avec le réchauffement, la chaleur monte et le gel recule.",
            "<br>Les colonnes <b>projetées</b> (TRACC, +2,7 °C et +4 °C) appliquent les",
            "mêmes définitions aux projections DRIAS : <b>médiane de 17 simulations",
            "régionales</b>."
          )))
        )
      )
    )
  )
}


# --- SERVER MIS À JOUR ---
# Remplacez l'intégralité de l'ancienne fonction mod_analyse_server par celle-ci

mod_analyse_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # Largeur réelle (px) du graphe d'évolution, transmise par le client.
    # Booléen (et non largeur brute) pour ne ré-invalider le rendu qu'au
    # franchissement du seuil ~500px (≈ smartphone en portrait), pas à chaque pixel.
    est_mobile_evolution <- reactive({
      largeur_sous_seuil(session, ns("evolution_plot"))
    })
    # Idem pour le graphe forte chaleur / gel (rendu statique ggplot).
    est_mobile_chaleur <- reactive({
      largeur_sous_seuil(session, ns("forte_chaleur_plot"))
    })

    resultats_defaut <- reactiveVal(NULL)
    
    donnees_long_terme <- reactive({
      req(input$ville_analyse, input$annee_range_analyse)
      
      annee_debut <- input$annee_range_analyse[1]
      annee_fin <- input$annee_range_analyse[2]
      start_date <- as.Date(paste0(input$annee_range_analyse[1], "-01-01"))
      end_date <- as.Date(paste0(input$annee_range_analyse[2] + 1, "-01-01"))
      
      tbl(db_pool, "temperatures_max") %>%
        filter(
          ville == !!input$ville_analyse,
          date >= !!start_date,
          date < !!end_date
        ) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max) %>%
        mutate(
          annee = year(date),
          mois = month(date)
        ) %>%
        arrange(date)

    }) %>% bindCache(input$ville_analyse, input$annee_range_analyse)

    # Anomalies annuelles : écart de la moyenne annuelle des températures
    # maximales par rapport à la normale de la période de référence choisie.
    anomalies_annuelles <- reactive({
      req(input$ville_analyse, input$periode_ref_analyse)
      df <- donnees_long_terme()
      req(nrow(df) > 0)

      # Normales JOURNALIÈRES (par jour calendaire) pour la période de référence.
      # On compare ainsi chaque jour observé à la normale du même jour : une année
      # partielle (ex. janvier-juin) est confrontée aux seules normales de ces
      # jours, et non à la normale annuelle complète qui inclut l'été.
      normales_jour <- tbl(db_pool, "stats_normales") %>%
        filter(
          ville == !!input$ville_analyse,
          periode_ref == !!input$periode_ref_analyse
        ) %>%
        select(mois, jour_mois, t_moy) %>%
        collect()

      req(nrow(normales_jour) > 0)

      # Seule la dernière année de l'historique peut être partielle : on la repère
      # si ses données ne vont pas jusqu'au 31 décembre.
      derniere_date <- max(df$date)
      fin_derniere_annee <- as.Date(sprintf("%d-12-31", year(derniere_date)))
      annee_incomplete <- if (derniere_date < fin_derniere_annee) year(derniere_date) else NA_integer_

      df %>%
        mutate(jour_mois = day(date)) %>%
        inner_join(normales_jour, by = c("mois", "jour_mois")) %>%
        group_by(annee) %>%
        summarise(
          moy_annuelle = mean(tmax_celsius, na.rm = TRUE),
          # Normale moyenne sur exactement les mêmes jours que ceux observés.
          normale = mean(t_moy, na.rm = TRUE),
          n_jours = n(),
          .groups = "drop"
        ) %>%
        mutate(
          anomalie = moy_annuelle - normale,
          complete = is.na(annee_incomplete) | annee != annee_incomplete,
          signe = ifelse(anomalie >= 0, "Au-dessus de la normale", "En-dessous de la normale")
        )
    }) %>% bindCache(input$ville_analyse, input$annee_range_analyse, input$periode_ref_analyse)
    
    observeEvent(donnees_long_terme(), {
      
      df_analyse <- donnees_long_terme()
      req(nrow(df_analyse) > 0) # S'assurer qu'on a des données

      # 1. Calcul de l'analyse du réchauffement
      # On exclut la dernière année si elle est partielle (données n'allant pas
      # jusqu'au 31/12) : une année incomplète fausserait la moyenne de la
      # dernière trentaine d'années (mois chauds manquants).
      derniere_date <- max(df_analyse$date)
      fin_derniere_annee <- as.Date(sprintf("%d-12-31", year(derniere_date)))
      annee_incomplete <- if (derniere_date < fin_derniere_annee) year(derniere_date) else NA_integer_
      df_complet <- if (!is.na(annee_incomplete)) {
        df_analyse %>% filter(annee != annee_incomplete)
      } else {
        df_analyse
      }

      annee_debut <- min(df_complet$annee)
      annee_fin <- max(df_complet$annee)

      stats_30ans <- NULL
      if ((annee_fin - annee_debut) >= 29) {
        moyenne_debut_periode <- df_complet %>%
          filter(annee >= annee_debut, annee <= annee_debut + 29) %>%
          summarise(moyenne = mean(tmax_celsius, na.rm = TRUE)) %>% pull(moyenne)

        moyenne_fin_periode <- df_complet %>%
          filter(annee >= annee_fin - 29, annee <= annee_fin) %>%
          summarise(moyenne = mean(tmax_celsius, na.rm = TRUE)) %>% pull(moyenne)

        stats_30ans <- list(
          periode1_str = paste0(annee_debut, "-", annee_debut + 29), moyenne1 = moyenne_debut_periode,
          periode2_str = paste0(annee_fin - 29, "-", annee_fin), moyenne2 = moyenne_fin_periode
        )
      }
      
      # On stocke ces résultats dans notre reactiveVal
      resultats_defaut(list(
        ville = input$ville_analyse,
        annee_range = input$annee_range_analyse,
        stats_30ans = stats_30ans
      ))

    }, ignoreNULL = TRUE)
    
    output$evolution_plot <- renderPlotly({
      df_anom <- anomalies_annuelles()
      req(nrow(df_anom) > 0)

      # Infobulle : valeur de l'anomalie, moyenne annuelle et normale de référence.
      df_anom <- df_anom %>%
        mutate(
          text = paste0(
            "Année : ", annee,
            "<br>Écart à la normale : ", sprintf("%+.2f", anomalie), " °C",
            "<br>Moyenne annuelle : ", round(moy_annuelle, 1), " °C",
            "<br>Normale (", input$periode_ref_analyse, ") : ", round(normale, 1), " °C",
            ifelse(complete, "", "<br><i>année incomplète</i>")
          )
        )

      # Rendu adapté à la largeur réelle : sur smartphone (portrait), police plus
      # petite, ticks plus espacés et légende compacte pour rester lisible.
      mob <- est_mobile_evolution()
      taille_police  <- if (mob) 11 else 14
      taille_legende <- if (mob) 10 else 12
      pas_ticks      <- if (mob) 20 else 10

      # Ticks de l'axe X calculés ici puis imposés à plotly : ggplotly ignore les
      # `breaks` de ggplot et génère sinon une nuée d'étiquettes illisibles.
      rng_annees <- range(df_anom$annee)
      tickvals <- seq(ceiling(rng_annees[1] / pas_ticks) * pas_ticks,
                      floor(rng_annees[2] / pas_ticks) * pas_ticks, by = pas_ticks)

      p <- ggplot(df_anom, aes(x = annee, y = anomalie)) +
        # Barres colorées bleu (sous la normale) / rouge (au-dessus),
        # transparence réduite pour les années incomplètes.
        geom_col(aes(fill = signe, alpha = complete, text = text), width = 0.8) +
        # Ligne de référence : la normale (anomalie nulle).
        geom_hline(yintercept = 0, color = "#343a40", linewidth = 0.5) +
        # Tendance de fond pour visualiser la trajectoire de réchauffement.
        geom_smooth(method = "loess", se = FALSE, color = "#343a40",
                    linewidth = 0.9, linetype = "dashed", na.rm = TRUE) +
        scale_fill_manual(
          values = c(
            "Au-dessus de la normale" = "#E41A1C",
            "En-dessous de la normale" = "#1f77b4"
          )
        ) +
        scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4), guide = "none") +
        labs(y = "Écart à la normale (°C)", x = "Année", fill = "") +
        theme_minimal(base_size = taille_police) +
        theme(legend.position = "bottom")

      gp <- ggplotly(p, tooltip = "text") %>%
        # Axes verrouillés (pas de zoom/déplacement) ; ticks X imposés ; légende
        # en bande horizontale compacte au-dessus du graphe.
        layout(
          xaxis = list(fixedrange = TRUE, tickmode = "array",
                       tickvals = tickvals, ticktext = as.character(tickvals),
                       tickangle = -45),
          yaxis = list(fixedrange = TRUE),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = 1.02, yanchor = "bottom",
                        font = list(size = taille_legende))
        ) %>%
        config(displayModeBar = FALSE, responsive = TRUE)

      # ggplotly combine les esthétiques fill+alpha dans les libellés de légende
      # (ex. "(Au-dessus de la normale,TRUE)"). On retire la partie TRUE/FALSE et
      # les parenthèses, puis on masque les entrées en double ainsi générées.
      deja_vu <- character(0)
      for (i in seq_along(gp$x$data)) {
        nom <- gp$x$data[[i]]$name
        if (is.null(nom)) next
        nom <- gsub("[()]", "", nom)
        nom <- trimws(gsub(",?(TRUE|FALSE)", "", nom))
        # Libellés courts ("Au-dessus" / "En-dessous") : plus compacts sur mobile,
        # le code rouge/bleu autour de la ligne zéro restant explicite.
        nom <- sub(" de la normale", "", nom)
        gp$x$data[[i]]$name <- nom
        gp$x$data[[i]]$legendgroup <- nom
        if (nom == "" || nom %in% deja_vu) {
          gp$x$data[[i]]$showlegend <- FALSE
        } else {
          deja_vu <- c(nom, deja_vu)
        }
      }

      gp
    }) %>%
      bindCache(
        input$ville_analyse,
        input$annee_range_analyse,
        input$periode_ref_analyse,
        est_mobile_evolution()
      )
    
    # Texte « Analyse du réchauffement » — affiché JUSTE AU-DESSUS du graphe
    # d'évolution (comparaison des moyennes de tmax entre les deux trentaines).
    output$analyse_rechauffement_ui <- renderUI({
      req(resultats_defaut())
      stats <- resultats_defaut()$stats_30ans
      if (is.null(stats)) return(NULL)
      diff <- round(stats$moyenne2 - stats$moyenne1, 2)
      p(class = "text-muted small mb-2", HTML(paste0(
        "<b>Analyse du réchauffement :</b> ",
        "La température maximale moyenne sur la période <b>", stats$periode1_str, "</b> était de <b>", round(stats$moyenne1, 2), "°C</b>. ",
        "Pour la période <b>", stats$periode2_str, "</b>, elle a atteint <b>", round(stats$moyenne2, 2), "°C</b>, ",
        "soit une augmentation de <b>", diff, "°C</b>."
      )))
    })
    
    # --- Forte chaleur & gel (nécessitent la mise à jour tmin du pipeline) ---
    # État de repli affiché tant que les données ne sont pas disponibles.
    etat_indisponible <- function(msg) {
      ggplot() +
        annotate("text", x = 0, y = 0, size = 5, color = "grey40",
                 label = paste(strwrap(msg, width = 50), collapse = "\n")) +
        theme_void()
    }

    # Jours de FORTE CHALEUR projetés (TRACC) par niveau, pour la ville.
    # Indépendants de la plage d'années observée (ce sont des horizons futurs).
    extremes_proj_ville <- reactive({
      req(input$ville_analyse)
      tryCatch(
        tbl(db_pool, "extremes_projetes") %>%
          filter(ville == !!input$ville_analyse) %>%
          collect(),
        error = function(e) NULL)
    }) %>% bindCache(input$ville_analyse)

    # Seuil de forte chaleur de la ville (90e pct de la tmax estivale 1973-2003),
    # recalculé à la volée — même définition que le pipeline — pour l'afficher.
    seuil_fc_ville <- reactive({
      req(input$ville_analyse)
      tryCatch({
        d <- tbl(db_pool, "temperatures_max") %>%
          filter(ville == !!input$ville_analyse, annee >= 1973L, annee <= 2003L,
                 mois %in% c(6L, 7L, 8L)) %>%
          select(temperature_max) %>% collect()
        if (nrow(d) == 0) return(NA_real_)
        as.numeric(quantile(d$temperature_max, 0.90, na.rm = TRUE))
      }, error = function(e) NA_real_)
    }) %>% bindCache(input$ville_analyse)

    indicateurs_ville <- reactive({
      req(input$ville_analyse, input$annee_range_analyse)
      if (!indicateurs_disponibles) return(NULL)
      a1 <- input$annee_range_analyse[1]; a2 <- input$annee_range_analyse[2]
      tryCatch(
        tbl(db_pool, "indicateurs_annuels") %>%
          filter(ville == !!input$ville_analyse, annee >= !!a1, annee <= !!a2) %>%
          select(annee, jours_gel, jours_forte_chaleur, nb_jours) %>%
          collect(),
        error = function(e) NULL)
    }) %>% bindCache(input$ville_analyse, input$annee_range_analyse, indicateurs_disponibles)

    # Titres dynamiques des cartes : la ville étant choisie dans la sidebar (en
    # bas de page sur mobile), on la rappelle dans l'en-tête de chaque graphe.
    output$titre_evolution <- renderUI({
      req(input$ville_analyse)
      tagList(
        HTML(paste0("Évolution des températures maximales à <strong>",
                    input$ville_analyse, "</strong>")),
        tags$div(class = "small text-muted fw-normal",
                 paste0("Écart annuel à la normale ", input$periode_ref_analyse))
      )
    })

    output$titre_forte_chaleur <- renderUI({
      req(input$ville_analyse)
      seuil <- seuil_fc_ville()
      sous <- if (is.finite(seuil))
        paste0("Seuil de forte chaleur : ",
               format(round(seuil, 1), nsmall = 1, decimal.mark = ","),
               " °C (90ᵉ pct des étés 1973-2003)") else NULL
      tagList(
        HTML(paste0("Jours de forte chaleur vs jours de gel à <strong>",
                    input$ville_analyse, "</strong>, par an")),
        if (!is.null(sous)) tags$div(class = "small text-muted fw-normal", sous)
      )
    })

    # Graphe DIVERGENT fusionné : forte chaleur (P90) vers le haut, gel (≤0°C)
    # vers le bas ; décennies observées + horizons projetés (médiane TRACC).
    output$forte_chaleur_plot <- renderPlot({
      df   <- indicateurs_ville()
      proj <- extremes_proj_ville()
      obs_ok  <- !is.null(df) && nrow(df) > 0 && "jours_forte_chaleur" %in% names(df)
      proj_ok <- !is.null(proj) && nrow(proj) > 0
      if (!obs_ok && !proj_ok) return(etat_indisponible(
        "Indicateur disponible après la mise à jour des données."))

      # Observé par décennie (jours/an moyens). Années incomplètes (ex. 2026
      # partielle) écartées pour ne pas sous-estimer le compte estival.
      obs <- if (obs_ok) {
        df %>%
          filter(nb_jours >= 360) %>%
          mutate(decennie = floor(annee / 10) * 10) %>%
          group_by(decennie) %>%
          summarise(chaleur = mean(jours_forte_chaleur, na.rm = TRUE),
                    gel = mean(jours_gel, na.rm = TRUE), .groups = "drop") %>%
          transmute(cat = as.character(decennie), ordre = decennie, chaleur, gel)
      } else {
        tibble(cat = character(), ordre = numeric(), chaleur = numeric(), gel = numeric())
      }

      # Projeté (médiane TRACC) : forte chaleur + gel, à droite.
      barres <- obs
      if (proj_ok) {
        labels_niv <- c("2050_+2.7" = "~2050\n+2,7 °C", "2100_+4.0" = "~2100\n+4 °C")
        ordres_niv <- c("2050_+2.7" = 9990, "2100_+4.0" = 9991)
        pr <- proj %>%
          filter(niveau_rechauffement %in% names(labels_niv)) %>%
          transmute(cat = unname(labels_niv[niveau_rechauffement]),
                    ordre = unname(ordres_niv[niveau_rechauffement]),
                    chaleur = jours_chaleur, gel = jours_gel)
        barres <- bind_rows(obs, pr)
      }
      barres <- barres %>% arrange(ordre) %>% mutate(cat = factor(cat, levels = cat))

      # Format long divergent : chaleur vers le haut, gel vers le bas (négatif).
      long <- barres %>%
        tidyr::pivot_longer(c(chaleur, gel), names_to = "indic", values_to = "n") %>%
        mutate(indicateur = ifelse(indic == "chaleur", "Forte chaleur", "Gel (≤ 0 °C)"),
               valeur = ifelse(indic == "gel", -n, n))

      # Le seuil de forte chaleur est désormais affiché dans l'en-tête de carte
      # (titre_forte_chaleur) où il peut s'enrouler — plus de sous-titre tronqué.

      # Rendu adapté à la largeur : sur mobile, police réduite, libellés X inclinés
      # (sinon les décennies se chevauchent) et étiquettes de valeurs plus petites.
      mob <- est_mobile_chaleur()
      taille_police  <- if (mob) 10 else 14
      taille_valeurs <- if (mob) 3   else 3.5
      angle_x        <- if (mob) 45  else 0
      hjust_x        <- if (mob) 1   else 0.5

      p <- ggplot(long, aes(x = cat, y = valeur, fill = indicateur)) +
        geom_col(alpha = 0.85) +
        geom_text(aes(label = round(abs(valeur)),
                      vjust = ifelse(valeur >= 0, -0.3, 1.2)), size = taille_valeurs) +
        geom_hline(yintercept = 0, color = "#343a40", linewidth = 0.4) +
        scale_fill_manual(values = c("Forte chaleur" = "#E41A1C", "Gel (≤ 0 °C)" = "#1f77b4"),
                          name = NULL) +
        labs(x = NULL, y = "Jours par an") +
        theme_minimal(base_size = taille_police) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = angle_x, hjust = hjust_x)) +
        scale_y_continuous(expand = expansion(mult = c(0.12, 0.12)))
      # Séparateur pointillé entre décennies observées et horizons projetés.
      if (proj_ok && nrow(obs) > 0)
        p <- p + geom_vline(xintercept = nrow(obs) + 0.5, linetype = "dashed", color = "grey55")
      p
    })

  })
}