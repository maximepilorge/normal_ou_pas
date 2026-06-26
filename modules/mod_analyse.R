# modules/mod_analyse.R

# --- UI MISE À JOUR ---
mod_analyse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Analyser l'évolution des températures",
      fillable = FALSE,
      
      sidebar = sidebar(
        width = "350px",
        card(
          card_header("Paramètres de la série temporelle"),
          pickerInput(ns("ville_analyse"),
                      "Choisissez une ville :", 
                      choices = villes_triees,
                      selected = villes_triees[1],
                      options = list('live-search' = FALSE)),
          sliderInput(ns("annee_range_analyse"),
                      "Période d'analyse :",
                      min = an_min_data,
                      max = an_max_data,
                      value = c(an_min_data, an_max_data),
                      sep = ""),
          pickerInput(ns("periode_ref_analyse"),
                      "Période de référence (normale) :",
                      choices = periodes_disponibles,
                      selected = periodes_disponibles[1],
                      options = list('live-search' = FALSE)),
          helpText("Chaque barre indique l'écart de la moyenne annuelle des températures maximales par rapport à la normale de la période de référence choisie.")
        ),
        card(
          card_header("Analyser un seuil de température"),
          p("Définissez un seuil pour identifier les jours de chaleur et analyser leur fréquence."),
          numericInput(ns("temp_seuil_analyse"), "Température maximale à analyser (°C) :",
                       value = 30, min = -20, max = 50),
          pickerInput(ns("saison_analyse"), 
                      "Filtrer par saison (optionnel) :",
                      choices = c("Toutes les saisons" = "all", 
                                  "Hiver" = "hiver", 
                                  "Printemps" = "printemps", 
                                  "Été" = "ete", 
                                  "Automne" = "automne"),
                      selected = "all",
                      options = list('live-search' = FALSE)),
          actionButton(ns("lancer_analyse_btn"), "Analyser le seuil", icon = icon("magnifying-glass-chart"), class = "btn-primary w-100 mt-3"),
          actionButton(ns("reset_analyse_btn"), "Réinitialiser l'analyse", icon = icon("refresh"), class = "btn-light w-100 mt-2")
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Évolution des températures maximales annuelles (écart à la normale)"),
        uiOutput(ns("analyse_rechauffement_ui")),
        plotlyOutput(ns("evolution_plot"), height = "500px"),
        card_footer(
          class = "small text-muted",
          icon("circle-info"),
          HTML(paste(
            "Ces indicateurs reposent sur les températures <b>maximales</b> journalières",
            "(réanalyse ERA5-Land), choisies pour leur lisibilité.",
            "Les références climatiques officielles (Météo-France, GIEC) s'appuient sur la",
            "température <b>moyenne</b> et sur des séries de stations homogénéisées.",
            "Ces valeurs ne sont donc pas directement comparables aux chiffres officiels :",
            "elles illustrent des tendances, et non des références absolues."
          ))
        )
      ),
      
      uiOutput(ns("resultats_seuil_ui")),

      card(
        full_screen = TRUE,
        card_header("Jours de forte chaleur vs jours de gel, par an"),
        plotOutput(ns("forte_chaleur_plot"), height = "380px"),
        card_footer(
          class = "small text-muted",
          icon("circle-info"),
          HTML(paste(
            "Nombre moyen de jours par an. Vers le <b>haut</b>, les <b>jours de forte",
            "chaleur</b> : tmax au-dessus du <b>seuil local</b> (90ᵉ percentile des",
            "tmax estivales 1973-2003 — une journée parmi les 10 % d'étés les plus chauds).",
            "Vers le <b>bas</b>, les <b>jours de gel</b> (tmin ≤ 0 °C).",
            "Avec le réchauffement, la chaleur monte et le gel recule.",
            "<br>Les colonnes <b>projetées</b> (TRACC, +2,7 °C et +4 °C) appliquent les",
            "mêmes définitions aux projections DRIAS : <b>médiane de 17 simulations",
            "régionales</b>."
          ))
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
    
    resultats_defaut <- reactiveVal(NULL)
    resultats_seuil <- reactiveVal(NULL) 
    
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
      
      # 4. On réinitialise l'analyse de seuil quand les données de base changent
      resultats_seuil(NULL)
      
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
        scale_x_continuous(breaks = scales::breaks_width(10)) +
        labs(y = "Écart à la normale (°C)", x = "Année", fill = "") +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      gp <- ggplotly(p, tooltip = "text") %>%
        # On verrouille les axes pour désactiver le zoom et le déplacement
        layout(
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
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
        input$periode_ref_analyse
      )
    
    observeEvent(list(input$ville_analyse, input$annee_range_analyse, input$saison_analyse), {
      shinyjs::click("reset_analyse_btn")
    }, ignoreInit = TRUE)
    
    observeEvent(input$lancer_analyse_btn, {
      req(donnees_long_terme(), input$temp_seuil_analyse)
      
      df_analyse <- donnees_long_terme()
      
      # Filtrage saisonnier (identique à avant)
      df_pour_stats <- df_analyse
      lookup_saison <- c("all" = "Toutes les saisons", "hiver" = "en hiver", "printemps" = "au printemps", "ete" = "en été", "automne" = "en automne")
      saison_choisie_nom <- lookup_saison[input$saison_analyse]
      
      if (input$saison_analyse != "all") {
        mois_saison <- switch(input$saison_analyse, "hiver" = c(12, 1, 2), "printemps" = c(3, 4, 5), "ete" = c(6, 7, 8), "automne" = c(9, 10, 11))
        df_pour_stats <- df_analyse %>% filter(mois %in% mois_saison)
      }
      
      # Calcul du nombre de jours au-dessus du seuil
      donnees_filtrees <- df_pour_stats %>% filter(tmax_celsius >= input$temp_seuil_analyse)
      
      # On stocke les résultats dans le reactiveVal dédié au seuil
      resultats_seuil(list(
        data = donnees_filtrees, seuil = input$temp_seuil_analyse,
        annee_range = input$annee_range_analyse, saison_nom = saison_choisie_nom
      ))
    })
    
    observeEvent(input$reset_analyse_btn, {
      resultats_seuil(NULL)
    })
    
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
    
    # --- MODIFICATION ---
    # L'ancienne UI est renommée et ne génère plus que la partie "seuil"
    output$resultats_seuil_ui <- renderUI({
      req(resultats_seuil())
      res <- resultats_seuil()
      
      saison_texte <- if (res$saison_nom != "Toutes les saisons") paste0(" ", res$saison_nom) else ""
      texte_synthese_seuil <- p(HTML(paste0(
        "Il y a eu <b>", nrow(res$data), " jours</b>", saison_texte, 
        " où <b>", res$seuil, "°C</b> a été atteint ou dépassé."
      )))
      
      card(
        texte_synthese_seuil,
        card(card_header("Nombre de jours au-dessus du seuil par décennie"), plotOutput(ns("decennie_plot"), height = "300px"))
      )
    })
    
    output$decennie_plot <- renderPlot({
      req(resultats_seuil())
      resultats_figes <- resultats_seuil()
      annee_debut <- resultats_figes$annee_range[1]; annee_fin <- resultats_figes$annee_range[2]
      decennie_debut <- floor(annee_debut / 10) * 10; decennie_fin <- floor(annee_fin / 10) * 10
      toutes_les_decennies <- tibble(decennie = seq(decennie_debut, decennie_fin, by = 10))
      
      if (nrow(resultats_figes$data) > 0) {
        comptes_par_decennie <- resultats_figes$data %>% mutate(decennie = floor(annee / 10) * 10) %>% count(decennie, name = "nb_jours")
        donnees_plot <- toutes_les_decennies %>% left_join(comptes_par_decennie, by = "decennie") %>% mutate(nb_jours = ifelse(is.na(nb_jours), 0, nb_jours))
      } else {
        donnees_plot <- toutes_les_decennies %>% mutate(nb_jours = 0)
      }
      
      donnees_plot <- donnees_plot %>%
        mutate(statut = case_when(
          decennie == max(decennie) & annee_fin %% 10 != 9 ~ "Partielle",
          decennie == min(decennie) & annee_debut %% 10 != 0 ~ "Partielle",
          TRUE ~ "Complète"
        ))
      
      ggplot(donnees_plot, aes(x = as.factor(decennie), y = nb_jours, fill = statut)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = nb_jours), vjust = -0.5, size = 4.5) +
        scale_fill_manual(name = "Statut",
                          values = c("Complète" = "steelblue", "Partielle" = "grey70")) +
        labs(x = "Décennie", y = "Nombre de jours") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    })

    # --- Canicules & records (nécessitent la mise à jour tmin du pipeline) ---
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

      seuil <- seuil_fc_ville()
      sous_titre <- if (is.finite(seuil))
        sprintf("Seuil de forte chaleur à %s : %s °C (90ᵉ pct des étés 1973-2003)",
                input$ville_analyse,
                format(round(seuil, 1), nsmall = 1, decimal.mark = ",")) else NULL

      p <- ggplot(long, aes(x = cat, y = valeur, fill = indicateur)) +
        geom_col(alpha = 0.85) +
        geom_text(aes(label = round(abs(valeur)),
                      vjust = ifelse(valeur >= 0, -0.3, 1.2)), size = 3.5) +
        geom_hline(yintercept = 0, color = "#343a40", linewidth = 0.4) +
        scale_fill_manual(values = c("Forte chaleur" = "#E41A1C", "Gel (≤ 0 °C)" = "#1f77b4"),
                          name = NULL) +
        labs(x = NULL, y = "Jours par an", subtitle = sous_titre) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom", plot.subtitle = element_text(color = "grey30")) +
        scale_y_continuous(expand = expansion(mult = c(0.12, 0.12)))
      # Séparateur pointillé entre décennies observées et horizons projetés.
      if (proj_ok && nrow(obs) > 0)
        p <- p + geom_vline(xintercept = nrow(obs) + 0.5, linetype = "dashed", color = "grey55")
      p
    })

  })
}