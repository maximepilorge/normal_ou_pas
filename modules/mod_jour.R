# modules/mod_jour.R
#
# Onglet « Une journée » — analyse d'UN jour précis dans UNE ville : à quel point
# la température maximale de ce jour est-elle exceptionnelle ?
#   - verdict : valeur observée, écart à la normale, RANG (« jour le plus chaud
#     autour du 23 juin depuis 1950 ») et fréquence (période de retour) ;
#   - distribution : nuage des tmax de la fenêtre ±7 j (toutes années), le jour
#     marqué en rouge, zone normale (p10–p90) ombrée ;
#   - carte de résultat partageable (réseaux sociaux, mobile et bureau), comme le quiz.
# Rang/fréquence sur fenêtre ±7 j (cohérent avec les normales) pour la robustesse.

mod_jour_ui <- function(id) {
  ns <- NS(id)
  date_min <- as.Date(paste0(an_min_data, "-01-01"))
  # Défaut = dernière donnée disponible (ERA5-Land est décalé, « aujourd'hui » est
  # vide). Affiné à la dernière date de la VILLE sélectionnée côté serveur.
  val_def  <- derniere_date_dispo

  tagList(
    div(
      class = "p-2",
      h5(class = "mb-2", "Analyser une journée précise"),

      # Barre de réglages EN LIGNE (remplace la sidebar) : ville, date et normale de
      # référence, visibles d'emblée. Menus hors flux (container = "body").
      card(
        class = "mb-2",
        card_body(
          class = "py-2",
          layout_columns(
            col_widths = breakpoints(sm = c(4, 4, 4)),
            pickerInput(ns("ville_jour"), "Ville :",
                        choices = villes_triees, selected = villes_triees[1],
                        options = list(container = "body", `live-search` = TRUE, size = 8)),
            dateInput(ns("date_jour"), "Date :", value = val_def,
                      min = date_min, max = derniere_date_dispo,
                      format = "dd/mm/yyyy", language = "fr", weekstart = 1),
            pickerInput(ns("periode_jour"), "Normale de référence :",
                        choices = periodes_disponibles,
                        selected = periodes_disponibles[length(periodes_disponibles)],
                        options = list(container = "body", `live-search` = FALSE))
          ),
          helpText(paste0("Rang et fréquence sont calculés sur une fenêtre de ±7 ",
                          "jours autour de la date, toutes années depuis ",
                          an_min_data, "."))
        )
      ),

      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_jour"))),
        uiOutput(ns("verdict_ui")),
        hr(),
        div(class = "small text-muted mb-1",
            "Distribution des températures maximales autour de cette date (±7 j, toutes années)"),
        plotlyOutput(ns("dist_plot"), height = "260px"),
        div(class = "text-center mt-3",
            p(class = "text-muted small mb-2",
              "Partagez cette journée autour de vous :"),
            actionButton(ns("partager_btn"), "Partager cette journée",
                         icon = icon("share-nodes"), class = "btn-primary")),
        # Boucle de circulation : rebond vers le quiz, pré-réglé sur cette ville.
        div(class = "text-center mt-2 mb-2",
            actionLink(ns("quiz_ville_btn"),
                       label = tagList(icon("dice"),
                                       " Testez vos repères sur cette ville avec le quiz"))),
        card_footer(
          tags$small(class = "text-muted", HTML(paste0(
            "Température = moyenne spatiale pondérée sur la commune (réanalyse ",
            "ERA5-Land) : elle peut différer d'une mesure de station locale. ",
            "Historique disponible depuis ", an_min_data, "."
          )))
        )
      )
    )
  )
}

mod_jour_server <- function(id, db_pool, prefill = reactive(NULL), naviguer = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    est_mobile <- reactive({ largeur_sous_seuil(session, ns("dist_plot")) })

    # Pré-remplissage (permalien ?onglet=jour&ville=...&date=... ou lien interne) :
    # applique ville et date demandées ; le recalage sur la dernière date dispo de
    # la ville (observeEvent ville_jour ci-dessous) reste le garde-fou.
    observeEvent(prefill(), {
      pf <- prefill()
      req(pf)
      if (!is.null(pf$ville)) updatePickerInput(session, "ville_jour", selected = pf$ville)
      if (!is.null(pf$date))  updateDateInput(session, "date_jour", value = pf$date)
    })

    # À chaque changement de ville (et au démarrage) : on cale le calendrier sur la
    # dernière date RÉELLEMENT disponible pour cette ville. La date n'est ramenée à
    # ce maximum que si la sélection courante le dépasse (donnée absente) — sinon on
    # respecte la date choisie (utile pour comparer une même date entre villes).
    observeEvent(input$ville_jour, {
      # Lecture du cache pré-calculé au démarrage (derniere_date_par_ville) ; repli
      # sur une requête ponctuelle si le cache est indisponible ou la ville absente.
      dmax <- if (!is.null(derniere_date_par_ville) &&
                  input$ville_jour %in% names(derniere_date_par_ville)) {
        derniere_date_par_ville[[input$ville_jour]]
      } else {
        tryCatch(
          tbl(db_pool, "temperatures_max") %>%
            filter(ville == !!input$ville_jour) %>%
            summarise(d = max(date, na.rm = TRUE)) %>% collect() %>% pull(d),
          error = function(e) NA)
      }
      dmax <- suppressWarnings(as.Date(dmax))
      if (length(dmax) == 1 && !is.na(dmax)) {
        cur <- input$date_jour
        nouvelle <- if (is.null(cur) || as.Date(cur) > dmax) dmax else NULL
        updateDateInput(session, "date_jour", value = nouvelle, max = dmax)
      }
    })

    # Date sans l'année (« 23 juin ») pour les phrases de rang.
    date_sans_annee <- function(d) paste(lubridate::day(d),
                                          tolower(mois_fr[lubridate::month(d)]))

    # --- Calcul central : tout ce dont les sorties ont besoin pour (ville, date,
    #     période). Renvoie ok = FALSE + message si la date n'est pas couverte. ---
    donnees_jour <- reactive({
      req(input$ville_jour, input$date_jour, input$periode_jour)
      d <- as.Date(input$date_jour)
      ville <- input$ville_jour

      # Fenêtre ±7 jours, toutes années (clé mois*100 + jour, comme le quiz). La
      # journée demandée (date == d, offset 0) est COMPRISE dans cette fenêtre : on
      # en extrait l'observation du jour plutôt qu'une requête `obs` séparée (une
      # requête de moins sur le chemin critique du verdict).
      jours_fenetre <- d + (-7:7)
      cles <- unique(lubridate::month(jours_fenetre) * 100 + lubridate::day(jours_fenetre))
      win <- tbl(db_pool, "temperatures_max") %>%
        filter(ville == !!ville, (mois * 100L + jour_mois) %in% !!cles) %>%
        select(date, annee, temperature_max) %>% collect() %>%
        rename(tmax = temperature_max)

      obs_jour <- win$tmax[win$date == d]
      if (length(obs_jour) == 0 || !is.finite(obs_jour[1])) {
        return(list(ok = FALSE, msg = paste0(
          "Pas de donnée ", autour_de(ville), " le ", format(d, "%d/%m/%Y"),
          " (couverture : ", an_min_data, "–", an_max_data, ").")))
      }
      temp <- round(obs_jour[1], 1)

      # Normale + bornes p10/p90 du jour calendaire pour la période choisie.
      norm <- tbl(db_pool, "stats_normales") %>%
        filter(ville == !!ville, mois == !!lubridate::month(d),
               jour_mois == !!lubridate::day(d), periode_ref == !!input$periode_jour) %>%
        select(t_moy, seuil_bas_p10, seuil_haut_p90) %>% collect()
      a_normale <- nrow(norm) > 0 && is.finite(norm$t_moy[1])
      t_moy <- if (a_normale) norm$t_moy[1] else NA_real_
      p10   <- if (a_normale) norm$seuil_bas_p10[1] else NA_real_
      p90   <- if (a_normale) norm$seuil_haut_p90[1] else NA_real_

      # p10/p90 sont NA si la normale est indisponible -> classer_normale renvoie
      # « Dans les normales de saison » (comportement historique).
      categorie <- classer_normale(temp, p10, p90)

      rang <- classer_jour_extreme(win$tmax, temp)

      # Sens de l'événement (chaud/froid) par rapport au centre de la distribution.
      ref_centre <- if (a_normale) t_moy else stats::median(win$tmax, na.rm = TRUE)
      chaud <- temp >= ref_centre

      # Fréquence : nb d'années (hors année du jour) dont la fenêtre atteint cette
      # intensité (≥ valeur si chaud, ≤ valeur si froid). Base de la période de retour.
      annee_jour <- lubridate::year(d)
      par_annee <- win %>% group_by(annee) %>%
        summarise(extr = if (chaud) max(tmax, na.rm = TRUE) else min(tmax, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(annee != annee_jour)
      n_autres <- if (chaud) sum(par_annee$extr >= temp, na.rm = TRUE)
                  else sum(par_annee$extr <= temp, na.rm = TRUE)

      # Rang ABSOLU sur toute la série (tous mois confondus) : compté en base pour
      # éviter de rapatrier ~27 000 lignes. 1 = record. Sens selon chaud/froid.
      n_depasse <- if (chaud) {
        tbl(db_pool, "temperatures_max") %>%
          filter(ville == !!ville, temperature_max > !!temp) %>%
          summarise(n = n()) %>% pull(n)
      } else {
        tbl(db_pool, "temperatures_max") %>%
          filter(ville == !!ville, temperature_max < !!temp) %>%
          summarise(n = n()) %>% pull(n)
      }
      rang_abs <- as.numeric(n_depasse) + 1

      list(ok = TRUE, ville = ville, date = d, temp = temp,
           t_moy = t_moy, p10 = p10, p90 = p90, a_normale = a_normale,
           categorie = categorie, periode = input$periode_jour,
           rang_haut = rang$rang_haut, rang_bas = rang$rang_bas, rang_abs = rang_abs,
           n_annees = dplyr::n_distinct(win$annee), chaud = chaud,
           n_autres = n_autres, win = win)
    }) %>% bindCache(input$ville_jour, input$date_jour, input$periode_jour)

    # Phrase de rang (« Jour le plus chaud autour du 23 juin depuis 1950 »).
    phrase_rang <- function(res) {
      jour <- date_sans_annee(res$date)
      if (res$chaud) {
        if (res$rang_haut == 1) paste0("Jour le plus chaud autour du ", jour, " depuis ", an_min_data)
        else paste0(res$rang_haut, "ᵉ jour le plus chaud autour du ", jour, " depuis ", an_min_data)
      } else {
        if (res$rang_bas == 1) paste0("Jour le plus froid autour du ", jour, " depuis ", an_min_data)
        else paste0(res$rang_bas, "ᵉ jour le plus froid autour du ", jour, " depuis ", an_min_data)
      }
    }

    # Phrase de fréquence / période de retour.
    phrase_frequence <- function(res) {
      if (res$n_autres == 0) {
        if (res$chaud) "Une telle chaleur n'a jamais été atteinte sur tout l'historique disponible."
        else "Un tel froid n'a jamais été atteint sur tout l'historique disponible."
      } else {
        retour <- max(1, round(res$n_annees / (res$n_autres + 1)))
        if (retour <= 1) "Une intensité atteinte presque chaque année."
        else paste0("Une intensité atteinte en moyenne une fois tous les ", retour, " ans.")
      }
    }

    # Phrase de rang ABSOLU (tous mois confondus), affichée en valeur seulement si
    # le jour est un record (rang 1) ou proche (top SEUIL_TOP_ABS). NULL sinon.
    SEUIL_TOP_ABS <- 10
    phrase_record_abs <- function(res) {
      if (res$rang_abs > SEUIL_TOP_ABS) return(NULL)
      if (res$chaud) {
        if (res$rang_abs == 1)
          paste0("Record absolu de chaleur depuis ", an_min_data, " (tous mois confondus)")
        else
          paste0(res$rang_abs, "ᵉ jour le plus chaud depuis ", an_min_data, ", tous mois confondus")
      } else {
        if (res$rang_abs == 1)
          paste0("Température maximale la plus basse depuis ", an_min_data, " (tous mois confondus)")
        else
          paste0(res$rang_abs, "ᵉ température maximale la plus basse depuis ", an_min_data, ", tous mois confondus")
      }
    }

    output$titre_jour <- renderUI({
      req(input$ville_jour, input$date_jour)
      HTML(paste0("Le <strong>", format(as.Date(input$date_jour), "%d/%m/%Y"),
                  "</strong> ", autour_de(input$ville_jour)))
    })

    output$verdict_ui <- renderUI({
      res <- donnees_jour()
      req(res)
      if (!isTRUE(res$ok)) return(div(class = "alert alert-warning mb-0", res$msg))

      couleur <- couleur_categorie(res$categorie)
      ligne_ecart <- if (res$a_normale) {
        ecart <- round(res$temp - res$t_moy, 1)
        sens <- if (ecart > 0) "au-dessus" else if (ecart < 0) "en-dessous" else "pile dans"
        HTML(paste0("<b>", sprintf("%+.1f", ecart), " °C</b> ", sens,
                    " de la normale ", res$periode, " (", fmt_temp(res$t_moy), " °C)"))
      } else "Normale indisponible pour cette date."

      # Bannière « record absolu » (tous mois confondus) mise en valeur si rang 1 ;
      # simple ligne pour un top-N. NULL si le jour n'est pas remarquable à l'échelle
      # de toute la série (la fenêtre ±7 j reste alors la lecture principale).
      rec <- phrase_record_abs(res)
      banniere <- if (!is.null(rec)) {
        if (res$rang_abs == 1)
          tags$div(class = "alert mb-2 py-2",
                   style = paste0("background:", couleur, "; color:#fff; font-weight:700;"),
                   tags$span(class = "badge bg-light text-dark me-2", "RECORD"), rec)
        else
          tags$div(class = "mb-2 fw-semibold", style = paste0("color:", couleur, ";"), rec)
      }

      tagList(
        tags$div(style = paste0("font-size:2.4rem; font-weight:800; line-height:1.1; color:", couleur, ";"),
                 paste0(fmt_temp(res$temp), " °C")),
        banniere,
        tags$div(class = "mb-2", ligne_ecart),
        tags$span(class = "badge",
                  style = paste0("background:", couleur, "; color:#fff; font-size:0.95rem; white-space:normal;"),
                  phrase_rang(res)),
        tags$div(class = "text-muted mt-2", phrase_frequence(res))
      )
    })

    output$dist_plot <- renderPlotly({
      res <- donnees_jour()
      req(res, isTRUE(res$ok))
      mob <- est_mobile()
      win <- res$win

      # Distribution 1D horizontale en plot_ly NATIF (au lieu de ggplot + ggplotly,
      # dont la conversion est le poste dominant du délai d'affichage). plot_ly ne
      # jitte pas : on génère le décalage vertical des points ici.
      jitter_y <- runif(nrow(win), -0.28, 0.28)

      # Zone normale (p10–p90) et bornes en shapes de layout (sous les points).
      shapes <- if (res$a_normale) list(
        list(type = "rect", xref = "x", yref = "paper", x0 = res$p10, x1 = res$p90,
             y0 = 0, y1 = 1, fillcolor = "#2E8B57", opacity = 0.13,
             line = list(width = 0), layer = "below"),
        list(type = "line", xref = "x", yref = "paper", x0 = res$p10, x1 = res$p10,
             y0 = 0, y1 = 1, line = list(color = "#2E8B57", dash = "dash", width = 1.4),
             layer = "below"),
        list(type = "line", xref = "x", yref = "paper", x0 = res$p90, x1 = res$p90,
             y0 = 0, y1 = 1, line = list(color = "#2E8B57", dash = "dash", width = 1.4),
             layer = "below")
      ) else list()

      fig <- plot_ly() %>%
        add_markers(x = win$tmax, y = jitter_y,
                    marker = list(color = "rgba(31,119,180,0.4)", size = 6),
                    text = paste0(format(win$date, "%d/%m/%Y"), " : ", round(win$tmax, 1), " °C"),
                    hoverinfo = "text", showlegend = FALSE)

      # Moyenne (normale) en croix noire, comme le boxplot du quiz. Ajoutée avant le
      # point du jour pour que ce dernier reste au-dessus quand les deux se touchent.
      if (res$a_normale)
        fig <- fig %>% add_markers(x = res$t_moy, y = 0,
                    marker = list(symbol = "x", color = "black", size = 10),
                    text = paste0("Moyenne : ", fmt_temp(res$t_moy), " °C"),
                    hoverinfo = "text", showlegend = FALSE)

      fig %>%
        add_markers(x = res$temp, y = 0,
                    marker = list(symbol = "x", color = "#E41A1C", size = 13,
                                  line = list(color = "#E41A1C", width = 1.8)),
                    text = paste0(format(res$date, "%d/%m/%Y"), " : ",
                                  fmt_temp(res$temp), " °C"),
                    hoverinfo = "text", showlegend = FALSE) %>%
        layout(shapes = shapes,
               xaxis = list(fixedrange = TRUE, ticksuffix = " °C", title = "", zeroline = FALSE),
               yaxis = list(fixedrange = TRUE, visible = FALSE, range = c(-0.5, 0.5)),
               font = list(size = if (mob) 11 else 13),
               margin = list(t = 10), showlegend = FALSE) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    }) %>% bindCache(input$ville_jour, input$date_jour, input$periode_jour, est_mobile())

    # --- Partage : carte PNG 1200×630 régénérée + modal (mêmes canaux que le quiz) ---
    observeEvent(input$partager_btn, {
      res <- donnees_jour()
      req(res, isTRUE(res$ok))

      # Sur la carte/texte, on met en avant la phrase la PLUS forte : le record
      # absolu (tous mois confondus) s'il s'applique, sinon le rang saisonnier.
      rec <- phrase_record_abs(res)
      phrase_principale <- if (!is.null(rec) && res$rang_abs == 1) rec else phrase_rang(res)

      normale_aff <- if (res$a_normale) res$t_moy else NA_real_
      params <- list(ville = res$ville, date = res$date, temp = res$temp,
                     normale_moy = normale_aff, periode_ref = res$periode,
                     categorie = res$categorie, rang_txt = phrase_principale,
                     p10 = if (res$a_normale) res$p10 else NA_real_,
                     p90 = if (res$a_normale) res$p90 else NA_real_)

      f <- tempfile(fileext = ".png")
      sauver_carte_jour(params, f)
      on.exit(unlink(f), add = TRUE)
      data_uri <- paste0("data:image/png;base64,",
                         jsonlite::base64_enc(readBin(f, "raw", n = file.info(f)$size)))

      texte <- paste0(fmt_temp(res$temp), "°C ",
                      autour_de(res$ville), " le ", format(res$date, "%d/%m/%Y"), " : ",
                      phrase_principale, ". Et vous, sauriez-vous situer ce qui est normal ?")
      nom_fichier <- paste0("normal-ou-pas_jour_",
                            gsub("[^A-Za-z0-9]+", "_", res$ville), ".png")

      showModal(modal_partage(data_uri, texte, nom_fichier, titre = "Partager cette journée"))
    })

    # Rebond vers le quiz, pré-réglé sur la ville affichée.
    observeEvent(input$quiz_ville_btn, {
      req(input$ville_jour)
      if (is.function(naviguer)) naviguer("quiz", ville = input$ville_jour)
    })

    # État exposé à server.R pour le permalien (?onglet=jour&ville=...&date=...).
    return(list(
      etat_url = reactive(list(ville = input$ville_jour, date = input$date_jour))
    ))

  })
}
