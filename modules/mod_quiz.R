# modules/mod_quiz.R
#
# Quiz « Série de 10 » : machine à états en 3 actes — accueil (paramétrer) ->
# jeu (10 manches) -> résultats (score + commentaire + partage + rejouer).
# La génération d'une question et le calcul du feedback réutilisent la mécanique
# historique du quiz, extraite ici en fonctions de fichier réutilisables.

# --- Pré-chargement des candidats d'une série -------------------------------
# Une requête agrégée unique : pour le combo (période, ville, saison), une ligne
# par couple (ville, jour calendaire, catégorie) existant avec min/max/normale.
# On échantillonne ensuite EN MÉMOIRE (echantillonner_serie, helpers.R), ce qui
# supprime les allers-retours BDD et garantit qu'on sait d'emblée si le combo
# fournit assez de questions.
charger_candidats_quiz <- function(db_pool, periode, ville, saison) {
  mois_s <- mois_saison(saison)

  # Chemin rapide : agrégat pré-calculé (quiz_candidats) — une simple lecture
  # filtrée/indexée (~33k lignes max) au lieu d'agréger ~4 M lignes à chaque
  # « Lancer la série » (24 s en prod). Repli sur l'agrégation à la volée si la
  # table est absente (dégradation gracieuse, cf. utils/quiz_candidats.sql).
  if (isTRUE(quiz_candidats_disponibles)) {
    requete <- tbl(db_pool, "quiz_candidats") %>% filter(periode_ref == !!periode)
    if (!identical(ville, "Toutes les villes")) requete <- requete %>% filter(ville == !!ville)
    if (!is.null(mois_s)) requete <- requete %>% filter(mois %in% !!mois_s)
    return(requete %>%
      select(ville, mois, jour_mois, categorie, min_temp, max_temp, normale_moy) %>%
      collect())
  }

  requete <- tbl(db_pool, "quiz_data_precalculee") %>% filter(periode_ref == !!periode)
  if (!identical(ville, "Toutes les villes")) requete <- requete %>% filter(ville == !!ville)
  if (!is.null(mois_s)) requete <- requete %>% filter(mois %in% !!mois_s)
  requete %>%
    group_by(ville, mois, jour_mois, categorie) %>%
    summarise(
      min_temp    = min(tmax_celsius, na.rm = TRUE),
      max_temp    = max(tmax_celsius, na.rm = TRUE),
      normale_moy = min(t_moy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    collect()
}

# --- Pré-calcul groupé des données STATIQUES d'une série --------------------
# Au lancement d'une série, on récupère EN 2-3 REQUÊTES GROUPÉES (par ville) les
# seuils p10/p90 (période choisie) et les normales projetées (base 1991-2020 +
# deltas TRACC) de toutes les manches, au lieu d'une requête par manche à chaque
# révélation. Renvoie une liste alignée sur `questions` : list(seuils, projections)
# par manche (NULL si absent). Toute erreur -> repli sur les requêtes à la volée
# dans calculer_feedback_manche (statique = NULL).
precalculer_feedback_serie <- function(db_pool, questions, periode_ref) {
  if (length(questions) == 0) return(list())
  villes <- unique(vapply(questions, function(q) q$city, character(1)))

  seuils_tbl <- tbl(db_pool, "stats_normales") %>%
    filter(periode_ref == !!periode_ref, ville %in% !!villes) %>%
    select(ville, mois, jour_mois, seuil_bas_p10, seuil_haut_p90) %>%
    collect()

  # Projections (facultatives) : normale 1991-2020 + deltas par niveau, par ville.
  base_tbl <- NULL; deltas_tbl <- NULL
  if (isTRUE(projections_disponibles)) {
    base_tbl <- tbl(db_pool, "stats_normales") %>%
      filter(periode_ref == !!PERIODE_REF_PROJECTION, ville %in% !!villes) %>%
      select(ville, mois, jour_mois, t_moy, seuil_bas_p10, seuil_haut_p90) %>%
      collect()
    deltas_tbl <- tbl(db_pool, "stats_normales_projetees") %>%
      filter(ville %in% !!villes) %>%
      collect()
  }

  lapply(questions, function(q) {
    m <- lubridate::month(q$date); j <- lubridate::day(q$date)
    sr <- seuils_tbl[seuils_tbl$ville == q$city &
                       seuils_tbl$mois == m & seuils_tbl$jour_mois == j, ]
    seuils <- if (nrow(sr) > 0)
      list(p10 = sr$seuil_bas_p10[1], p90 = sr$seuil_haut_p90[1]) else NULL
    projections <- NULL
    if (!is.null(base_tbl) && !is.null(deltas_tbl)) {
      br <- base_tbl[base_tbl$ville == q$city &
                       base_tbl$mois == m & base_tbl$jour_mois == j, ]
      dr <- deltas_tbl[deltas_tbl$ville == q$city &
                         deltas_tbl$mois == m & deltas_tbl$jour_mois == j, ]
      projections <- construire_projections(br, dr)
    }
    list(seuils = seuils, projections = projections)
  })
}

# Comptage SQL SCALAIRE : nombre de jours d'une `ville` dont la tmax atteint
# l'intensité `temp` dans le sens `direction` ("supérieure" -> >=, sinon <=), sur
# les années `annees`. Restreint soit à une fenêtre ±7 j (cles_fenetre = clés
# mois*100+jour_mois), soit à des mois de saison (mois_saison). Le COUNT est fait
# côté PostgreSQL : on ne rapatrie qu'un entier (au lieu de centaines/milliers de
# lignes juste pour un sum() en R).
.compter_depassements <- function(db_pool, ville, annees, temp, direction,
                                  cles_fenetre = NULL, mois_saison = NULL) {
  requete <- tbl(db_pool, "temperatures_max") %>%
    filter(ville == !!ville, annee %in% !!annees)
  if (!is.null(cles_fenetre))
    requete <- requete %>% filter((mois * 100L + jour_mois) %in% !!cles_fenetre)
  if (!is.null(mois_saison))
    requete <- requete %>% filter(mois %in% !!mois_saison)
  requete <- if (direction == "supérieure")
    requete %>% filter(temperature_max >= !!temp)
  else
    requete %>% filter(temperature_max <= !!temp)
  as.numeric(requete %>% summarise(n = n()) %>% pull(n))
}

# --- Calcul du feedback d'une manche ----------------------------------------
# Reprend la logique historique (seuils p10/p90, projections TRACC, comptages de
# fréquence, explication écart) MAIS sous forme de fonction pilotée par les données
# de la manche : aucune déclaration d'output ici (celles-ci vivent, une seule fois,
# dans le serveur). Les seuils/projections viennent du préchargement `statique`
# (repli requête à la volée) ; les comptages sont des agrégats SQL scalaires ; les
# lignes du boxplot sont chargées à part (paresseusement, à l'ouverture de
# l'accordéon). Renvoie list(explication, seuils, projections, projection_txt,
# projection_couleur).
calculer_feedback_manche <- function(db_pool, data, periode_ref, statique = NULL) {
  annees_periode <- as.numeric(unlist(strsplit(periode_ref, "-")))
  annee_debut <- annees_periode[1]
  annee_fin   <- annees_periode[2]
  annees_a_filtrer <- seq(annee_debut, annee_fin)
  jour_quiz <- lubridate::day(data$date)
  mois_quiz <- lubridate::month(data$date)

  # Seuils p10/p90 du jour (zone normale du boxplot) : préchargés au lancement de
  # la série (`statique`) si disponibles, sinon requête ponctuelle (repli gracieux).
  seuils <- if (!is.null(statique)) statique$seuils else {
    seuils_normaux <- tbl(db_pool, "stats_normales") %>%
      filter(ville == !!data$city, mois == !!mois_quiz, jour_mois == !!jour_quiz,
             periode_ref == !!periode_ref) %>%
      select(seuil_bas_p10, seuil_haut_p90) %>%
      collect()
    if (nrow(seuils_normaux) > 0)
      list(p10 = seuils_normaux$seuil_bas_p10[1], p90 = seuils_normaux$seuil_haut_p90[1])
    else NULL
  }

  # Normales projetées (TRACC) : normale 1991-2020 + delta DRIAS par niveau.
  # Préchargées au lancement de la série (`statique`) si disponibles ; sinon on
  # récupère base + deltas à la volée et on assemble via construire_projections().
  projections <- if (!is.null(statique)) statique$projections else {
    if (isTRUE(projections_disponibles)) {
      base_9120 <- tbl(db_pool, "stats_normales") %>%
        filter(ville == !!data$city, mois == !!mois_quiz, jour_mois == !!jour_quiz,
               periode_ref == !!PERIODE_REF_PROJECTION) %>%
        select(t_moy, seuil_bas_p10, seuil_haut_p90) %>% collect()
      deltas_proj <- tbl(db_pool, "stats_normales_projetees") %>%
        filter(ville == !!data$city, mois == !!mois_quiz, jour_mois == !!jour_quiz) %>%
        collect()
      construire_projections(base_9120, deltas_proj)
    } else NULL
  }

  moyenne_reelle <- data$normale_moy
  diff <- round(abs(data$temp - moyenne_reelle), 1)
  direction <- if (data$temp > moyenne_reelle) "supérieure" else "inférieure"

  if (data$correct_answer == "Dans les normales de saison") {
    explication_text <- paste0("Cette température est <b>", diff, "°C</b> ", direction,
      " à la moyenne de saison (", round(moyenne_reelle, 1), "°C) et est considérée comme normale à cette période de l'année (vers le ",
      paste(format(data$date, "%d"), mois_fr[as.numeric(format(data$date, "%m"))]), ") ",
      autour_de(data$city), ".")
  } else {
    explication_principale <- paste0("Cette température est <b>", diff, "°C</b> ", direction,
      " à la moyenne de saison (", round(moyenne_reelle, 1), "°C) pour la période ", periode_ref, ".")

    # Comptages d'occurrence en agrégats SQL scalaires (COUNT côté PostgreSQL) :
    # plus de rapatriement de ~450 (fenêtre) / ~2700 (saison) lignes pour un sum().
    jours_fenetre <- data$date + (-7:7)
    cles_fenetre <- unique(lubridate::month(jours_fenetre) * 100 + lubridate::day(jours_fenetre))
    nombre_occurrences_jour <- .compter_depassements(
      db_pool, data$city, annees_a_filtrer, data$temp, direction, cles_fenetre = cles_fenetre)
    frequence_jour_text <- if (nombre_occurrences_jour == 0)
      paste0("Autour de cette date (fenêtre de ±7 jours), un événement de cette intensité ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".")
    else paste0("Autour de cette date (fenêtre de ±7 jours), une température égale ou ", direction,
                " est arrivée <b>", nombre_occurrences_jour, " fois</b> entre ", annee_debut, " et ", annee_fin, ".")

    saison <- get_season_info(data$date)
    nombre_occurrences_saison <- .compter_depassements(
      db_pool, data$city, annees_a_filtrer, data$temp, direction, mois_saison = saison$mois)
    message_occurrence_saison <- if (round(nombre_occurrences_saison / (annee_fin - annee_debut + 1) >= 1)) {
      paste0(round(nombre_occurrences_saison / (annee_fin - annee_debut + 1), 0))
    } else if (round(nombre_occurrences_saison / (annee_fin - annee_debut + 1) > 0)) {
      "moins d'une"
    } else {
      "0"
    }
    frequence_saison_text <- if (nombre_occurrences_saison == 0)
      paste0("À l'échelle de la saison (", saison$nom, "), une température aussi ",
             if (direction == "supérieure") "élevée" else "basse",
             " ne s'est <b>jamais produit</b> entre ", annee_debut, " et ", annee_fin, ".")
    else paste0("À l'échelle de la saison (", saison$nom, "), une température égale ou ", direction,
                " est arrivée en moyenne <b>", message_occurrence_saison, " fois</b> par an entre ",
                annee_debut, " et ", annee_fin, ".")

    explication_text <- paste(explication_principale, frequence_jour_text, frequence_saison_text,
                              sep = "<br><br>")
  }

  # Phrase projections (TRACC) : situe la température aux horizons futurs.
  if (!is.null(projections)) {
    classer <- function(temp, p10, p90) {
      if (temp < p10) "<b>en dessous des normales</b>"
      else if (temp > p90) "<b>au-dessus des normales</b>"
      else "<b>dans les normales de saison</b>"
    }
    phrases <- vapply(projections$niveaux, function(nv)
      paste0("à l'horizon <b>", nv$label, "</b>, elle serait ",
             classer(data$temp, nv$p10, nv$p90)), character(1))
    explication_text <- paste0(explication_text,
      "<br><br><span style='color:#555'>🌡️ Avec le réchauffement (trajectoire de référence ",
      "TRACC ; les niveaux +2,7 °C et +4 °C s'entendent par rapport à l'ère préindustrielle) : ",
      paste(phrases, collapse = " ; "), ".</span>")
  }

  # Phrase « nouvelle normale » 2100 (et sa couleur) pour la carte de partage de
  # la manche : où se situerait cette température à l'horizon 2100 (réf. TRACC).
  projection_txt <- NULL; projection_couleur <- NULL
  if (!is.null(projections) && length(projections$niveaux) > 0) {
    n_fin <- Filter(function(x) startsWith(x$niveau, "2100"), projections$niveaux)
    n_fin <- if (length(n_fin) > 0) n_fin[[1]] else projections$niveaux[[length(projections$niveaux)]]
    annee_h <- sub("_.*", "", n_fin$niveau)
    ph <- phrase_projection_futur(data$temp, data$correct_answer, n_fin$p10, n_fin$p90, annee_h)
    if (!is.null(ph)) { projection_txt <- ph$txt; projection_couleur <- ph$couleur }
  }

  list(explication = explication_text, seuils = seuils, projections = projections,
       projection_txt = projection_txt, projection_couleur = projection_couleur)
}

# Libellé court d'une catégorie (pour le récap du bilan).
verdict_court_quiz <- function(categorie) {
  if (grepl("Au-dessus", categorie)) "au-dessus des normales"
  else if (grepl("En-dessous", categorie)) "en-dessous des normales"
  else "dans les normales de saison"
}

# --- UI : simple conteneur plein écran (les 3 écrans sont rendus côté serveur) --
mod_quiz_ui <- function(id) {
  ns <- NS(id)
  div(class = "quiz-module", uiOutput(ns("quiz_zone")))
}

mod_quiz_server <- function(id, db_pool, visitor_id = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    N_QUESTIONS <- 10L

    # --- État de la machine -------------------------------------------------
    etat            <- reactiveVal("accueil")   # accueil | jeu | resultats
    serie           <- reactiveVal(NULL)        # liste des questions pré-tirées
    idx             <- reactiveVal(1L)          # manche courante
    reponses        <- reactiveVal(list())      # par manche : list(user_answer, juste)
    phase_manche    <- reactiveVal("question")  # question | feedback
    feedback_courant<- reactiveVal(NULL)
    filtres_serie   <- reactiveVal(NULL)        # list(periode, ville, saison, poli)
    debut_serie     <- reactiveVal(NULL)
    serie_terminee  <- reactiveVal(NULL)        # événement remonté à server.R (BDD)

    # Données de la manche courante (alimentent les outputs du boxplot, déclarés
    # une seule fois plus bas).
    quiz_data       <- reactiveVal(NULL)
    seuils_quiz     <- reactiveVal(NULL)
    projections_quiz<- reactiveVal(NULL)
    dernier_resultat<- reactiveVal(NULL)        # params de la carte de partage de la manche
    taquin_manche   <- reactiveVal(NULL)        # commentaire taquin de la manche révélée
    # Données STATIQUES (seuils + projections) des 10 manches, préchargées en bloc
    # au lancement de la série (cf. precalculer_feedback_serie) ; NULL -> repli sur
    # les requêtes à la volée dans calculer_feedback_manche.
    feedback_statique <- reactiveVal(NULL)

    # Score CUMULÉ sur la session (compat analytics_visits / server.R) — incrémenté
    # à chaque validation, exactement comme le quiz historique.
    score_succes <- reactiveVal(0)
    score_echecs <- reactiveVal(0)

    # Score de la SÉRIE courante, dérivé des réponses (indépendant du cumul session).
    score_serie <- reactive({
      sum(vapply(reponses(), function(x) isTRUE(x$juste), logical(1)))
    })

    # Repère « normale » actif selon le bouton horizon (présent / 2050 / 2100).
    repere_actif <- reactive({
      data_quiz <- quiz_data()
      req(data_quiz)
      seuils <- seuils_quiz()
      proj   <- projections_quiz()
      horizon <- if (is.null(input$horizon_proj)) "present" else input$horizon_proj
      if (horizon == "present") {
        if (!is.null(seuils) && is.finite(seuils$p10) && is.finite(seuils$p90))
          return(list(p10 = seuils$p10, p90 = seuils$p90, moy = data_quiz$normale_moy,
                      titre = paste0("Normale actuelle (", filtres_serie()$periode, ")"),
                      couleur = "#2E8B57"))
      } else if (!is.null(proj)) {
        n <- Filter(function(x) startsWith(x$niveau, horizon), proj$niveaux)
        if (length(n) > 0) {
          n <- n[[1]]
          return(list(p10 = n$p10, p90 = n$p90, moy = n$moy,
                      titre = paste0("Normale projetée ", n$label),
                      couleur = if (horizon == "2050") "#E8A33D" else "#E4572E"))
        }
      }
      NULL
    })

    # === ÉCRAN 0 : ACCUEIL (paramétrer la série) ============================
    ecran_accueil <- function() {
      f <- filtres_serie()
      sel <- function(cle, defaut) if (!is.null(f) && !is.null(f[[cle]])) f[[cle]] else defaut
      div(class = "quiz-card card",
        div(class = "card-body",
          h2("Prêt à tester vos repères climatiques ?", class = "quiz-titre"),
          p(class = "text-muted",
            "10 températures, 10 verdicts. Saurez-vous dire si chacune est normale ou pas ?"),
          layout_columns(
            col_widths = c(4, 4, 4), gap = "0.75rem",
            pickerInput(ns("periode_normale"), "Période de référence",
                        choices = periodes_disponibles, selected = sel("periode", periodes_disponibles[1]),
                        options = list(container = "body")),
            pickerInput(ns("ville_select_quiz"), "Ville",
                        choices = c("Toutes les villes", villes_triees),
                        selected = sel("ville", "Toutes les villes"),
                        options = list(container = "body", `live-search` = TRUE, size = 8)),
            pickerInput(ns("saison_select"), "Saison",
                        choices = c("Toutes les saisons", "Hiver", "Printemps", "Été", "Automne"),
                        selected = sel("saison", "Toutes les saisons"),
                        options = list(container = "body"))
          ),
          checkboxInput(ns("trash_talk_mode"), "Me forcer à vous répondre poliment",
                        value = if (!is.null(f)) isTRUE(f$poli) else FALSE),
          actionButton(ns("lancer_btn"), "Lancer la série", icon = icon("dice"),
                       class = "btn-primary btn-lg w-100 mt-2"),
          p(class = "text-muted small text-center mt-2",
            "Une série = 10 questions tirées au hasard.")
        ))
    }

    # === ÉCRAN 1 : JEU (manche courante) ====================================
    ecran_jeu <- function() {
      q <- serie()[[idx()]]
      req(q)
      n <- length(serie())
      formatted_date <- paste(format(q$date, "%d"), mois_fr[as.numeric(format(q$date, "%m"))])
      enonce <- paste0("Le ", formatted_date, ", ", autour_de(q$city),
                       ", la température maximale observée est de ", q$temp, " °C. Normal ou pas ?")

      # Barre de progression : pastilles juste / faux / courante / à venir.
      pastilles <- lapply(seq_len(n), function(i) {
        rp <- reponses()[[i]]
        cls <- if (!is.null(rp)) (if (isTRUE(rp$juste)) "pastille pastille-juste" else "pastille pastille-faux")
               else if (i == idx()) "pastille pastille-courante"
               else "pastille pastille-avenir"
        div(class = cls)
      })
      barre <- tagList(
        div(class = "quiz-progress", pastilles),
        p(class = "text-muted small text-center mb-3", sprintf("Question %d / %d", idx(), n))
      )

      corps <- if (phase_manche() == "question") {
        tagList(
          div(class = "carte-question", h3(enonce, class = "enonce")),
          # radioButtons (et non radioGroupButtons, mal rendu sous Bootstrap 5) :
          # stylé en boutons pleine largeur, toute la zone est cliquable.
          div(class = "quiz-reponses",
              radioButtons(ns(paste0("rep_", idx())),
                label = "Cette température est :",
                choices = CATEGORIES_QUIZ, selected = character(0))),
          actionButton(ns("valider_btn"), "Valider", icon = icon("check"),
                       class = "btn-success btn-lg w-100")
        )
      } else {
        rp <- reponses()[[idx()]]
        juste <- isTRUE(rp$juste)
        badge <- div(class = "text-center mb-2",
          span(class = paste("badge fs-6", if (juste) "bg-success" else "bg-danger"),
               if (juste) "BONNE RÉPONSE" else "RATÉ"))
        taquin <- if (!is.null(taquin_manche()))
          div(class = "taquin-manche text-center mb-3", strong(taquin_manche()))
        # Révélation : chaque option marquée correcte / mauvais choix.
        options <- lapply(CATEGORIES_QUIZ, function(o) {
          cls <- "option-reveal"; ic <- NULL
          if (identical(o, q$correct_answer)) { cls <- paste(cls, "option-correcte"); ic <- icon("check") }
          else if (identical(o, rp$user_answer)) { cls <- paste(cls, "option-incorrecte"); ic <- icon("xmark") }
          div(class = cls, ic, span(o))
        })
        fb <- feedback_courant()
        distribution <- accordion(open = FALSE,
          accordion_panel("Voir la distribution",
            if (!is.null(projections_quiz()))
              div(class = "text-center mb-2",
                  radioGroupButtons(ns("horizon_proj"), label = "Comparer à la normale de :",
                    choices = c("Aujourd'hui" = "present", "2050 (+2,7 °C)" = "2050",
                                "2100 (+4 °C)" = "2100"),
                    selected = "present", size = "sm", status = "primary")),
            uiOutput(ns("boxplot_titre")),
            plotlyOutput(ns("feedback_boxplot"), height = "340px")))
        tagList(
          div(class = "carte-question carte-reveal", h3(enonce, class = "enonce")),
          badge,
          taquin,
          div(class = "options-reveal", options),
          div(class = "feedback-explication mt-3", HTML(fb$explication)),
          distribution,
          div(class = "text-center mt-3",
              p(class = "text-muted small mb-2",
                "Partagez ce repère de température autour de vous :"),
              actionButton(ns("partager_btn"), "Partager ce résultat",
                           icon = icon("share-nodes"), class = "btn-outline-primary")),
          actionButton(ns("suivant_btn"),
                       if (idx() < n) "Question suivante" else "Voir mon bilan",
                       icon = icon(if (idx() < n) "arrow-right" else "flag-checkered"),
                       class = "btn-primary btn-lg w-100 mt-3")
        )
      }

      div(class = "quiz-card card", div(class = "card-body",
        barre,
        corps,
        # Abandon possible à tout moment : ramène au paramétrage (filtres conservés).
        div(class = "text-center mt-3",
            actionLink(ns("abandonner_btn"), "Abandonner la série",
                       icon = icon("xmark"), class = "text-muted small"))
      ))
    }

    # === ÉCRAN 2 : RÉSULTATS (bilan) ========================================
    ecran_resultats <- function() {
      f <- filtres_serie(); req(f)
      n <- length(serie()); sc <- score_serie()
      couleur <- couleur_score(sc, n)
      comm <- commentaire_serie(sc, n, isTRUE(f$poli))

      lignes <- lapply(seq_len(n), function(i) {
        q <- serie()[[i]]; rp <- reponses()[[i]]; juste <- isTRUE(rp$juste)
        date_txt <- paste(format(q$date, "%d"), mois_fr[as.numeric(format(q$date, "%m"))])
        div(class = paste("recap-ligne", if (juste) "recap-juste" else "recap-faux"),
          span(class = "recap-icone", if (juste) icon("check") else icon("xmark")),
          div(class = "recap-info",
            div(class = "recap-fait", paste0(date_txt, " · ", autour_de(q$city), " · ", q$temp, " °C")),
            if (!juste) div(class = "recap-detail",
              sprintf("Votre réponse : %s — Bonne réponse : %s",
                      verdict_court_quiz(rp$user_answer), verdict_court_quiz(q$correct_answer)))))
      })

      div(class = "quiz-card card", div(class = "card-body text-center",
        div(class = "anneau-score",
            style = sprintf("--accent:%s; --pct:%d;", couleur, as.integer(round(100 * sc / n))),
            span(class = "anneau-chiffre", sprintf("%d/%d", sc, n))),
        p(comm, class = "lead mt-3"),
        uiOutput(ns("record_perso")),
        hr(),
        h5("Le détail de votre série", class = "text-start"),
        div(class = "recap text-start", lignes),
        p(class = "text-muted small mt-2", .contexte_serie(f$ville, f$saison, f$periode)),
        div(class = "mt-3",
          actionButton(ns("rejouer_btn"), "Rejouer une série", icon = icon("rotate-right"),
                       class = "btn-primary btn-lg")),
        div(class = "mt-2", actionLink(ns("reglages_btn"), "Changer les réglages"))
      ))
    }

    # --- Rendu maître : un seul uiOutput pilote l'écran courant -------------
    output$quiz_zone <- renderUI({
      switch(etat(),
             accueil   = ecran_accueil(),
             jeu       = ecran_jeu(),
             resultats = ecran_resultats())
    })

    # --- Outputs du boxplot (déclarés UNE seule fois) -----------------------
    # Lignes ±7 j de la manche pour le boxplot, chargées PARESSEUSEMENT : le graphe
    # vit dans un accordéon fermé (« Voir la distribution »), donc suspendu tant
    # qu'il n'est pas déplié — la requête (~450 lignes) ne part QUE si l'utilisateur
    # ouvre la distribution, et sort ainsi du chemin de révélation d'une manche.
    boxplot_rows <- reactive({
      dq <- quiz_data()
      req(dq, phase_manche() == "feedback")
      annees <- as.numeric(strsplit(filtres_serie()$periode, "-")[[1]])
      annees_a_filtrer <- seq(annees[1], annees[2])
      jours_fenetre <- dq$date + (-7:7)
      cles <- unique(lubridate::month(jours_fenetre) * 100 + lubridate::day(jours_fenetre))
      tbl(db_pool, "temperatures_max") %>%
        filter(ville == !!dq$city, annee %in% !!annees_a_filtrer,
               (mois * 100L + jour_mois) %in% !!cles) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max)
    })

    output$boxplot_titre <- renderUI({
      data_quiz <- quiz_data()
      req(data_quiz)
      fmt1 <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",")
      main_title <- paste0("Distribution historique ", autour_de(data_quiz$city),
                           " (vers le ", paste(format(data_quiz$date, "%d"),
                           mois_fr[as.numeric(format(data_quiz$date, "%m"))]), ", ±7 j)")
      actif <- repere_actif()
      sous_titre <- if (!is.null(actif))
        paste0(actif$titre, " : zone normale ", fmt1(actif$p10), "–", fmt1(actif$p90),
               " °C · moyenne ", fmt1(actif$moy), " °C")
      else paste("Période", filtres_serie()$periode)
      tagList(
        tags$p(class = "fw-bold mb-1", main_title),
        tags$p(class = "text-muted small mb-2", sous_titre)
      )
    })

    output$feedback_boxplot <- renderPlotly({
      donnees_historiques_jour_plot <- boxplot_rows()
      req(nrow(donnees_historiques_jour_plot) > 0)
      data_quiz <- quiz_data()
      fmt1 <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",")
      seuils <- seuils_quiz()
      proj   <- projections_quiz()
      actif  <- repere_actif()

      all_y <- c(donnees_historiques_jour_plot$tmax_celsius, data_quiz$temp)
      if (!is.null(seuils)) all_y <- c(all_y, seuils$p10, seuils$p90)
      if (!is.null(proj)) for (n in proj$niveaux) all_y <- c(all_y, n$p10, n$p90)
      yr <- range(all_y, na.rm = TRUE); yr <- yr + c(-1, 1) * 0.06 * diff(yr)

      p <- ggplot(donnees_historiques_jour_plot, aes(x = "", y = tmax_celsius)) +
        # outlier.shape = NA masque les outliers au rendu ggplot statique. NB :
        # ggplotly l'IGNORE et réaffiche les outliers du box trace ; ils sont donc
        # masqués côté plotly plus bas (boxpoints = FALSE).
        geom_boxplot(width = 0.5, fill = "skyblue", alpha = 0.7, outlier.shape = NA) +
        geom_jitter(aes(text = paste("Date :", format(date, "%d %b %Y"),
                                     "<br>Température :", round(tmax_celsius, 1), "°C")),
                    width = 0.18, alpha = 0.4, color = "darkblue") +
        geom_point(data = data.frame(temp_quiz = data_quiz$temp),
                   aes(x = "", y = temp_quiz,
                       text = paste("Température du quiz :", data_quiz$temp, "°C")),
                   color = "red", size = 4, shape = 4, stroke = 1.5, alpha = 0.85) +
        scale_y_continuous(labels = ~paste(.x, "°C")) +
        labs(x = "", y = "Température Maximale") +
        theme_minimal(base_size = 12)

      if (!is.null(actif)) {
        p <- p +
          annotate("rect", xmin = 0.5, xmax = 1.5, ymin = actif$p10, ymax = actif$p90,
                   fill = actif$couleur, alpha = 0.15) +
          geom_hline(yintercept = c(actif$p10, actif$p90),
                     linetype = "dashed", color = actif$couleur, linewidth = 0.8) +
          geom_point(data = data.frame(y = actif$moy),
                     aes(x = "", y = y, text = paste("Moyenne :", fmt1(actif$moy), "°C")),
                     shape = 4, size = 4, color = "black")
      }

      gp <- ggplotly(p, tooltip = "text") %>%
        layout(xaxis = list(fixedrange = TRUE),
               yaxis = list(fixedrange = TRUE, range = yr),
               margin = list(t = 10)) %>%
        config(displayModeBar = FALSE, responsive = TRUE)

      # ggplotly réintroduit les outliers du box trace (points noirs) malgré
      # outlier.shape = NA : on les coupe côté plotly (le jitter montre déjà tout).
      for (i in seq_along(gp$x$data)) {
        if (isTRUE(gp$x$data[[i]]$type == "box")) gp$x$data[[i]]$boxpoints <- FALSE
      }
      gp
    })

    # Meilleur score personnel (lecture BDD sous garde) — affiché au bilan. On
    # combine avec le score de la série courante (max) pour rester juste quelle
    # que soit la fenêtre d'écriture en base (course avec server.R sans incidence).
    output$record_perso <- renderUI({
      req(etat() == "resultats")
      if (!isTRUE(quiz_scores_disponibles)) return(NULL)
      vid <- visitor_id()
      if (is.null(vid) || !nzchar(vid)) return(NULL)
      n <- length(serie()); sc <- score_serie()
      meilleur_bdd <- tryCatch({
        res <- tbl(db_pool, "quiz_series_scores") %>%
          filter(visitor_id == !!vid) %>%
          summarise(m = max(score, na.rm = TRUE)) %>% collect()
        if (nrow(res) == 0 || is.na(res$m[1])) NA_integer_ else as.integer(res$m[1])
      }, error = function(e) NA_integer_)
      record <- max(c(sc, meilleur_bdd), na.rm = TRUE)
      div(class = "record-perso text-muted small mt-1",
          sprintf("Votre meilleur score : %d/%d", record, n))
    })

    # --- Transitions --------------------------------------------------------
    tirer_serie <- function(periode, ville, saison) {
      candidats <- tryCatch(
        charger_candidats_quiz(db_pool, periode, ville, saison),
        error = function(e) { log_debug("charger_candidats_quiz : ", conditionMessage(e)); NULL })
      echantillonner_serie(candidats, N_QUESTIONS)
    }

    demarrer_serie <- function(nouvelle) {
      serie(nouvelle)
      idx(1L)
      reponses(vector("list", length(nouvelle)))
      phase_manche("question")
      feedback_courant(NULL)
      quiz_data(nouvelle[[1]])
      # Préchargement groupé des données statiques (seuils + projections) des 10
      # manches : chaque révélation lira ces valeurs en mémoire au lieu d'émettre
      # 2-3 requêtes. Repli gracieux (NULL) -> requêtes à la volée si échec.
      periode <- filtres_serie()$periode
      feedback_statique(tryCatch(
        precalculer_feedback_serie(db_pool, nouvelle, periode),
        error = function(e) {
          log_debug("precalculer_feedback_serie : ", conditionMessage(e)); NULL }))
      debut_serie(Sys.time())
      etat("jeu")
    }

    observeEvent(input$lancer_btn, {
      req(input$periode_normale)
      periode <- input$periode_normale
      ville   <- if (is.null(input$ville_select_quiz)) "Toutes les villes" else input$ville_select_quiz
      saison  <- if (is.null(input$saison_select)) "Toutes les saisons" else input$saison_select
      poli    <- isTRUE(input$trash_talk_mode)

      nouvelle <- tirer_serie(periode, ville, saison)
      if (length(nouvelle) == 0) {
        showNotification(
          "Aucune donnée pour ce choix. Essayez « Toutes les villes » ou « Toutes les saisons ».",
          type = "error", duration = 8)
        return()
      }
      if (length(nouvelle) < N_QUESTIONS) {
        showNotification(
          sprintf("Données limitées pour ce choix : série de %d questions. Élargissez la ville ou la saison pour 10.",
                  length(nouvelle)),
          type = "warning", duration = 7)
      }
      filtres_serie(list(periode = periode, ville = ville, saison = saison, poli = poli))
      demarrer_serie(nouvelle)
    })

    observeEvent(input$valider_btn, {
      req(etat() == "jeu", phase_manche() == "question")
      ans <- input[[paste0("rep_", idx())]]
      if (is.null(ans) || !nzchar(ans)) {
        showNotification("Choisissez une réponse avant de valider.", type = "message", duration = 3)
        return()
      }
      q <- serie()[[idx()]]
      is_correct <- identical(ans, q$correct_answer)
      if (is_correct) score_succes(score_succes() + 1) else score_echecs(score_echecs() + 1)

      r <- reponses(); r[[idx()]] <- list(user_answer = ans, juste = is_correct); reponses(r)

      # Commentaire taquin de la manche : escalade selon le n-ième succès/échec de
      # la série (cohérent avec le quiz historique), bascule taquin/poli.
      repondus <- Filter(Negate(is.null), r)
      corrects <- sum(vapply(repondus, function(x) isTRUE(x$juste), logical(1)))
      rang <- if (is_correct) corrects else (length(repondus) - corrects)
      taquin_manche(commentaire_manche(is_correct, rang, isTRUE(filtres_serie()$poli)))

      quiz_data(q)
      statique <- feedback_statique()
      stat_manche <- if (!is.null(statique) && length(statique) >= idx()) statique[[idx()]] else NULL
      fb <- calculer_feedback_manche(db_pool, q, filtres_serie()$periode, statique = stat_manche)
      feedback_courant(fb)
      seuils_quiz(fb$seuils)
      projections_quiz(fb$projections)

      # Paramètres de la carte de partage de la manche (situe la température vs
      # les normales, comme dans le quiz historique).
      dernier_resultat(list(
        ville = q$city, date = q$date, temp = q$temp, normale_moy = q$normale_moy,
        periode_ref = filtres_serie()$periode, categorie = q$correct_answer,
        juste = is_correct, reponse_utilisateur = ans,
        p10 = if (!is.null(fb$seuils)) fb$seuils$p10 else NA_real_,
        p90 = if (!is.null(fb$seuils)) fb$seuils$p90 else NA_real_,
        projection_txt = fb$projection_txt, projection_couleur = fb$projection_couleur))
      phase_manche("feedback")
    })

    observeEvent(input$suivant_btn, {
      req(etat() == "jeu", phase_manche() == "feedback")
      if (idx() < length(serie())) {
        idx(idx() + 1L)
        quiz_data(serie()[[idx()]])
        feedback_courant(NULL)
        phase_manche("question")
      } else {
        # Fin de série : on fige le bilan et on remonte l'événement à server.R.
        f <- filtres_serie()
        duree <- if (!is.null(debut_serie()))
          as.integer(round(difftime(Sys.time(), debut_serie(), units = "secs"))) else NA_integer_
        serie_terminee(list(
          score = score_serie(), nb_questions = length(serie()),
          periode_ref = f$periode, ville_filtre = f$ville, saison_filtre = f$saison,
          duree_seconds = duree, stamp = Sys.time()))
        etat("resultats")
      }
    })

    observeEvent(input$rejouer_btn, {
      f <- filtres_serie(); req(f)
      nouvelle <- tirer_serie(f$periode, f$ville, f$saison)
      if (length(nouvelle) == 0) {
        showNotification("Impossible de regénérer une série pour ce choix.", type = "error")
        return()
      }
      demarrer_serie(nouvelle)
    })

    observeEvent(input$reglages_btn, { etat("accueil") })

    # Abandon d'une série en cours : confirmation (la progression est perdue) puis
    # retour au paramétrage. On ne fige AUCUN score en base (seules les séries
    # terminées sont enregistrées) ; les manches déjà validées restent comptées
    # dans le cumul de session (analytics), comme lors d'une fermeture d'onglet.
    observeEvent(input$abandonner_btn, {
      showModal(modalDialog(
        title = "Abandonner la série ?",
        "Votre progression sur cette série sera perdue et vous reviendrez au paramétrage.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Continuer la série"),
          actionButton(ns("abandonner_confirme"), "Abandonner",
                       icon = icon("xmark"), class = "btn-danger")
        )
      ))
    })

    observeEvent(input$abandonner_confirme, {
      removeModal()
      etat("accueil")
    })

    # --- Partage de la carte de résultat de la manche (PNG 1200×630) --------
    # On situe la température de la question vs les normales (carte historique),
    # générée à la volée et embarquée (base64) dans le modal de partage partagé.
    observeEvent(input$partager_btn, {
      res <- dernier_resultat()
      req(res)
      fpng <- tempfile(fileext = ".png")
      sauver_carte_partage(res, fpng)
      on.exit(unlink(fpng), add = TRUE)
      data_uri <- paste0("data:image/png;base64,",
                         jsonlite::base64_enc(readBin(fpng, "raw", n = file.info(fpng)$size)))
      ecart <- round(res$temp - res$normale_moy, 1)
      sens <- if (ecart > 0) "au-dessus" else if (ecart < 0) "en-dessous" else "dans"
      texte <- if (ecart == 0)
        paste0(res$temp, "°C ", autour_de(res$ville),
               " : exactement dans la normale de saison. Et vous, sauriez-vous situer ce qui est normal ?")
      else
        paste0(res$temp, "°C ", autour_de(res$ville), " : ", sprintf("%+.1f", ecart),
               "°C ", sens, " de la normale de saison. Et vous, sauriez-vous situer ce qui est normal ?")
      nom_fichier <- paste0("normal-ou-pas_", gsub("[^A-Za-z0-9]+", "_", res$ville), ".png")
      showModal(modal_partage(data_uri, texte, nom_fichier))
    })

    # Contrat avec server.R : cumul de session INCHANGÉ (analytics_visits) +
    # événement de série terminée (persistance BDD, câblée dans server.R).
    return(list(
      successes = score_succes,
      failures = score_echecs,
      serie_terminee = serie_terminee
    ))
  })
}
