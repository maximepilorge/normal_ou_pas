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

mod_quiz_server <- function(id, db_pool, visitor_id = reactive(NULL),
                            prefill = reactive(NULL), defi_recu = reactive(NULL),
                            naviguer = NULL) {
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
    defi_attente    <- reactiveVal(NULL)        # défi reçu par lien, pas encore accepté
    mode_defi       <- reactiveVal(NULL)        # défi de la série EN COURS (score à battre)

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
      # Un défi reçu prend le pas sur le paramétrage : écran d'invitation dédié.
      da <- defi_attente()
      if (!is.null(da)) return(ecran_defi(da))
      f <- filtres_serie()
      sel <- function(cle, defaut) if (!is.null(f) && !is.null(f[[cle]])) f[[cle]] else defaut
      div(class = "quiz-card card",
        div(class = "card-body",
          h2("Prêt à tester vos repères climatiques ?", class = "quiz-titre"),
          p(class = "text-muted",
            "10 températures, 10 verdicts. Saurez-vous dire si chacune est normale ou pas ?"),
          layout_columns(
            col_widths = c(4, 4, 4), gap = "0.75rem",
            pickerInput(ns("periode_normale"), "Époque de référence",
                        choices = libelles_periodes(periodes_disponibles),
                        selected = sel("periode", periodes_disponibles[1]),
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

    # Écran d'invitation quand un DÉFI a été reçu par lien : la série est imposée
    # par le payload (mêmes questions que l'expéditeur), pas de paramétrage.
    ecran_defi <- function(da) {
      n <- length(da$serie)
      div(class = "quiz-card card",
        div(class = "card-body text-center",
          h2("On vous a défié !", class = "quiz-titre"),
          p(class = "lead mb-1",
            if (is.finite(da$score))
              sprintf("Un ami a fait %d/%d sur cette série. À vous de faire mieux.", da$score, n)
            else
              sprintf("Jouez exactement la même série de %d questions que votre ami.", n)),
          p(class = "text-muted small",
            sprintf("Les %d questions sont identiques aux siennes (normale de référence %s).",
                    n, da$periode)),
          actionButton(ns("defi_btn"), "Relever le défi", icon = icon("bolt"),
                       class = "btn-warning btn-lg w-100 mt-2"),
          div(class = "mt-2",
              actionLink(ns("defi_refus"), "Non merci, je préfère régler ma propre série"))
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
      # Ville du lien de rebond vers « Évolution » : celle de la série si elle est
      # filtrée, sinon celle de la dernière manche (le libellé la nomme, aucune
      # ambiguïté pour l'utilisateur).
      ville_bilan <- if (!identical(f$ville, "Toutes les villes")) f$ville
                     else serie()[[n]]$city
      # Ligne de comparaison si la série était un défi chiffré.
      ligne_defi <- if (!is.null(mode_defi()) && is.finite(mode_defi()$score)) {
        sa <- mode_defi()$score
        verdict <- if (sc > sa) "défi remporté !"
                   else if (sc == sa) "égalité parfaite."
                   else "défi perdu… revanche ?"
        p(class = "fw-semibold mt-1",
          sprintf("Score de votre ami : %d/%d — %s", sa, n, verdict))
      }

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
        ligne_defi,
        uiOutput(ns("record_perso")),
        hr(),
        h5("Le détail de votre série", class = "text-start"),
        div(class = "recap text-start", lignes),
        p(class = "text-muted small mt-2", .contexte_serie(f$ville, f$saison, f$periode)),
        # CTA principal du bilan : la bascule vers « Évolution » (pré-remplie sur
        # la ville de la série), mise en avant dans un encart dédié — rejouer et
        # défier passent en actions secondaires.
        div(class = "mt-3 p-3 rounded text-start",
            style = "background:#eef7f2; border:1px solid #cfe5da;",
            p(class = "mb-2 fw-semibold",
              paste0("La suite : découvrez comment le climat a réellement changé ",
                     autour_de(ville_bilan), ", année par année.")),
            actionButton(ns("voir_evolution_btn"), "Voir l'évolution du climat",
                         icon = icon("chart-line"), class = "btn-success btn-lg w-100")),
        div(class = "mt-3",
            actionButton(ns("rejouer_btn"), "Rejouer une série",
                         icon = icon("rotate-right"), class = "btn-outline-primary m-1"),
            actionButton(ns("defier_btn"), "Défier un ami",
                         icon = icon("bolt"), class = "btn-outline-primary m-1"))
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
      main_title <- paste0("Distribution historique ", autour_de(data_quiz$city),
                           " (vers le ", paste(format(data_quiz$date, "%d"),
                           mois_fr[as.numeric(format(data_quiz$date, "%m"))]), ", ±7 j)")
      actif <- repere_actif()
      sous_titre <- if (!is.null(actif))
        paste0(actif$titre, " : zone normale ", fmt_temp(actif$p10), "–", fmt_temp(actif$p90),
               " °C · moyenne ", fmt_temp(actif$moy), " °C")
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
      seuils <- seuils_quiz()
      proj   <- projections_quiz()
      actif  <- repere_actif()

      yv <- donnees_historiques_jour_plot$tmax_celsius
      all_y <- c(yv, data_quiz$temp)
      if (!is.null(seuils)) all_y <- c(all_y, seuils$p10, seuils$p90)
      if (!is.null(proj)) for (n in proj$niveaux) all_y <- c(all_y, n$p10, n$p90)
      yr <- range(all_y, na.rm = TRUE); yr <- yr + c(-1, 1) * 0.06 * diff(yr)

      # Boxplot en plot_ly NATIF (au lieu de ggplot + ggplotly, dont la conversion
      # est le poste dominant du délai). Le CORPS du box (Q1/médiane/Q3, moustaches
      # à 1,5×IQR, capuchons) est dessiné en SHAPES : le box trace plotly sur un axe
      # numérique ne se rend pas de façon fiable ici, alors que les shapes sont
      # déterministes. On maîtrise ainsi les moustaches (bornes explicites, pas de
      # min/max) et il n'y a aucun point d'outlier à masquer. Le jitter (à la main,
      # plot_ly ne jitte pas) affiche tous les points.
      vf <- yv[is.finite(yv)]
      qs <- as.numeric(stats::quantile(vf, c(.25, .5, .75), type = 7))
      iqr <- qs[3] - qs[1]
      lf <- min(vf[vf >= qs[1] - 1.5 * iqr]); uf <- max(vf[vf <= qs[3] + 1.5 * iqr])
      jitter_x <- 1 + runif(length(yv), -0.18, 0.18)

      dark <- "rgba(51,51,51,1)"; bx0 <- 0.75; bx1 <- 1.25   # box centré sur x = 1

      # Repère « normale » actif (présent/projeté) : zone p10–p90 + bornes (pleine
      # largeur, sous les points).
      shapes <- list()
      if (!is.null(actif)) shapes <- c(shapes, list(
        list(type = "rect", xref = "paper", yref = "y", x0 = 0, x1 = 1,
             y0 = actif$p10, y1 = actif$p90, fillcolor = actif$couleur,
             opacity = 0.15, line = list(width = 0), layer = "below"),
        list(type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1,
             y0 = actif$p10, y1 = actif$p10,
             line = list(color = actif$couleur, dash = "dash", width = 1.6), layer = "below"),
        list(type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1,
             y0 = actif$p90, y1 = actif$p90,
             line = list(color = actif$couleur, dash = "dash", width = 1.6), layer = "below")))

      # Corps du boxplot : remplissage sous les points, contour + médiane +
      # moustaches + capuchons au-dessus (lisibles par-dessus le nuage).
      shapes <- c(shapes, list(
        list(type = "rect", xref = "x", yref = "y", x0 = bx0, x1 = bx1,
             y0 = qs[1], y1 = qs[3], fillcolor = "rgba(135,206,235,0.7)",
             line = list(width = 0), layer = "below"),
        list(type = "rect", xref = "x", yref = "y", x0 = bx0, x1 = bx1,
             y0 = qs[1], y1 = qs[3], fillcolor = "rgba(0,0,0,0)",
             line = list(color = dark, width = 1.5), layer = "above"),
        list(type = "line", xref = "x", yref = "y", x0 = bx0, x1 = bx1,
             y0 = qs[2], y1 = qs[2], line = list(color = dark, width = 2), layer = "above"),
        list(type = "line", xref = "x", yref = "y", x0 = 1, x1 = 1,
             y0 = qs[3], y1 = uf, line = list(color = dark, width = 1.2), layer = "above"),
        list(type = "line", xref = "x", yref = "y", x0 = 1, x1 = 1,
             y0 = qs[1], y1 = lf, line = list(color = dark, width = 1.2), layer = "above"),
        list(type = "line", xref = "x", yref = "y", x0 = 0.87, x1 = 1.13,
             y0 = uf, y1 = uf, line = list(color = dark, width = 1.2), layer = "above"),
        list(type = "line", xref = "x", yref = "y", x0 = 0.87, x1 = 1.13,
             y0 = lf, y1 = lf, line = list(color = dark, width = 1.2), layer = "above")))

      fig <- plot_ly() %>%
        add_markers(x = jitter_x, y = yv,
                    marker = list(color = "rgba(0,0,139,0.4)", size = 5),
                    text = paste("Date :", format(donnees_historiques_jour_plot$date, "%d %b %Y"),
                                 "<br>Température :", round(yv, 1), "°C"),
                    hoverinfo = "text", showlegend = FALSE) %>%
        add_markers(x = 1, y = data_quiz$temp,
                    marker = list(symbol = "x", color = "rgba(255,0,0,0.85)", size = 12,
                                  line = list(color = "rgba(255,0,0,0.85)", width = 1.5)),
                    text = paste("Température du quiz :", data_quiz$temp, "°C"),
                    hoverinfo = "text", showlegend = FALSE)

      if (!is.null(actif))
        fig <- fig %>% add_markers(x = 1, y = actif$moy,
                    marker = list(symbol = "x", color = "black", size = 10),
                    text = paste("Moyenne :", fmt_temp(actif$moy), "°C"),
                    hoverinfo = "text", showlegend = FALSE)

      fig %>%
        layout(shapes = shapes,
               xaxis = list(fixedrange = TRUE, showticklabels = FALSE, showgrid = FALSE,
                            zeroline = FALSE, range = c(0.5, 1.5)),
               yaxis = list(fixedrange = TRUE, range = yr, title = "Température Maximale",
                            ticksuffix = " °C"),
               font = list(size = 12), margin = list(t = 10), showlegend = FALSE) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
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
      # Une série fraîchement démarrée n'est pas un défi ; l'acceptation d'un
      # défi (defi_btn) repose mode_defi APRÈS cet appel.
      mode_defi(NULL)
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

    # « Rejouer une série » repasse par le paramétrage (écran d'accueil, filtres
    # de la dernière série pré-remplis) : on re-choisit avant de relancer.
    observeEvent(input$rejouer_btn, { etat("accueil") })

    # Défi reçu par lien (?defi=..., déjà validé par server.R) : mis en attente et
    # affiché sur l'écran d'accueil, sans jamais interrompre une série en cours.
    observeEvent(defi_recu(), {
      da <- defi_recu()
      req(da)
      defi_attente(da)
      if (etat() != "jeu") etat("accueil")
    })

    # Acceptation du défi : la série du payload remplace tout tirage aléatoire.
    observeEvent(input$defi_btn, {
      da <- defi_attente(); req(da)
      filtres_serie(list(periode = da$periode, ville = "Toutes les villes",
                         saison = "Toutes les saisons", poli = FALSE))
      demarrer_serie(da$serie)   # remet mode_defi à NULL...
      mode_defi(da)              # ...donc on le pose APRÈS
      defi_attente(NULL)
    })

    observeEvent(input$defi_refus, { defi_attente(NULL) })

    # « Défier un ami » : sérialise la série jouée dans un lien absolu ; l'ami
    # rejouera exactement les mêmes questions (?defi=..., validé à la réception).
    observeEvent(input$defier_btn, {
      f <- filtres_serie(); req(f, etat() == "resultats", length(serie()) > 0)
      payload <- serialiser_defi(serie(), f$periode, score_serie())
      lien <- paste0(url_base_app(session), "?defi=",
                     utils::URLencode(payload, reserved = TRUE))
      texte <- sprintf(
        "J'ai fait %d/%d au quiz « Climat : normal ou pas ? ». Mêmes questions pour toi : tu fais mieux ?",
        score_serie(), length(serie()))
      showModal(modalDialog(
        title = "Défier un ami", easyClose = TRUE,
        p(sprintf(
          "Envoyez ce lien : votre ami jouera exactement la même série de %d questions, avec votre score à battre.",
          length(serie()))),
        div(class = "input-group",
            tags$input(id = ns("lien_defi"), type = "text", class = "form-control",
                       readonly = "readonly", value = lien),
            tags$button(class = "btn btn-primary", type = "button",
                        onclick = sprintf("partageCopierLien('%s')", ns("lien_defi")),
                        icon("copy"), " Copier")),
        div(class = "text-center mt-3",
            tags$button(class = "btn btn-outline-secondary", type = "button",
                        `data-texte` = texte,
                        onclick = sprintf("partagePartagerLien('%s', this.dataset.texte)",
                                          ns("lien_defi")),
                        icon("share-nodes"), " Partager (mobile)")),
        footer = modalButton("Fermer")))
    })

    # Ville pré-remplie (lien interne « Testez vos repères sur cette ville » ou
    # permalien) : ne touche jamais une série EN COURS — on ajuste le paramétrage
    # de l'écran d'accueil et on y ramène l'utilisateur.
    observeEvent(prefill(), {
      pf <- prefill()
      req(pf, pf$ville)
      if (etat() == "jeu") {
        showNotification(
          "Une série est en cours : terminez-la (ou abandonnez-la) pour en lancer une sur cette ville.",
          type = "message", duration = 5)
        return()
      }
      f <- filtres_serie()
      if (is.null(f)) f <- list(periode = periodes_disponibles[1],
                                saison = "Toutes les saisons", poli = FALSE)
      f$ville <- pf$ville
      filtres_serie(f)
      etat("accueil")
    })

    # Rebond du bilan vers « Évolution », pré-rempli sur la ville de la série.
    observeEvent(input$voir_evolution_btn, {
      f <- filtres_serie(); req(f, length(serie()) > 0)
      v <- if (!identical(f$ville, "Toutes les villes")) f$ville
           else serie()[[length(serie())]]$city
      if (is.function(naviguer)) naviguer("evolution", ville = v)
    })

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
