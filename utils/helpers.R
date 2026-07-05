# utils/helpers.R
#
# Utilitaires partagés de l'application (aucune dépendance BDD), regroupés ici
# pour être partagés par les modules et, pour les fonctions pures, testables
# isolément (tests/testthat). Sourcé par global.R, donc disponible dans tous les
# modules. Seul `modal_partage` construit de l'UI Shiny (au moment de l'appel).

# Saison (hémisphère nord) d'une date : libellé pour les phrases + mois associés.
get_season_info <- function(date_input) {
  mois <- lubridate::month(date_input)
  if (mois %in% c(12, 1, 2)) {
    list(nom = "en hiver", mois = c(12, 1, 2))
  } else if (mois %in% c(3, 4, 5)) {
    list(nom = "au printemps", mois = c(3, 4, 5))
  } else if (mois %in% c(6, 7, 8)) {
    list(nom = "en été", mois = c(6, 7, 8))
  } else { # 9, 10, 11
    list(nom = "en automne", mois = c(9, 10, 11))
  }
}

# Détection « petit écran » : vrai si la largeur réelle (px) transmise par le
# client pour un output donné est sous le seuil (~smartphone en portrait).
# Centralise le motif jusqu'ici recopié dans chaque module. Booléen (et non
# largeur brute) pour ne ré-invalider les rendus qu'au franchissement du seuil.
#   session   : objet session du module
#   output_id : identifiant (namespacé) de l'output, ex. ns("evolution_plot")
largeur_sous_seuil <- function(session, output_id, seuil = 500) {
  w <- session$clientData[[paste0("output_", output_id, "_width")]]
  !is.null(w) && w > 0 && w < seuil
}

# Dimensions rendues valides d'un output (largeur ET hauteur > 0). Sert de garde
# req() dans les renders de graphes vivant dans des (sous-)onglets : un rendu
# déclenché pendant la transition d'affichage — taille encore nulle — plante le
# device (« 'width' ou 'height' incorrecte », « figure margins too large ») et
# laisse le graphe vide. Booléen : le render se relance quand la taille devient
# valide (le faux -> vrai ré-invalide), puis reste stable.
dimensions_valides <- function(session, output_id) {
  w <- session$clientData[[paste0("output_", output_id, "_width")]]
  h <- session$clientData[[paste0("output_", output_id, "_height")]]
  !is.null(w) && !is.null(h) && w > 0 && h > 0
}

# Journalisation de debug conditionnelle : n'émet un message() que si l'option
# `normaloupas.debug` est active (options(normaloupas.debug = TRUE) en dev, ou
# variable d'env NORMALOUPAS_DEBUG=1). Silencieux en production par défaut.
log_debug <- function(...) {
  actif <- isTRUE(getOption(
    "normaloupas.debug",
    default = identical(Sys.getenv("NORMALOUPAS_DEBUG"), "1")
  ))
  if (actif) message(...)
}

# Formate une température en français : arrondi à `dec` décimales, virgule
# décimale, SANS suffixe (le « °C » est ajouté par l'appelant). Centralise le
# motif format(round(...), decimal.mark = ",") recopié dans chaque module.
fmt_temp <- function(x, dec = 1) {
  format(round(as.numeric(x), dec), nsmall = dec, decimal.mark = ",")
}

# Bornes (début, fin) d'un libellé de période « AAAA-AAAA ». Pure.
.periode_bornes <- function(p) as.numeric(strsplit(p, "-")[[1]])

# Sous-titres « grand public » des périodes de référence, affichés dans les
# menus déroulants SOUS chaque période (via choicesOpt$subtext de pickerInput) :
# « il y a environ 60 ans », la plus récente devenant « la normale actuelle ».
# Le libellé principal reste la période elle-même (« 1951-1980 ») : court, il ne
# se tronque pas dans le bouton fermé, et l'ancienneté ne présume pas de l'âge
# de l'utilisateur (contrairement à « l'époque de vos grands-parents »).
# Ancienneté du centre de la période, arrondie aux 5 ans (stable d'une année
# sur l'autre) ; `annee` paramétrable pour la testabilité. Pure.
soustitres_periodes <- function(periodes, annee = as.integer(format(Sys.Date(), "%Y"))) {
  if (length(periodes) == 0) return(character(0))
  debuts <- vapply(periodes, function(p) .periode_bornes(p)[1], numeric(1))
  recente <- periodes[which.max(debuts)]
  unname(vapply(periodes, function(p) {
    if (identical(p, recente)) return("la normale actuelle")
    age <- 5 * round((annee - mean(.periode_bornes(p))) / 5)
    paste0("il y a environ ", age, " ans")
  }, character(1)))
}

# --- Permaliens & navigation inter-onglets ------------------------------------
# Slugs des onglets dans l'URL (?onglet=...). Source de vérité des `value` des
# nav_panel de ui.R et des cibles de nav_select (server.R).
ONGLETS_APP <- c("quiz", "comparer", "jour", "evolution", "methodo")

# Construit la query string d'un permalien (?onglet=...&ville=...&date=...&annee=...)
# à partir de l'onglet actif et de l'état du module affiché (liste avec, au choix,
# ville / date / annee ; les champs NULL sont omis). Encodage URL complet des
# valeurs (accents, espaces). Pure, testable.
construire_query_string <- function(onglet, etat = NULL) {
  params <- c(onglet = onglet)
  if (!is.null(etat)) {
    if (!is.null(etat$ville) && nzchar(etat$ville))
      params <- c(params, ville = as.character(etat$ville))
    if (!is.null(etat$date))
      params <- c(params, date = format(as.Date(etat$date), "%Y-%m-%d"))
    if (!is.null(etat$annee))
      params <- c(params, annee = as.character(etat$annee))
  }
  valeurs <- vapply(params, utils::URLencode, character(1), reserved = TRUE)
  paste0("?", paste0(names(params), "=", valeurs, collapse = "&"))
}

# URL de base de l'app (protocole, hôte, port non standard, chemin) reconstituée
# depuis le clientData d'une session Shiny. Sert aux liens absolus (défi de série).
url_base_app <- function(session) {
  cd <- session$clientData
  port <- cd$url_port
  suffixe_port <- if (!is.null(port) && nzchar(port) && !port %in% c("80", "443"))
    paste0(":", port) else ""
  paste0(cd$url_protocol, "//", cd$url_hostname, suffixe_port, cd$url_pathname)
}

# --- Défi de série : (dé)sérialisation d'une série dans un lien ----------------
# Une série jouée peut être envoyée à un ami sous forme de lien (?defi=<payload>) :
# il rejoue EXACTEMENT les mêmes questions. Format texte compact (URL-encodé par
# l'appelant) :
#   "v1;periode;score;manche;manche;..."
#   manche = "ville~AAAA-MM-JJ~temp~c~normale"  (c = index dans CATEGORIES_QUIZ)
# Fonctions pures, testables.
serialiser_defi <- function(serie, periode, score = NA_integer_) {
  manches <- vapply(serie, function(q) paste(
    q$city,
    format(as.Date(q$date), "%Y-%m-%d"),
    sprintf("%.1f", q$temp),
    match(q$correct_answer, CATEGORIES_QUIZ),
    sprintf("%.1f", q$normale_moy),
    sep = "~"), character(1))
  paste(c("v1", periode,
          if (is.na(score)) "" else as.character(as.integer(score)),
          manches), collapse = ";")
}

# Relit un payload de défi et le valide STRICTEMENT (ville connue, date lisible,
# température plausible, catégorie dans les bornes, 1 à 15 manches) : renvoie
# list(periode, score, serie) ou NULL si quoi que ce soit cloche — le lien vient
# de l'extérieur, il ne doit jamais pouvoir casser l'app ni y injecter du texte.
deserialiser_defi <- function(chaine, villes_valides, periodes_valides) {
  if (is.null(chaine) || !is.character(chaine) || length(chaine) != 1) return(NULL)
  bits <- strsplit(chaine, ";", fixed = TRUE)[[1]]
  if (length(bits) < 4 || !identical(bits[1], "v1")) return(NULL)
  periode <- bits[2]
  if (!periode %in% periodes_valides) return(NULL)
  score <- suppressWarnings(as.integer(bits[3]))   # NA si absent : score facultatif
  brutes <- bits[-(1:3)]
  if (length(brutes) < 1 || length(brutes) > 15) return(NULL)
  manches <- lapply(brutes, function(m) {
    ch <- strsplit(m, "~", fixed = TRUE)[[1]]
    if (length(ch) != 5) return(NULL)
    d    <- suppressWarnings(as.Date(ch[2]))
    temp <- suppressWarnings(as.numeric(ch[3]))
    ci   <- suppressWarnings(as.integer(ch[4]))
    nm   <- suppressWarnings(as.numeric(ch[5]))
    if (!ch[1] %in% villes_valides || length(d) != 1 || is.na(d) ||
        !is.finite(temp) || temp < -45 || temp > 55 ||
        is.na(ci) || ci < 1 || ci > length(CATEGORIES_QUIZ) || !is.finite(nm)) return(NULL)
    list(city = ch[1], date = d, temp = temp,
         correct_answer = CATEGORIES_QUIZ[ci], normale_moy = nm)
  })
  if (any(vapply(manches, is.null, logical(1)))) return(NULL)
  if (!is.na(score) && (score < 0 || score > length(manches))) score <- NA_integer_
  list(periode = periode, score = score, serie = manches)
}

# Réchauffement (°C) « depuis une année d'origine » à partir des anomalies
# annuelles d'une ville : moyenne des anomalies des `fenetre` dernières années
# disponibles MOINS moyenne sur la fenêtre de même taille centrée sur l'année
# d'origine (couvrant exactement `fenetre` années, parité comprise). Chaque
# moyenne exige au moins `min_annees` valeurs (séries trouées) ; NA_real_ sinon.
# `anomalies` : data.frame(annee, anomalie). Sert à l'accroche « jour de ma
# naissance » (15 ans) et aux rayures climatiques (30 ans, cohérent avec
# l'« Analyse du réchauffement » de l'onglet Évolution). Pure.
rechauffement_depuis <- function(anomalies, annee_origine, fenetre = 15, min_annees = 8) {
  if (is.null(anomalies) || nrow(anomalies) == 0 || !is.finite(annee_origine))
    return(NA_real_)
  a <- anomalies[is.finite(anomalies$annee) & is.finite(anomalies$anomalie), ]
  a <- a[order(a$annee), ]
  if (nrow(a) == 0) return(NA_real_)
  demi <- (fenetre - 1) %/% 2
  origine <- a$anomalie[a$annee >= annee_origine - demi &
                        a$annee <= annee_origine + (fenetre - 1 - demi)]
  recentes <- utils::tail(a$anomalie, fenetre)
  if (length(origine) < min_annees || length(recentes) < min_annees) return(NA_real_)
  mean(recentes) - mean(origine)
}

# Rang d'une valeur dans une distribution (onglet « Une journée »). Sur le vecteur
# des tmax de la fenêtre ±7 j (toutes années), renvoie :
#   - rang_haut : 1 = valeur la plus CHAUDE (nb de valeurs strictement supérieures + 1)
#   - rang_bas  : 1 = valeur la plus FROIDE
#   - n         : nombre de valeurs valides considérées
# Les ex æquo ne gonflent pas le rang (comparaisons strictes). Fonction pure.
classer_jour_extreme <- function(valeurs_fenetre, valeur_jour) {
  v <- valeurs_fenetre[is.finite(valeurs_fenetre)]
  list(
    n = length(v),
    rang_haut = sum(v > valeur_jour) + 1L,
    rang_bas  = sum(v < valeur_jour) + 1L
  )
}

# --- Quiz « Série de 10 » : fonctions pures (tirage et commentaires) -----------
# Regroupées ici (sans dépendance BDD) pour être testées isolément. La requête
# qui alimente `candidats` (agrégat de quiz_data_precalculee) vit dans
# modules/mod_quiz.R ; le tirage des questions et le commentaire de bilan, eux,
# sont purs.

# Catégories du quiz (ordre stable, sert au tirage équilibré et aux tests).
CATEGORIES_QUIZ <- c("En-dessous des normales",
                     "Dans les normales de saison",
                     "Au-dessus des normales")

# Classe une température vs la zone normale (p10–p90) d'un jour : renvoie l'une des
# CATEGORIES_QUIZ. Bornes indisponibles (NA/non finies) -> « Dans les normales de
# saison » (comportement historique des onglets). Fonction pure, testable.
classer_normale <- function(temp, p10, p90) {
  if (!is.finite(p10) || !is.finite(p90)) return("Dans les normales de saison")
  if (temp > p90) "Au-dessus des normales"
  else if (temp < p10) "En-dessous des normales"
  else "Dans les normales de saison"
}

# Couleur d'accent d'une catégorie : rouge (au-dessus) / bleu (en-dessous) / vert
# (dans les normales). Miroir de .couleur_categorie de render_partage.R, qui reste
# autonome (réutilisé par le sidecar plumber sans charger helpers.R).
couleur_categorie <- function(categorie) {
  if (grepl("Au-dessus", categorie)) "#E41A1C"
  else if (grepl("En-dessous", categorie)) "#1f77b4"
  else "#2E8B57"
}

# Mois (numéros) d'une saison à partir de son libellé d'interface. NULL pour
# « Toutes les saisons » (aucun filtre) ou tout libellé inconnu. Sert au
# pré-chargement des candidats de la série.
mois_saison <- function(saison) {
  switch(saison,
         "Hiver"     = c(12L, 1L, 2L),
         "Printemps" = c(3L, 4L, 5L),
         "Été"       = c(6L, 7L, 8L),
         "Automne"   = c(9L, 10L, 11L),
         NULL)
}

# Répartition ~équilibrée de n questions entre des catégories : floor(n/k)
# chacune, le reste distribué aléatoirement. Renvoie un vecteur nommé d'entiers
# sommant à n (l'aléa ne porte que sur l'attribution du reste).
repartition_cibles <- function(n, categories = CATEGORIES_QUIZ) {
  base <- n %/% length(categories)
  cibles <- stats::setNames(rep.int(base, length(categories)), categories)
  reste <- n - sum(cibles)
  if (reste > 0) {
    for (c in sample(categories, reste)) cibles[[c]] <- cibles[[c]] + 1L
  }
  cibles
}

# Matérialise UNE question à partir d'une ligne candidate agrégée
# (ville, mois, jour_mois, categorie, min_temp, max_temp, normale_moy) : tire une
# température au hasard dans [min, max] (pas 0,1 °C), exactement comme le quiz
# historique. La date est reconstruite sur une année bissextile (seuls le jour et
# le mois comptent pour l'affichage et le feedback).
materialiser_question <- function(ligne) {
  min_v <- round(ligne$min_temp, 1)
  max_v <- round(ligne$max_temp, 1)
  temp <- if (min_v == max_v) min_v else sample(seq(min_v, max_v, by = 0.1), 1)
  list(
    city           = ligne$ville,
    date           = as.Date(sprintf("2024-%02d-%02d", ligne$mois, ligne$jour_mois)),
    temp           = temp,
    correct_answer = ligne$categorie,
    normale_moy    = round(ligne$normale_moy, 1)
  )
}

# Tirage sûr d'une permutation des éléments d'un vecteur d'INDICES : évite le
# piège de sample(x) qui, pour un x scalaire, échantillonne 1:x.
.melanger <- function(v) v[sample.int(length(v))]

# Échantillonne n questions dans le pool de candidats agrégés (une ligne par
# couple (ville, jour calendaire, catégorie) existant). Garanties :
#   - catégories ~équilibrées (repartition_cibles), avec complément si pénurie ;
#   - jamais deux questions sur le même couple (ville, jour) ;
#   - ordre des manches mélangé.
# Renvoie une liste de questions (cf. materialiser_question), de longueur
# min(n, nb de couples (ville, jour) distincts disponibles).
echantillonner_serie <- function(candidats, n = 10) {
  if (is.null(candidats) || nrow(candidats) == 0) return(list())
  cles <- paste(candidats$ville, candidats$mois, candidats$jour_mois, sep = "|")
  cibles <- repartition_cibles(n, CATEGORIES_QUIZ)
  pris <- integer(0)
  cles_prises <- character(0)
  prendre <- function(pool, k) {
    out <- integer(0)
    for (i in .melanger(pool)) {
      if (length(out) >= k) break
      if (cles[i] %in% cles_prises) next
      out <- c(out, i)
      cles_prises <<- c(cles_prises, cles[i])
    }
    out
  }
  for (cat in CATEGORIES_QUIZ) {
    pool <- which(candidats$categorie == cat & !(seq_len(nrow(candidats)) %in% pris))
    pris <- c(pris, prendre(pool, cibles[[cat]]))
  }
  if (length(pris) < n) {                       # complément si une catégorie manque
    pool <- which(!(seq_len(nrow(candidats)) %in% pris))
    pris <- c(pris, prendre(pool, n - length(pris)))
  }
  lapply(.melanger(pris), function(i) materialiser_question(candidats[i, , drop = FALSE]))
}

# Commentaire taquin AFFICHÉ APRÈS CHAQUE RÉPONSE (révélation d'une manche).
# Taquin par défaut (messages escaladant selon le `rang` = n-ième bonne/mauvaise
# réponse de la série, cohérent avec le quiz historique), neutre si l'utilisateur
# a coché « me répondre poliment ». Au-delà du pool, repli sur un message stable.
# Fonction pure, testable.
commentaire_manche <- function(juste, rang, poli = FALSE) {
  if (isTRUE(poli)) return(if (isTRUE(juste)) "Bonne réponse !" else "Mauvaise réponse.")
  succes <- c(
    "C'est la chance du débutant, j'imagine.",
    "Tu es vraiment obligé de montrer que tu sais tout mieux que tout le monde.",
    "Je pourrais presque commencer à t'apprécier, à force.",
    "Là, tu commences à me faire douter : tu n'aurais pas un thermomètre caché ?",
    "D'accord, tu sais lire les normales. Pas la peine d'en rajouter.")
  echecs <- c(
    "Tu feras mieux la prochaine fois. Enfin, j'espère pour toi.",
    "Tu ne pouvais pas mieux te tromper, félicitations !",
    "Ta détermination à échouer force l'admiration.",
    "À ce stade, je commence à croire que tu le fais exprès.",
    "Bon. On va dire que les normales, ce n'est pas tout à fait ton truc.")
  pool <- if (isTRUE(juste)) succes else echecs
  classique <- if (isTRUE(juste)) "Bien vu !" else "Raté !"
  if (rang >= 1 && rang <= length(pool)) pool[[rang]] else classique
}

# Commentaire de BILAN d'une série, indexé sur le score /n et le ton. CALIBRÉ sur
# le hasard réel d'un quiz à 3 choix : le pur hasard donne ~n/3 (≈ 3,3/10), pas
# n/2. Les paliers : < 0,3 (sous le hasard), 0,3-0,5 (autour du hasard), 0,5-0,7
# (au-dessus), 0,7-0,9 (bon), ≥ 0,9 (excellent). Taquin par défaut, neutre et
# porteur du message climatique si « poliment ». Fonction pure, testable.
commentaire_serie <- function(score, n = 10, poli = FALSE) {
  pct <- if (n > 0) score / n else 0
  if (isTRUE(poli)) {
    if (pct < 0.3)
      sprintf("Score : %d/%d. Sur trois réponses possibles à chaque question, situer le « normal » demande de l'habitude — les repères se faussent vite avec le réchauffement. Réessayez, on apprend en jouant.", score, n)
    else if (pct < 0.5)
      sprintf("Score : %d/%d. Autour du niveau du pur hasard (une chance sur trois) : pas si évident de dire ce qui est encore normal aujourd'hui.", score, n)
    else if (pct < 0.7)
      sprintf("Score : %d/%d. Au-dessus du hasard : vos repères de température se précisent.", score, n)
    else if (pct < 0.9)
      sprintf("Score : %d/%d. Beau résultat, vos repères climatiques sont solides.", score, n)
    else
      sprintf("Score : %d/%d. Excellent — vous lisez le climat comme une carte.", score, n)
  } else {
    if (score == 0)
      "Zéro pointé. Sur trois choix à chaque question, ne pas en placer une seule, il fallait presque le faire."
    else if (pct < 0.3)
      sprintf("%d sur %d. Tu as réussi à faire moins bien que le hasard. Statistiquement, c'est presque un exploit.", score, n)
    else if (pct < 0.5)
      sprintf("%d sur %d. Félicitations, te voilà à peine au-dessus du pur hasard.", score, n)
    else if (pct < 0.7)
      sprintf("%d sur %d. C'est que tu commencerais presque à être doué.", score, n)
    else if (pct < 0.9)
      sprintf("%d sur %d. Joli. Ne prends pas trop la confiance cependant.", score, n)
    else if (pct < 1)
      sprintf("%d sur %d. À un cheveu du sans-faute. Insupportable de maîtrise.", score, n)
    else
      sprintf("%d sur %d. Sans faute. Soit tu es météorologue, soit tu as triché. Dans le doute, bravo.", score, n)
  }
}

# Couleur d'accent de l'anneau de score : rouge (0-2), ambre (3-6), vert (7-8),
# or (9-10). Calibrée sur le hasard d'un quiz à 3 choix. Pure et testable.
couleur_score <- function(score, n = 10) {
  pct <- if (n > 0) score / n else 0
  if (pct < 0.3)      "#C0392B"   # 0-2  : rouge
  else if (pct < 0.7) "#E8A33D"   # 3-6  : ambre
  else if (pct < 0.9) "#2E8B57"   # 7-8  : vert
  else                "#B8860B"   # 9-10 : or
}

# Phrase « nouvelle normale du futur » pour la carte de partage d'une manche :
# situe la température vs la zone normale PROJETÉE (p10_fin/p90_fin) à l'horizon
# `annee`. La zone normale MONTE avec le réchauffement ; on tient donc compte de
# la catégorie ACTUELLE pour ne pas annoncer « elle passerait en-dessous » d'une
# température déjà en-dessous aujourd'hui (elle le serait alors « encore plus »).
# Renvoie list(txt, couleur), ou NULL si les bornes projetées sont indisponibles.
phrase_projection_futur <- function(temp, categorie_actuelle, p10_fin, p90_fin, annee) {
  if (!is.finite(p10_fin) || !is.finite(p90_fin)) return(NULL)
  if (temp > p90_fin) {
    list(txt = paste0("Même en ", annee, ", elle resterait au-dessus des normales"),
         couleur = "#E41A1C")
  } else if (temp < p10_fin) {
    txt <- if (identical(categorie_actuelle, "En-dessous des normales"))
      paste0("En ", annee, ", elle serait encore plus en-dessous des normales")
    else
      paste0("En ", annee, ", elle passerait en-dessous des normales")
    list(txt = txt, couleur = "#1f77b4")
  } else {
    list(txt = paste0("En ", annee, ", une telle température sera dans les normales"),
         couleur = "#2E8B57")
  }
}

# Construit l'objet `projections` (normale « présente » + niveaux TRACC) d'une
# manche de quiz, à partir de la normale 1991-2020 `base_9120` (data.frame à 1
# ligne : t_moy, seuil_bas_p10, seuil_haut_p90) et des deltas DRIAS par niveau
# `deltas_proj` (data.frame : niveau_rechauffement + delta_moy/p10/p90/moy_bas/
# moy_haut). Renvoie list(present, niveaux) ou NULL si l'une des entrées est vide.
# Fonction PURE (aucune BDD) — extraite de calculer_feedback_manche pour être
# testable et pour permettre le pré-calcul groupé des projections d'une série.
construire_projections <- function(base_9120, deltas_proj) {
  if (is.null(base_9120) || nrow(base_9120) == 0 ||
      is.null(deltas_proj) || nrow(deltas_proj) == 0) return(NULL)
  labels_niv <- c("2050_+2.7" = "2050 (+2,7 °C)", "2100_+4.0" = "2100 (+4 °C)")
  niveaux <- lapply(names(labels_niv), function(niv) {
    d <- deltas_proj[deltas_proj$niveau_rechauffement == niv, ]
    if (nrow(d) == 0) return(NULL)
    list(niveau = niv, label = unname(labels_niv[niv]),
         moy      = base_9120$t_moy[1]          + d$delta_moy[1],
         p10      = base_9120$seuil_bas_p10[1]  + d$delta_p10[1],
         p90      = base_9120$seuil_haut_p90[1] + d$delta_p90[1],
         moy_bas  = base_9120$t_moy[1]          + d$delta_moy_bas[1],
         moy_haut = base_9120$t_moy[1]          + d$delta_moy_haut[1])
  })
  niveaux <- Filter(Negate(is.null), niveaux)
  if (length(niveaux) == 0) return(NULL)
  list(
    present = list(p10 = base_9120$seuil_bas_p10[1],
                   p90 = base_9120$seuil_haut_p90[1], moy = base_9120$t_moy[1]),
    niveaux = niveaux)
}

# Construit le modal de partage d'une image (carte de résultat). Partagé par le
# quiz et l'onglet « Une journée » : même UX, mêmes identifiants que www/partage.js
# (#apercu-partage-img, #partage-zone[data-texte]) qui pilote copie / partage natif
# / réseaux. À passer à showModal(). Utilise des fonctions Shiny (préfixées) au
# moment de l'appel.
modal_partage <- function(data_uri, texte, nom_fichier, titre = "Partager mon résultat") {
  btn <- function(label, icone, onclick, classe) {
    shiny::tags$button(type = "button", class = paste("btn", classe, "m-1"),
                       onclick = onclick, shiny::icon(icone), label)
  }
  shiny::modalDialog(
    title = titre, easyClose = TRUE, size = "l",
    shiny::div(
      id = "partage-zone", `data-texte` = texte,
      shiny::div(class = "text-center",
                 shiny::tags$img(id = "apercu-partage-img", src = data_uri,
                                 style = "max-width:100%; height:auto; border:1px solid #dee2e6; border-radius:8px;")),
      shiny::div(class = "text-center mt-3",
                 btn("Copier l'image", "copy", "partageCopier()", "btn-primary"),
                 shiny::tags$a(class = "btn btn-outline-secondary m-1", href = data_uri,
                               download = nom_fichier, shiny::icon("download"), " Télécharger"),
                 btn("Partager (mobile)", "share-nodes", "partagePartager()", "btn-outline-secondary")),
      shiny::tags$p(class = "text-muted small text-center mt-3 mb-1", "Publier sur un réseau :"),
      shiny::div(class = "text-center",
                 btn("LinkedIn", "linkedin", "partageReseau('linkedin')", "btn-outline-primary"),
                 btn("Facebook", "facebook", "partageReseau('facebook')", "btn-outline-primary"),
                 btn("Instagram", "instagram", "partageReseau('instagram')", "btn-outline-primary")),
      shiny::tags$p(class = "text-muted small text-center mt-3 mb-0",
                    "Les boutons réseau téléchargent l'image : importez-la via le bouton photo de la ",
                    "publication (LinkedIn, Instagram et Facebook n'acceptent pas le collage). ",
                    "« Copier l'image » convient aux applis qui acceptent le collage (X, WhatsApp, e-mail…). ",
                    "Sur mobile, « Partager » publie directement.")
    ),
    footer = shiny::modalButton("Fermer")
  )
}
