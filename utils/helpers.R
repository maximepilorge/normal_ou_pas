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

# Commentaire de bilan d'une série, indexé sur le score /n et le ton : taquin par
# défaut (sarcastique, cohérent avec les messages du quiz historique), neutre et
# porteur du message climatique si l'utilisateur a coché « me répondre poliment ».
# Fonction pure, testable par paliers.
commentaire_serie <- function(score, n = 10, poli = FALSE) {
  pct <- if (n > 0) score / n else 0
  if (isTRUE(poli)) {
    if (pct < 0.4)
      sprintf("Score : %d/%d. Les repères de température se faussent vite avec le réchauffement — réessayez, on apprend en jouant.", score, n)
    else if (pct < 0.7)
      sprintf("Score : %d/%d. Une bonne moitié de juste : pas évident de situer ce qui est normal aujourd'hui.", score, n)
    else if (pct < 0.9)
      sprintf("Score : %d/%d. Beau résultat, vos repères climatiques sont solides.", score, n)
    else
      sprintf("Score : %d/%d. Excellent — vous lisez le climat comme une carte.", score, n)
  } else {
    if (score == 0)
      "Zéro pointé. À ce niveau, ce n'est plus de la malchance, c'est un don."
    else if (pct < 0.4)
      sprintf("%d sur %d. Le thermomètre te résiste encore un peu, on va dire.", score, n)
    else if (pct < 0.6)
      sprintf("%d sur %d. Pile poil le hasard : tu aurais fait aussi bien à pile ou face.", score, n)
    else if (pct < 0.8)
      sprintf("%d sur %d ! Pas mal du tout… j'ai presque envie de te féliciter. Presque.", score, n)
    else if (pct < 1)
      sprintf("%d sur %d. Bon, tu commences sérieusement à m'agacer de tout savoir.", score, n)
    else
      sprintf("%d sur %d. Sans faute. Soit tu es météorologue, soit tu as triché. Dans le doute, bravo.", score, n)
  }
}

# Palier qualitatif d'un score (couleur d'accent + libellé), partagé par l'anneau
# de l'écran de bilan et la carte de partage de série. Pur et testable.
palier_score <- function(score, n = 10) {
  pct <- if (n > 0) score / n else 0
  if (pct < 0.4)      list(niveau = "faible",  libelle = "Des repères à recalibrer", couleur = "#C0392B")
  else if (pct < 0.7) list(niveau = "moyen",   libelle = "Pas mal !",                couleur = "#E8A33D")
  else if (pct < 0.9) list(niveau = "bon",     libelle = "Bon climatologue !",       couleur = "#2E8B57")
  else                list(niveau = "parfait", libelle = "Maître des normales",      couleur = "#B8860B")
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
