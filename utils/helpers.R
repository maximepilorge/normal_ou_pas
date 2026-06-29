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
