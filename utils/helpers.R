# utils/helpers.R
#
# Petits utilitaires « purs » de l'application (aucune dépendance BDD), regroupés
# ici pour être à la fois partagés par les modules ET testables isolément
# (tests/testthat). Sourcé par global.R, donc disponible dans tous les modules.

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
