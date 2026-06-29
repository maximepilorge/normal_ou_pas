# utils/fenetre_glissante.R
#
# Machinerie COMMUNE de la fenêtre glissante de ±N jours autour de chaque jour
# calendaire. Source de vérité partagée entre :
#   - preparer_data.R          (seuils p10/p90 lissés des normales) ;
#   - calculer_projections.R   (deltas TRACC, mêmes fenêtres que les normales).
#
# Factorisée ici pour garantir une définition IDENTIQUE du lissage des deux côtés :
# si l'observé et le projeté utilisaient deux fenêtres différentes, leurs seuils
# divergeraient silencieusement. Fonctions pures (testées dans tests/testthat).

library(dplyr)
library(lubridate)

# Table de référence des jours calendaires : jour_ref (1..366, indexé sur une
# année bissextile fixe — 2000 — pour inclure le 29 février, indépendamment de
# l'année d'observation) -> (mois, jour_mois).
construire_ref_jours <- function() {
  data.frame(date_ref = seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")) %>%
    mutate(jour_ref = yday(date_ref), mois = month(date_ref), jour_mois = day(date_ref)) %>%
    select(jour_ref, mois, jour_mois)
}

# Pour chaque jour cible (1..366), la liste des jours sources de sa fenêtre
# centrée de ±`fenetre` jours, circulaire sur l'année (366) : le 1er janvier
# emprunte ainsi à la fin décembre. Retour : data.frame(jour_cible, jour_ref).
construire_fenetre_map <- function(fenetre = 7L) {
  expand.grid(jour_cible = 1:366, offset = -fenetre:fenetre) %>%
    mutate(jour_ref = ((jour_cible - 1 + offset) %% 366) + 1) %>%
    select(jour_cible, jour_ref)
}
