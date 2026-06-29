# calculer_indicateurs.R
# Fonctions de calcul des indicateurs dérivés des températures journalières
# (tmax + tmin) : indicateurs annuels par ville et seuil « jour de forte chaleur »
# (90e percentile de la tmax estivale, par ville).
#
# Sourcé par preparer_data.R et calculer_projections.R. En `source()`, ce fichier
# n'expose que des fonctions (aucun effet de bord), conformément au reste du pipeline.

library(dplyr)
library(lubridate)

# --- Indicateurs annuels par ville -----------------------------------------
# Attendu en entrée : data.frame avec colonnes ville, date, temperature_max,
# temperature_min, annee. Retourne une ligne par (ville, annee).
#
# Indicateurs (parlants pour le grand public, anti « c'est juste l'été ») :
#   - nuits_tropicales      : nb de nuits où tmin >= 20 °C (santé / sommeil)
#   - jours_chaleur_30      : nb de jours où tmax >= 30 °C
#   - jours_forte_chaleur_35: nb de jours où tmax >= 35 °C
#   - jours_gel             : nb de jours où tmin <= 0 °C (le pendant « froid »,
#       qui RECULE avec le réchauffement — lecture grand public, sans le biais
#       1/n des records cumulés)
#   - tmax_annuel           : journée la plus chaude de l'année
#   - tmin_annuel           : nuit la plus froide de l'année
#   - records_chaud / records_froid : nb de records quotidiens battus dans l'année
#       (un record chaud = tmax du jour calendaire supérieure à toutes les années
#        ANTÉRIEURES ; symétrique pour le froid sur tmin). Le ratio chaud/froid
#        est un signal climatique très intuitif (≈ 1 en climat stable).
#   - nb_jours              : couverture (pour écarter les années incomplètes).
calculer_indicateurs_annuels <- function(donnees) {
  # Records quotidiens : pour chaque jour calendaire, on compare à l'historique
  # strictement antérieur (cummax/cummin décalés d'un an).
  records <- donnees %>%
    arrange(ville, mois, jour_mois, annee) %>%
    group_by(ville, mois, jour_mois) %>%
    mutate(
      max_anterieur = lag(cummax(temperature_max)),
      min_anterieur = lag(cummin(temperature_min)),
      record_chaud = !is.na(max_anterieur) & temperature_max > max_anterieur,
      record_froid = !is.na(min_anterieur) & temperature_min < min_anterieur
    ) %>%
    ungroup() %>%
    group_by(ville, annee) %>%
    summarise(
      records_chaud = sum(record_chaud, na.rm = TRUE),
      records_froid = sum(record_froid, na.rm = TRUE),
      .groups = "drop"
    )

  donnees %>%
    group_by(ville, annee) %>%
    summarise(
      nuits_tropicales       = sum(temperature_min >= 20, na.rm = TRUE),
      jours_chaleur_30       = sum(temperature_max >= 30, na.rm = TRUE),
      jours_forte_chaleur_35 = sum(temperature_max >= 35, na.rm = TRUE),
      jours_gel              = sum(temperature_min <= 0, na.rm = TRUE),
      tmax_annuel            = max(temperature_max, na.rm = TRUE),
      tmin_annuel            = min(temperature_min, na.rm = TRUE),
      nb_jours               = n(),
      .groups = "drop"
    ) %>%
    left_join(records, by = c("ville", "annee"))
}

# --- Seuil "jour de forte chaleur" : 90e pct de la tmax estivale (JJA) --------
# Indicateur SIMPLE (remplace la canicule) : un jour de forte chaleur = tmax >=
# seuil, où seuil = 90e percentile de la tmax des étés (JJA) de la période de
# référence, PAR ville. Recalculé DANS la source (ERA5 / DRIAS) → cohérent
# observé/projeté ; seuil local (plus haut au sud) ; pas de bagage « canicule »
# (ni IBM, ni dual tmin/tmax, ni volet sanitaire).
#   donnees : ville, date, temperature_max (1+ villes)
# Retour : data.frame(ville, seuil).
calculer_seuil_forte_chaleur <- function(donnees, periode_ref = c(1973, 2003), pct = 0.90) {
  donnees %>%
    mutate(annee = year(date), mois = month(date)) %>%
    filter(annee >= periode_ref[1], annee <= periode_ref[2], mois %in% 6:8) %>%
    group_by(ville) %>%
    summarise(seuil = as.numeric(quantile(temperature_max, pct, na.rm = TRUE)), .groups = "drop")
}
