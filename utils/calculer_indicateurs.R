# calculer_indicateurs.R
# Fonctions de calcul des indicateurs dérivés des températures journalières
# (tmax + tmin) : indicateurs annuels par ville et épisodes de canicule inspirés
# du dispositif SACS (indicateur IBM 3 jours), seuils recalibrés sur la donnée.
#
# Sourcé par preparer_data.R. En `source()`, ce fichier n'expose que des
# fonctions (aucun effet de bord), conformément au reste du pipeline.

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

# --- Seuils de canicule RECALIBRÉS sur la donnée (cohérence interne) ---------
# [CONSERVÉ pour référence — n'est plus utilisé par le pipeline depuis le passage
#  à l'indicateur « jour de forte chaleur » ci-dessus, plus lisible grand public.]
# INSPIRÉ du dispositif SACS (Santé publique France & Météo-France) : on reprend
# l'indicateur IBM (moyennes glissantes 3 j des tmin/tmax) ET le percentile de la
# méthode officielle. Dans SACS, les seuils calibrés sur la surmortalité (doublement
# de la mortalité, 14 villes pilotes) se sont avérés proches du PERCENTILE 99,5 de
# la distribution des IBM ; ce percentile a ensuite été appliqué aux stations de
# référence des autres départements (cf. doc SACS, Santé publique France).
# On applique donc le même percentile (99,5) à NOTRE source (ERA5-Land observé /
# DRIAS projeté), sur les étés (JJA) de la période de référence, par ville. Les
# seuils « reflètent le climat local » (plus hauts à Marseille/Lyon qu'à Rennes),
# exactement comme l'officiel. La canicule = « top 0,5 % des étés 1973-2003 »,
# cohérent et comparable observé/projeté. (On ne reproduit PAS le volet sanitaire.)
#   donnees : ville, date, temperature_max, temperature_min (1+ villes)
# Retour : data.frame(departement, smin, smax) directement utilisable par
#          calculer_canicules().
calculer_seuils_recalibres <- function(donnees, villes_insee,
                                       periode_ref = c(1973, 2003), pct = 0.995) {
  ville_dept <- villes_insee %>% transmute(ville, departement = substr(insee, 1, 2))
  donnees %>%
    arrange(ville, date) %>%
    group_by(ville) %>%
    mutate(
      consec = !is.na(lag(date, 2)) & as.integer(date - lag(date, 2)) == 2,
      ibmn = if_else(consec, (temperature_min + lag(temperature_min, 1) + lag(temperature_min, 2)) / 3, NA_real_),
      ibmx = if_else(consec, (temperature_max + lag(temperature_max, 1) + lag(temperature_max, 2)) / 3, NA_real_),
      annee = year(date), mois = month(date)
    ) %>%
    ungroup() %>%
    filter(annee >= periode_ref[1], annee <= periode_ref[2], mois %in% 6:8) %>%
    group_by(ville) %>%
    summarise(smin = as.numeric(quantile(ibmn, pct, na.rm = TRUE)),
              smax = as.numeric(quantile(ibmx, pct, na.rm = TRUE)), .groups = "drop") %>%
    inner_join(ville_dept, by = "ville") %>%
    select(departement, smin, smax)
}

# --- Canicules (indicateur IBM façon SACS, seuils recalibrés sur la donnée) --
# IBM = moyennes glissantes sur 3 jours consécutifs des tmin (IBMn) et tmax
# (IBMx). Un jour est « caniculaire » quand IBMn >= smin ET IBMx >= smax, où
# (smin, smax) sont les seuils du DÉPARTEMENT de la ville. Un épisode est une
# suite de jours caniculaires consécutifs.
#
# Paramètres :
#   donnees      : ville, date, temperature_max, temperature_min
#   villes_insee : table ville -> code INSEE (pour déduire le département)
#   seuils       : data.frame departement, smin, smax (NA => ville exclue)
# Retour : une ligne par épisode (ville, date_debut, date_fin, duree_jours,
#          intensite_max = pic d'IBMx, depassement_max = max(IBMx - smax), annee).
calculer_canicules <- function(donnees, villes_insee, seuils) {
  # Département = 2 premiers caractères du code INSEE (gère la Corse 2A/2B).
  ville_dept <- villes_insee %>%
    transmute(ville, departement = substr(insee, 1, 2))

  seuils_valides <- seuils %>%
    filter(!is.na(smin), !is.na(smax)) %>%
    select(departement, smin, smax)

  base <- donnees %>%
    inner_join(ville_dept, by = "ville") %>%
    inner_join(seuils_valides, by = "departement")

  villes_exclues <- setdiff(unique(donnees$ville), unique(base$ville))
  if (length(villes_exclues) > 0) {
    warning("Canicules non calculées (seuils départementaux manquants) pour : ",
            paste(sort(villes_exclues), collapse = ", "))
  }
  # Structure de retour vide (typée) si aucune ville n'a de seuils renseignés :
  # garantit des colonnes cohérentes pour dbWriteTable et les contraintes SQL.
  canicules_vides <- tibble(
    ville = character(), departement = character(),
    date_debut = as.Date(character()), date_fin = as.Date(character()),
    duree_jours = integer(), intensite_max = numeric(),
    depassement_max = numeric(), annee = integer()
  )
  if (nrow(base) == 0) return(canicules_vides)

  # IBM glissants sur 3 jours STRICTEMENT consécutifs (gère les éventuels trous).
  jours <- base %>%
    arrange(ville, date) %>%
    group_by(ville) %>%
    mutate(
      consecutif = !is.na(lag(date, 2)) & as.integer(date - lag(date, 2)) == 2,
      ibmn = if_else(consecutif, (temperature_min + lag(temperature_min, 1) + lag(temperature_min, 2)) / 3, NA_real_),
      ibmx = if_else(consecutif, (temperature_max + lag(temperature_max, 1) + lag(temperature_max, 2)) / 3, NA_real_),
      jour_canicule = !is.na(ibmn) & ibmn >= smin & ibmx >= smax,
      # Identifiant d'épisode = n° de run de jours caniculaires consécutifs.
      debut_run = jour_canicule & !lag(jour_canicule, default = FALSE),
      episode_id = if_else(jour_canicule, cumsum(debut_run), NA_integer_)
    ) %>%
    ungroup()

  jours %>%
    filter(jour_canicule) %>%
    group_by(ville, departement, episode_id) %>%
    summarise(
      date_debut      = min(date),
      date_fin        = max(date),
      duree_jours     = n(),
      intensite_max   = max(ibmx),
      depassement_max = max(ibmx - smax),
      .groups = "drop"
    ) %>%
    mutate(annee = year(date_debut)) %>%
    select(ville, departement, date_debut, date_fin, duree_jours,
           intensite_max, depassement_max, annee) %>%
    arrange(ville, date_debut)
}
