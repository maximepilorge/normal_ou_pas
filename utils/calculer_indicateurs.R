# calculer_indicateurs.R
# Fonctions de calcul des indicateurs dérivés des températures journalières
# (tmax + tmin) : indicateurs annuels par ville et épisodes de canicule selon la
# définition officielle Météo-France (IBM 3 jours vs seuils départementaux).
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
      tmax_annuel            = max(temperature_max, na.rm = TRUE),
      tmin_annuel            = min(temperature_min, na.rm = TRUE),
      nb_jours               = n(),
      .groups = "drop"
    ) %>%
    left_join(records, by = c("ville", "annee"))
}

# --- Canicules (définition officielle Météo-France) -------------------------
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
