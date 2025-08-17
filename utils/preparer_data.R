# data_prep.R
# Ce script récupère les températures maximales journalières et pré-calcule les statistiques des normales climatiques
# À n'exécuter qu'une seule fois ou lors de la mise à jour des données

# Chargement des librairies
library(dplyr)
library(lubridate)
library(here)

source(here("global.R")) 

cat("Début du pré-calcul des normales climatiques...\n")

# Chargement des données
donnees_brutes <- readRDS(paste0(dirApp, "/data/era5_temperatures_france.rds")) %>%
  rename(city = ville, tmax_celsius = temperature_max)

# Ajout des informations temporelles et assignation aux périodes de référence
donnees_augmentees <- donnees_brutes %>%
  mutate(
    annee = year(date),
    jour_annee = yday(date),
    # On définit les différentes périodes de référence de 30 ans
    periode_ref = case_when(
      annee >= 1951 & annee <= 1980 ~ "1951-1980",
      annee >= 1961 & annee <= 1990 ~ "1961-1990",
      annee >= 1971 & annee <= 2000 ~ "1971-2000",
      annee >= 1981 & annee <= 2010 ~ "1981-2010",
      annee >= 1991 & annee <= 2020 ~ "1991-2020",
      TRUE ~ NA_character_ # Les années hors de ces périodes ne seront pas utilisées pour les normales
    )
  ) %>%
  filter(!is.na(periode_ref))

# Calcul des statistiques par ville, par jour de l'année et par période
stats_normales <- donnees_augmentees %>%
  group_by(city, jour_annee, periode_ref) %>%
  summarise(
    t_min = min(tmax_celsius, na.rm = TRUE),
    t_q1 = quantile(tmax_celsius, probs = 0.25, na.rm = TRUE),
    t_moy = mean(tmax_celsius, na.rm = TRUE),
    t_q3 = quantile(tmax_celsius, probs = 0.75, na.rm = TRUE),
    t_max = max(tmax_celsius, na.rm = TRUE),
    .groups = "drop"
  )

# Filtrage final pour retirer les statistiques invalides
stats_normales_propres <- stats_normales %>%
  filter(
    !is.infinite(t_min) & 
      !is.infinite(t_max) & 
      !is.nan(t_moy) &
      !is.na(t_moy)
  )

# Sauvegarde du fichier
saveRDS(stats_normales_propres, paste0(dirApp, "/data/stats_normales_precalculees.rds"))

cat("✅ Fichier 'stats_normales_precalculees.rds' créé avec succès !\n")