# preparer_data.R
# Ce script récupère les températures maximales journalières, pré-calcule les statistiques des normales climatiques,
# et génère une table finale optimisée pour l'application Shiny.
# À n'exécuter qu'une seule fois ou lors de la mise à jour des données.

# Chargement des librairies
library(dplyr)
library(lubridate)
library(DBI)
library(RSQLite)

dirApp <- "C:/Users/maxp1/Documents/guess_climate"

cat("Début de la préparation complète de la base de données...\n")

# --- Définition des chemins ---
chemin_brutes_rds <- paste0(dirApp, "/data/era5_temperatures_france.rds")
chemin_db_sqlite <- paste0(dirApp, "/data/temperatures.sqlite")

# --- Connexion à la base de données ---
con <- dbConnect(RSQLite::SQLite(), chemin_db_sqlite)

# --- Étape 1 : Chargement et écriture des données brutes ---
cat("1/7 - Chargement des données brutes...\n")
donnees_brutes <- readRDS(chemin_brutes_rds) %>%
  filter(is.finite(temperature_max)) %>%
  mutate(jour_annee = yday(date))

cat("2/7 - Écriture des données dans la table 'temperatures_max'...\n")
dbWriteTable(con, "temperatures_max", as.data.frame(donnees_brutes), overwrite = TRUE)


# --- Étape 2 : Pré-calcul des normales climatiques ---
cat("3/7 - Pré-calcul des normales climatiques...\n")

# On ajoute l'année aux données brutes pour pouvoir filtrer
donnees_avec_annee <- donnees_brutes %>%
  rename(tmax_celsius = temperature_max) %>%
  mutate(annee = year(date))

# On définit les périodes de référence
periodes_ref <- list(
  "1951-1980" = c(1951, 1980),
  "1961-1990" = c(1961, 1990),
  "1971-2000" = c(1971, 2000),
  "1981-2010" = c(1981, 2010),
  "1991-2020" = c(1991, 2020)
)

# On boucle sur chaque période pour calculer les stats correctement
liste_stats <- lapply(names(periodes_ref), function(nom_periode) {
  annee_debut <- periodes_ref[[nom_periode]][1]
  annee_fin <- periodes_ref[[nom_periode]][2]
  
  cat(paste("    -> Calcul pour la période", nom_periode, "\n"))
  
  donnees_avec_annee %>%
    filter(annee >= annee_debut & annee <= annee_fin) %>%
    group_by(ville, jour_annee) %>%
    summarise(
      t_moy = mean(tmax_celsius, na.rm = TRUE),
      t_min = min(tmax_celsius, na.rm = TRUE),
      t_q1 = quantile(tmax_celsius, probs = 0.25, na.rm = TRUE),
      t_q3 = quantile(tmax_celsius, probs = 0.75, na.rm = TRUE),
      t_max = max(tmax_celsius, na.rm = TRUE),
      seuil_bas_p10 = quantile(tmax_celsius, probs = 0.1, na.rm = TRUE),
      seuil_haut_p90 = quantile(tmax_celsius, probs = 0.9, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(periode_ref = nom_periode)
})

# On combine tous les résultats en un seul dataframe
stats_normales <- bind_rows(liste_stats) %>%
  filter(!is.na(t_moy))

cat("4/7 - Écriture des statistiques dans la table 'stats_normales'...\n")
dbWriteTable(con, "stats_normales", as.data.frame(stats_normales), overwrite = TRUE)


# --- Étape 3 : Création de la table pour le quiz ---
cat("5/7 - Jointure et pré-calcul de la table optimisée pour le quiz...\n")

# On crée une table des périodes pour faciliter la jointure
periods_df <- tibble(
  periode_ref = names(periodes_ref),
  annee_debut = sapply(periodes_ref, `[`, 1),
  annee_fin = sapply(periodes_ref, `[`, 2)
)

# Jointure pour associer chaque année à TOUTES les périodes de référence auxquelles elle appartient
quiz_data_precalculee <- donnees_avec_annee %>%
  inner_join(periods_df, by = character()) %>%
  filter(annee >= annee_debut & annee <= annee_fin) %>%
  select(-annee_debut, -annee_fin) %>%
  # On joint les stats correspondantes
  left_join(stats_normales, by = c("ville", "jour_annee", "periode_ref")) %>%
  filter(!is.na(seuil_haut_p90)) %>%
  # On calcule la catégorie
  mutate(
    categorie = case_when(
      tmax_celsius > seuil_haut_p90 ~ "Au-dessus des normales",
      tmax_celsius < seuil_bas_p10  ~ "En-dessous des normales",
      TRUE                          ~ "Dans les normales de saison"
    ),
    mois = month(date)
  ) %>%
  select(
    ville, date, tmax_celsius, periode_ref, 
    categorie, mois, t_moy
  )

cat("6/7 - Écriture de la table 'quiz_data_precalculee'...\n")
dbWriteTable(con, "quiz_data_precalculee", as.data.frame(quiz_data_precalculee), overwrite = TRUE)


# --- Étape 4 : Créer des index pour optimiser les performances ---
cat("7/7 - Création des index pour l'optimisation...\n")
dbExecute(con, "CREATE INDEX idx_quiz_main ON quiz_data_precalculee (periode_ref, categorie, mois);")
dbExecute(con, "CREATE INDEX idx_temp_ville_date ON temperatures_max (ville, jour_annee);")
dbExecute(con, "CREATE INDEX idx_stats_ville_periode ON stats_normales (ville, periode_ref);")


# --- Étape 5 : Nettoyage ---
dbDisconnect(con)

cat("\n✅ Base de données '", basename(chemin_db_sqlite), "' créée/mise à jour avec succès !\n")