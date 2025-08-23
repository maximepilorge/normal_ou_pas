# preparer_data.R
# Ce script récupère les températures maximales journalières, pré-calcule les statistiques des normales climatiques,
# et génère une table finale optimisée pour l'application Shiny.
# À n'exécuter qu'une seule fois ou lors de la mise à jour des données.

# Chargement des librairies
library(dplyr)
library(lubridate)
library(DBI)
library(RPostgres)
library(dotenv)
library(zoo)
library(here)

cat("Début de la préparation complète de la base de données...\n")

# --- Chargement des variables ---
load_dot_env(file = here::here(".Renviron"))
stopifnot(Sys.getenv("DB_HOST") != "")

# --- Connexion à la base de données ---
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

tryCatch({
  
  chemin_brutes_rds <- here::here("data", "era5_temperatures_france.rds")

  # --- Étape 1 : Chargement et écriture des données brutes ---
  cat("1/5 - Chargement des données brutes...\n")
  donnees_brutes_initiales <- readRDS(chemin_brutes_rds) %>%
    filter(is.finite(temperature_max))
  
  cat("2/5 - Calcul de la moyenne mobile sur 365 jours...\n")
  donnees_brutes <- donnees_brutes_initiales %>%
    arrange(ville, date) %>%
    group_by(ville) %>%
    mutate(
      tmax_lisse_365j = zoo::rollmean(temperature_max, k = 365, fill = NA, align = "center")
    ) %>%
    ungroup() %>%
    mutate(    
      mois = month(date),
      jour_mois = day(date)
      )
  
  dbWriteTable(con, "temperatures_max", as.data.frame(donnees_brutes), overwrite = TRUE)
  
  # --- Étape 2 : Pré-calcul des normales climatiques ---
  cat("3/5 - Pré-calcul des normales climatiques...\n")
  
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
      group_by(ville, mois, jour_mois) %>%
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
  
  dbWriteTable(con, "stats_normales", as.data.frame(stats_normales), overwrite = TRUE)
  
  
  # --- Étape 3 : Création de la table pour le quiz ---
  cat("4/5 - Jointure et pré-calcul de la table optimisée pour le quiz...\n")
  
  # On crée une table des périodes pour faciliter la jointure
  periods_df <- tibble(
    periode_ref = names(periodes_ref),
    annee_debut = sapply(periodes_ref, `[`, 1),
    annee_fin = sapply(periodes_ref, `[`, 2)
  )
  
  # Jointure pour associer chaque année à TOUTES les périodes de référence auxquelles elle appartient
  quiz_data_precalculee <- donnees_avec_annee %>%
    cross_join(tibble(periode_ref = names(periodes_ref))) %>%
    # On joint les stats correspondantes
    left_join(stats_normales, by = c("ville", "mois", "jour_mois", "periode_ref")) %>%
    filter(!is.na(seuil_haut_p90)) %>%
    # On calcule la catégorie
    mutate(
      categorie = case_when(
        tmax_celsius > seuil_haut_p90 ~ "Au-dessus des normales",
        tmax_celsius < seuil_bas_p10  ~ "En-dessous des normales",
        TRUE                          ~ "Dans les normales de saison"
      ),
    ) %>%
    select(
      ville, date, tmax_celsius, periode_ref, 
      categorie, mois, t_moy
    )
  
  dbWriteTable(con, "quiz_data_precalculee", as.data.frame(quiz_data_precalculee), overwrite = TRUE)
  
  
  # --- Étape finale : Application de la structure sur la base locale ---
  cat("Application de la structure (Clés, Contraintes, Index) sur la base de données locale...\n")
  
  # -- Table: temperatures_max --
  dbExecute(con, "ALTER TABLE public.temperatures_max ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.temperatures_max ADD CONSTRAINT uc_temperatures_max_ville_date UNIQUE (ville, date);")
  dbExecute(con, "VACUUM ANALYZE public.temperatures_max;")
  
  # -- Table: stats_normales --
  dbExecute(con, "ALTER TABLE public.stats_normales ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.stats_normales ADD CONSTRAINT uc_stats_normales_ville_jour_periode UNIQUE (ville, mois, jour_mois, periode_ref);")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stats_ville_periode ON public.stats_normales (ville, periode_ref);")
  dbExecute(con, "VACUUM ANALYZE public.stats_normales;")
  
  # -- Table: quiz_data_precalculee --
  dbExecute(con, "ALTER TABLE public.quiz_data_precalculee ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.quiz_data_precalculee ADD CONSTRAINT uc_quiz_data_ville_date_periode UNIQUE (ville, date, periode_ref);")
  dbExecute(con, "CREATE INDEX idx_quiz_optimise ON public.quiz_data_precalculee (periode_ref, categorie, ville, mois);")
  dbExecute(con, "CREATE INDEX idx_quiz_saison ON public.quiz_data_precalculee (periode_ref, categorie, mois);")
  dbExecute(con, "VACUUM ANALYZE public.quiz_data_precalculee;")
  
  cat("✅ Structure appliquée avec succès sur la base locale.\n")
  
}, finally = {
  cat("Déconnexion de la base de données.\n")
  dbDisconnect(con)
})

cat("\n✅ Base de données '", Sys.getenv("DB_NAME"), "' alimentée avec succès !\n")