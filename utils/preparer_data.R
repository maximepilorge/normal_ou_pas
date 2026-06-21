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
  
  cat("2/5 - Préparation des données brutes...\n")
  donnees_brutes <- donnees_brutes_initiales %>%
    arrange(ville, date) %>%
    mutate(
      annee = year(date),
      mois = month(date),
      jour_mois = day(date)
    )

  dbWriteTable(con, "temperatures_max", as.data.frame(donnees_brutes), overwrite = TRUE)
  
  # --- Étape 2 : Pré-calcul des normales climatiques ---
  cat("3/5 - Pré-calcul des normales climatiques...\n")
  
  # On ajoute l'année aux données brutes pour pouvoir filtrer
  donnees_avec_annee <- donnees_brutes %>%
    rename(tmax_celsius = temperature_max)
  
  # On définit les périodes de référence
  periodes_ref <- list(
    "1951-1980" = c(1951, 1980),
    "1961-1990" = c(1961, 1990),
    "1971-2000" = c(1971, 2000),
    "1981-2010" = c(1981, 2010),
    "1991-2020" = c(1991, 2020)
  )
  
  # --- Fenêtre glissante de ±7 jours ---
  # Les percentiles d'un jour calendaire estimés sur ~30 valeurs (1 par an) sont
  # très bruités. On les calcule sur une fenêtre centrée de ±7 jours (≈ 450
  # valeurs) pour des seuils stables, conformément à la pratique (ETCCDI).
  FENETRE <- 7

  # Indice de jour 1..366 via une année bissextile fixe (2000) : ordre cohérent
  # incluant le 29 février, indépendant de l'année d'observation.
  ref_jours <- data.frame(date_ref = seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")) %>%
    mutate(jour_ref = yday(date_ref), mois = month(date_ref), jour_mois = day(date_ref)) %>%
    select(jour_ref, mois, jour_mois)

  # Pour chaque jour cible, les indices sources de sa fenêtre (circulaire sur 366).
  fenetre_map <- expand.grid(jour_cible = 1:366, offset = -FENETRE:FENETRE) %>%
    mutate(jour_ref = ((jour_cible - 1 + offset) %% 366) + 1) %>%
    select(jour_cible, jour_ref)

  # Indice de jour attaché à chaque observation.
  donnees_avec_jr <- donnees_avec_annee %>%
    left_join(ref_jours, by = c("mois", "jour_mois"))

  # On boucle sur chaque période en combinant deux calculs :
  #  - stats du JOUR EXACT (moyenne, min, quartiles, max) : utilisées par le
  #    graphique de comparaison, où l'on veut voir les pics du jour précis ;
  #  - seuils p10/p90 LISSÉS sur la fenêtre ±7 j : utilisés pour classer une
  #    température (au-dessus / normal / en-dessous) dans le quiz, de façon robuste.
  liste_stats <- lapply(names(periodes_ref), function(nom_periode) {
    annee_debut <- periodes_ref[[nom_periode]][1]
    annee_fin <- periodes_ref[[nom_periode]][2]

    cat(paste("    -> Calcul pour la période", nom_periode,
              "(jour exact + seuils ±", FENETRE, "j)\n"))

    donnees_periode <- donnees_avec_jr %>%
      filter(annee >= annee_debut & annee <= annee_fin)

    # Stats du jour exact (pour le graphique de comparaison).
    stats_jour <- donnees_periode %>%
      group_by(ville, mois, jour_mois) %>%
      summarise(
        t_moy = mean(tmax_celsius, na.rm = TRUE),
        t_min = min(tmax_celsius, na.rm = TRUE),
        t_q1 = quantile(tmax_celsius, probs = 0.25, na.rm = TRUE),
        t_q3 = quantile(tmax_celsius, probs = 0.75, na.rm = TRUE),
        t_max = max(tmax_celsius, na.rm = TRUE),
        .groups = "drop"
      )

    # Seuils p10/p90 lissés sur la fenêtre ±7 j (chaque observation contribue à
    # tous les jours cibles dont la fenêtre la contient).
    seuils_fenetre <- donnees_periode %>%
      inner_join(fenetre_map, by = "jour_ref", relationship = "many-to-many") %>%
      group_by(ville, jour_cible) %>%
      summarise(
        seuil_bas_p10 = quantile(tmax_celsius, probs = 0.1, na.rm = TRUE),
        seuil_haut_p90 = quantile(tmax_celsius, probs = 0.9, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(ref_jours, by = c("jour_cible" = "jour_ref")) %>%
      select(-jour_cible)

    stats_jour %>%
      left_join(seuils_fenetre, by = c("ville", "mois", "jour_mois")) %>%
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
      categorie, mois, jour_mois, t_moy
    )
  
  dbWriteTable(con, "quiz_data_precalculee", as.data.frame(quiz_data_precalculee), overwrite = TRUE)
  
  
  # --- Étape finale : Application de la structure sur la base locale ---
  cat("Application de la structure (Clés, Contraintes, Index) sur la base de données locale...\n")
  
  # -- Table: temperatures_max --
  cat("\nTable 'temperatures_max'...\n")
  dbExecute(con, "ALTER TABLE public.temperatures_max ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.temperatures_max ADD CONSTRAINT uc_temperatures_max_ville_date UNIQUE (ville, date);")
  dbExecute(con, "CREATE INDEX idx_temp_recherche_jour ON public.temperatures_max (ville, mois, jour_mois, annee);")
  dbExecute(con, "VACUUM ANALYZE public.temperatures_max;")
  
  # -- Table: stats_normales --
  cat("\nTable 'stats_normales'...\n")
  dbExecute(con, "ALTER TABLE public.stats_normales ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.stats_normales ADD CONSTRAINT uc_stats_normales_ville_jour_periode UNIQUE (ville, mois, jour_mois, periode_ref);")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stats_ville_periode ON public.stats_normales (ville, periode_ref);")
  dbExecute(con, "VACUUM ANALYZE public.stats_normales;")
  
  # -- Table: quiz_data_precalculee --
  cat("\nTable 'quiz_data_precalculee'...\n")
  dbExecute(con, "ALTER TABLE public.quiz_data_precalculee ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.quiz_data_precalculee ADD CONSTRAINT uc_quiz_data_ville_date_periode UNIQUE (ville, date, periode_ref);")
  dbExecute(con, "CREATE INDEX idx_quiz_optimise ON public.quiz_data_precalculee (periode_ref, categorie, ville, mois, jour_mois);")
  dbExecute(con, "CREATE INDEX idx_quiz_saison ON public.quiz_data_precalculee (periode_ref, categorie, mois);")
  dbExecute(con, "VACUUM ANALYZE public.quiz_data_precalculee;")
  
  cat("✅ Structure appliquée avec succès sur la base locale.\n")
  
}, finally = {
  cat("Déconnexion de la base de données.\n")
  dbDisconnect(con)
})

cat("\n✅ Base de données '", Sys.getenv("DB_NAME"), "' alimentée avec succès !\n")