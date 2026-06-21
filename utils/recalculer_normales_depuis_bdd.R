# =================================================================
# RECALCUL DES TABLES DÉRIVÉES À PARTIR DE LA BASE EXISTANTE
# =================================================================
# Régénère stats_normales (avec fenêtre ±7 j pour les percentiles) et
# quiz_data_precalculee À PARTIR du temperatures_max DÉJÀ présent en base
# (ancien téléchargement mono-maille, historique complet), SANS re-télécharger
# et SANS toucher à temperatures_max.
#
# Utile pour appliquer dès maintenant les nouveaux choix (fenêtre ±7 j, quiz
# équilibré) en attendant le nouveau téléchargement ERA5 multi-mailles.
#
# La logique de calcul est identique à celle de preparer_data.R ; seule la
# SOURCE des données change (la base au lieu du RDS).

library(dplyr)
library(lubridate)
library(DBI)
library(RPostgres)
library(dotenv)
library(here)

load_dot_env(file = here::here(".Renviron"))
stopifnot(Sys.getenv("DB_HOST") != "")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

tryCatch({

  # --- 1. Lecture de temperatures_max DEPUIS LA BASE (pas le RDS) ---
  cat("Lecture de temperatures_max depuis la base...\n")
  donnees_avec_annee <- tbl(con, "temperatures_max") %>%
    select(ville, date, temperature_max) %>%
    collect() %>%
    rename(tmax_celsius = temperature_max) %>%
    mutate(annee = year(date), mois = month(date), jour_mois = day(date))

  cat("  ", nrow(donnees_avec_annee), "lignes |",
      n_distinct(donnees_avec_annee$ville), "villes | de",
      as.character(min(donnees_avec_annee$date)), "à",
      as.character(max(donnees_avec_annee$date)), "\n")

  # --- 2. Périodes de référence + fenêtre glissante ±7 j ---
  periodes_ref <- list(
    "1951-1980" = c(1951, 1980),
    "1961-1990" = c(1961, 1990),
    "1971-2000" = c(1971, 2000),
    "1981-2010" = c(1981, 2010),
    "1991-2020" = c(1991, 2020)
  )
  FENETRE <- 7

  ref_jours <- data.frame(date_ref = seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")) %>%
    mutate(jour_ref = yday(date_ref), mois = month(date_ref), jour_mois = day(date_ref)) %>%
    select(jour_ref, mois, jour_mois)
  fenetre_map <- expand.grid(jour_cible = 1:366, offset = -FENETRE:FENETRE) %>%
    mutate(jour_ref = ((jour_cible - 1 + offset) %% 366) + 1) %>%
    select(jour_cible, jour_ref)
  donnees_avec_jr <- donnees_avec_annee %>%
    left_join(ref_jours, by = c("mois", "jour_mois"))

  # --- 3. stats_normales : jour exact (graphique) + p10/p90 lissés ±7 j (quiz) ---
  cat("Calcul des normales (jour exact + seuils ±", FENETRE, "j)...\n")
  liste_stats <- lapply(names(periodes_ref), function(nom_periode) {
    annee_debut <- periodes_ref[[nom_periode]][1]
    annee_fin <- periodes_ref[[nom_periode]][2]
    cat("    ->", nom_periode, "\n")

    donnees_periode <- donnees_avec_jr %>%
      filter(annee >= annee_debut & annee <= annee_fin)

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
  stats_normales <- bind_rows(liste_stats) %>% filter(!is.na(t_moy))
  dbWriteTable(con, "stats_normales", as.data.frame(stats_normales), overwrite = TRUE)

  # --- 4. quiz_data_precalculee ---
  cat("Calcul de quiz_data_precalculee...\n")
  quiz_data_precalculee <- donnees_avec_annee %>%
    cross_join(tibble(periode_ref = names(periodes_ref))) %>%
    left_join(stats_normales, by = c("ville", "mois", "jour_mois", "periode_ref")) %>%
    filter(!is.na(seuil_haut_p90)) %>%
    mutate(
      categorie = case_when(
        tmax_celsius > seuil_haut_p90 ~ "Au-dessus des normales",
        tmax_celsius < seuil_bas_p10  ~ "En-dessous des normales",
        TRUE                          ~ "Dans les normales de saison"
      )
    ) %>%
    select(ville, date, tmax_celsius, periode_ref, categorie, mois, jour_mois, t_moy)
  dbWriteTable(con, "quiz_data_precalculee", as.data.frame(quiz_data_precalculee), overwrite = TRUE)

  # --- 5. Clés / contraintes / index (uniquement les 2 tables régénérées) ---
  cat("Application des contraintes et index...\n")
  dbExecute(con, "ALTER TABLE public.stats_normales ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.stats_normales ADD CONSTRAINT uc_stats_normales_ville_jour_periode UNIQUE (ville, mois, jour_mois, periode_ref);")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stats_ville_periode ON public.stats_normales (ville, periode_ref);")
  dbExecute(con, "VACUUM ANALYZE public.stats_normales;")

  dbExecute(con, "ALTER TABLE public.quiz_data_precalculee ADD COLUMN IF NOT EXISTS id SERIAL PRIMARY KEY;")
  dbExecute(con, "ALTER TABLE public.quiz_data_precalculee ADD CONSTRAINT uc_quiz_data_ville_date_periode UNIQUE (ville, date, periode_ref);")
  dbExecute(con, "CREATE INDEX idx_quiz_optimise ON public.quiz_data_precalculee (periode_ref, categorie, ville, mois, jour_mois);")
  dbExecute(con, "CREATE INDEX idx_quiz_saison ON public.quiz_data_precalculee (periode_ref, categorie, mois);")
  dbExecute(con, "VACUUM ANALYZE public.quiz_data_precalculee;")

  cat("✅ stats_normales et quiz_data_precalculee régénérées (temperatures_max inchangée).\n")

}, finally = {
  cat("Déconnexion de la base de données.\n")
  dbDisconnect(con)
})
