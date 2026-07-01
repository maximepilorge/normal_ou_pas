# global.R — initialisation de l'application (chargé automatiquement par Shiny
# avant ui.R et server.R). Responsabilités : connexion BDD (pool), sourcing des
# utilitaires, chargement des listes et agrégats servant à toute l'UI.
#
# Pistes d'évolution : voir la section « TODOs connus » du CLAUDE.md.

# Librairies
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(plotly)
library(bslib)
library(DBI)
library(RPostgres)
library(pool)
library(dbplyr)
library(glue)
library(leaflet)

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# --- CHARGEMENT DES DONNÉES ---

# Établir une "promesse" de connexion à la BDD
db_pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# S'assurer que le pool se ferme quand l'app s'arrête
onStop(function() {
  pool::poolClose(db_pool)
})

# --- FONCTIONS ---
# Générateur de la carte de résultat partageable du quiz (fonction pure ggplot).
source(here::here("utils", "render_partage.R"))
# Utilitaires purs partagés (get_season_info, détection petit écran, log_debug).
source(here::here("utils", "helpers.R"))

# --- AUTRES VARIABLES ---
mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
             "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")

periodes_disponibles <- tbl(db_pool, "stats_normales") %>%
  distinct(periode_ref) %>%
  arrange(periode_ref) %>%
  pull()

# Villes RÉELLEMENT alimentées. Certaines villes côtières/insulaires (Le Havre,
# La Rochelle, Ajaccio…) n'ont pas de données ERA5-Land exploitables (grille terre
# uniquement) : on exige une couverture minimale (≥ 10 années distinctes) pour ne
# pas les proposer dans l'app. Liste dynamique : elle s'ajuste seule au contenu de
# la BDD, et sert de source de vérité à tous les onglets ET à la carte.
villes_triees <- tbl(db_pool, "temperatures_max") %>%
  filter(!is.na(temperature_max)) %>%
  group_by(ville) %>%
  summarise(n_annees = n_distinct(annee), .groups = "drop") %>%
  filter(n_annees >= 10) %>%
  arrange(ville) %>%
  pull(ville)

# Bornes d'années dérivées dynamiquement de la BDD pour rester synchronisées
# avec les données après chaque mise à jour (cf. pipeline utils/)
plage_annees <- tbl(db_pool, "temperatures_max") %>%
  summarise(
    min = min(annee, na.rm = TRUE),
    max = max(annee, na.rm = TRUE),
    derniere_date = max(date, na.rm = TRUE)
  ) %>%
  collect()

an_min_data <- plage_annees$min
an_max_data <- plage_annees$max
# Dernière date RÉELLEMENT disponible : ERA5-Land accuse un décalage (les données
# ne vont jamais jusqu'à aujourd'hui). Sert de date par défaut à l'onglet « Une
# journée » (affinée par ville côté serveur).
derniere_date_dispo <- as.Date(plage_annees$derniere_date)

# Dernière date disponible PAR VILLE (l'onglet « Une journée » cale son calendrier
# dessus). Pré-calculé en UNE requête (~30 lignes) pour éviter un max(date) à chaque
# changement de ville. Vecteur nommé ville -> Date ; NULL si indisponible (repli sur
# la requête ponctuelle côté module).
derniere_date_par_ville <- tryCatch({
  d <- tbl(db_pool, "temperatures_max") %>%
    filter(!is.na(temperature_max)) %>%
    group_by(ville) %>%
    summarise(d = max(date, na.rm = TRUE), .groups = "drop") %>%
    collect()
  stats::setNames(as.Date(d$d), d$ville)
}, error = function(e) NULL)

# Tables optionnelles alimentées par la version mise à jour du pipeline
# (tmin -> canicules + indicateurs annuels). On teste leur présence au démarrage
# pour que l'app dégrade proprement tant que le pipeline n'a pas été ré-exécuté
# (les onglets concernés affichent alors un message d'indisponibilité au lieu de
# planter).
table_existe <- function(nom) {
  tryCatch(DBI::dbExistsTable(db_pool, nom), error = function(e) FALSE)
}
indicateurs_disponibles <- table_existe("indicateurs_annuels")
# Projections TRACC (delta DRIAS par niveau de réchauffement) : alimentées par la
# chaîne utils/telecharger_drias.R -> calculer_projections.R -> deployer_projections.R.
projections_disponibles <- table_existe("stats_normales_projetees")
# Période de référence sur laquelle les deltas de projection sont ancrés (présent).
PERIODE_REF_PROJECTION <- "1991-2020"
# Table RUNTIME des scores de série du quiz (créée à la main, hors pipeline ;
# cf. utils/quiz_series_scores.sql). Absente -> l'app n'écrit pas les scores et
# masque le « meilleur score » personnel (dégradation gracieuse, cf. analytics).
quiz_scores_disponibles <- table_existe("quiz_series_scores")
# Agrégat pré-calculé des candidats du quiz (démarrage d'une série), dérivé de
# quiz_data_precalculee (cf. utils/quiz_candidats.sql). Absent -> l'app agrège à
# la volée : correct mais lent sur grosse base (~24 s en prod).
quiz_candidats_disponibles <- table_existe("quiz_candidats")

# .periode_bornes (bornes d'un libellé « AAAA-AAAA ») vit dans utils/helpers.R
# (sourcé plus haut), avec les autres utilitaires purs.

# --- Données pour la carte comparée des villes (onglet « Comparer ») ---
# Coordonnées WGS84 des villes, dérivées de la SOURCE DE VÉRITÉ unique
# (utils/villes_reference.R, table villes_insee). On source ce fichier « pur »
# (un simple tableau, sans sf ni accès réseau) plutôt que definir_mailles_
# communes.R, afin de ne pas charger sf au runtime de l'app.
source(here::here("utils", "villes_reference.R"))

# La carte (pastilles + filtre) ne montre que les villes effectivement alimentées
# en base : on aligne les coordonnées sur villes_triees.
villes_coords <- villes_insee %>%
  select(ville, latitude, longitude) %>%
  filter(ville %in% villes_triees)

# Référence pour la carte temporelle : on cartographie, par ville et par année,
# l'écart à la normale d'une période ANCIENNE (la plus ancienne disponible, p. ex.
# 1951-1980), afin de visualiser le réchauffement progresser dans le temps.
periode_ref_carte <- periodes_disponibles[which.min(
  vapply(periodes_disponibles, function(p) .periode_bornes(p)[1], numeric(1)))]

# Anomalies pré-calculées par VILLE et par ANNÉE (jointure faite en base -> ~30
# villes × ~76 ans). L'anomalie compare chaque jour observé à la normale du même
# jour calendaire (robuste aux années partielles). Sert UNIQUEMENT à la trajectoire
# de l'onglet « Comparer » (ville vs ensemble).
#
# Calcul PARESSEUX (au premier accès, mémoïsé pour le process) plutôt qu'au
# démarrage : cette jointure sur ~4 M lignes est le poste le plus lourd du boot ;
# la sortir du chemin de lancement accélère le time-to-first-paint après un
# démarrage à froid. Le résultat (y compris un NULL en cas d'erreur) est mis en
# cache pour ne pas être recalculé à chaque ouverture de l'onglet.
.anomalies_villes_annee_cache <- NULL
.anomalies_villes_annee_calcule <- FALSE
obtenir_anomalies_villes_annee <- function() {
  if (.anomalies_villes_annee_calcule) return(.anomalies_villes_annee_cache)
  .anomalies_villes_annee_cache <<- tryCatch(
    tbl(db_pool, "temperatures_max") %>%
      inner_join(
        tbl(db_pool, "stats_normales") %>%
          filter(periode_ref == !!periode_ref_carte) %>%
          select(ville, mois, jour_mois, t_moy),
        by = c("ville", "mois", "jour_mois")
      ) %>%
      group_by(ville, annee) %>%
      summarise(
        anomalie = mean(temperature_max - t_moy, na.rm = TRUE),
        moy_annuelle = mean(temperature_max, na.rm = TRUE),
        n_jours = n(),
        .groups = "drop"
      ) %>%
      collect(),
    error = function(e) {
      warning("Anomalies villes/année (carte) indisponibles : ", conditionMessage(e))
      NULL
    })
  .anomalies_villes_annee_calcule <<- TRUE
  .anomalies_villes_annee_cache
}