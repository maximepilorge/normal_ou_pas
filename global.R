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

# Utilitaire : bornes (début, fin) d'un libellé de période "AAAA-AAAA".
.periode_bornes <- function(p) as.numeric(strsplit(p, "-")[[1]])

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

# Anomalies pré-calculées par VILLE et par ANNÉE (une seule requête au démarrage,
# jointure faite en base -> ~30 villes × ~76 ans). L'anomalie compare chaque jour
# observé à la normale du même jour calendaire (robuste aux années partielles).
# Sert à la fois à colorer la carte (filtre par année, sans requête par cran de
# curseur) et à tracer la trajectoire d'une ville vs l'ensemble. Gardé contre
# toute erreur pour ne pas bloquer le lancement.
anomalies_villes_annee <- tryCatch(
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