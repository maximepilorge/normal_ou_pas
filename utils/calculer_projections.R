# calculer_projections.R
# -----------------------------------------------------------------------------
# Calcul des indicateurs de PROJECTION (TRACC) à partir des séries journalières
# DRIAS extraites par telecharger_drias.R (tibble ville/date/temperature_max/min).
#
# Deux sorties, alimentant les deux features (cf. docs/plan_projections_tracc.md) :
#   1) DELTAS fenêtrés ±7 j (moy, p10, p90) par ville × jour calendaire × niveau
#      de réchauffement — pour le quiz (à ADDITIONNER aux normales ERA5).
#      Méthode : delta = stat(fenêtre du niveau) − stat(fenêtre de référence).
#   2) EXTRÊMES PROJETÉS (jours de forte chaleur + jours de gel) par niveau —
#      méthode delta sur ERA5 (cf. calculer_extremes_projetes), cohérente avec
#      l'observé (même seuil, même échelle).
#
# Les fenêtres temporelles (référence + niveaux) sont des PARAMÈTRES : les bornes
# officielles TRACC restent à figer (cf. plan §11). Sourcé (fonctions pures).
# -----------------------------------------------------------------------------

library(dplyr)
library(lubridate)

# Réutilise les indicateurs (forte chaleur) et la fenêtre glissante — fonctions
# pures, MÊMES définitions que l'observé (clé de la cohérence observé/projeté).
source(here::here("utils", "calculer_indicateurs.R"))
source(here::here("utils", "fenetre_glissante.R"))

FENETRE_PROJ <- 7  # ±7 jours, cohérent avec les normales du quiz (preparer_data.R)

# Fenêtre glissante : définitions partagées (cf. utils/fenetre_glissante.R), pour
# garantir un lissage identique entre normales observées et deltas projetés.
.ref_jours <- construire_ref_jours()
.fenetre_map <- construire_fenetre_map(FENETRE_PROJ)

# Stats fenêtrées ±7 j par (ville, jour calendaire) sur un sous-ensemble.
# tmax : moyenne + p10/p90 (quiz) ; tmin : moyenne (delta pour le gel projeté).
.stats_fenetrees <- function(donnees_periode) {
  donnees_periode %>%
    mutate(mois = month(date), jour_mois = day(date)) %>%
    left_join(.ref_jours, by = c("mois", "jour_mois")) %>%
    inner_join(.fenetre_map, by = "jour_ref", relationship = "many-to-many") %>%
    group_by(ville, jour_cible) %>%
    summarise(
      t_moy    = mean(temperature_max, na.rm = TRUE),
      p10      = quantile(temperature_max, probs = 0.10, na.rm = TRUE),
      p90      = quantile(temperature_max, probs = 0.90, na.rm = TRUE),
      tmin_moy = mean(temperature_min, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(.ref_jours, by = c("jour_cible" = "jour_ref")) %>%
    select(ville, mois, jour_mois, t_moy, p10, p90, tmin_moy)
}

# --- Calibration de la fenêtre du niveau +4 °C (horizon 2100) ------------------
# La TRACC ≈ RCP8.5 en 2050 mais EN DESSOUS en 2100 : une fenêtre calendaire fixe
# 2081-2100 capterait ~+6 °C (RCP8.5 pur), pas le +4 °C TRACC. On ANCRE donc la
# fenêtre 2050 (= +2,7 °C, où TRACC ≈ RCP8.5) puis on cherche la fenêtre 20 ans
# centrée où la France se réchauffe de +1,3 °C de plus (= +4 °C), sur la moyenne
# France (villes) de l'ENSEMBLE. Retour : c(an_debut, an_fin).
calibrer_fenetre_4deg <- function(series, increment = 1.3,
                                  ref_window = c(2041, 2060), largeur = 20) {
  annuel <- series %>%
    mutate(annee = year(date), tmean = (temperature_max + temperature_min) / 2) %>%
    group_by(simulation, annee) %>% summarise(tmean = mean(tmean), .groups = "drop") %>%
    group_by(annee) %>% summarise(tmean = mean(tmean), .groups = "drop")   # moyenne ensemble

  niveau_ref <- annuel %>% filter(annee >= ref_window[1], annee <= ref_window[2]) %>%
    summarise(m = mean(tmean)) %>% pull(m)
  cible <- niveau_ref + increment

  demi <- largeur / 2
  an_max <- max(annuel$annee)
  centres <- ref_window[2]:(an_max - demi)
  moy_fenetre <- function(c) {
    f <- annuel %>% filter(annee >= c - demi + 1, annee <= c + demi)
    if (nrow(f) < largeur) NA_real_ else mean(f$tmean)
  }
  vals <- vapply(centres, moy_fenetre, numeric(1))
  c_hit <- centres[which(vals >= cible)][1]
  if (is.na(c_hit)) c_hit <- an_max - demi   # repli : dernière fenêtre complète
  c(c_hit - demi + 1, c_hit + demi)
}

# --- 1) Deltas fenêtrés par niveau (PAR simulation -> central + fourchette) ----
# series           : tibble ville/simulation/date/temperature_max/temperature_min
# fenetre_ref      : c(an_debut, an_fin) — référence du delta (présent, ex. 1991-2020)
# fenetres_niveaux : liste nommée "label" = c(an_debut, an_fin) (un par niveau)
# On calcule le delta PAR simulation (stat fenêtrée du niveau − stat de la
# référence), puis on agrège entre simulations : central (médiane) + fourchette
# (min-max = enveloppe des narratifs). Retour par (ville, mois, jour_mois, niveau):
#   delta_moy / delta_moy_bas / delta_moy_haut, delta_p10, delta_p90 (centraux).
calculer_deltas_fenetres <- function(series, fenetre_ref, fenetres_niveaux) {
  par_sim <- bind_rows(lapply(unique(series$simulation), function(sim) {
    s <- series %>% filter(simulation == sim) %>% mutate(annee = year(date))
    ref <- .stats_fenetrees(s %>% filter(annee >= fenetre_ref[1], annee <= fenetre_ref[2])) %>%
      rename(ref_moy = t_moy, ref_p10 = p10, ref_p90 = p90, ref_tmin = tmin_moy)
    bind_rows(lapply(names(fenetres_niveaux), function(niv) {
      w <- fenetres_niveaux[[niv]]
      .stats_fenetrees(s %>% filter(annee >= w[1], annee <= w[2])) %>%
        inner_join(ref, by = c("ville", "mois", "jour_mois")) %>%
        transmute(ville, mois, jour_mois, niveau_rechauffement = niv, simulation = sim,
                  delta_t_moy    = t_moy - ref_moy,
                  delta_p10      = p10  - ref_p10,
                  delta_p90      = p90  - ref_p90,
                  delta_tmin_moy = tmin_moy - ref_tmin)
    }))
  }))

  par_sim %>%
    group_by(ville, mois, jour_mois, niveau_rechauffement) %>%
    summarise(
      delta_moy      = median(delta_t_moy),
      delta_moy_bas  = min(delta_t_moy),
      delta_moy_haut = max(delta_t_moy),
      delta_p10      = median(delta_p10),
      delta_p90      = median(delta_p90),
      delta_tmin_moy = median(delta_tmin_moy),
      .groups = "drop"
    )
}

# --- 2) Extrêmes projetés par niveau : jours de FORTE CHALEUR et de GEL ---------
# MÉTHODE DELTA (cohérente avec l'observé, qui est sur ERA5) : on prend la série
# ERA5 récente (1991-2020), on la DÉCALE du réchauffement DRIAS (delta journalier,
# médiane inter-sim, déjà calculé par calculer_deltas_fenetres : delta_t_moy pour
# tmax, delta_tmin_moy pour tmin), puis on compte avec le seuil ERA5.
#   -> tout est sur l'échelle ERA5 (même seuil que l'observé) : projeté COMPARABLE
#      à l'observé et cohérent par construction (futur ≥ présent).
# NB : compter les séries DRIAS absolues avec le seuil DRIAS mélangerait deux
# « thermomètres » (DRIAS/SAFRAN ~1,5 °C plus chaud qu'ERA5-Land) -> incohérent.
#   deltas      : sortie de calculer_deltas_fenetres (delta_moy, delta_tmin_moy…)
#   series_era5 : tibble ville/date/temperature_max/temperature_min (ERA5, ≥1973)
#   fenetre_shift : période ERA5 « présent » décalée (défaut 1991-2020).
# Retour : ville, niveau_rechauffement, jours_chaleur, jours_gel (jours/an).
calculer_extremes_projetes <- function(deltas, series_era5,
                                       fenetre_shift = c(1991, 2020)) {
  seuils <- calculer_seuil_forte_chaleur(series_era5)            # 90e pct JJA 1973-2003 ERA5
  base <- series_era5 %>%
    mutate(annee = year(date), mois = month(date), jour_mois = day(date)) %>%
    filter(annee >= fenetre_shift[1], annee <= fenetre_shift[2]) %>%
    inner_join(seuils, by = "ville")
  n_ans <- fenetre_shift[2] - fenetre_shift[1] + 1

  bind_rows(lapply(unique(deltas$niveau_rechauffement), function(niv) {
    d <- deltas %>% filter(niveau_rechauffement == niv) %>%
      select(ville, mois, jour_mois, delta_t_moy = delta_moy, delta_tmin_moy)
    base %>%
      inner_join(d, by = c("ville", "mois", "jour_mois")) %>%
      group_by(ville) %>%
      summarise(
        jours_chaleur = sum((temperature_max + delta_t_moy)    >= seuil, na.rm = TRUE) / n_ans,
        jours_gel     = sum((temperature_min + delta_tmin_moy) <= 0,     na.rm = TRUE) / n_ans,
        .groups = "drop") %>%
      mutate(niveau_rechauffement = niv)
  }))
}

# =============================================================================
# Exécution autonome (Rscript) — test end-to-end sur les séries 5 simulations
# =============================================================================
if (sys.nframe() == 0L) {
  library(here)
  source(here::here("utils", "definir_mailles_communes.R"))  # villes_insee

  chemin <- here::here("data", "drias_villes_5sims.rds")
  if (!file.exists(chemin)) chemin <- here::here("data", "drias_paris_5sims.rds")
  if (!file.exists(chemin)) {
    message("Séries 5-sim absentes : lancer d'abord telecharger_drias.R."); quit(save = "no")
  }
  series <- readRDS(chemin)

  # On ne garde que les villes présentes dans TOUTES les simulations : une ville
  # avec moins de simulations (ex. Ajaccio, hors zone de 4 commandes sur 5)
  # n'aurait pas de fourchette inter-sim cohérente.
  n_sims <- dplyr::n_distinct(series$simulation)
  villes_completes <- series %>% group_by(ville) %>%
    summarise(n = dplyr::n_distinct(simulation), .groups = "drop") %>%
    filter(n == n_sims) %>% pull(ville)
  exclues <- setdiff(unique(series$ville), villes_completes)
  if (length(exclues) > 0)
    message("Villes exclues (simulations incomplètes) : ", paste(exclues, collapse = ", "))
  series <- series %>% filter(ville %in% villes_completes)

  message(sprintf("Données : %s — %d ville(s), %d simulations.", basename(chemin),
                  dplyr::n_distinct(series$ville), dplyr::n_distinct(series$simulation)))

  # Fenêtres 20 ans. Réf du delta = présent 1991-2020. Le niveau +2,7 °C (2050)
  # = 2041-2060 (TRACC ≈ RCP8.5). Le niveau +4 °C est CALIBRÉ sur le réchauffement
  # France de l'ensemble (sinon 2081-2100 RCP8.5 surestimerait, cf. plan §11).
  fenetre_ref <- c(1991, 2020)
  fen_4deg <- calibrer_fenetre_4deg(series)
  fenetres_niveaux <- list("2050_+2.7" = c(2041, 2060), "2100_+4.0" = fen_4deg)
  message(sprintf("Fenêtre +4 °C calibrée : %d-%d", fen_4deg[1], fen_4deg[2]))

  ref_ville <- if ("Paris" %in% series$ville) "Paris" else sort(unique(series$ville))[1]

  message("\n== DELTAS fenêtrés — central + fourchette inter-simulations ==")
  deltas <- calculer_deltas_fenetres(series, fenetre_ref, fenetres_niveaux)
  deltas %>% filter(ville == ref_ville) %>%
    group_by(niveau_rechauffement) %>%
    summarise(delta_central = round(mean(delta_moy), 1),
              fourchette = sprintf("[%.1f ; %.1f]", mean(delta_moy_bas), mean(delta_moy_haut)),
              .groups = "drop") %>% as.data.frame() %>% print(row.names = FALSE)
  message(sprintf("(ville repère : %s — delta tmax annuel moyen + enveloppe inter-sim)", ref_ville))

  saison <- deltas %>% filter(ville == ref_ville) %>%
    mutate(saison = ifelse(mois %in% c(12, 1, 2), "hiver",
                    ifelse(mois %in% 6:8, "ete", "autre"))) %>%
    filter(saison != "autre") %>%
    group_by(niveau_rechauffement, saison) %>%
    summarise(delta_central = round(mean(delta_moy), 1), .groups = "drop")
  message("\nDelta tmax central par saison :")
  print(as.data.frame(saison), row.names = FALSE)

  message("\n== EXTRÊMES projetés (méthode delta sur ERA5) — forte chaleur & gel ==")
  # Série ERA5 observée (même échelle/seuil que l'observé), décalée du delta DRIAS.
  suppressMessages({ library(DBI); library(RPostgres); library(dotenv) })
  load_dot_env(file = here::here(".Renviron"))
  con_era5 <- dbConnect(RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"), host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASS"))
  series_era5 <- tbl(con_era5, "temperatures_max") %>%
    filter(ville %in% !!villes_completes, annee >= 1973L, annee <= 2020L) %>%
    select(ville, date, temperature_max, temperature_min) %>% collect()
  dbDisconnect(con_era5)

  ext <- calculer_extremes_projetes(deltas, series_era5)
  ext %>% filter(ville == ref_ville) %>%
    transmute(ville, niveau_rechauffement,
              jours_chaleur = round(jours_chaleur, 1),
              jours_gel = round(jours_gel, 1)) %>%
    as.data.frame() %>% print(row.names = FALSE)

  saveRDS(deltas, here::here("data", "projections_deltas.rds"))
  saveRDS(ext,    here::here("data", "projections_extremes.rds"))
  message("\nOK — calculer_projections.R (multi-sim) validé. Sorties sauvées dans data/.")
}
