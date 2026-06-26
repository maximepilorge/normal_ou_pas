# inspecter_donnees.R
# -----------------------------------------------------------------------------
# Outil d'INSPECTION pour vérifier à la main les chiffres affichés dans l'app
# (graphe forte chaleur / gel, projections du quiz). Croise :
#   - la série DRIAS brute : data/drias_villes_5sims.rds
#   - les sorties calculées : data/projections_deltas.rds, projections_extremes.rds
#   - la donnée ERA5 + les tables déployées : base PostgreSQL (temperatures_max,
#     indicateurs_annuels, extremes_projetes, stats_normales_projetees)
#
# Usage (console R) :
#   source("utils/inspecter_donnees.R")
#   apercu()                      # ce que contiennent les fichiers/tables
#   verifier_ville("Lyon")        # tout le détail chiffré d'une ville
#   exporter_verifications()      # -> data/verification_projections.csv (Excel)
#   serie_brute("Lyon", sim = "ICHEC-EC-EARTH/KNMI-RACMO22E", an1 = 2003, an2 = 2003)
#   detail_fenetre("Lyon", 8, 11, "2050", csv = TRUE)   # vérifie le delta en base
#                                 # (détail par modèle/année/jour, fenêtre ±7 j)
#
# Ou en ligne de commande :  Rscript utils/inspecter_donnees.R Lyon
# -----------------------------------------------------------------------------

suppressMessages({
  library(DBI); library(RPostgres); library(dplyr); library(lubridate)
  library(dotenv); library(here)
})
# Sourcer calculer_projections.R donne aussi les helpers de fenêtrage (.ref_jours,
# .fenetre_map, calibrer_fenetre_4deg) + calculer_seuil_forte_chaleur (transitif).
# Le bloc autonome de ce fichier ne s'exécute pas quand il est sourcé (sys.nframe).
source(here::here("utils", "calculer_projections.R"))

.RDS_SERIE  <- here::here("data", "drias_villes_5sims.rds")
.RDS_DELTAS <- here::here("data", "projections_deltas.rds")

.connexion <- function() {
  load_dot_env(file = here::here(".Renviron"))
  dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("DB_NAME"), host = Sys.getenv("DB_HOST"),
            port = as.integer(Sys.getenv("DB_PORT")),
            user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASS"))
}

# --- Aperçu général ---------------------------------------------------------
apercu <- function() {
  con <- .connexion(); on.exit(dbDisconnect(con), add = TRUE)
  cat("== Tables BDD (lignes) ==\n")
  for (t in c("temperatures_max", "indicateurs_annuels", "extremes_projetes",
              "stats_normales_projetees")) {
    n <- tryCatch(dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n,
                  error = function(e) NA)
    cat(sprintf("  %-26s %s\n", t, ifelse(is.na(n), "(absente)", format(n, big.mark = " "))))
  }
  if (file.exists(.RDS_DELTAS)) {
    d <- readRDS(.RDS_DELTAS)
    cat(sprintf("\n== %s ==\n  %d lignes, %d villes, niveaux : %s\n",
                basename(.RDS_DELTAS), nrow(d), dplyr::n_distinct(d$ville),
                paste(unique(d$niveau_rechauffement), collapse = ", ")))
  }
  if (file.exists(.RDS_SERIE)) {
    info <- file.info(.RDS_SERIE)
    cat(sprintf("\n== %s ==\n  %.0f Mo sur disque (série DRIAS brute ; chargement via serie_brute()).\n",
                basename(.RDS_SERIE), info$size / 1e6))
  }
  invisible(NULL)
}

# --- Métriques d'une ville (cœur, réutilisé par verifier_ville + export) -----
# Reproduit EXACTEMENT la logique de l'app : seuil ERA5 (90e pct JJA 1973-2003),
# observé forte chaleur / gel par décennie, projeté (table déployée).
.metriques_ville <- function(ville, con) {
  d <- tbl(con, "temperatures_max") %>%
    filter(ville == !!ville) %>%
    select(ville, date, temperature_max, temperature_min) %>% collect()
  seuil <- calculer_seuil_forte_chaleur(d)$seuil

  obs <- d %>%
    mutate(an = year(date)) %>%
    group_by(an) %>%
    summarise(ch = sum(temperature_max >= seuil, na.rm = TRUE),
              gel = sum(temperature_min <= 0, na.rm = TRUE),
              nb = n(), .groups = "drop") %>%
    filter(nb >= 360) %>%
    mutate(dec = floor(an / 10) * 10) %>%
    group_by(dec) %>%
    summarise(chaleur = round(mean(ch), 1), gel = round(mean(gel), 1), .groups = "drop")

  proj <- tbl(con, "extremes_projetes") %>% filter(ville == !!ville) %>% collect() %>%
    transmute(niveau_rechauffement,
              chaleur = round(jours_chaleur, 1), gel = round(jours_gel, 1))

  list(seuil = round(seuil, 1), observe = obs, projete = proj)
}

# --- Vérification détaillée d'une ville (console) ----------------------------
verifier_ville <- function(ville) {
  con <- .connexion(); on.exit(dbDisconnect(con), add = TRUE)
  m <- .metriques_ville(ville, con)
  cat(sprintf("===== %s =====\n", ville))
  cat(sprintf("Seuil forte chaleur ERA5 (90e pct tmax JJA 1973-2003) : %s °C\n\n", m$seuil))
  cat("OBSERVÉ — jours/an par décennie :\n"); print(as.data.frame(m$observe), row.names = FALSE)
  cat("\nPROJETÉ (méthode delta, médiane 17 sims) — jours/an :\n")
  print(as.data.frame(m$projete), row.names = FALSE)

  # Delta tmax/tmin par saison (depuis le RDS deltas) — explique le décalage.
  if (file.exists(.RDS_DELTAS)) {
    dl <- readRDS(.RDS_DELTAS) %>% filter(ville == !!ville) %>%
      mutate(saison = ifelse(mois %in% c(12,1,2), "hiver",
                      ifelse(mois %in% 6:8, "été", "autre"))) %>%
      filter(saison != "autre") %>%
      group_by(niveau_rechauffement, saison) %>%
      summarise(delta_tmax = round(mean(delta_moy), 1),
                delta_tmin = round(mean(delta_tmin_moy), 1), .groups = "drop")
    cat("\nDELTA appliqué (réchauffement DRIAS médian) — °C :\n")
    print(as.data.frame(dl), row.names = FALSE)
  }

  # Contrôle de cohérence : projeté 2050 >= observé décennie 2020 ?
  o2020 <- m$observe$chaleur[m$observe$dec == 2020]
  p2050 <- m$projete$chaleur[grepl("2050", m$projete$niveau_rechauffement)]
  if (length(o2020) && length(p2050))
    cat(sprintf("\n[contrôle] forte chaleur 2050 (%.1f) vs 2020 observé (%.1f) : %s\n",
                p2050, o2020, ifelse(p2050 >= o2020, "OK ✓", "INCOHÉRENT ✗")))
  invisible(m)
}

# --- Export CSV de vérification (toutes villes, ouvrable dans Excel) ---------
exporter_verifications <- function(chemin = here::here("data", "verification_projections.csv")) {
  con <- .connexion(); on.exit(dbDisconnect(con), add = TRUE)
  villes <- tbl(con, "extremes_projetes") %>% distinct(ville) %>% pull() %>% sort()
  lignes <- lapply(villes, function(v) {
    m <- .metriques_ville(v, con)
    o <- m$observe; p <- m$projete
    get_o <- function(dec, col) { x <- o[[col]][o$dec == dec]; if (length(x)) x else NA }
    get_p <- function(niv, col) { x <- p[[col]][grepl(niv, p$niveau_rechauffement)]; if (length(x)) x else NA }
    data.frame(ville = v, seuil = m$seuil,
               chaleur_2010 = get_o(2010, "chaleur"), chaleur_2020 = get_o(2020, "chaleur"),
               chaleur_2050 = get_p("2050", "chaleur"), chaleur_2100 = get_p("2100", "chaleur"),
               gel_2010 = get_o(2010, "gel"), gel_2020 = get_o(2020, "gel"),
               gel_2050 = get_p("2050", "gel"), gel_2100 = get_p("2100", "gel"))
  })
  res <- bind_rows(lignes)
  write.csv(res, chemin, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("Export : %s (%d villes)\n", chemin, nrow(res)))
  invisible(res)
}

# --- Série DRIAS BRUTE d'une ville (pour creuser jour par jour) --------------
# sim : nom complet d'une simulation (cf. apercu()/serie_brute()), ou NULL (toutes).
# Si csv = TRUE, écrit data/serie_brute_<ville>.csv.
serie_brute <- function(ville, sim = NULL, an1 = NULL, an2 = NULL, csv = FALSE) {
  if (!file.exists(.RDS_SERIE)) stop("RDS série absent : lancer telecharger_drias.R.")
  message("Chargement de la série DRIAS (peut prendre quelques secondes)…")
  s <- readRDS(.RDS_SERIE) %>% filter(ville == !!ville)
  if (!is.null(sim)) s <- s %>% filter(simulation == sim)
  if (!is.null(an1)) s <- s %>% filter(year(date) >= an1)
  if (!is.null(an2)) s <- s %>% filter(year(date) <= an2)
  s <- s %>% arrange(simulation, date)
  if (isTRUE(csv)) {
    f <- here::here("data", paste0("serie_brute_", gsub("[^A-Za-z0-9]+", "_", ville), ".csv"))
    write.csv(s, f, row.names = FALSE, fileEncoding = "UTF-8")
    cat(sprintf("Export : %s (%d lignes)\n", f, nrow(s)))
  }
  cat(sprintf("%d lignes | simulations : %s\n", nrow(s),
              paste(unique(s$simulation), collapse = ", ")))
  invisible(s)
}

# --- DÉTAIL d'une fenêtre ±7 j (vérifie le delta de stats_normales_projetees) -
# Pour (ville, date, niveau), reconstitue TOUTE la chaîne de calcul du delta :
#   1. données DRIAS brutes (par simulation / année / jour) dans la fenêtre ±7 j,
#      sur les années du niveau ET sur la référence 1991-2020 ;
#   2. moyenne fenêtrée par simulation (niveau et réf) -> delta par simulation ;
#   3. médiane inter-simulations = la valeur attendue en base ;
#   4. comparaison à stats_normales_projetees (delta_moy, delta_tmin_moy).
# mois, jour : entiers (ex. 8, 11 pour le 11 août). niveau : "2050" ou "2100".
# csv = TRUE -> exporte le détail brut (par sim/jour) dans data/.
detail_fenetre <- function(ville, mois, jour, niveau = c("2050", "2100"), csv = FALSE) {
  niveau <- match.arg(niveau)
  if (!file.exists(.RDS_SERIE)) stop("RDS série absent : lancer telecharger_drias.R.")
  message("Chargement de la série DRIAS (quelques secondes)…")
  s_all <- readRDS(.RDS_SERIE)

  # Fenêtres : référence 1991-2020 ; niveau 2050 = 2041-2060 ; 2100 = calibré.
  fen_ref <- c(1991, 2020)
  fen     <- if (niveau == "2050") c(2041, 2060) else calibrer_fenetre_4deg(s_all)
  niv_key <- if (niveau == "2050") "2050_+2.7" else "2100_+4.0"

  # Clés calendaires de la fenêtre ±7 j (logique EXACTE du pipeline).
  jc    <- .ref_jours$jour_ref[.ref_jours$mois == mois & .ref_jours$jour_mois == jour]
  jrefs <- .fenetre_map$jour_ref[.fenetre_map$jour_cible == jc]
  keys  <- .ref_jours %>% filter(jour_ref %in% jrefs) %>%
    transmute(k_mois = mois, k_jour = jour_mois)

  s <- s_all %>% filter(ville == !!ville) %>%
    mutate(annee = year(date), k_mois = month(date), k_jour = day(date)) %>%
    semi_join(keys, by = c("k_mois", "k_jour"))

  fenetre_label <- sprintf("%d-%d", fen[1], fen[2])
  cat(sprintf("===== %s · %02d/%02d · niveau %s (fenêtre %s) =====\n",
              ville, jour, mois, niveau, fenetre_label))
  cat(sprintf("Jours calendaires de la fenêtre ±7 j : %s\n",
              paste(sprintf("%02d/%02d", keys$k_jour, keys$k_mois), collapse = " ")))

  detail_niv <- s %>% filter(annee >= fen[1], annee <= fen[2]) %>%
    transmute(simulation, date, annee, temperature_max, temperature_min, periode = "niveau")
  detail_ref <- s %>% filter(annee >= fen_ref[1], annee <= fen_ref[2]) %>%
    transmute(simulation, date, annee, temperature_max, temperature_min, periode = "reference")

  # Par sim : moyenne ET percentiles p10/p90 fenêtrés (mêmes def que le pipeline).
  agg <- function(df) df %>% group_by(simulation) %>%
    summarise(n = n(), tmax = mean(temperature_max),
              p10 = quantile(temperature_max, 0.10, na.rm = TRUE),
              p90 = quantile(temperature_max, 0.90, na.rm = TRUE),
              tmin = mean(temperature_min), .groups = "drop")
  per_sim <- agg(detail_niv) %>%
    inner_join(agg(detail_ref), by = "simulation", suffix = c("_niv", "_ref")) %>%
    mutate(delta_moy = round(tmax_niv - tmax_ref, 2),
           delta_p10 = round(p10_niv - p10_ref, 2),
           delta_p90 = round(p90_niv - p90_ref, 2),
           delta_tmin = round(tmin_niv - tmin_ref, 2)) %>%
    arrange(delta_moy)

  cat(sprintf("\nPar simulation (%d sims) — deltas fenêtrés (°C) :\n", nrow(per_sim)))
  print(as.data.frame(per_sim %>% transmute(
    simulation = substr(simulation, 1, 34), n_niv = n_niv,
    delta_moy, delta_p10, delta_p90, delta_tmin)), row.names = FALSE)

  med <- per_sim %>% summarise(
    delta_moy = round(median(delta_moy), 2), delta_p10 = round(median(delta_p10), 2),
    delta_p90 = round(median(delta_p90), 2), delta_tmin = round(median(delta_tmin), 2))
  con <- .connexion(); on.exit(dbDisconnect(con), add = TRUE)
  db <- tbl(con, "stats_normales_projetees") %>%
    filter(ville == !!ville, mois == !!mois, jour_mois == !!jour,
           niveau_rechauffement == !!niv_key) %>%
    select(delta_moy, delta_p10, delta_p90, delta_tmin_moy) %>% collect()

  cat(sprintf("\nMÉDIANE inter-sim (recalculée) : moy=%.2f  p10=%.2f  p90=%.2f  tmin=%.2f\n",
              med$delta_moy, med$delta_p10, med$delta_p90, med$delta_tmin))
  if (nrow(db) > 0) {
    cat(sprintf("VALEUR EN BASE               : moy=%.2f  p10=%.2f  p90=%.2f  tmin=%.2f\n",
                db$delta_moy, db$delta_p10, db$delta_p90, db$delta_tmin_moy))
    ok <- abs(med$delta_moy - db$delta_moy) < 0.05 && abs(med$delta_p10 - db$delta_p10) < 0.05 &&
          abs(med$delta_p90 - db$delta_p90) < 0.05 && abs(med$delta_tmin - db$delta_tmin_moy) < 0.05
    cat(sprintf("[contrôle] %s\n", ifelse(ok, "concordance OK ✓ (delta_moy/p10/p90/tmin)",
                "ÉCART (la base diffère du recalcul) ✗")))
    cat(sprintf("[forme] delta_p90 (%.2f) %s delta_p10 (%.2f) -> la distribution %s.\n",
                db$delta_p90, ifelse(db$delta_p90 > db$delta_p10, ">", "<="), db$delta_p10,
                ifelse(db$delta_p90 > db$delta_p10,
                       "s'élargit côté chaud (extrêmes montent plus vite)",
                       "ne s'élargit pas")))

    # SUITE DU CALCUL : c'est ainsi que l'app construit la « normale projetée »
    # du boxplot = normale ERA5 (1991-2020) + delta. On la reconstitue ici.
    base9120 <- tbl(con, "stats_normales") %>%
      filter(ville == !!ville, mois == !!mois, jour_mois == !!jour,
             periode_ref == "1991-2020") %>%
      select(t_moy, seuil_bas_p10, seuil_haut_p90) %>% collect()
    if (nrow(base9120) > 0) {
      cat("\nSUITE DU CALCUL — normale projetée du boxplot = présent (1991-2020) + delta :\n")
      cat(sprintf("  moyenne : %.1f + %.2f = %.1f °C\n",
                  base9120$t_moy, db$delta_moy, base9120$t_moy + db$delta_moy))
      cat(sprintf("  p10     : %.1f + %.2f = %.1f °C\n",
                  base9120$seuil_bas_p10, db$delta_p10, base9120$seuil_bas_p10 + db$delta_p10))
      cat(sprintf("  p90     : %.1f + %.2f = %.1f °C\n",
                  base9120$seuil_haut_p90, db$delta_p90, base9120$seuil_haut_p90 + db$delta_p90))
      cat("  (ces 3 valeurs = la zone normale affichée pour ce niveau dans le boxplot quiz)\n")
    }
  } else cat("(aucune ligne en base pour cette sélection)\n")

  if (isTRUE(csv)) {
    f <- here::here("data", sprintf("detail_%s_%02d%02d_%s.csv",
                                    gsub("[^A-Za-z0-9]+", "_", ville), jour, mois, niveau))
    write.csv(bind_rows(detail_niv, detail_ref) %>% arrange(periode, simulation, date),
              f, row.names = FALSE, fileEncoding = "UTF-8")
    cat(sprintf("\nDétail brut (par sim/jour) exporté : %s (%d lignes)\n",
                f, nrow(detail_niv) + nrow(detail_ref)))
  }
  invisible(list(detail_niveau = detail_niv, detail_reference = detail_ref,
                 par_sim = per_sim, mediane = med, base = db))
}

# =============================================================================
# Ligne de commande : Rscript utils/inspecter_donnees.R [Ville]
# =============================================================================
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  apercu()
  cat("\n")
  verifier_ville(if (length(args) >= 1) args[1] else "Lyon")
}
