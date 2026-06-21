# Script R pour télécharger les données de température ERA5-Land
# Objectif : Télécharger les températures journalières MAXIMALES pour chaque
# ville de 1950 à aujourd'hui, avec une sauvegarde incrémentale.
#
# Optimisations :
#  - Dataset HORAIRE archivé `reanalysis-era5-land` (récupération rapide, ~2 min
#    par mois) plutôt que les statistiques journalières `derived-...-daily-
#    statistics`, calculées à la demande et beaucoup trop lentes/congestionnées.
#  - Téléchargement par SEMESTRE (6 mois = plus grande granularité acceptée par
#    le CDS) : ~154 requêtes au lieu de ~924, moins d'overhead.
#  - Requêtes CONCURRENTES via wf_request_batch (workers) : c'est le vrai levier,
#    le temps total étant dominé par l'attente côté CDS.
#  - Le max journalier est calculé localement à partir des heures (par maille),
#    puis agrégé en moyenne spatiale pondérée par la surface communale couverte.

# --------------------
# 1. Chargement des librairies
# --------------------
library(ecmwfr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ncdf4)
library(sf)
library(here)
library(dotenv)

load_dot_env(file = here::here(".Renviron"))

# Liste des villes (+ code INSEE) et fonctions d'association maille/commune.
# Le `source` ne déclenche pas le bloc autonome du fichier (cf. sys.nframe()).
source(here::here("utils", "definir_mailles_communes.R"))
villes <- villes_insee

# --- Traitement d'un fichier HORAIRE -> max journalier pondéré par ville ---
# Pour chaque maille associée à une ville, on extrait UNIQUEMENT sa série horaire
# (lecture ciblée start/count : ~120 mailles, donc peu de mémoire), on en déduit
# le maximum JOURNALIER par maille, puis on agrège en moyenne spatiale pondérée
# par la surface communale couverte. Fonction de haut niveau : réutilisable par
# les scripts de vérification.
traiter_fichier_horaire <- function(nc_path, villes_avec_coords_grille) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)

  nom_lon <- intersect(c("longitude", "lon"), names(nc$dim))[1]
  nom_lat <- intersect(c("latitude", "lat"), names(nc$dim))[1]
  lon <- round(as.numeric(ncvar_get(nc, nom_lon)), 2)
  lat <- round(as.numeric(ncvar_get(nc, nom_lat)), 2)

  # Axe temporel : dimension dont l'unité contient "since" (ex. "hours since 1970-01-01").
  noms_time <- names(nc$dim)[vapply(nc$dim, function(d)
    isTRUE(grepl("since", tolower(paste0(d$units, "")))), logical(1))]
  td <- nc$dim[[noms_time[1]]]
  parts <- strsplit(td$units, " since ")[[1]]
  facteur <- switch(tolower(trimws(parts[1])), "seconds"=1, "minutes"=60, "hours"=3600, "days"=86400, 1)
  dates <- as.Date(as.POSIXct(trimws(parts[2]), tz = "UTC") + as.numeric(td$vals) * facteur)
  nt <- length(dates)

  # Variable de données = celle qui a le PLUS de dimensions (champ 3D
  # lon×lat×temps). Indispensable : les NetCDF du CDS contiennent des variables
  # auxiliaires scalaires (number, expver) qui, prises par erreur, donnent des
  # valeurs constantes (0) au lieu de la température.
  nom_var <- names(nc$var)[which.max(vapply(nc$var, function(v) length(v$dim), integer(1)))]
  unites_var <- tryCatch(ncatt_get(nc, nom_var, "units")$value, error = function(e) "")
  kelvin <- is.character(unites_var) && grepl("^k", tolower(unites_var))

  # Mailles uniques à extraire (une maille peut desservir plusieurs villes).
  mailles <- distinct(villes_avec_coords_grille, lon_grille, lat_grille)

  max_journalier <- bind_rows(lapply(seq_len(nrow(mailles)), function(k) {
    i <- match(mailles$lon_grille[k], lon)
    j <- match(mailles$lat_grille[k], lat)
    if (is.na(i) || is.na(j)) return(NULL)
    serie <- as.numeric(ncvar_get(nc, nom_var, start = c(i, j, 1), count = c(1, 1, nt)))
    if (kelvin) serie <- serie - 273.15
    tibble(lon_grille = mailles$lon_grille[k], lat_grille = mailles$lat_grille[k],
           date = dates, t = serie) %>%
      filter(is.finite(t)) %>%
      group_by(lon_grille, lat_grille, date) %>%
      summarise(tmax_maille = max(t), .groups = "drop")
  }))

  # Moyenne spatiale pondérée par ville.
  max_journalier %>%
    inner_join(villes_avec_coords_grille, by = c("lon_grille", "lat_grille")) %>%
    group_by(ville, date) %>%
    summarise(temperature_max = weighted.mean(tmax_maille, poids), .groups = "drop")
}

# --------------------
# 2. Définition de la fonction principale
# --------------------
recuperer_donnees_era5 <- function(villes_a_telecharger = NULL) {

  # Si des villes spécifiques sont fournies, on filtre la liste.
  if (!is.null(villes_a_telecharger)) {
    villes <- villes %>% filter(ville %in% villes_a_telecharger)
  }

  # --- Configuration des fichiers et des chemins ---
  path_to_save <- here::here("data")
  dir.create(path_to_save, showWarnings = FALSE)
  nom_fichier_final <- "era5_temperatures_france.rds"
  full_path_final <- file.path(path_to_save, nom_fichier_final)

  # --- Chargement de l'existant ---
  # La reprise « consciente des trous » (plus bas) déterminera les semestres
  # manquants ou incomplets à partir des dates réellement présentes.
  if (file.exists(full_path_final)) {
    print(paste("Fichier existant trouvé. Chargement de", nom_fichier_final, "et poursuite du travail."))
    df_final_daily_max <- readRDS(full_path_final)
    print(paste("Dernière date présente :", as.character(max(df_final_daily_max$date, na.rm = TRUE))))
  } else {
    print(paste("Fichier", nom_fichier_final, "non trouvé. Démarrage de zéro."))
    df_final_daily_max <- data.frame()
  }

  # --- Pré-calcul des coordonnées de grille ---
  # On télécharge un petit échantillon pour récupérer les coordonnées exactes
  # de la grille ERA5-Land, nécessaires à l'association ville -> mailles.
  print("Détermination des points de grille pour chaque ville...")
  nc_grid_file <- "era5_grid_temp"
  full_nc_grid_path <- file.path(path_to_save, nc_grid_file)

  request_list_grid <- list(
    dataset_short_name = "reanalysis-era5-land",
    product_type = "reanalysis",
    variable = "2m_temperature",
    year = "2023",
    month = "01",
    day = "01",
    time = "00:00",
    format = "netcdf",
    area = c(max(villes$latitude) + 1, min(villes$longitude) - 1,
             min(villes$latitude) - 1, max(villes$longitude) + 1),
    target = paste0(nc_grid_file, ".nc")
  )

  wf_request(user = "maxp17.mp@gmail.com",
             request_list_grid,
             path = path_to_save,
             transfer = TRUE,
             verbose = FALSE)

  # Décompression éventuelle du fichier de grille.
  downloaded_file <- file.path(path_to_save, paste0(nc_grid_file, ".zip"))
  if (file.exists(downloaded_file)) {
    unzip(zipfile = downloaded_file, exdir = path_to_save)
    extracted_nc_file <- file.path(path_to_save, "data_0.nc")
    if (file.exists(extracted_nc_file)) {
      file.rename(from = extracted_nc_file, to = file.path(path_to_save, paste0(nc_grid_file, ".nc")))
      print("Fichier de grille décompressé et renommé avec succès.")
    } else {
      stop("Erreur : Le fichier n'a pas été trouvé après la décompression.")
    }
  }

  # Lecture du fichier de grille pour extraire les coordonnées (arrondies au
  # centième : la grille ERA5-Land est à 0,1°, l'arrondi fiabilise la jointure
  # entre fichiers/datasets en absorbant le bruit de représentation flottante).
  nc_grid_path <- paste0(full_nc_grid_path, ".nc")
  nc_grid <- nc_open(nc_grid_path)
  lon_grid <- round(ncvar_get(nc_grid, "longitude"), 2)
  lat_grid <- round(ncvar_get(nc_grid, "latitude"), 2)
  nc_close(nc_grid)
  file.remove(nc_grid_path)

  # --- Association ville -> mailles ERA5 par emprise communale ---
  # On construit les mailles réelles de la grille puis on retient pour chaque
  # ville TOUTES les mailles qui intersectent sa commune, avec un poids = part
  # de surface couverte (plusieurs lignes par ville : lon_grille, lat_grille, poids).
  sf_cells <- construire_cellules_era5(lon_grid, lat_grid)
  villes_avec_coords_grille <- associer_mailles_communes(villes, sf_cells)

  chemin_complet_assoc <- file.path(path_to_save, "villes_et_mailles_associees.rds")
  saveRDS(villes_avec_coords_grille, file = chemin_complet_assoc)
  print(paste("Fichier d'association villes/mailles sauvegardé dans :", chemin_complet_assoc))

  # (traiter_fichier_horaire est défini au niveau supérieur, en début de script.)

  # --- Téléchargement CONCURRENT (dataset HORAIRE archivé, par semestre) ---
  # Le dataset horaire reanalysis-era5-land (archivé) se récupère bien plus vite
  # que les statistiques journalières calculées à la demande. On télécharge par
  # SEMESTRE (6 mois = plus grande granularité acceptée par le CDS), on
  # parallélise via wf_request_batch, et on sauvegarde après chaque lot
  # (points de reprise).
  print("Début du téléchargement concurrent ERA5-Land (horaire, par semestre)...")
  zone <- c(max(villes$latitude) + 1, min(villes$longitude) - 1,
            min(villes$latitude) - 1, max(villes$longitude) + 1)
  annee_courante <- year(Sys.Date())
  heures_24 <- sprintf("%02d:00", 0:23)

  WORKERS    <- 6    # requêtes CDS simultanées (à ajuster selon ce que le serveur accepte)
  TAILLE_LOT <- 12   # unités (semestres) par lot (sauvegarde après chaque lot)

  # Liste des semestres ATTENDUS (1950 -> aujourd'hui), puis on ne retient que
  # ceux manquants ou incomplets : on compare les jours réellement présents dans
  # le RDS aux jours calendaires attendus de chaque semestre. Cela rebouche les
  # trous laissés par un éventuel échec passé (ex. un semestre non téléchargé)
  # et rafraîchit le semestre en cours.
  aujourd_hui <- Sys.Date()
  dates_presentes <- if (nrow(df_final_daily_max) > 0) unique(as.Date(df_final_daily_max$date)) else as.Date(character(0))

  unites <- list()
  for (an in 1950:annee_courante) {
    for (sem in 1:2) {
      mois_sem <- ((sem - 1) * 6 + 1):(sem * 6)
      d1 <- as.Date(sprintf("%d-%02d-01", an, mois_sem[1]))
      if (d1 > aujourd_hui) next                       # semestre pas encore commencé
      # Dernier jour attendu = fin du semestre, borné à aujourd'hui pour l'année en cours.
      d2 <- min(ceiling_date(as.Date(sprintf("%d-%02d-01", an, mois_sem[6])), "month") - days(1), aujourd_hui)
      dates_attendues <- seq(d1, d2, by = "day")
      if (sum(dates_attendues %in% dates_presentes) >= length(dates_attendues)) next  # déjà complet
      mois <- mois_sem[as.Date(sprintf("%d-%02d-01", an, mois_sem)) <= d2]  # mois à demander
      unites[[length(unites) + 1]] <- list(
        an = an, sem = sem, mois = mois, d1 = d1, d2 = d2,
        target = sprintf("era5_h_%d_S%d.nc", an, sem)  # cible UNIQUE par semestre
      )
    }
  }

  # On retire du RDS les jours couverts par les semestres à (re)télécharger
  # (semestre en cours partiel, trous à reboucher) pour éviter les doublons.
  if (length(unites) > 0 && nrow(df_final_daily_max) > 0) {
    a_retirer <- Reduce(`|`, lapply(unites, function(u)
      df_final_daily_max$date >= u$d1 & df_final_daily_max$date <= u$d2))
    df_final_daily_max <- df_final_daily_max[!a_retirer, , drop = FALSE]
  }

  if (length(unites) == 0) {
    print("Toutes les données attendues sont déjà présentes — rien à (re)télécharger.")
  } else if (length(unites) <= 20) {
    print(paste(length(unites), "semestre(s) à (re)télécharger :",
                paste(vapply(unites, function(u) paste0(u$an, "/S", u$sem), character(1)), collapse = ", ")))
  } else {
    print(paste0(length(unites), " semestre(s) à (re)télécharger (de ",
                 unites[[1]]$an, "/S", unites[[1]]$sem, " à ",
                 unites[[length(unites)]]$an, "/S", unites[[length(unites)]]$sem, ")."))
  }

  construire_requete <- function(u) {
    list(
      dataset_short_name = "reanalysis-era5-land",
      product_type = "reanalysis",
      variable = "2m_temperature",
      year = as.character(u$an),
      month = sprintf("%02d", u$mois),
      day = sprintf("%02d", 1:31),
      time = heures_24,
      data_format = "netcdf",
      download_format = "unarchived",
      area = zone,
      target = u$target
    )
  }

  agreger_unite <- function(u) {
    f  <- file.path(path_to_save, u$target)
    fz <- sub("\\.nc$", ".zip", f)
    if (!file.exists(f) && file.exists(fz)) {            # le CDS peut renvoyer un .zip
      unzip(fz, exdir = path_to_save)
      ex <- file.path(path_to_save, "data_0.nc")
      if (file.exists(ex)) file.rename(ex, f)
      file.remove(fz)
    }
    if (!file.exists(f)) { warning("NetCDF manquant : ", u$target); return(NULL) }
    res <- traiter_fichier_horaire(f, villes_avec_coords_grille)
    file.remove(f)
    res
  }

  lots <- split(unites, ceiling(seq_along(unites) / TAILLE_LOT))

  for (lot in lots) {
    etiquette <- paste0(lot[[1]]$an, "/S", lot[[1]]$sem, " -> ",
                        lot[[length(lot)]]$an, "/S", lot[[length(lot)]]$sem)
    print(paste0("Lot ", etiquette, " (", length(lot), " requêtes, workers = ", WORKERS, ")..."))
    requetes <- lapply(lot, construire_requete)

    # On n'interrompt pas tout le run si un lot échoue partiellement : on agrège
    # ensuite ce qui a effectivement été téléchargé.
    tryCatch(
      wf_request_batch(request_list = requetes, workers = WORKERS,
                       user = "maxp17.mp@gmail.com", path = path_to_save, time_out = 7200),
      error = function(e) warning("Échec (partiel ?) du lot ", etiquette, " : ", conditionMessage(e))
    )

    df_lot <- bind_rows(lapply(lot, agreger_unite))
    if (nrow(df_lot) > 0) {
      # Garde-fou de plausibilité : un lot couvrant des mois d'été doit présenter
      # des maxima nettement positifs. Des valeurs toutes ~0 trahiraient une
      # mauvaise variable lue (cf. number/expver) ou une conversion ratée.
      if (max(df_lot$temperature_max, na.rm = TRUE) < 10) {
        warning("Valeurs suspectes pour le lot ", etiquette,
                " (max = ", round(max(df_lot$temperature_max, na.rm = TRUE), 1),
                " °C) : vérifier la variable lue dans le NetCDF.")
      }
      df_final_daily_max <- bind_rows(df_final_daily_max, df_lot)
      saveRDS(df_final_daily_max, full_path_final)
      print(paste0("  Lot sauvegardé (", nrow(df_lot), " lignes ville-jour, max ",
                   round(max(df_lot$temperature_max, na.rm = TRUE), 1), " °C). Total cumulé : ",
                   nrow(df_final_daily_max)))
    } else {
      warning("Aucune donnée agrégée pour le lot ", etiquette)
    }
  }

  print("Traitement terminé. Le fichier final est à jour.")
  return(df_final_daily_max)
}

# --------------------
# 3. Exécution du script
# --------------------
# On n'exécute le téléchargement que si le script est lancé directement
# (Rscript). En cas de `source()` (scripts de vérification), on ne récupère que
# les fonctions, sans rien télécharger.
if (sys.nframe() == 0L) {
  wf_set_key(key = Sys.getenv("KEY_CDS"), user = "maxp17.mp@gmail.com")
  recuperer_donnees_era5()
}
