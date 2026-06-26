# telecharger_drias.R
# -----------------------------------------------------------------------------
# Récupération + extraction des projections climatiques DRIAS/TRACC.
# SCRIPT DISTINCT de telecharger_data.R (ERA5-Land) : toute source AUTRE
# qu'ERA5-Land a son propre script de téléchargement (cf. docs/plan_projections_tracc.md §6).
#
# ⚠️ ACCÈS AUX DONNÉES DRIAS — étape MANUELLE préalable
# -----------------------------------------------------------------------------
# Contrairement à ERA5 (API CDS via {ecmwfr}), DRIAS n'expose PAS d'API
# clé-en-main. Les fichiers s'obtiennent via l'« Espace de commande » web :
#   https://www.drias-climat.fr/  ->  Espace « Données et Produits »
#
# Recette de commande pour le prototype (à adapter une fois la doc TRACC figée) :
#   - Jeu          : Explore2 — données CORRIGÉES (ADAMONT)
#   - Variables    : tasmaxAdjust (tmax) ET tasminAdjust (tmin)  [noms « Adjust »]
#   - Pas de temps : QUOTIDIEN
#   - Scénario     : RCP8.5 (réservoir de simulations pour l'approche par niveau
#                    de réchauffement — PAS « le pire cas », cf. plan §3)
#   - Zone         : France métropolitaine (ou une emprise réduite autour de la
#                    ville pour le prototype, pour limiter la volumétrie)
#   - Périodes     : (a) référence historique 1976-2005 ; (b) fenêtres des
#                    niveaux +2,7 °C et +4,0 °C (bornes exactes à confirmer)
#
# Déposer les .nc obtenus dans  data/drias_raw/  puis lancer ce script :
#   Rscript utils/telecharger_drias.R
# -----------------------------------------------------------------------------

library(ncdf4)
library(dplyr)
library(tibble)
library(here)

# villes_insee (table villes + lat/lon + INSEE). `source` ne déclenche pas le
# bloc autonome du fichier (garde sys.nframe()).
source(here::here("utils", "definir_mailles_communes.R"))

DRIAS_RAW_DIR <- here::here("data", "drias_raw")

# --- Décodage de l'axe temporel (mutualisé avec la logique ERA5) -------------
# Trouve la dimension dont l'unité contient "since" et convertit en Date.
decoder_temps_nc <- function(nc) {
  noms_time <- names(nc$dim)[vapply(nc$dim, function(d)
    isTRUE(grepl("since", tolower(paste0(d$units, "")))), logical(1))]
  if (length(noms_time) == 0) stop("Aucune dimension temporelle ('... since ...') trouvée.")
  td <- nc$dim[[noms_time[1]]]
  parts <- strsplit(td$units, " since ")[[1]]
  facteur <- switch(tolower(trimws(parts[1])),
                    "seconds" = 1, "minutes" = 60, "hours" = 3600, "days" = 86400, 1)
  origine <- as.POSIXct(trimws(parts[2]), tz = "UTC")
  list(
    nom = noms_time[1],
    dates = as.Date(origine + as.numeric(td$vals) * facteur),
    n = length(td$vals)
  )
}

# --- Identification de la variable de données --------------------------------
# Indice : nom contenant tasmax/tasmin (ev. suffixe "Adjust"), sinon la variable
# au plus grand nombre de dimensions (champ spatial×temps), comme pour ERA5.
identifier_var_donnees <- function(nc, indice = NULL) {
  noms <- names(nc$var)
  if (!is.null(indice)) {
    cand <- noms[grepl(indice, noms, ignore.case = TRUE)]
    if (length(cand) >= 1) return(cand[1])
  }
  noms[which.max(vapply(nc$var, function(v) length(v$dim), integer(1)))]
}

# --- INSPECTEUR : à lancer EN PREMIER sur un vrai fichier DRIAS ---------------
# Affiche dimensions, variables, coordonnées et axe temporel pour adapter, si
# besoin, le localisateur de point ci-dessous au layout réel.
inspecter_drias_nc <- function(nc_path) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)
  cat("== Fichier :", basename(nc_path), "==\n\n")
  cat("Dimensions :\n")
  for (d in nc$dim) cat(sprintf("  - %-12s n=%-6d units=%s\n", d$name, d$len, d$units))
  cat("\nVariables :\n")
  for (v in nc$var) cat(sprintf("  - %-16s dims=[%s] units=%s\n",
                                v$name, paste(vapply(v$dim, `[[`, "", "name"), collapse = ","),
                                tryCatch(ncatt_get(nc, v$name, "units")$value, error = function(e) "")))
  tps <- tryCatch(decoder_temps_nc(nc), error = function(e) NULL)
  if (!is.null(tps)) cat(sprintf("\nTemps : %d pas, de %s à %s\n",
                                 tps$n, min(tps$dates), max(tps$dates)))
  invisible(NULL)
}

# --- Localisation du point de grille le plus proche d'une ville --------------
# Gère les trois layouts plausibles des NetCDF DRIAS :
#   (A) grille régulière : dims lon/lat 1D  -> indices (i, j)
#   (B) grille curvilinéaire : lat/lon en variables 2D (dims x,y) -> indices (i, j)
#   (C) points aplatis : lat/lon 1D sur une dimension spatiale unique -> indice k
# Retourne une closure extraire(nom_var, n_temps) -> vecteur de la série au point.
# Construit l'extracteur : fige les dimensions spatiales aux indices voulus et
# lit la dimension restante (temps) en entier. Robuste à l'ordre des dimensions
# et à d'éventuelles dimensions supplémentaires.
faire_extracteur_drias <- function(nc, indices_fixes) {
  function(nom_var, nt) {
    dd <- nc$var[[nom_var]]$dim
    noms <- vapply(dd, `[[`, "", "name")
    start <- integer(length(noms)); count <- integer(length(noms))
    for (k in seq_along(noms)) {
      if (!is.null(indices_fixes[[noms[k]]])) {
        start[k] <- indices_fixes[[noms[k]]]; count[k] <- 1L
      } else {
        start[k] <- 1L; count[k] <- dd[[k]]$len
      }
    }
    as.numeric(ncvar_get(nc, nom_var, start = start, count = count))
  }
}

localiser_point_drias <- function(nc, lon_ville, lat_ville, quiet = FALSE) {
  dire <- function(...) if (!quiet) message(sprintf(...))
  noms_var <- names(nc$var)

  # On PRIORISE les variables lat/lon (vraies coordonnées géographiques) sur les
  # dimensions, qui peuvent être projetées (ex. SAFRAN : dims x/y en mètres).
  nom_lon_var <- intersect(c("lon", "longitude"), noms_var)[1]
  nom_lat_var <- intersect(c("lat", "latitude"), noms_var)[1]

  if (!is.na(nom_lon_var) && !is.na(nom_lat_var)) {
    lon2 <- ncvar_get(nc, nom_lon_var); lat2 <- ncvar_get(nc, nom_lat_var)
    dims_lon <- vapply(nc$var[[nom_lon_var]]$dim, `[[`, "", "name")
    dims_lat <- vapply(nc$var[[nom_lat_var]]$dim, `[[`, "", "name")

    # (B) Curvilinéaire : lat/lon en 2D (dims x,y projetées). Plus proche par
    # distance euclidienne sur la grille géographique.
    if (length(dim(lon2)) == 2) {
      idx <- which.min((lon2 - lon_ville)^2 + (lat2 - lat_ville)^2)
      ij <- arrayInd(idx, dim(lon2)); i <- ij[1]; j <- ij[2]
      dire("  Layout B (curvilinéaire) — point lon=%.3f lat=%.3f", lon2[i, j], lat2[i, j])
      return(list(lon = lon2[i, j], lat = lat2[i, j],
                  extraire = faire_extracteur_drias(nc, setNames(list(i, j), dims_lon))))
    }

    # (C) Points aplatis : lat et lon 1D sur la MÊME dimension spatiale unique.
    if (length(dims_lon) == 1 && identical(dims_lon, dims_lat)) {
      idx <- which.min((lon2 - lon_ville)^2 + (lat2 - lat_ville)^2)
      dire("  Layout C (points aplatis) — point lon=%.3f lat=%.3f", lon2[idx], lat2[idx])
      return(list(lon = lon2[idx], lat = lat2[idx],
                  extraire = faire_extracteur_drias(nc, setNames(list(idx), dims_lon))))
    }

    # (A) Régulier : lat et lon 1D sur des dimensions DIFFÉRENTES → grille.
    if (length(dims_lon) == 1 && length(dims_lat) == 1) {
      i <- which.min(abs(lon2 - lon_ville)); j <- which.min(abs(lat2 - lat_ville))
      dire("  Layout A (régulier) — point lon=%.3f lat=%.3f", lon2[i], lat2[j])
      return(list(lon = lon2[i], lat = lat2[j],
                  extraire = faire_extracteur_drias(nc, setNames(list(i, j), c(dims_lon, dims_lat)))))
    }
  }

  # (A bis) Pas de variables lat/lon : se rabattre sur des DIMENSIONS
  # géographiques 1D (cas grille régulière sans variables de coordonnées).
  noms_dim <- names(nc$dim)
  nl <- intersect(c("lon", "longitude"), noms_dim)[1]
  nb <- intersect(c("lat", "latitude"), noms_dim)[1]
  if (!is.na(nl) && !is.na(nb)) {
    lon <- as.numeric(nc$dim[[nl]]$vals); lat <- as.numeric(nc$dim[[nb]]$vals)
    i <- which.min(abs(lon - lon_ville)); j <- which.min(abs(lat - lat_ville))
    dire("  Layout A bis (dims géographiques) — point lon=%.3f lat=%.3f", lon[i], lat[j])
    return(list(lon = lon[i], lat = lat[j],
                extraire = faire_extracteur_drias(nc, setNames(list(i, j), c(nl, nb)))))
  }

  stop("Coordonnées lon/lat introuvables — lancer inspecter_drias_nc() et adapter localiser_point_drias().")
}

# --- Extraction de TOUTES les villes depuis 1 fichier (ouvert une seule fois) -
# Bien plus efficace que d'ouvrir le NetCDF par ville : on ne lit que le point
# de grille le plus proche de chaque ville (slab 1×1×temps), pas les 12 654 mailles.
# villes : data.frame ville/latitude/longitude. Retour : tibble ville/date/valeur.
extraire_villes_fichier <- function(nc_path, villes, indice_var) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)
  tps <- decoder_temps_nc(nc)
  nom_var <- identifier_var_donnees(nc, indice = indice_var)
  unites <- tryCatch(ncatt_get(nc, nom_var, "units")$value, error = function(e) "")
  kelvin <- is.character(unites) && grepl("^k", tolower(unites))

  bind_rows(lapply(seq_len(nrow(villes)), function(k) {
    loc <- localiser_point_drias(nc, villes$longitude[k], villes$latitude[k], quiet = TRUE)
    serie <- loc$extraire(nom_var, tps$n)
    if (kelvin) serie <- serie - 273.15
    tibble(ville = villes$ville[k], lon_grille = loc$lon, lat_grille = loc$lat,
           date = tps$dates, valeur = as.numeric(serie))
  })) %>% filter(is.finite(valeur))
}

# --- Extraction d'une série journalière au point ville, depuis 1 fichier -----
lire_serie_drias <- function(nc_path, lon_ville, lat_ville, indice_var) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)

  tps <- decoder_temps_nc(nc)
  nom_var <- identifier_var_donnees(nc, indice = indice_var)
  loc <- localiser_point_drias(nc, lon_ville, lat_ville)
  serie <- loc$extraire(nom_var, tps$n)

  unites <- tryCatch(ncatt_get(nc, nom_var, "units")$value, error = function(e) "")
  if (is.character(unites) && grepl("^k", tolower(unites))) serie <- serie - 273.15

  tibble(date = tps$dates, valeur = as.numeric(serie)) %>% filter(is.finite(valeur))
}

# --- Métadonnées d'un fichier DRIAS depuis son nom -----------------------------
# Nom type : <var>Adjust_France_<GCM>_<scenario>_<run>_<RCM>_<...>.nc
parser_meta_drias <- function(nc_path) {
  parts <- strsplit(basename(nc_path), "_")[[1]]
  list(
    variable   = sub("Adjust$", "", parts[1]),  # "tasmax" | "tasmin"
    gcm        = parts[3],
    scenario   = parts[4],                       # "historical" | "rcp85"
    rcm        = parts[6],
    simulation = paste(parts[3], parts[6], sep = "/")
  )
}

# --- Extraction multi-villes / multi-simulations -------------------------------
# Pour CHAQUE simulation, on concatène les fichiers historiques et rcp85 (simple
# empilement temporel — aucun chevauchement de dates) et on GARDE l'identité de
# chaque simulation. Indispensable pour calculer les indicateurs PAR simulation
# puis agréger en central + fourchette (cf. plan §3) : médianer les séries
# journalières ENTRE modèles lisserait les extrêmes (canicules).
# Retour : tibble ville / simulation / date / temperature_max / temperature_min.
extraire_villes_drias <- function(fichiers, villes) {
  longs <- bind_rows(lapply(fichiers, function(f) {
    m <- parser_meta_drias(f)
    extraire_villes_fichier(f, villes, indice_var = m$variable) %>%
      mutate(simulation = m$simulation, variable = m$variable)
  }))

  tmax <- longs %>% filter(variable == "tasmax") %>%
    transmute(ville, simulation, date, temperature_max = valeur)
  tmin <- longs %>% filter(variable == "tasmin") %>%
    transmute(ville, simulation, date, temperature_min = valeur)

  inner_join(tmax, tmin, by = c("ville", "simulation", "date")) %>%
    arrange(ville, simulation, date)
}

# =============================================================================
# Exécution autonome (Rscript) — extraction des 30 villes depuis les .nc France
# =============================================================================
if (sys.nframe() == 0L) {
  dir.create(DRIAS_RAW_DIR, showWarnings = FALSE, recursive = TRUE)
  fichiers <- list.files(DRIAS_RAW_DIR, pattern = "[.]nc$", full.names = TRUE)

  if (length(fichiers) == 0) {
    message("Aucun fichier .nc dans ", DRIAS_RAW_DIR, ".")
    message("→ Commande DRIAS manuelle requise (voir l'en-tête de ce script),")
    message("  puis déposer les .nc ici et relancer.")
    quit(save = "no", status = 0)
  }

  message("== INSPECTION du premier fichier ==")
  inspecter_drias_nc(fichiers[1])

  meta <- lapply(fichiers, parser_meta_drias)
  sims <- unique(vapply(meta, `[[`, "", "simulation"))
  message(sprintf("\n%d fichiers, %d simulation(s) : %s",
                  length(fichiers), length(sims), paste(sims, collapse = ", ")))

  message(sprintf("\n== EXTRACTION %d villes × %d simulations (hist + rcp85 concaténés) ==",
                  nrow(villes_insee), length(sims)))
  t0 <- Sys.time()
  series <- extraire_villes_drias(fichiers, villes_insee)
  message(sprintf("Terminé en %.0f s.", as.numeric(Sys.time() - t0, units = "secs")))

  out <- file.path(here::here("data"), "drias_villes_5sims.rds")
  saveRDS(series, out)

  # Contrôle qualité par simulation : couverture temporelle + extrêmes.
  qc <- series %>%
    group_by(simulation) %>%
    summarise(
      n_villes = dplyr::n_distinct(ville),
      debut    = min(date), fin = max(date),
      tmax_med = round(median(temperature_max), 1),
      tmax_max = round(max(temperature_max), 1),
      tmin_min = round(min(temperature_min), 1),
      .groups  = "drop"
    )
  message(sprintf("\nOK — %d villes × %d simulations. Sauvé : %s\n",
                  dplyr::n_distinct(series$ville), length(sims), out))
  print(as.data.frame(qc), row.names = FALSE)
}
