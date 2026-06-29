# =================================================================
# DÉFINITION DES MAILLES ERA5-LAND PERTINENTES PAR VILLE
# =================================================================
# Au lieu de retenir la seule maille contenant le centroïde de la ville,
# on retient TOUTES les mailles ERA5-Land qui intersectent l'emprise
# communale, avec un poids = part de la surface communale couverte par
# chaque maille. La valeur quotidienne d'une ville est ensuite une moyenne
# spatiale pondérée des maxima journaliers de ces mailles (cf. telecharger_data.R).
#
# Source des contours communaux : API officielle geo.api.gouv.fr (par INSEE).
# Ce fichier expose des fonctions ; lancé seul (Rscript), il produit un
# aperçu + une carte de contrôle à partir d'une grille reconstruite à 0,1°.

library(sf)
library(dplyr)
library(tibble)
library(here)

sf_use_s2(FALSE)  # géométries planes lon/lat : suffisant pour l'intersection

# --- Table de référence villes + code INSEE (commune) ---
# Source de vérité unique : utils/villes_reference.R (partagée avec global.R).
source(here::here("utils", "villes_reference.R"))

# --- Construit les mailles ERA5 comme rectangles (pas°) centrés sur les
#     points de grille fournis. lon_grid / lat_grid doivent être les
#     coordonnées RÉELLES de la grille (issues du NetCDF) pour garantir la
#     jointure exacte avec les données téléchargées. ---
construire_cellules_era5 <- function(lon_grid, lat_grid, pas = 0.1) {
  demi <- pas / 2
  centres <- expand.grid(lon = lon_grid, lat = lat_grid)
  geoms <- lapply(seq_len(nrow(centres)), function(i) {
    x <- centres$lon[i]; y <- centres$lat[i]
    st_polygon(list(rbind(
      c(x - demi, y - demi), c(x + demi, y - demi),
      c(x + demi, y + demi), c(x - demi, y + demi),
      c(x - demi, y - demi)
    )))
  })
  st_sf(
    lon_grille = centres$lon,
    lat_grille = centres$lat,
    geometry = st_sfc(geoms, crs = 4326)
  )
}

# --- Récupère le contour d'une commune (GeoJSON) via geo.api.gouv.fr ---
charger_contour_commune <- function(insee, essais = 3, pause = 2) {
  url <- sprintf(
    "https://geo.api.gouv.fr/communes?code=%s&fields=contour&format=geojson&geometry=contour",
    insee
  )
  # Plusieurs tentatives : l'API peut échouer ponctuellement (réseau), et un
  # contour manquant ferait disparaître toute une ville du jeu de données.
  for (tentative in seq_len(essais)) {
    dst <- tempfile(fileext = ".geojson")
    ok <- tryCatch({ download.file(url, dst, quiet = TRUE); TRUE }, error = function(e) FALSE)
    commune <- if (ok) tryCatch(st_read(dst, quiet = TRUE), error = function(e) NULL) else NULL
    unlink(dst)
    if (!is.null(commune) && nrow(commune) > 0) return(st_make_valid(commune))
    if (tentative < essais) Sys.sleep(pause)
  }
  NULL
}

# --- Associe à chaque ville l'ensemble de ses mailles + poids surfacique ---
# seuil_poids : on écarte les mailles qui n'effleurent la commune que d'une
# part négligeable (slivers de coin), puis on renormalise les poids à 1.
associer_mailles_communes <- function(villes_insee, sf_cells, seuil_poids = 0.02) {
  res <- lapply(seq_len(nrow(villes_insee)), function(k) {
    v <- villes_insee[k, ]
    commune <- charger_contour_commune(v$insee)
    if (is.null(commune)) {
      warning("Contour introuvable pour ", v$ville, " (INSEE ", v$insee, ")")
      return(NULL)
    }
    emprise <- st_union(commune)
    touche <- st_intersects(sf_cells, emprise, sparse = FALSE)[, 1]
    hit <- sf_cells[touche, ]
    if (nrow(hit) == 0) {
      warning("Aucune maille pour ", v$ville)
      return(NULL)
    }
    inter <- st_intersection(hit, emprise)
    aire <- as.numeric(st_area(st_transform(inter, 2154)))  # surfaces en m² (Lambert-93)
    out <- hit %>%
      st_drop_geometry() %>%
      mutate(ville = v$ville, poids = aire / sum(aire)) %>%
      filter(poids >= seuil_poids) %>%        # on retire les éclats négligeables
      mutate(poids = poids / sum(poids)) %>%  # renormalisation
      select(ville, lon_grille, lat_grille, poids)
    out
  })
  assoc <- bind_rows(res)

  # Garde-fou : on refuse une association incomplète (une ville sans maille
  # produirait silencieusement un jeu de données amputé d'une ville entière).
  manquantes <- setdiff(villes_insee$ville, unique(assoc$ville))
  if (length(manquantes) > 0) {
    stop("Association incomplète — aucune maille pour : ", paste(manquantes, collapse = ", "),
         ". Vérifier la connexion à geo.api.gouv.fr puis relancer.")
  }
  assoc
}

# =================================================================
# Exécution autonome (Rscript) : aperçu + carte de contrôle
# (grille reconstruite à 0,1° — la pipeline, elle, utilise la grille réelle)
# =================================================================
if (sys.nframe() == 0L) {
  message("Construction de l'aperçu d'association (grille reconstruite 0,1°)...")
  lon_grid <- round(seq(-5.0, 9.5, by = 0.1), 1)
  lat_grid <- round(seq(41.0, 51.5, by = 0.1), 1)
  sf_cells <- construire_cellules_era5(lon_grid, lat_grid)

  assoc <- associer_mailles_communes(villes_insee, sf_cells)

  chemin <- here::here("data", "villes_et_mailles_associees_preview.rds")
  saveRDS(assoc, chemin)
  message("Aperçu sauvegardé : ", chemin)
  message("Mailles par ville :")
  print(as.data.frame(dplyr::count(assoc, ville, name = "nb_mailles")), row.names = FALSE)
}
