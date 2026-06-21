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
# À garder synchronisée avec la liste de telecharger_data.R.
villes_insee <- tribble(
  ~ville, ~latitude, ~longitude, ~insee,
  "Paris",            48.8566,  2.3522, "75056",
  "Marseille",        43.2965,  5.3698, "13055",
  "Lyon",             45.7640,  4.8357, "69123",
  "Toulouse",         43.6047,  1.4442, "31555",
  "Nice",             43.7102,  7.2620, "06088",
  "Nantes",           47.2184, -1.5536, "44109",
  "Strasbourg",       48.5833,  7.7458, "67482",
  "Montpellier",      43.6108,  3.8767, "34172",
  "Bordeaux",         44.8378, -0.5792, "33063",
  "Lille",            50.6292,  3.0573, "59350",
  "Rennes",           48.1173, -1.6778, "35238",
  "Reims",            49.2583,  4.0317, "51454",
  "Le Havre",         49.4944,  0.1079, "76351",
  "Saint-Étienne",    45.4397,  4.3872, "42218",
  "Toulon",           43.1242,  5.9280, "83137",
  "Angers",           47.4784, -0.5632, "49007",
  "Dijon",            47.3220,  5.0415, "21231",
  "Brest",            48.3904, -4.4869, "29019",
  "Clermont-Ferrand", 45.7772,  3.0870, "63113",
  "Limoges",          45.8336,  1.2611, "87085",
  "Tours",            47.3941,  0.6849, "37261",
  "Amiens",           49.8941,  2.2958, "80021",
  "Metz",             49.1193,  6.1757, "57463",
  "Besançon",         47.2378,  6.0240, "25056",
  "Perpignan",        42.6887,  2.8948, "66136",
  "La Rochelle",      46.1603, -1.1511, "17300",
  "Avignon",          43.9493,  4.8068, "84007",
  "Carcassonne",      43.2105,  2.3486, "11069",
  "Poitiers",         46.5802,  0.3405, "86194",
  "Ajaccio",          41.9207,  8.7397, "2A004"
)

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
