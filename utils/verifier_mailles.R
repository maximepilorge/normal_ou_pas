# =================================================================
# SCRIPT DE CONTRÔLE VISUEL DES MAILLES ERA5
# =================================================================

# --------------------
# 1. Chargement des librairies
# --------------------
library(leaflet)
library(sf)
library(dplyr)
library(htmlwidgets)

dirApp <- "C:/Users/maxp1/Documents/guess_climate"

# --------------------
# 2. Définition de la fonction de visualisation
# --------------------
# (C'est la même fonction que précédemment, incluse ici pour que le script soit autonome)
visualiser_mailles_villes_polygones <- function(villes_avec_grille, taille_maille_km = 9) {
  
  # Création des objets sf à partir des coordonnées initiales en WGS 84 (CRS 4326)
  sf_villes_visu <- st_as_sf(villes_avec_grille, coords = c("longitude", "latitude"), crs = 4326)
  sf_grilles_centres <- st_as_sf(villes_avec_grille, coords = c("lon_grille", "lat_grille"), crs = 4326)
  
  # --- Création des polygones pour les mailles avec la projection la plus adaptée ---
  
  # Calcul du demi-côté en mètres
  dist_buffer_m <- (taille_maille_km / 2) * 1000
  
  sf_mailles_polygones <- sf_grilles_centres %>%
    # 1. Transformer les coordonnées en Lambert-93 (EPSG:2154)
    st_transform(2154) %>%
    # 2. Créer un buffer carré de 4500m autour de chaque point
    st_buffer(dist = dist_buffer_m, endCapStyle = "SQUARE") %>%
    # 3. Re-transformer en WGS 84 (CRS 4326) car leaflet l'exige pour l'affichage
    st_transform(4326)
  
  
  # --- Création de la carte leaflet ---
  
  carte <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addCircleMarkers(data = sf_villes_visu, radius = 5, color = "blue",
                     fillOpacity = 0.8, stroke = TRUE, weight = 2,
                     label = ~paste("Ville :", ville), group = "Villes") %>%
    addPolygons(data = sf_mailles_polygones,
                color = "red", weight = 2, fillOpacity = 0.3,
                label = ~paste("Maille pour :", sf_mailles_polygones$ville),
                group = "Mailles ERA5",
                highlightOptions = highlightOptions(weight = 4, color = "white", bringToFront = TRUE)) %>%
    addPolylines(data = st_sfc(mapply(function(ville_geom, grille_geom) {
      st_linestring(rbind(st_coordinates(ville_geom), st_coordinates(grille_geom)))
    }, sf_villes_visu$geometry, sf_grilles_centres$geometry, SIMPLIFY = FALSE), crs = 4326),
    color = "grey", weight = 1.5, dashArray = "5,5", group = "Liaisons") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = c("Villes", "Mailles ERA5", "Liaisons"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend("bottomright", colors = c("blue", "red"), labels = c("Ville", "Maille de 9x9 km"),
              title = "Légende", opacity = 0.7)
  
  return(carte)
}

# --------------------
# 3. Exécution du script
# --------------------

# Définir le chemin vers le fichier de données
chemin_donnees <- file.path(dirApp, "data", "villes_et_mailles_associees.rds")

if (!file.exists(chemin_donnees)) {
  stop("Le fichier de données '", chemin_donnees, "' n'a pas été trouvé.")
}

donnees_villes_grille <- readRDS(chemin_donnees)
carte_finale <- visualiser_mailles_villes_polygones(donnees_villes_grille)

print(carte_finale)

chemin_sauvegarde_carte <- here("carte_controle_mailles_polygones_lambert93.html")
saveWidget(carte_finale, file = chemin_sauvegarde_carte, selfcontained = TRUE)
print(paste("Carte interactive sauvegardée dans :", chemin_sauvegarde_carte))