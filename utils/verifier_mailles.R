# =================================================================
# SCRIPT DE CONTRÔLE VISUEL DES MAILLES ERA5 (multi-mailles par ville)
# =================================================================
# Affiche, pour chaque ville, l'ensemble des mailles retenues (colorées par
# poids surfacique) et le contour de la commune, à partir du fichier
# d'association produit par la pipeline (ou de l'aperçu reconstruit).

library(leaflet)
library(sf)
library(dplyr)
library(htmlwidgets)
library(here)

# Fonctions + table villes_insee (le source ne lance pas le bloc autonome)
source(here::here("utils", "definir_mailles_communes.R"))

# --------------------
# 1. Chargement de l'association ville -> mailles
# --------------------
chemin_prod   <- here::here("data", "villes_et_mailles_associees.rds")
chemin_apercu <- here::here("data", "villes_et_mailles_associees_preview.rds")
lire <- function(p) if (file.exists(p)) readRDS(p) else NULL
a_prod <- lire(chemin_prod)
a_prev <- lire(chemin_apercu)

# On privilégie le fichier de production s'il a déjà le nouveau schéma (poids).
if (!is.null(a_prod) && "poids" %in% names(a_prod)) {
  assoc <- a_prod; src <- chemin_prod
} else if (!is.null(a_prev)) {
  assoc <- a_prev; src <- chemin_apercu
  if (!is.null(a_prod)) {
    message("NB : le fichier de production est encore au schéma mono-maille (sans 'poids') ; ",
            "utilisation de l'aperçu. Relancez telecharger_data.R pour le régénérer.")
  }
} else {
  stop("Aucun fichier d'association exploitable (colonne 'poids' absente partout). ",
       "Lancez d'abord utils/definir_mailles_communes.R ou la pipeline.")
}
message("Association utilisée : ", src)

# --------------------
# 2. Construction des géométries (mailles + contours communes)
# --------------------
# Polygones des mailles retenues, avec leur poids
sf_mailles <- construire_cellules_era5(
  sort(unique(assoc$lon_grille)),
  sort(unique(assoc$lat_grille))
) %>%
  inner_join(assoc, by = c("lon_grille", "lat_grille"))

# Contours communaux (un appel API par ville) + centroïdes
villes_presentes <- villes_insee %>% filter(ville %in% unique(assoc$ville))
contours <- do.call(rbind, lapply(seq_len(nrow(villes_presentes)), function(k) {
  v <- villes_presentes[k, ]
  cc <- charger_contour_commune(v$insee)
  if (is.null(cc)) return(NULL)
  st_sf(ville = v$ville, geometry = st_union(cc))
}))
sf_centroides <- st_as_sf(villes_presentes, coords = c("longitude", "latitude"), crs = 4326)

# Palette de couleur selon le poids de la maille
pal <- colorNumeric("Reds", domain = sf_mailles$poids)

# --------------------
# 3. Carte leaflet
# --------------------
carte <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addPolygons(data = sf_mailles,
              color = "red", weight = 1, fillColor = ~pal(poids), fillOpacity = 0.5,
              label = ~sprintf("%s — poids %.2f", ville, poids),
              group = "Mailles ERA5",
              highlightOptions = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
  addPolygons(data = contours,
              color = "black", weight = 2, fill = FALSE,
              label = ~paste("Commune :", ville),
              group = "Communes") %>%
  addCircleMarkers(data = sf_centroides, radius = 4, color = "blue",
                   fillOpacity = 0.9, stroke = FALSE,
                   label = ~paste("Centroïde :", ville), group = "Centroïdes") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Mailles ERA5", "Communes", "Centroïdes"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright", pal = pal, values = sf_mailles$poids,
            title = "Poids surfacique<br>de la maille", opacity = 0.7)

print(carte)

chemin_sauvegarde_carte <- here::here("carte_controle_mailles_communes.html")
saveWidget(carte, file = chemin_sauvegarde_carte, selfcontained = TRUE)
message("Carte interactive sauvegardée dans : ", chemin_sauvegarde_carte)
