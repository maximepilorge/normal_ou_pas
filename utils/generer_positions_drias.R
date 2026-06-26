# generer_positions_drias.R
# -----------------------------------------------------------------------------
# Génère la liste des positions des 30 villes au format attendu par l'« Espace
# de commande » DRIAS, option « Sélectionner automatiquement les points de grille
# les plus proches de positions connues » :
#
#     LambertX;LambertY;Commentaire        (X, Y en Lambert 93 / EPSG:2154, mètres)
#     ex. 569026;6281519;Toulouse-Blagnac
#
# DRIAS retient alors UN point de grille (8 km) par position → ~30 mailles au
# lieu des 8981 points terrestres de toute la France (volumétrie minimale).
#
# Sortie : data/positions_drias_lambert93.txt (à copier-coller dans le portail).
# -----------------------------------------------------------------------------

library(sf)
library(dplyr)
library(here)

# villes_insee (ville, latitude, longitude, insee). `source` n'exécute pas le
# bloc autonome du fichier.
source(here::here("utils", "definir_mailles_communes.R"))

# WGS84 (degrés) -> Lambert 93 (mètres).
pts <- villes_insee %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2154)

coords <- st_coordinates(pts)
positions <- pts %>%
  st_drop_geometry() %>%
  mutate(
    lambert_x = round(coords[, "X"]),
    lambert_y = round(coords[, "Y"]),
    # Commentaire = nom de ville sans séparateur « ; » (sécurité format).
    commentaire = gsub(";", " ", ville),
    ligne = paste(lambert_x, lambert_y, commentaire, sep = ";")
  )

chemin <- here::here("data", "positions_drias_lambert93.txt")
writeLines(positions$ligne, chemin)

cat("Positions DRIAS (Lambert 93, mètres) — ", nrow(positions), " villes\n\n", sep = "")
cat(positions$ligne, sep = "\n")
cat("\n\nFichier écrit :", chemin, "\n")
cat("→ Copier-coller son contenu dans l'option 2 de l'Espace de commande DRIAS.\n")
