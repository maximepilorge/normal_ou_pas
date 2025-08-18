# Librairies
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ecmwfr) 
library(dplyr) 
library(tidyr) 
library(lubridate) 
library(ncdf4) 
library(sf) 
library(here)
library(plotly)

# A travailler :
# Module analyse : pourquoi ça regarde ce qui est inférieur ou égale en-dessous d'une certaine température ? Forcer le supérieur ou égal
# Découper l'application en plusieurs modules pour améliorer la lisibilité du code
# Réduire nombre décimales infobulle dans onglet Explorateur

dirApp <- Sys.getenv("DIR_APP")
key_cds <- Sys.getenv("KEY_CDS")

villes <- tibble::tribble(
  ~ville, ~latitude, ~longitude,
  "Paris", 48.8566, 2.3522,
  "Marseille", 43.2965, 5.3698,
  "Lyon", 45.7640, 4.8357,
  "Toulouse", 43.6047, 1.4442,
  "Nice", 43.7102, 7.2620,
  "Nantes", 47.2184, -1.5536,
  "Strasbourg", 48.5833, 7.7458,
  "Montpellier", 43.6108, 3.8767,
  "Bordeaux", 44.8378, -0.5792,
  "Lille", 50.6292, 3.0573,
  "Rennes", 48.1173, -1.6778,
  "Reims", 49.2583, 4.0317,
  "Le Havre", 49.4944, 0.1079,
  "Saint-Étienne", 45.4397, 4.3872,
  "Toulon", 43.1242, 5.9280,
  "Angers", 47.4784, -0.5632,
  "Dijon", 47.3220, 5.0415,
  "Brest", 48.3904, -4.4869,
  "Clermont-Ferrand", 45.7772, 3.0870,
  "Limoges", 45.8336, 1.2611,
  "Tours", 47.3941, 0.6849,
  "Amiens", 49.8941, 2.2958,
  "Metz", 49.1193, 6.1757,
  "Besançon", 47.2378, 6.0240,
  "Perpignan", 42.6887, 2.8948,
  "La Rochelle", 46.1603, -1.1511,
  "Avignon", 43.9493, 4.8068,
  "Carcassonne", 43.2105, 2.3486,
  "Poitiers", 46.5802, 0.3405,
  "Ajaccio", 41.9207, 8.7397
)