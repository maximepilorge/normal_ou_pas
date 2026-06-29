# utils/villes_reference.R
#
# SOURCE DE VÉRITÉ unique de la liste des villes : nom, coordonnées WGS84 et code
# INSEE. Fichier volontairement « pur » (aucune dépendance lourde : ni sf, ni
# accès réseau) pour pouvoir être sourcé partout :
#   - definir_mailles_communes.R (association maille/commune via l'INSEE) ;
#   - global.R (carte de l'app : pastilles aux coordonnées WGS84), sans charger sf.
#
# Pour ajouter/retirer une ville, modifier UNIQUEMENT ce tableau.

library(tibble)

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
