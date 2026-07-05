# utils/inspecter_ponderation.R
#
# INSPECTEUR DE DONNÉES SOURCES — outil Shiny de contrôle qualité (hors app,
# hors Docker) : confirme visuellement que les températures d'une ville sont
# fidèles aux données brutes, pour les DEUX sources du projet.
#
#   Rscript utils/inspecter_ponderation.R        # http://127.0.0.1:5001
#
# Source « ERA5-Land (observé) » — moyenne pondérée par surface :
#   - carte : contour communal + mailles 0,1° colorées par température (extrêmes
#     du jour ou heure par heure UTC), étiquetées M1..Mn avec leur poids ;
#   - tableau par maille : tmin/tmax du jour, surface de la maille, surface
#     intersectant la commune, poids recalculé ici vs poids du pipeline ;
#   - tableau horaire : chaque maille heure par heure + moyenne pondérée ;
#   - vérification : tmax/tmin pondérées RECALCULÉES ICI, confrontées à la
#     fonction du pipeline (traiter_fichier_horaire), au RDS de staging
#     (era5_temperatures_france.rds) et à la BDD (temperatures_max).
#   Rappel méthodo : le pipeline moyenne les EXTRÊMES JOURNALIERS par maille
#   (max/min des 24 h UTC), pas les heures. Les NetCDF horaires étant supprimés
#   après agrégation, le jour inspecté est re-téléchargé via le CDS (bbox de la
#   commune, ~1-5 min) puis mis en cache dans data/verif_mailles/.
#
# Source « DRIAS (projections) » — POINT LE PLUS PROCHE (pas de pondération) :
#   - carte : contour communal + points de grille 8 km voisins (emprise ~8 km)
#     colorés par tasmax/tasmin du jour ; le point RETENU par le pipeline
#     (règle : minimum de (Δlon² + Δlat²) vs le centre-ville) est surligné ;
#   - tableau par point : valeur du jour, distance au centre-ville, retenu ou
#     non — signale aussi si le plus proche « en km » diffère du retenu ;
#   - série ±7 jours du point retenu : recalcul vs cache RDS (contrôle de
#     l'alignement des dates, calendriers de modèles) ;
#   - vérification : valeurs au point RECALCULÉES ICI, confrontées à la fonction
#     du pipeline (lire_serie_drias) et au cache data/drias_villes_5sims.rds.
#     Pas de ligne BDD : la base ne stocke que des agrégats DRIAS (deltas ±7 j,
#     canicules), pas de série journalière.
#
# Prérequis : .Renviron (KEY_CDS pour ERA5 ; DB_* facultatifs),
# data/villes_et_mailles_associees.rds, et pour DRIAS les .nc dans
# data/drias_raw/ (+ data/drias_villes_5sims.rds pour la comparaison cache).

library(shiny)
library(leaflet)

# Fonctions du pipeline : villes_insee, construire_cellules_era5,
# charger_contour_commune, traiter_fichier_horaire (ERA5) ; decoder_temps_nc,
# identifier_var_donnees, localiser_point_drias, lire_serie_drias,
# parser_meta_drias, DRIAS_RAW_DIR (DRIAS). Les gardes sys.nframe()
# garantissent qu'aucun téléchargement/extraction ne part au source().
source(here::here("utils", "telecharger_data.R"))
source(here::here("utils", "telecharger_drias.R"))

CHEMIN_ASSOC     <- here::here("data", "villes_et_mailles_associees.rds")
CHEMIN_RDS       <- here::here("data", "era5_temperatures_france.rds")
CHEMIN_DRIAS_RDS <- here::here("data", "drias_villes_5sims.rds")
DOSSIER_CACHE    <- here::here("data", "verif_mailles")
dir.create(DOSSIER_CACHE, showWarnings = FALSE)

if (!file.exists(CHEMIN_ASSOC))
  stop("Association mailles/communes introuvable (", CHEMIN_ASSOC,
       ") — lancer d'abord le pipeline (telecharger_data.R).")
assoc_pipeline <- readRDS(CHEMIN_ASSOC)

# RDS de staging ERA5 (facultatif : comparaison seulement).
staging <- if (file.exists(CHEMIN_RDS)) readRDS(CHEMIN_RDS) else NULL

# Catalogue des NetCDF DRIAS présents (métadonnées de nom de fichier, sans
# ouverture) : un fichier par (variable, simulation, scénario).
catalogue_drias <- {
  fichiers <- list.files(DRIAS_RAW_DIR, pattern = "[.]nc$", full.names = TRUE)
  if (length(fichiers) == 0) NULL else
    bind_rows(lapply(fichiers, function(f) {
      m <- parser_meta_drias(f)
      tibble(fichier = f, variable = m$variable, scenario = m$scenario,
             simulation = m$simulation)
    }))
}
drias_dispo <- !is.null(catalogue_drias)

# Cache DRIAS par ville (séries extraites par le pipeline) : ~260 Mo, chargé
# PARESSEUSEMENT à la première vérification DRIAS puis gardé en mémoire.
.env_drias <- new.env()
cache_drias_villes <- function() {
  if (is.null(.env_drias$series) && file.exists(CHEMIN_DRIAS_RDS))
    .env_drias$series <- readRDS(CHEMIN_DRIAS_RDS)
  .env_drias$series
}

# Connexion BDD facultative (comparaison au chiffre servi par l'app),
# PARESSEUSE et auto-réparante : (re)tentée à chaque vérification si absente ou
# invalide — la base peut être démarrée APRÈS l'outil (conteneur Docker) ou
# redémarrer en cours de session, sans qu'il faille relancer l'inspecteur.
.env_bdd <- new.env()
connexion_bdd <- function() {
  con <- .env_bdd$con
  if (!is.null(con) &&
      isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE)))
    return(con)
  .env_bdd$con <- tryCatch(
    DBI::dbConnect(RPostgres::Postgres(),
                   host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
                   dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
                   password = Sys.getenv("DB_PASS")),
    error = function(e) NULL)
  .env_bdd$con
}
shiny::onStop(function()
  if (!is.null(.env_bdd$con)) try(DBI::dbDisconnect(.env_bdd$con), silent = TRUE))

# Clé CDS (facultative tant qu'on reste sur des jours ERA5 déjà en cache).
cds_ok <- tryCatch({
  wf_set_key(key = Sys.getenv("KEY_CDS"), user = "maxp17.mp@gmail.com"); TRUE
}, error = function(e) FALSE)

# --- Géométrie d'une ville : contour, mailles candidates, surfaces, poids ------
# Reconstruit l'analyse surfacique DEPUIS ZÉRO (contour geo.api.gouv.fr, grille
# alignée au dixième de degré, intersections Lambert-93) pour la confronter aux
# poids du pipeline. Montre aussi les mailles ÉCARTÉES (part < 2 %) que le
# pipeline a filtrées. Contour mis en cache sur disque (l'API est lente).
analyser_geometrie_ville <- function(v) {
  cache_contour <- file.path(DOSSIER_CACHE, paste0("contour_", v$insee, ".rds"))
  emprise <- if (file.exists(cache_contour)) readRDS(cache_contour) else {
    commune <- charger_contour_commune(v$insee)
    if (is.null(commune)) stop("Contour introuvable pour ", v$ville)
    e <- st_union(st_make_valid(commune))
    saveRDS(e, cache_contour)
    e
  }

  # Grille candidate alignée aux dixièmes (la grille ERA5-Land l'est) autour de
  # la commune, avec une marge pour attraper les mailles qui l'effleurent.
  bb <- st_bbox(emprise)
  lon_grid <- round(seq(floor((bb["xmin"] - 0.15) * 10) / 10,
                        ceiling((bb["xmax"] + 0.15) * 10) / 10, by = 0.1), 2)
  lat_grid <- round(seq(floor((bb["ymin"] - 0.15) * 10) / 10,
                        ceiling((bb["ymax"] + 0.15) * 10) / 10, by = 0.1), 2)
  cellules <- construire_cellules_era5(lon_grid, lat_grid)

  touche <- st_intersects(cellules, emprise, sparse = FALSE)[, 1]
  cellules <- cellules[touche, ]
  inter <- suppressWarnings(st_intersection(cellules, emprise))
  aire_inter  <- as.numeric(st_area(st_transform(inter, 2154))) / 1e6      # km²
  aire_maille <- as.numeric(st_area(st_transform(cellules, 2154))) / 1e6   # km²

  # Poids : mêmes règles que le pipeline (part de la surface intersectée totale,
  # éclats < 2 % écartés, renormalisation) — cf. associer_mailles_communes().
  part_brute <- aire_inter / sum(aire_inter)
  retenue <- part_brute >= 0.02
  poids_recalc <- ifelse(retenue, part_brute / sum(part_brute[retenue]), NA_real_)

  poids_pipe <- assoc_pipeline %>%
    filter(ville == v$ville) %>%
    select(lon_grille, lat_grille, poids_pipeline = poids)

  cellules %>%
    mutate(aire_maille_km2 = aire_maille, aire_inter_km2 = aire_inter,
           part_brute = part_brute, retenue = retenue,
           poids_recalc = poids_recalc) %>%
    left_join(poids_pipe, by = c("lon_grille", "lat_grille")) %>%
    arrange(desc(part_brute)) %>%
    mutate(id = paste0("M", dplyr::row_number())) %>%
    list(cellules = ., emprise = emprise,
         aire_commune_km2 = as.numeric(st_area(st_transform(emprise, 2154))) / 1e6)
}

# --- Lecture HORAIRE indépendante d'un NetCDF CDS (ERA5) -----------------------
# Volontairement distincte de traiter_fichier_horaire (qu'on veut contrôler) :
# mêmes conventions (variable au plus de dimensions, Kelvin, axe « since »,
# coordonnées arrondies au centième) mais ré-implémentées. Renvoie une ligne
# par (maille, heure UTC) : lon_grille, lat_grille, heure, t (°C).
lire_horaires_nc <- function(nc_path, coords) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)
  nom_lon <- intersect(c("longitude", "lon"), names(nc$dim))[1]
  nom_lat <- intersect(c("latitude", "lat"), names(nc$dim))[1]
  lon <- round(as.numeric(ncvar_get(nc, nom_lon)), 2)
  lat <- round(as.numeric(ncvar_get(nc, nom_lat)), 2)
  noms_time <- names(nc$dim)[vapply(nc$dim, function(d)
    isTRUE(grepl("since", tolower(paste0(d$units, "")))), logical(1))]
  td <- nc$dim[[noms_time[1]]]
  parts <- strsplit(td$units, " since ")[[1]]
  facteur <- switch(tolower(trimws(parts[1])),
                    "seconds" = 1, "minutes" = 60, "hours" = 3600, "days" = 86400, 1)
  heures <- as.POSIXct(trimws(parts[2]), tz = "UTC") + as.numeric(td$vals) * facteur
  nt <- length(heures)
  nom_var <- names(nc$var)[which.max(vapply(nc$var, function(v) length(v$dim), integer(1)))]
  unites_var <- tryCatch(ncatt_get(nc, nom_var, "units")$value, error = function(e) "")
  kelvin <- is.character(unites_var) && grepl("^k", tolower(unites_var))

  bind_rows(lapply(seq_len(nrow(coords)), function(k) {
    i <- match(coords$lon_grille[k], lon)
    j <- match(coords$lat_grille[k], lat)
    if (is.na(i) || is.na(j)) return(NULL)
    serie <- as.numeric(ncvar_get(nc, nom_var, start = c(i, j, 1), count = c(1, 1, nt)))
    if (kelvin) serie <- serie - 273.15
    tibble(lon_grille = coords$lon_grille[k], lat_grille = coords$lat_grille[k],
           heure = heures, t = serie) %>% filter(is.finite(t))
  }))
}

# --- Téléchargement CDS d'UN jour ERA5 sur la bbox de la ville -----------------
telecharger_jour_cds <- function(cellules, date, cible) {
  if (!cds_ok) stop("Clé CDS indisponible (KEY_CDS dans .Renviron).")
  bb <- st_bbox(cellules)
  requete <- list(
    dataset_short_name = "reanalysis-era5-land",
    product_type = "reanalysis",
    variable = "2m_temperature",
    year = format(date, "%Y"), month = format(date, "%m"), day = format(date, "%d"),
    time = sprintf("%02d:00", 0:23),
    data_format = "netcdf", download_format = "unarchived",
    area = c(bb["ymax"] + 0.05, bb["xmin"] - 0.05, bb["ymin"] - 0.05, bb["xmax"] + 0.05),
    target = basename(cible)
  )
  wf_request(user = "maxp17.mp@gmail.com", request = requete,
             path = dirname(cible), transfer = TRUE, verbose = FALSE)
  # Le CDS renvoie parfois un zip malgré download_format.
  fz <- sub("\\.nc$", ".zip", cible)
  if (!file.exists(cible) && file.exists(fz)) {
    unzip(fz, exdir = dirname(cible))
    ex <- file.path(dirname(cible), "data_0.nc")
    if (file.exists(ex)) file.rename(ex, cible)
    file.remove(fz)
  }
  if (!file.exists(cible)) stop("NetCDF non récupéré depuis le CDS.")
  cible
}

# --- Voisinage DRIAS : points de grille autour de la ville, valeurs d'un jour --
# Lecture INDÉPENDANTE de localiser_point_drias (qu'on veut contrôler) : on
# énumère les points de la grille (layouts B curvilinéaire / C aplati / A
# régulier), on garde les voisins du centre-ville, on lit la valeur du jour de
# chacun (slab 1×1×1) et la série complète du point RETENU. Le point retenu
# applique la même règle que le pipeline : minimum de (Δlon² + Δlat²), en
# degrés. La distance en km (équirectangulaire) est fournie à titre de contrôle
# — si le plus proche « en km » diffère du retenu, l'outil le signale.
lire_voisinage_drias <- function(nc_path, lon_v, lat_v, date_cible,
                                 rayon_km = 15, n_min = 9) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)

  tps <- decoder_temps_nc(nc)
  it <- match(as.Date(date_cible), tps$dates)
  if (is.na(it)) return(list(erreur = sprintf(
    "Le %s est absent de ce fichier (couverture %s → %s).",
    format(as.Date(date_cible), "%d/%m/%Y"), min(tps$dates), max(tps$dates))))

  m <- parser_meta_drias(nc_path)
  nom_var <- identifier_var_donnees(nc, indice = m$variable)
  unites <- tryCatch(ncatt_get(nc, nom_var, "units")$value, error = function(e) "")
  kelvin <- is.character(unites) && grepl("^k", tolower(unites))
  nom_temps <- tps$nom

  # Énumération des points : lon/lat + indices à figer pour chaque point.
  noms_var <- names(nc$var)
  nom_lon <- intersect(c("lon", "longitude"), noms_var)[1]
  nom_lat <- intersect(c("lat", "latitude"), noms_var)[1]
  if (is.na(nom_lon) || is.na(nom_lat))
    stop("Variables lat/lon introuvables — lancer inspecter_drias_nc().")
  lon2 <- ncvar_get(nc, nom_lon); lat2 <- ncvar_get(nc, nom_lat)
  dims_lon <- vapply(nc$var[[nom_lon]]$dim, `[[`, "", "name")

  if (length(dim(lon2)) == 2) {                      # (B) curvilinéaire
    pts <- tibble(lon = as.numeric(lon2), lat = as.numeric(lat2),
                  i = rep(seq_len(nrow(lon2)), times = ncol(lon2)),
                  j = rep(seq_len(ncol(lon2)), each = nrow(lon2)))
    fixes <- function(p) setNames(list(p$i, p$j), dims_lon)
  } else if (length(dims_lon) == 1 &&
             identical(dims_lon, vapply(nc$var[[nom_lat]]$dim, `[[`, "", "name"))) {
    pts <- tibble(lon = as.numeric(lon2), lat = as.numeric(lat2),  # (C) aplati
                  i = seq_along(lon2), j = NA_integer_)
    fixes <- function(p) setNames(list(p$i), dims_lon)
  } else {                                           # (A) grille régulière
    pts <- tibble(lon = rep(as.numeric(lon2), times = length(lat2)),
                  lat = rep(as.numeric(lat2), each = length(lon2)),
                  i = rep(seq_along(lon2), times = length(lat2)),
                  j = rep(seq_along(lat2), each = length(lon2)))
    fixes <- function(p) setNames(list(p$i, p$j),
                                  c(dims_lon, vapply(nc$var[[nom_lat]]$dim, `[[`, "", "name")))
  }

  pts <- pts %>%
    mutate(dist2_deg = (lon - lon_v)^2 + (lat - lat_v)^2,
           dist_km = sqrt((111.320 * cos(lat_v * pi / 180) * (lon - lon_v))^2 +
                          (110.574 * (lat - lat_v))^2))
  # Voisinage : tous les points sous rayon_km, complétés aux n_min plus proches.
  garder <- pts$dist_km <= rayon_km
  garder[order(pts$dist_km)[seq_len(min(n_min, nrow(pts)))]] <- TRUE
  voisins <- pts[garder, ] %>% arrange(dist_km)
  voisins$retenu <- with(voisins, dist2_deg == min(pts$dist2_deg))

  # Valeur du jour de chaque voisin + série complète du point retenu.
  lire_point <- function(p, seulement_jour = TRUE) {
    ind <- fixes(p)
    if (seulement_jour) ind[[nom_temps]] <- it
    v <- faire_extracteur_drias(nc, ind)(nom_var, tps$n)
    if (kelvin) v <- v - 273.15
    v
  }
  voisins$valeur <- vapply(seq_len(nrow(voisins)),
                           function(k) lire_point(voisins[k, ])[1], numeric(1))
  p_ret <- voisins[voisins$retenu, ][1, ]
  serie_retenu <- tibble(date = tps$dates,
                         valeur = lire_point(p_ret, seulement_jour = FALSE))

  list(voisins = voisins, serie_retenu = serie_retenu,
       variable = m$variable, point_retenu = p_ret)
}

fmt1 <- function(x, dec = 3) ifelse(is.finite(x), format(round(x, dec), nsmall = dec, decimal.mark = ","), "—")

# =================================== UI ========================================
ui <- fluidPage(
  titlePanel("Inspecteur des données sources — ERA5-Land & DRIAS par commune"),
  p(class = "text-muted", style = "max-width: 1100px;",
    "Contrôle qualité des extractions par ville. ERA5-Land (observé) : moyenne des ",
    "mailles 0,1° recouvrant la commune, pondérée par la surface couverte — le ",
    "pipeline moyenne les extrêmes JOURNALIERS de chaque maille (max/min des 24 h ",
    "UTC). DRIAS (projections) : valeur du point de grille 8 km le plus proche du ",
    "centre-ville, sans pondération."),
  fluidRow(
    column(3,
      wellPanel(
        radioButtons("source_donnees", "Source :",
                     choices = c("ERA5-Land (observé)" = "era5",
                                 "DRIAS (projections)" = "drias"),
                     inline = TRUE),
        selectInput("ville", "Ville :", choices = sort(villes_insee$ville), selected = "Lyon"),
        dateInput("date", "Jour :", value = "2003-08-12",
                  min = "1950-01-01", max = Sys.Date() - 6,
                  format = "dd/mm/yyyy", language = "fr", weekstart = 1),
        conditionalPanel("input.source_donnees == 'era5'",
          uiOutput("etat_cache"),
          actionButton("charger_btn", "Charger les températures de ce jour",
                       class = "btn-primary", width = "100%"),
          radioButtons("mode", "Colorer les mailles par :",
                       choices = c("Tmax du jour" = "tmax", "Tmin du jour" = "tmin",
                                   "Température d'une heure" = "heure")),
          conditionalPanel("input.mode == 'heure'",
            sliderInput("heure", "Heure (UTC) :", min = 0, max = 23, value = 14, step = 1))
        ),
        conditionalPanel("input.source_donnees == 'drias'",
          selectInput("simulation", "Simulation (GCM/RCM) :", choices = NULL),
          radioButtons("mode_drias", "Colorer les points par :",
                       choices = c("Tasmax (tmax)" = "tasmax", "Tasmin (tmin)" = "tasmin")),
          p(class = "text-muted small",
            "Lecture locale des NetCDF de data/drias_raw/ — pas de téléchargement.")
        )
      ),
      uiOutput("panneau_verif")
    ),
    column(5,
      leafletOutput("carte", height = "560px"),
      uiOutput("legende_carte")
    ),
    column(4,
      h4(textOutput("titre_tableau")),
      tableOutput("tableau_mailles"),
      uiOutput("note_tableau")
    )
  ),
  fluidRow(
    column(9, offset = 3,
      uiOutput("titre_detail"),
      tableOutput("tableau_detail")
    )
  )
)

# ================================= SERVER ======================================
server <- function(input, output, session) {

  # --- Bascule de source : bornes du calendrier + simulations disponibles ------
  observeEvent(input$source_donnees, {
    if (input$source_donnees == "drias") {
      if (!drias_dispo)
        showNotification("Aucun NetCDF dans data/drias_raw/ — mode DRIAS indisponible.",
                         type = "error", duration = 8)
      sims <- sort(unique(catalogue_drias$simulation))
      updateSelectInput(session, "simulation", choices = sims, selected = sims[1])
      # Les projections couvrent 1950-2100 : on élargit, et on propose un jour
      # de plein réchauffement si la date courante est « observée ».
      updateDateInput(session, "date", min = "1950-01-01", max = "2100-12-31",
                      value = if (input$date <= Sys.Date()) as.Date("2085-08-11") else input$date)
    } else {
      updateDateInput(session, "date", min = "1950-01-01", max = Sys.Date() - 6,
                      value = if (input$date > Sys.Date()) as.Date("2003-08-12") else input$date)
    }
  })

  # Géométrie de la ville (contour + mailles + surfaces + poids), mémoïsée.
  cache_geo <- new.env()
  geo <- reactive({
    req(input$ville)
    if (is.null(cache_geo[[input$ville]])) {
      v <- villes_insee[villes_insee$ville == input$ville, ]
      cache_geo[[input$ville]] <- withProgress(
        analyser_geometrie_ville(v),
        message = paste("Contour et surfaces de", input$ville, "…"))
    }
    cache_geo[[input$ville]]
  })

  ville_ref <- reactive({
    req(input$ville)
    villes_insee[villes_insee$ville == input$ville, ]
  })

  # =========================== BRANCHE ERA5-LAND ================================

  chemin_nc <- reactive({
    req(input$ville, input$date)
    file.path(DOSSIER_CACHE,
              sprintf("era5_verif_%s_%s.nc", ville_ref()$insee, format(input$date, "%Y%m%d")))
  })

  output$etat_cache <- renderUI({
    if (file.exists(chemin_nc()))
      p(class = "text-success small", icon("check"), " Jour en cache : chargement instantané.")
    else
      p(class = "text-muted small", icon("cloud-arrow-down"),
        " Jour absent du cache : téléchargement CDS (~1-5 min).")
  })

  # Chargement des heures du jour : bouton explicite, ou automatique si le
  # NetCDF est déjà en cache (relecture locale instantanée).
  declencheur <- reactiveVal(0)
  observeEvent(input$charger_btn, declencheur(declencheur() + 1))
  observeEvent(list(input$ville, input$date, input$source_donnees), {
    if (input$source_donnees == "era5" && file.exists(chemin_nc()))
      declencheur(declencheur() + 1)
  })

  horaires <- eventReactive(declencheur(), {
    # declencheur vaut 0 tant que rien n'a demandé de chargement (pas de cache,
    # bouton pas encore cliqué) : on ne déclenche alors rien — surtout pas un
    # téléchargement CDS involontaire au démarrage.
    req(declencheur() > 0)
    g <- geo()
    cible <- chemin_nc()
    if (!file.exists(cible)) {
      withProgress(message = "Téléchargement CDS du jour (bbox de la commune)…",
                   value = 0.3,
                   tryCatch(telecharger_jour_cds(g$cellules, input$date, cible),
                            error = function(e) {
                              showNotification(paste("Téléchargement impossible :",
                                                     conditionMessage(e)),
                                               type = "error", duration = 10)
                              return(NULL)
                            }))
      if (!file.exists(cible)) return(NULL)
    }
    coords <- st_drop_geometry(g$cellules)[, c("lon_grille", "lat_grille")]
    lire_horaires_nc(cible, coords) %>%
      filter(as.Date(heure, tz = "UTC") == input$date)
  })

  # Extrêmes journaliers par maille (le recalcul indépendant de cet outil).
  extremes <- reactive({
    h <- horaires()
    req(!is.null(h), nrow(h) > 0)
    h %>%
      group_by(lon_grille, lat_grille) %>%
      summarise(tmax_maille = max(t), tmin_maille = min(t), .groups = "drop")
  })

  # Table maîtresse ERA5 : géométrie + poids + extrêmes du jour.
  table_mailles <- reactive({
    g <- geo()
    df <- st_drop_geometry(g$cellules)
    h <- tryCatch(extremes(), error = function(e) NULL)
    if (!is.null(h)) df <- left_join(df, h, by = c("lon_grille", "lat_grille"))
    else { df$tmax_maille <- NA_real_; df$tmin_maille <- NA_real_ }
    df
  })

  # =========================== BRANCHE DRIAS ====================================

  # Fichier DRIAS pour (simulation, variable, date) : historical jusqu'à 2005,
  # rcp85 ensuite (la concaténation du pipeline suit la même règle).
  fichier_drias <- function(variable) {
    req(drias_dispo, input$simulation)
    scen <- if (input$date <= as.Date("2005-12-31")) "historical" else "rcp85"
    f <- catalogue_drias %>%
      filter(simulation == input$simulation, variable == !!variable, scenario == scen)
    if (nrow(f) == 0) return(NULL)
    f$fichier[1]
  }

  # Voisinages du jour (tasmax ET tasmin, deux fichiers d'une même grille).
  voisinage <- reactive({
    req(input$source_donnees == "drias", input$ville, input$date, input$simulation)
    v <- ville_ref()
    lire_un <- function(variable) {
      f <- fichier_drias(variable)
      if (is.null(f)) return(list(erreur = paste0(
        "Aucun fichier ", variable, " pour cette simulation/période dans data/drias_raw/.")))
      lire_voisinage_drias(f, v$longitude, v$latitude, input$date)
    }
    withProgress(message = "Lecture des NetCDF DRIAS…", value = 0.4, {
      res <- list(tasmax = lire_un("tasmax"), tasmin = lire_un("tasmin"))
    })
    res
  })

  # Cache RDS du pipeline pour (ville, simulation), fenêtre ±7 j.
  cache_drias_fenetre <- reactive({
    req(input$source_donnees == "drias")
    series <- withProgress(cache_drias_villes(),
                           message = "Chargement du cache DRIAS (~260 Mo, une seule fois)…")
    if (is.null(series)) return(NULL)
    series %>%
      filter(ville == input$ville, simulation == input$simulation,
             date >= input$date - 7, date <= input$date + 7)
  })

  # --- Vérification DRIAS : point retenu vs pipeline vs cache RDS --------------
  verifs_drias <- reactive({
    vs <- voisinage()
    req(is.null(vs$tasmax$erreur), is.null(vs$tasmin$erreur))
    v <- ville_ref()

    recalc <- list(tmax = vs$tasmax$voisins$valeur[vs$tasmax$voisins$retenu][1],
                   tmin = vs$tasmin$voisins$valeur[vs$tasmin$voisins$retenu][1])

    # Fonction du pipeline (localiser_point_drias + extraction) sur les mêmes nc.
    pipe <- tryCatch({
      lire_un <- function(variable) {
        s <- lire_serie_drias(fichier_drias(variable), v$longitude, v$latitude, variable)
        s$valeur[s$date == input$date][1]
      }
      list(tmax = lire_un("tasmax"), tmin = lire_un("tasmin"))
    }, error = function(e) list(tmax = NA_real_, tmin = NA_real_))

    cache <- {
      cf <- cache_drias_fenetre()
      j <- if (is.null(cf)) NULL else cf[cf$date == input$date, ]
      list(tmax = if (!is.null(j) && nrow(j)) j$temperature_max[1] else NA_real_,
           tmin = if (!is.null(j) && nrow(j)) j$temperature_min[1] else NA_real_)
    }

    list(recalc = recalc, pipe = pipe, cache = cache)
  })

  # --- Vérification ERA5 : moyennes pondérées confrontées aux quatre sources ---
  verifs <- reactive({
    df <- table_mailles()
    req(any(is.finite(df$tmax_maille)))
    avec_poids <- df %>% filter(is.finite(poids_pipeline), is.finite(tmax_maille))
    recalc_tmax <- weighted.mean(avec_poids$tmax_maille, avec_poids$poids_pipeline)
    recalc_tmin <- weighted.mean(avec_poids$tmin_maille, avec_poids$poids_pipeline)

    # Fonction du pipeline sur le MÊME NetCDF (référence d'implémentation).
    pipe <- tryCatch({
      r <- traiter_fichier_horaire(chemin_nc(),
                                   assoc_pipeline %>% filter(ville == input$ville)) %>%
        filter(date == input$date)
      list(tmax = r$temperature_max[1], tmin = r$temperature_min[1])
    }, error = function(e) list(tmax = NA_real_, tmin = NA_real_))

    stag <- if (!is.null(staging)) {
      s <- staging %>% filter(ville == input$ville, date == input$date)
      list(tmax = if (nrow(s)) s$temperature_max[1] else NA_real_,
           tmin = if (nrow(s)) s$temperature_min[1] else NA_real_)
    } else list(tmax = NA_real_, tmin = NA_real_)

    con <- connexion_bdd()
    bdd_statut <- if (is.null(con)) "hors_ligne" else "ok"
    bdd <- if (!is.null(con)) tryCatch({
      b <- DBI::dbGetQuery(con, paste0(
        "SELECT temperature_max, temperature_min FROM temperatures_max ",
        "WHERE ville = $1 AND date = $2"), params = list(input$ville, input$date))
      if (nrow(b) == 0) bdd_statut <- "jour_absent"
      list(tmax = if (nrow(b)) b$temperature_max[1] else NA_real_,
           tmin = if (nrow(b)) b$temperature_min[1] else NA_real_)
    }, error = function(e) {
      # Connexion périmée (base redémarrée) : on la jette, le prochain passage
      # retentera une connexion fraîche.
      .env_bdd$con <- NULL
      bdd_statut <<- "hors_ligne"
      list(tmax = NA_real_, tmin = NA_real_)
    })
    else list(tmax = NA_real_, tmin = NA_real_)

    list(recalc = list(tmax = recalc_tmax, tmin = recalc_tmin),
         pipe = pipe, stag = stag, bdd = bdd, bdd_statut = bdd_statut)
  })

  # --- Panneau de vérification (les deux sources) ------------------------------
  ligne_verif <- function(nom, val, ref) {
    if (!is.finite(val)) return(tags$tr(tags$td(nom), tags$td("—"), tags$td("—")))
    ecart <- val - ref
    ok <- abs(ecart) < 0.05
    tags$tr(
      tags$td(nom),
      tags$td(paste0(fmt1(val), " °C")),
      tags$td(style = paste0("color:", if (ok) "#2E8B57" else "#C0392B", "; font-weight:600;"),
              paste0(if (ok) "✔ " else "✘ ", sprintf("%+.3f", ecart)))
    )
  }
  bloc_verif <- function(titre, lignes) {
    tags$table(class = "table table-sm mb-2",
      tags$thead(tags$tr(tags$th(titre), tags$th("Valeur"), tags$th("Écart"))),
      tags$tbody(lignes))
  }

  output$panneau_verif <- renderUI({
    if (input$source_donnees == "drias") {
      vs <- tryCatch(voisinage(), error = function(e) NULL)
      err <- c(vs$tasmax$erreur, vs$tasmin$erreur)
      if (!is.null(vs) && length(err) > 0)
        return(wellPanel(p(class = "small mb-0", style = "color:#C0392B;", err[1])))
      v <- tryCatch(verifs_drias(), error = function(e) NULL)
      if (is.null(v))
        return(wellPanel(p(class = "text-muted mb-0",
                           "Choisissez une simulation et un jour couverts par les NetCDF.")))
      bloc <- function(titre, champ) bloc_verif(titre, tagList(
        ligne_verif("Recalcul (cet outil)", v$recalc[[champ]], v$recalc[[champ]]),
        ligne_verif("Fonction pipeline", v$pipe[[champ]], v$recalc[[champ]]),
        ligne_verif("Cache RDS (villes)", v$cache[[champ]], v$recalc[[champ]])))
      wellPanel(
        h4("Vérification au point retenu"),
        bloc("Tasmax (tmax)", "tmax"),
        bloc("Tasmin (tmin)", "tmin"),
        p(class = "text-muted small mb-0",
          "Écarts vs le recalcul indépendant ; ✔ si < 0,05 °C. Pas de ligne BDD : ",
          "la base ne stocke que des agrégats DRIAS (deltas ±7 j, canicules)."))
    } else {
      v <- tryCatch(verifs(), error = function(e) NULL)
      if (is.null(v))
        return(wellPanel(p(class = "text-muted mb-0",
                           "Chargez un jour pour lancer la vérification.")))
      note_bdd <- switch(v$bdd_statut,
        hors_ligne = tags$p(class = "small mb-0", style = "color:#C0392B;",
          icon("database"),
          sprintf(" BDD injoignable (hôte %s) — démarrez la base puis recliquez « Charger ».",
                  Sys.getenv("DB_HOST"))),
        jour_absent = p(class = "text-muted small mb-0", icon("database"),
                        " BDD connectée, mais ce jour n'y figure pas."),
        NULL)
      bloc <- function(titre, champ) bloc_verif(titre, tagList(
        ligne_verif("Recalcul (cet outil)", v$recalc[[champ]], v$recalc[[champ]]),
        ligne_verif("Fonction pipeline", v$pipe[[champ]], v$recalc[[champ]]),
        ligne_verif("RDS de staging", v$stag[[champ]], v$recalc[[champ]]),
        ligne_verif("BDD (app)", v$bdd[[champ]], v$recalc[[champ]])))
      wellPanel(
        h4("Vérification des moyennes pondérées"),
        bloc("Tmax pondérée", "tmax"),
        bloc("Tmin pondérée", "tmin"),
        p(class = "text-muted small mb-1",
          "Écarts vs le recalcul indépendant ; ✔ si < 0,05 °C. Source absente : « — »."),
        note_bdd)
    }
  })

  # --- Carte (les deux sources) -------------------------------------------------
  output$carte <- renderLeaflet({
    g <- geo()

    if (input$source_donnees == "drias") {
      vs <- tryCatch(voisinage(), error = function(e) NULL)
      vv <- if (!is.null(vs) && is.null(vs[[input$mode_drias]]$erreur))
        vs[[input$mode_drias]]$voisins else NULL
      v <- ville_ref()

      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = g$emprise, fill = FALSE, color = "#1f4e9c", weight = 2.5) %>%
        addMarkers(lng = v$longitude, lat = v$latitude,
                   label = paste0("Centre-ville (référence du pipeline) : ",
                                  sprintf("%.4f, %.4f", v$longitude, v$latitude)))
      if (is.null(vv)) return(m)

      # Emprises ~8 km des points (carrés en Lambert-93, indicatifs).
      pts_sf <- st_as_sf(vv, coords = c("lon", "lat"), crs = 4326)
      carres <- st_buffer(st_transform(pts_sf, 2154), dist = 4000,
                          endCapStyle = "SQUARE") %>% st_transform(4326)
      pal <- colorNumeric("Spectral", domain = range(vv$valeur, na.rm = TRUE),
                          reverse = TRUE)
      infob <- sprintf(
        "<b>P%d</b> (%.3f, %.3f)<br/>%s : %s °C<br/>Distance : %.2f km%s",
        seq_len(nrow(vv)), vv$lon, vv$lat, input$mode_drias, fmt1(vv$valeur, 2),
        vv$dist_km, ifelse(vv$retenu, "<br/><b>POINT RETENU par le pipeline</b>", ""))

      m %>%
        addPolygons(data = carres, fillColor = pal(vv$valeur), fillOpacity = 0.5,
                    color = ifelse(vv$retenu, "#000000", "#666666"),
                    weight = ifelse(vv$retenu, 3, 1), popup = infob) %>%
        addLabelOnlyMarkers(lng = vv$lon, lat = vv$lat,
                            label = paste0("P", seq_len(nrow(vv)),
                                           ifelse(vv$retenu, " ★", "")),
                            labelOptions = labelOptions(
                              noHide = TRUE, direction = "center", textOnly = TRUE,
                              style = list("font-weight" = "700", "font-size" = "12px",
                                           "text-shadow" = "0 0 3px #ffffff"))) %>%
        addLegend("bottomright", pal = pal, values = vv$valeur, title = "°C", opacity = 0.8)
    } else {
      df <- table_mailles()
      cellules <- g$cellules
      cellules$tmax_maille <- df$tmax_maille
      cellules$tmin_maille <- df$tmin_maille

      valeur <- switch(input$mode,
        tmax = cellules$tmax_maille,
        tmin = cellules$tmin_maille,
        heure = {
          h <- tryCatch(horaires(), error = function(e) NULL)
          if (is.null(h)) rep(NA_real_, nrow(cellules)) else {
            hh <- h %>% filter(as.integer(format(heure, "%H", tz = "UTC")) == input$heure)
            st_drop_geometry(cellules) %>%
              left_join(hh, by = c("lon_grille", "lat_grille")) %>% pull(t)
          }
        })
      cellules$valeur <- valeur

      pal <- if (any(is.finite(valeur)))
        colorNumeric("Spectral", domain = range(valeur, na.rm = TRUE), reverse = TRUE)
      else function(x) "#b8bcc0"

      centres <- suppressWarnings(st_coordinates(st_centroid(cellules)))
      etiquettes <- ifelse(is.finite(cellules$poids_pipeline),
                           paste0(cellules$id, " · ", round(100 * cellules$poids_pipeline), " %"),
                           paste0(cellules$id, " · écartée"))
      infobulles <- sprintf(
        "<b>%s</b> (%.2f, %.2f)<br/>Poids pipeline : %s<br/>Tmax : %s °C · Tmin : %s °C<br/>Valeur affichée : %s °C",
        cellules$id, cellules$lon_grille, cellules$lat_grille,
        ifelse(is.finite(cellules$poids_pipeline),
               sprintf("%.1f %%", 100 * cellules$poids_pipeline), "écartée"),
        fmt1(cellules$tmax_maille, 2), fmt1(cellules$tmin_maille, 2), fmt1(cellules$valeur, 2))

      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = cellules,
                    fillColor = ~ifelse(is.finite(valeur), pal(valeur), "#b8bcc0"),
                    fillOpacity = 0.55, color = "#444444", weight = 1,
                    popup = infobulles) %>%
        addLabelOnlyMarkers(lng = centres[, 1], lat = centres[, 2],
                            label = etiquettes,
                            labelOptions = labelOptions(
                              noHide = TRUE, direction = "center", textOnly = TRUE,
                              style = list("font-weight" = "700", "font-size" = "12px",
                                           "text-shadow" = "0 0 3px #ffffff"))) %>%
        addPolygons(data = g$emprise, fill = FALSE, color = "#1f4e9c", weight = 2.5)
      if (any(is.finite(valeur)))
        m <- m %>% addLegend("bottomright", pal = pal,
                             values = valeur[is.finite(valeur)],
                             title = "°C", opacity = 0.8)
      m
    }
  })

  output$legende_carte <- renderUI({
    if (input$source_donnees == "drias")
      p(class = "text-muted small",
        "★ = point retenu (règle du pipeline : minimum de Δlon² + Δlat², en degrés). ",
        "Carrés ~8 km indicatifs autour des points de grille SAFRAN.")
    else
      p(class = "text-muted small",
        "Étiquettes : identifiant de maille · poids pipeline. Mailles grises : écartées ",
        "(part de surface < 2 %) ou sans donnée chargée.")
  })

  # --- Tableau principal (les deux sources) --------------------------------------
  output$titre_tableau <- renderText({
    g <- geo()
    if (input$source_donnees == "drias")
      sprintf("%s — points de grille DRIAS voisins (%s)", input$ville,
              if (is.null(input$simulation)) "" else input$simulation)
    else
      sprintf("%s — commune de %s km² · %d mailles retenues",
              input$ville, format(round(g$aire_commune_km2, 1), decimal.mark = ","),
              sum(st_drop_geometry(g$cellules)$retenue))
  })

  output$tableau_mailles <- renderTable({
    if (input$source_donnees == "drias") {
      vs <- tryCatch(voisinage(), error = function(e) NULL)
      if (is.null(vs) || !is.null(vs$tasmax$erreur) || !is.null(vs$tasmin$erreur)) return(NULL)
      vmax <- vs$tasmax$voisins
      vmin <- vs$tasmin$voisins %>% select(lon, lat, tasmin = valeur)
      d <- vmax %>%
        left_join(vmin, by = c("lon", "lat")) %>%
        arrange(dist_km)
      # Contrôle : le retenu (règle degrés²) est-il aussi le plus proche en km ?
      divergence <- which(d$retenu)[1] != 1
      tibble(
        Point = paste0("P", seq_len(nrow(d)), ifelse(d$retenu, " ★", "")),
        `Lon/Lat` = sprintf("%.3f / %.3f", d$lon, d$lat),
        `Distance (km)` = fmt1(d$dist_km, 2),
        `Tasmax (°C)` = fmt1(d$valeur, 2),
        `Tasmin (°C)` = fmt1(d$tasmin, 2),
        Statut = ifelse(d$retenu, "RETENU (pipeline)",
                        ifelse(seq_len(nrow(d)) == 1 & divergence,
                               "plus proche en km !", ""))
      )
    } else {
      df <- table_mailles()
      tot <- df %>% filter(is.finite(poids_pipeline))
      tibble(
        Maille = paste0(df$id, ifelse(df$retenue, "", " (écartée)")),
        `Lon/Lat` = sprintf("%.2f / %.2f", df$lon_grille, df$lat_grille),
        `Tmin (°C)` = fmt1(df$tmin_maille, 2),
        `Tmax (°C)` = fmt1(df$tmax_maille, 2),
        `Maille (km²)` = fmt1(df$aire_maille_km2, 1),
        `Intersection (km²)` = fmt1(df$aire_inter_km2, 2),
        `Part brute` = sprintf("%.1f %%", 100 * df$part_brute),
        `Poids recalculé` = ifelse(is.finite(df$poids_recalc),
                                   sprintf("%.4f", df$poids_recalc), "—"),
        `Poids pipeline` = ifelse(is.finite(df$poids_pipeline),
                                  sprintf("%.4f", df$poids_pipeline), "—")
      ) %>%
        bind_rows(tibble(
          Maille = "TOTAL (retenues)", `Lon/Lat` = "",
          `Tmin (°C)` = "", `Tmax (°C)` = "",
          `Maille (km²)` = "",
          `Intersection (km²)` = fmt1(sum(tot$aire_inter_km2), 2),
          `Part brute` = "",
          `Poids recalculé` = sprintf("%.4f", sum(tot$poids_recalc, na.rm = TRUE)),
          `Poids pipeline` = sprintf("%.4f", sum(tot$poids_pipeline, na.rm = TRUE))))
    }
  }, striped = TRUE, spacing = "xs", width = "100%")

  output$note_tableau <- renderUI({
    if (input$source_donnees == "drias")
      p(class = "text-muted small",
        "DRIAS n'utilise PAS de pondération surfacique : une seule valeur, celle du ",
        "point le plus proche du centre-ville (règle en degrés² du pipeline). Si le ",
        "classement en km divergeait, la colonne Statut le signalerait.")
    else
      p(class = "text-muted small",
        "part brute = surface intersectée / total intersecté ; le poids recalculé écarte ",
        "les éclats < 2 % puis renormalise — mêmes règles que le pipeline.")
  })

  # --- Tableau détail : horaire (ERA5) ou série ±7 j (DRIAS) ---------------------
  output$titre_detail <- renderUI({
    if (input$source_donnees == "drias")
      h4("Série ±7 jours au point retenu — recalcul vs cache RDS du pipeline")
    else
      h4("Détail horaire (UTC) — température de chaque maille et moyenne pondérée")
  })

  output$tableau_detail <- renderTable({
    if (input$source_donnees == "drias") {
      vs <- tryCatch(voisinage(), error = function(e) NULL)
      if (is.null(vs) || !is.null(vs$tasmax$erreur) || !is.null(vs$tasmin$erreur)) return(NULL)
      fen <- tibble(date = seq(input$date - 7, input$date + 7, by = "day"))
      recal <- fen %>%
        left_join(vs$tasmax$serie_retenu %>% rename(tmax_outil = valeur), by = "date") %>%
        left_join(vs$tasmin$serie_retenu %>% rename(tmin_outil = valeur), by = "date")
      cachef <- cache_drias_fenetre()
      if (!is.null(cachef))
        recal <- recal %>% left_join(
          cachef %>% select(date, tmax_cache = temperature_max, tmin_cache = temperature_min),
          by = "date")
      else { recal$tmax_cache <- NA_real_; recal$tmin_cache <- NA_real_ }
      tibble(
        Date = format(recal$date, "%d/%m/%Y"),
        `Tasmax outil` = fmt1(recal$tmax_outil, 2),
        `Tasmax cache` = fmt1(recal$tmax_cache, 2),
        `Écart tmax` = ifelse(is.finite(recal$tmax_outil) & is.finite(recal$tmax_cache),
                              sprintf("%+.3f", recal$tmax_outil - recal$tmax_cache), "—"),
        `Tasmin outil` = fmt1(recal$tmin_outil, 2),
        `Tasmin cache` = fmt1(recal$tmin_cache, 2),
        `Écart tmin` = ifelse(is.finite(recal$tmin_outil) & is.finite(recal$tmin_cache),
                              sprintf("%+.3f", recal$tmin_outil - recal$tmin_cache), "—")
      )
    } else {
      h <- tryCatch(horaires(), error = function(e) NULL)
      if (is.null(h) || nrow(h) == 0) return(NULL)
      g <- st_drop_geometry(geo()$cellules)
      h2 <- h %>%
        inner_join(g[, c("lon_grille", "lat_grille", "id", "poids_pipeline")],
                   by = c("lon_grille", "lat_grille")) %>%
        mutate(heure_txt = format(heure, "%H h", tz = "UTC"))
      large <- h2 %>%
        select(heure_txt, id, t) %>%
        tidyr::pivot_wider(names_from = id, values_from = t)
      moyennes <- h2 %>%
        filter(is.finite(poids_pipeline)) %>%
        group_by(heure_txt) %>%
        summarise(`Moyenne pondérée` = weighted.mean(t, poids_pipeline), .groups = "drop")
      large %>%
        left_join(moyennes, by = "heure_txt") %>%
        rename(`Heure (UTC)` = heure_txt) %>%
        mutate(across(-`Heure (UTC)`, ~ fmt1(.x, 2)))
    }
  }, striped = TRUE, spacing = "xs")
}

# --- Lancement autonome ---------------------------------------------------------
if (sys.nframe() == 0L) {
  message("Inspecteur des données sources : http://127.0.0.1:5001")
  runApp(shinyApp(ui, server), port = 5001L, host = "127.0.0.1", launch.browser = FALSE)
}
