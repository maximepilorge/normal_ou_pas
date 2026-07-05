# utils/inspecter_ponderation.R
#
# INSPECTEUR DE PONDÉRATION — outil Shiny de contrôle qualité (hors app, hors
# Docker) : confirme visuellement que la température d'une ville est bien la
# moyenne pondérée par surface des mailles ERA5-Land recouvrant sa commune.
#
#   Rscript utils/inspecter_ponderation.R        # http://127.0.0.1:5001
#
# Pour une ville et un jour donnés :
#   - carte : contour communal + mailles colorées par température (extrêmes du
#     jour ou heure par heure), étiquetées M1..Mn avec leur poids ;
#   - tableau par maille : tmin/tmax du jour, surface de la maille, surface
#     intersectant la commune, poids recalculé ici vs poids du pipeline ;
#   - tableau horaire : température de chaque maille heure par heure (UTC,
#     la convention du pipeline) + moyenne pondérée de chaque heure ;
#   - panneau de vérification : tmax/tmin pondérées RECALCULÉES ICI, confrontées
#     à la fonction du pipeline (traiter_fichier_horaire), au RDS de staging
#     (era5_temperatures_france.rds) et à la BDD (temperatures_max).
#
# Rappel méthodo : le pipeline calcule d'abord les EXTRÊMES JOURNALIERS de
# chaque maille (max/min des 24 h UTC), puis leur moyenne pondérée — il ne
# moyenne pas les heures. La vue horaire sert à inspecter la matière première.
#
# Les NetCDF horaires étant supprimés après agrégation par le pipeline, l'outil
# re-télécharge le jour inspecté via le CDS (bbox de la commune, ~1-5 min) et le
# met en cache dans data/verif_mailles/ (chargements suivants instantanés).
# Prérequis : .Renviron (KEY_CDS ; DB_* facultatifs pour la comparaison BDD) et
# data/villes_et_mailles_associees.rds (l'association produite par le pipeline).

library(shiny)
library(leaflet)

# Fonctions du pipeline : villes_insee, construire_cellules_era5,
# charger_contour_commune (definir_mailles_communes.R, sourcé en cascade) et
# traiter_fichier_horaire (agrégation de référence). Les gardes sys.nframe()
# garantissent qu'aucun téléchargement ne part au source().
source(here::here("utils", "telecharger_data.R"))

CHEMIN_ASSOC  <- here::here("data", "villes_et_mailles_associees.rds")
CHEMIN_RDS    <- here::here("data", "era5_temperatures_france.rds")
DOSSIER_CACHE <- here::here("data", "verif_mailles")
dir.create(DOSSIER_CACHE, showWarnings = FALSE)

if (!file.exists(CHEMIN_ASSOC))
  stop("Association mailles/communes introuvable (", CHEMIN_ASSOC,
       ") — lancer d'abord le pipeline (telecharger_data.R).")
assoc_pipeline <- readRDS(CHEMIN_ASSOC)

# RDS de staging (facultatif : comparaison seulement).
staging <- if (file.exists(CHEMIN_RDS)) readRDS(CHEMIN_RDS) else NULL

# Connexion BDD facultative (comparaison au chiffre servi par l'app).
con_bdd <- tryCatch(
  DBI::dbConnect(RPostgres::Postgres(),
                 host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
                 dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASS")),
  error = function(e) NULL)
shiny::onStop(function() if (!is.null(con_bdd)) try(DBI::dbDisconnect(con_bdd), silent = TRUE))

# Clé CDS (facultative tant qu'on reste sur des jours déjà en cache).
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

# --- Lecture HORAIRE indépendante d'un NetCDF CDS ------------------------------
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

# --- Téléchargement CDS d'UN jour sur la bbox de la ville ----------------------
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

fmt1 <- function(x, dec = 3) ifelse(is.finite(x), format(round(x, dec), nsmall = dec, decimal.mark = ","), "—")

# =================================== UI ========================================
ui <- fluidPage(
  titlePanel("Inspecteur de pondération — mailles ERA5-Land par commune"),
  p(class = "text-muted", style = "max-width: 1100px;",
    "Contrôle qualité : la température d'une ville doit être la moyenne des mailles ",
    "ERA5-Land recouvrant sa commune, pondérée par la surface communale couverte. ",
    "Le pipeline moyenne les extrêmes JOURNALIERS de chaque maille (max/min des 24 h UTC), ",
    "pas les heures — la vue horaire sert à inspecter la matière première."),
  fluidRow(
    column(3,
      wellPanel(
        selectInput("ville", "Ville :", choices = sort(villes_insee$ville), selected = "Lyon"),
        dateInput("date", "Jour :", value = "2003-08-12",
                  min = "1950-01-01", max = Sys.Date() - 6,
                  format = "dd/mm/yyyy", language = "fr", weekstart = 1),
        uiOutput("etat_cache"),
        actionButton("charger_btn", "Charger les températures de ce jour",
                     class = "btn-primary", width = "100%"),
        radioButtons("mode", "Colorer les mailles par :",
                     choices = c("Tmax du jour" = "tmax", "Tmin du jour" = "tmin",
                                 "Température d'une heure" = "heure")),
        conditionalPanel("input.mode == 'heure'",
          sliderInput("heure", "Heure (UTC) :", min = 0, max = 23, value = 14, step = 1))
      ),
      uiOutput("panneau_verif")
    ),
    column(5,
      leafletOutput("carte", height = "560px"),
      p(class = "text-muted small",
        "Étiquettes : identifiant de maille · poids pipeline. Mailles grises : écartées ",
        "(part de surface < 2 %) ou sans donnée chargée.")
    ),
    column(4,
      h4(textOutput("titre_tableau")),
      tableOutput("tableau_mailles"),
      p(class = "text-muted small",
        "part brute = surface intersectée / total intersecté ; le poids recalculé écarte ",
        "les éclats < 2 % puis renormalise — mêmes règles que le pipeline.")
    )
  ),
  fluidRow(
    column(9, offset = 3,
      h4("Détail horaire (UTC) — température de chaque maille et moyenne pondérée"),
      tableOutput("tableau_horaire")
    )
  )
)

# ================================= SERVER ======================================
server <- function(input, output, session) {

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

  chemin_nc <- reactive({
    req(input$ville, input$date)
    v <- villes_insee[villes_insee$ville == input$ville, ]
    file.path(DOSSIER_CACHE,
              sprintf("era5_verif_%s_%s.nc", v$insee, format(input$date, "%Y%m%d")))
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
  observeEvent(list(input$ville, input$date), {
    if (file.exists(chemin_nc())) declencheur(declencheur() + 1)
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

  # Table maîtresse : géométrie + poids + extrêmes du jour.
  table_mailles <- reactive({
    g <- geo()
    df <- st_drop_geometry(g$cellules)
    h <- tryCatch(extremes(), error = function(e) NULL)
    if (!is.null(h)) df <- left_join(df, h, by = c("lon_grille", "lat_grille"))
    else { df$tmax_maille <- NA_real_; df$tmin_maille <- NA_real_ }
    df
  })

  # --- Vérification : moyennes pondérées confrontées aux quatre sources -------
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

    bdd <- if (!is.null(con_bdd)) tryCatch({
      b <- DBI::dbGetQuery(con_bdd, paste0(
        "SELECT temperature_max, temperature_min FROM temperatures_max ",
        "WHERE ville = $1 AND date = $2"), params = list(input$ville, input$date))
      list(tmax = if (nrow(b)) b$temperature_max[1] else NA_real_,
           tmin = if (nrow(b)) b$temperature_min[1] else NA_real_)
    }, error = function(e) list(tmax = NA_real_, tmin = NA_real_))
    else list(tmax = NA_real_, tmin = NA_real_)

    list(recalc = list(tmax = recalc_tmax, tmin = recalc_tmin),
         pipe = pipe, stag = stag, bdd = bdd)
  })

  output$panneau_verif <- renderUI({
    v <- tryCatch(verifs(), error = function(e) NULL)
    if (is.null(v))
      return(wellPanel(p(class = "text-muted mb-0",
                         "Chargez un jour pour lancer la vérification.")))
    ligne <- function(nom, val, ref) {
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
    bloc <- function(titre, champ) {
      ref <- v$recalc[[champ]]
      tags$table(class = "table table-sm mb-2",
        tags$thead(tags$tr(tags$th(titre), tags$th("Valeur"), tags$th("Écart"))),
        tags$tbody(
          ligne("Recalcul (cet outil)", ref, ref),
          ligne("Fonction pipeline", v$pipe[[champ]], ref),
          ligne("RDS de staging", v$stag[[champ]], ref),
          ligne("BDD (app)", v$bdd[[champ]], ref)
        ))
    }
    wellPanel(
      h4("Vérification des moyennes pondérées"),
      bloc("Tmax pondérée", "tmax"),
      bloc("Tmin pondérée", "tmin"),
      p(class = "text-muted small mb-0",
        "Écarts vs le recalcul indépendant ; ✔ si < 0,05 °C. RDS/BDD absents : « — »."))
  })

  # --- Carte -------------------------------------------------------------------
  output$carte <- renderLeaflet({
    g <- geo()
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
  })

  # --- Tableau par maille --------------------------------------------------------
  output$titre_tableau <- renderText({
    g <- geo()
    sprintf("%s — commune de %s km² · %d mailles retenues",
            input$ville, format(round(g$aire_commune_km2, 1), decimal.mark = ","),
            sum(st_drop_geometry(g$cellules)$retenue))
  })

  output$tableau_mailles <- renderTable({
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
  }, striped = TRUE, spacing = "xs", width = "100%")

  # --- Tableau horaire -----------------------------------------------------------
  output$tableau_horaire <- renderTable({
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
  }, striped = TRUE, spacing = "xs")
}

# --- Lancement autonome ---------------------------------------------------------
if (sys.nframe() == 0L) {
  message("Inspecteur de pondération : http://127.0.0.1:5001")
  runApp(shinyApp(ui, server), port = 5001L, host = "127.0.0.1", launch.browser = FALSE)
}
