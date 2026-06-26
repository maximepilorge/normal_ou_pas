# utils/share_api.R
#
# Sidecar plumber FACULTATIF servant l'aperçu social (Open Graph dynamique) des
# résultats de quiz. Sans état : tous les paramètres sont passés dans l'URL, et
# l'image est régénérée à la volée (aucun stockage).
#
#   GET /s?ville=&date=&temp=&normale=&periode=&cat=   -> page HTML avec meta OG
#   GET /s/img.png?<mêmes params>                       -> image PNG 1200×630
#
# NON câblé dans le CMD du Dockerfile (pour ne pas risquer le déploiement Shiny
# existant). Pour l'activer en production, voir DEPLOY_PARTAGE.md :
#   - lancer ce service sur un port interne (ex. 8000) en parallèle de Shiny ;
#   - router /s et /s/img.png vers ce port (reverse proxy / règle Render) ;
#   - le bouton « Partager » de l'app peut alors pointer vers /s?<params>.
#
# Lancement local :
#   Rscript -e "plumber::pr_run(plumber::pr('utils/share_api.R'), port = 8000)"

library(plumber)
source(here::here("utils", "render_partage.R"))

BASE_URL <- Sys.getenv("PUBLIC_BASE_URL", "https://normal-ou-pas.com")

# Construit la liste de paramètres attendue par le générateur d'image à partir
# des paramètres de requête (avec valeurs de repli sûres).
.parse_params <- function(query) {
  list(
    ville = query$ville %||% "—",
    date = tryCatch(as.Date(query$date %||% Sys.Date()), error = function(e) Sys.Date()),
    temp = suppressWarnings(as.numeric(query$temp %||% 0)),
    normale_moy = suppressWarnings(as.numeric(query$normale %||% 0)),
    periode_ref = query$periode %||% "",
    categorie = query$cat %||% "Dans les normales de saison",
    juste = { v <- query$juste; if (is.null(v)) NA else isTRUE(v %in% c("1", "true", "TRUE", "oui")) }
  )
}

# Encode les paramètres pour les réinjecter dans l'URL de l'image.
.querystring <- function(query) {
  champs <- c("ville", "date", "temp", "normale", "periode", "cat", "juste")
  morceaux <- vapply(champs, function(k) {
    v <- query[[k]]
    if (is.null(v)) "" else paste0(k, "=", utils::URLencode(as.character(v), reserved = TRUE))
  }, character(1))
  paste(morceaux[morceaux != ""], collapse = "&")
}

#* Page d'aperçu social avec meta Open Graph dynamiques.
#* @get /s
#* @serializer html
function(req) {
  p <- .parse_params(req$args)
  ecart <- round(p$temp - p$normale_moy, 1)
  sens <- if (ecart > 0) "au-dessus" else if (ecart < 0) "en-dessous" else "dans"
  titre <- paste0(p$temp, "°C ", autour_de(p$ville), " — ", sprintf("%+.1f", ecart),
                  "°C ", sens, " de la normale")
  desc <- paste0("Normale ", p$periode_ref, " : ", p$normale_moy,
                 "°C. Et vous, sauriez-vous situer ce qui est normal ?")
  img <- paste0(BASE_URL, "/s/img.png?", .querystring(req$args))

  sprintf('<!DOCTYPE html><html lang="fr"><head>
<meta charset="utf-8">
<meta property="og:title" content="%s">
<meta property="og:description" content="%s">
<meta property="og:image" content="%s">
<meta property="og:type" content="website">
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="%s">
<meta name="twitter:image" content="%s">
<meta http-equiv="refresh" content="0; url=%s">
<title>%s</title></head>
<body><p>Redirection vers <a href="%s">Climat : Normal ou pas ?</a></p></body></html>',
    titre, desc, img, titre, img, BASE_URL, titre, BASE_URL)
}

#* Image PNG 1200×630 régénérée à partir des paramètres de l'URL.
#* @get /s/img.png
#* @serializer contentType list(type="image/png")
function(req) {
  p <- .parse_params(req$args)
  f <- tempfile(fileext = ".png")
  sauver_carte_partage(p, f)
  on.exit(unlink(f), add = TRUE)
  readBin(f, "raw", n = file.info(f)$size)
}
