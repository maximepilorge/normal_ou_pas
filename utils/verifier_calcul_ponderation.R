# =================================================================
# VÉRIFICATION DU CALCUL — moyenne pondérée par ville
# =================================================================
# Recalcule INDÉPENDAMMENT (chemin de code différent de la production) le maximum
# journalier pondéré d'une ville, à partir des données ERA5-Land brutes + des
# poids de l'association, et compare aux valeurs stockées dans le RDS.
#
# Principe :
#   1. on récupère les mailles + poids de la ville depuis l'association ;
#   2. on re-télécharge un petit échantillon (boîte autour de la ville) ;
#   3. pour chaque maille : max journalier = max des 24 valeurs horaires ;
#   4. moyenne pondérée des mailles = somme(poids_i * tmax_i) / somme(poids_i) ;
#   5. on compare au RDS pour les mêmes (ville, date).

VILLE <- "Lyon"
ANNEE <- 1955
SEM   <- 2            # 2 = juillet-décembre (contient l'été)

suppressMessages({ library(here); library(dplyr); library(ncdf4); library(ecmwfr); library(dotenv) })
load_dot_env(file = here::here(".Renviron"))
wf_set_key(key = Sys.getenv("KEY_CDS"), user = "maxp17.mp@gmail.com")
path_to_save <- here::here("data")

# --- 1. Mailles + poids de la ville ---
assoc <- readRDS(file.path(path_to_save, "villes_et_mailles_associees.rds"))
cells <- assoc %>% filter(ville == VILLE)
if (nrow(cells) == 0) stop("Ville absente de l'association : ", VILLE)
cat("Mailles de", VILLE, ":\n"); print(as.data.frame(cells %>% mutate(poids = round(poids, 3))))
cat("Somme des poids :", round(sum(cells$poids), 4), "\n\n")

# --- 2. Petit téléchargement autour de la ville ---
marge <- 0.2
zone <- c(max(cells$lat_grille) + marge, min(cells$lon_grille) - marge,
          min(cells$lat_grille) - marge, max(cells$lon_grille) + marge)  # N, O, S, E
mois <- ((SEM - 1) * 6 + 1):(SEM * 6)
cible <- sprintf("verif_calc_%s_%d_S%d.nc", VILLE, ANNEE, SEM)
full_nc <- file.path(path_to_save, cible)
invisible(file.remove(Sys.glob(file.path(path_to_save, "verif_calc_*"))))

req <- list(
  dataset_short_name = "reanalysis-era5-land", product_type = "reanalysis",
  variable = "2m_temperature", year = as.character(ANNEE),
  month = sprintf("%02d", mois), day = sprintf("%02d", 1:31),
  time = sprintf("%02d:00", 0:23), data_format = "netcdf",
  download_format = "unarchived", area = zone, target = cible
)
message("Téléchargement (petit échantillon autour de ", VILLE, ")...")
wf_request(user = "maxp17.mp@gmail.com", request = req, path = path_to_save,
           transfer = TRUE, verbose = TRUE)
fz <- sub("\\.nc$", ".zip", full_nc)
if (!file.exists(full_nc) && file.exists(fz)) {
  unzip(fz, exdir = path_to_save); ex <- file.path(path_to_save, "data_0.nc")
  if (file.exists(ex)) file.rename(ex, full_nc); file.remove(fz)
}
if (!file.exists(full_nc)) stop("Téléchargement échoué.")

# --- 3. Recalcul INDÉPENDANT (lecture du tableau complet + indexation manuelle) ---
nc <- nc_open(full_nc)
lon <- round(as.numeric(ncvar_get(nc, intersect(c("longitude","lon"), names(nc$dim))[1])), 2)
lat <- round(as.numeric(ncvar_get(nc, intersect(c("latitude","lat"), names(nc$dim))[1])), 2)
ntime_name <- names(nc$dim)[vapply(nc$dim, function(d) isTRUE(grepl("since", tolower(paste0(d$units,"")))), logical(1))][1]
td <- nc$dim[[ntime_name]]; parts <- strsplit(td$units, " since ")[[1]]
fac <- switch(tolower(trimws(parts[1])), "seconds"=1,"minutes"=60,"hours"=3600,"days"=86400,1)
dts <- as.Date(as.POSIXct(trimws(parts[2]), tz="UTC") + as.numeric(td$vals) * fac)
nom_var <- names(nc$var)[which.max(vapply(nc$var, function(v) length(v$dim), integer(1)))]
arr <- ncvar_get(nc, nom_var)   # [lon, lat, temps]
kelvin <- grepl("^k", tolower(paste0(ncatt_get(nc, nom_var, "units")$value, "")))
nc_close(nc)

# max journalier par maille (indexation manuelle arr[i,j,])
maxj_par_maille <- lapply(seq_len(nrow(cells)), function(k) {
  i <- which(lon == cells$lon_grille[k]); j <- which(lat == cells$lat_grille[k])
  serie <- arr[i, j, ]; if (kelvin) serie <- serie - 273.15
  agg <- tapply(serie, dts, max)          # longueur = nb de jours
  # On apparie chaque max à SA date (les noms de `agg`), pas au vecteur horaire,
  # pour éviter tout recyclage/désalignement.
  data.frame(date = as.Date(names(agg)), maille = k,
             lon_grille = cells$lon_grille[k], lat_grille = cells$lat_grille[k],
             poids = cells$poids[k], tmax = as.numeric(agg))
})
maxj <- bind_rows(maxj_par_maille)

# moyenne pondérée par jour : somme(poids*tmax)/somme(poids)
recalcul <- maxj %>% group_by(date) %>%
  summarise(tmax_manuel = sum(poids * tmax) / sum(poids), .groups = "drop")

# --- 4. Comparaison au RDS ---
df <- readRDS(file.path(path_to_save, "era5_temperatures_france.rds"))
rds <- df %>% filter(ville == VILLE) %>% select(date, tmax_rds = temperature_max)
comp <- inner_join(recalcul, rds, by = "date") %>% mutate(diff = tmax_manuel - tmax_rds)

# --- DÉTAIL DU CALCUL sur un échantillon de dates ---
# Pour chaque date échantillonnée : chaque maille (coord, poids, max journalier),
# puis la moyenne pondérée recalculée et la valeur stockée dans le RDS.
cat("\n=== DÉTAIL DU CALCUL —", VILLE, "(échantillon de dates) ===\n")
dates_dispo <- sort(unique(maxj$date))
ech <- dates_dispo[unique(round(seq(1, length(dates_dispo), length.out = 6)))]
for (d in ech) {
  dd <- as.Date(d, origin = "1970-01-01")
  sub <- maxj %>% filter(date == dd) %>% arrange(desc(poids))
  moy_pond <- sum(sub$poids * sub$tmax) / sum(sub$poids)
  val_rds <- rds$tmax_rds[rds$date == dd]
  cat(sprintf("\n%s :\n", as.character(dd)))
  for (r in seq_len(nrow(sub))) {
    cat(sprintf("   maille (%.2f, %.2f)   poids %.3f   tmax_maille %6.2f °C   (contrib %5.2f)\n",
                sub$lon_grille[r], sub$lat_grille[r], sub$poids[r], sub$tmax[r],
                sub$poids[r] * sub$tmax[r]))
  }
  cat(sprintf("   --> moyenne pondérée = %.3f °C   |   RDS = %.3f °C   |   écart = %+.4f\n",
              moy_pond, ifelse(length(val_rds) == 1, val_rds, NA),
              moy_pond - ifelse(length(val_rds) == 1, val_rds, NA)))
}

cat("\n=== COMPARAISON recalcul indépendant vs RDS (", VILLE, ANNEE, "S", SEM, ") ===\n")
cat("Jours comparés :", nrow(comp), "\n")
cat(sprintf("Écart absolu : max %.6f °C | moyen %.6f °C\n",
            max(abs(comp$diff)), mean(abs(comp$diff))))
cat("\nÉchantillon :\n")
print(as.data.frame(comp %>% mutate(across(c(tmax_manuel, tmax_rds, diff), ~round(., 4))) %>% head(6)), row.names = FALSE)

cat("\n=== VERDICT ===\n")
if (nrow(comp) > 0 && max(abs(comp$diff)) < 0.01) {
  cat("✅ Le calcul de la moyenne pondérée est correct (écart < 0,01 °C).\n")
} else {
  cat("❌ Écart anormal — à investiguer.\n")
}
file.remove(full_nc)
