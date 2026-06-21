# =================================================================
# VÉRIFICATION RAPIDE — un mois (ou semestre) téléchargé
# =================================================================
# Télécharge un petit échantillon RÉEL (par défaut août, ~2 min sur la France),
# applique la VRAIE fonction de production traiter_fichier_horaire sur
# l'association réelle, et affiche un verdict de plausibilité. Sert à confirmer
# immédiatement que la température (t2m) est bien lue (et non des zéros).
#
# Réglages :
ANNEE <- 2020
MOIS  <- 8           # un mois d'été : le max doit être nettement chaud
                     # (mettre p.ex. 7:12 pour tester un semestre complet)

suppressMessages(library(here))
# Charge les fonctions (traiter_fichier_horaire, villes_insee, libs) SANS lancer
# le téléchargement complet (l'auto-exécution est protégée par sys.nframe()).
source(here::here("utils", "telecharger_data.R"))

wf_set_key(key = Sys.getenv("KEY_CDS"), user = "maxp17.mp@gmail.com")
path_to_save <- here::here("data")

# Association ville -> mailles (réutilise celle déjà produite, sinon la construit).
f_assoc <- file.path(path_to_save, "villes_et_mailles_associees.rds")
if (file.exists(f_assoc)) {
  assoc <- readRDS(f_assoc)
  message("Association chargée : ", nrow(assoc), " lignes, ", dplyr::n_distinct(assoc$ville), " villes.")
} else {
  stop("Fichier d'association introuvable — lance d'abord definir_mailles_communes.R ou la pipeline.")
}

villes <- villes_insee
zone <- c(max(villes$latitude) + 1, min(villes$longitude) - 1,
          min(villes$latitude) - 1, max(villes$longitude) + 1)
heures_24 <- sprintf("%02d:00", 0:23)

cible <- sprintf("verif_%d_%s.nc", ANNEE, paste(sprintf("%02d", MOIS), collapse = "-"))
full_nc <- file.path(path_to_save, cible)
invisible(file.remove(Sys.glob(file.path(path_to_save, "verif_*"))))

req <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type = "reanalysis",
  variable = "2m_temperature",
  year = as.character(ANNEE),
  month = sprintf("%02d", MOIS),
  day = sprintf("%02d", 1:31),
  time = heures_24,
  data_format = "netcdf",
  download_format = "unarchived",
  area = zone,
  target = cible
)

message("Téléchargement de ", ANNEE, " mois ", paste(MOIS, collapse = ","), " (peut prendre quelques minutes)...")
t0 <- Sys.time()
wf_request(user = "maxp17.mp@gmail.com", request = req, path = path_to_save,
           transfer = TRUE, verbose = TRUE)
message("Durée : ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")

# Le CDS peut renvoyer un .zip
fz <- sub("\\.nc$", ".zip", full_nc)
if (!file.exists(full_nc) && file.exists(fz)) {
  unzip(fz, exdir = path_to_save)
  ex <- file.path(path_to_save, "data_0.nc"); if (file.exists(ex)) file.rename(ex, full_nc)
  file.remove(fz)
}
if (!file.exists(full_nc)) stop("NetCDF non téléchargé : ", cible)

res <- traiter_fichier_horaire(full_nc, assoc)
file.remove(full_nc)

cat("\n================= RÉSULTAT =================\n")
cat("Lignes :", nrow(res), "| villes :", dplyr::n_distinct(res$ville),
    "| dates :", as.character(min(res$date)), "->", as.character(max(res$date)), "\n")
cat(sprintf("tmax °C — min %.1f | moyenne %.1f | max %.1f\n",
            min(res$temperature_max), mean(res$temperature_max), max(res$temperature_max)))

cat("\nMax par ville (5 plus chaudes / 5 plus fraîches) :\n")
parv <- res %>% dplyr::group_by(ville) %>%
  dplyr::summarise(max = round(max(temperature_max), 1), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(max))
print(as.data.frame(rbind(head(parv, 5), tail(parv, 5))), row.names = FALSE)

cat("\n=== VERDICT ===\n")
mx <- max(res$temperature_max)
if (mx >= 25 && mx <= 48 && dplyr::n_distinct(res$temperature_max) > 10) {
  cat("✅ Plausible : la température (t2m) est correctement lue.\n")
} else {
  cat("❌ SUSPECT (max =", round(mx, 1), "°C) : variable mal lue ou conversion ratée — NE PAS lancer le run complet.\n")
}
