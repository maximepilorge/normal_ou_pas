# =================================================================
# INTÉGRATION DES FICHIERS era5_h_YYYY_S{1,2}.nc DÉJÀ PRÉSENTS
# =================================================================
# Agrège dans era5_temperatures_france.rds les semestres déjà téléchargés
# (fichiers data/era5_h_*_S*.nc) mais pas encore intégrés (run interrompu en
# plein lot), SANS re-télécharger. Après ça, le détecteur de trous de
# telecharger_data.R verra ces dates comme présentes et ne les redemandera pas.
#
# On réutilise la VRAIE fonction de production traiter_fichier_horaire
# (le `source` ne déclenche aucun téléchargement grâce à la garde sys.nframe()).

library(here)
suppressMessages(source(here::here("utils", "telecharger_data.R")))

path_to_save <- here::here("data")
f_assoc <- file.path(path_to_save, "villes_et_mailles_associees.rds")
f_rds   <- file.path(path_to_save, "era5_temperatures_france.rds")

stopifnot(file.exists(f_assoc))
assoc  <- readRDS(f_assoc)
df_rds <- if (file.exists(f_rds)) readRDS(f_rds) else data.frame()

fichiers <- Sys.glob(file.path(path_to_save, "era5_h_*_S*.nc"))
if (length(fichiers) == 0) stop("Aucun fichier era5_h_*_S*.nc trouvé dans data/.")
cat(length(fichiers), "fichier(s) candidat(s) :\n  ", paste(basename(fichiers), collapse = ", "), "\n\n")

# Agrégation de chaque fichier (avec garde-fou de plausibilité).
nouveaux <- bind_rows(lapply(fichiers, function(f) {
  res <- tryCatch(traiter_fichier_horaire(f, assoc),
                  error = function(e) { warning(basename(f), " : ", conditionMessage(e)); NULL })
  if (is.null(res) || nrow(res) == 0) { cat(sprintf("  %-22s : ÉCHEC / vide (ignoré)\n", basename(f))); return(NULL) }
  mx <- max(res$temperature_max, na.rm = TRUE)
  if (mx < 10) {  # valeurs suspectes (fichier corrompu / mauvaise variable) -> on n'intègre pas
    cat(sprintf("  %-22s : SUSPECT (max %.1f °C) -> ignoré\n", basename(f), mx)); return(NULL)
  }
  cat(sprintf("  %-22s : %d lignes | %s -> %s | max %.1f °C\n", basename(f), nrow(res),
              as.character(min(res$date)), as.character(max(res$date)), mx))
  res
}))

if (nrow(nouveaux) == 0) stop("Aucun fichier exploitable à intégrer.")

# Dédoublonnage : on retire du RDS les dates couvertes par les nouveaux fichiers
# (chaque semestre couvre toutes les villes), puis on concatène.
if (nrow(df_rds) > 0) {
  df_rds <- df_rds %>% filter(!(date %in% unique(nouveaux$date)))
}
df_final <- bind_rows(df_rds, nouveaux) %>% arrange(ville, date)
saveRDS(df_final, f_rds)

cat("\n✅ RDS mis à jour :", nrow(df_final), "lignes |",
    n_distinct(df_final$ville), "villes |",
    as.character(min(df_final$date)), "->", as.character(max(df_final$date)), "\n")
cat("Les .nc intégrés peuvent être supprimés (ils seraient de toute façon ignorés au prochain run).\n")
