# utils/verifier_donnees_completes.R
#
# Ce script analyse le fichier de données (era5_temperatures_france.rds)
# pour produire un rapport sur la complétude des données pour chaque ville.

# --- 1. Chargement des librairies ---
library(dplyr)
library(lubridate)
library(tidyr)
library(here)

# --- 2. Chargement des dépendances du projet ---
dirApp <- "C:/Users/maxp1/Documents/guess_climate"
key_cds <- "9e71b600-abdb-4b74-92f5-94721d1f774f"

#' Analyse la complétude des données de température
#'
#' @param chemin_fichier_rds Chemin vers le fichier .rds à analyser.
#' @param df_villes Dataframe de référence contenant la colonne 'ville'.
#'
#' @return Un tibble résumant la complétude pour chaque ville.
analyser_completude_donnees <- function(chemin_fichier_rds, df_villes) {
  
  # --- Vérification de l'existence du fichier ---
  if (!file.exists(chemin_fichier_rds)) {
    stop("Le fichier de données spécifié n'a pas été trouvé : ", chemin_fichier_rds)
  }
  
  # --- Chargement et préparation des données ---
  cat("Chargement du fichier de données...\n")
  donnees_brutes <- readRDS(chemin_fichier_rds) %>%
    # S'assurer que les noms de colonnes sont standards
    rename_with(tolower) %>%
    rename(tmax = temperature_max)
  
  # --- A. Vérification de la présence de toutes les villes ---
  cat("Vérification de la présence de toutes les villes de référence...\n")
  villes_attendues <- unique(df_villes$ville)
  villes_presentes <- unique(donnees_brutes$ville)
  villes_manquantes <- setdiff(villes_attendues, villes_presentes)
  
  if (length(villes_manquantes) > 0) {
    warning("Certaines villes de la liste de référence sont absentes du fichier de données : ", 
            paste(villes_manquantes, collapse = ", "))
  } else {
    cat("✅ Toutes les villes de référence sont présentes.\n")
  }
  
  # --- B. Calcul du rapport détaillé par ville ---
  cat("Calcul des statistiques de complétude par ville...\n")
  
  # On s'assure que chaque ville a une ligne pour chaque jour
  # entre sa première et sa dernière date de mesure
  donnees_completes <- donnees_brutes %>%
    group_by(ville) %>%
    complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    ungroup()
  
  # Maintenant que la grille est complète, les calculs sont simples
  rapport <- donnees_completes %>%
    group_by(ville) %>%
    summarise(
      # Date de début et de fin des données pour cette ville
      date_debut = min(date, na.rm = TRUE),
      date_fin = max(date, na.rm = TRUE),
      
      # Nombre total de jours attendus dans l'intervalle
      jours_attendus = as.integer(date_fin - date_debut + 1),
      
      # Compter les jours où la valeur est invalide (NA, NaN, Inf, -Inf)
      jours_valides = sum(is.finite(tmax)),
      
      # Le nombre de jours manquants est simplement la différence
      jours_manquants = jours_attendus - jours_valides,
      
      # On peut ajouter un pourcentage pour une lecture plus facile
      pourcentage_completude = round(100 * jours_valides / jours_attendus, 2)
    ) %>%
    # On ajoute les villes qui n'avaient aucune donnée pour qu'elles apparaissent dans le rapport
    full_join(tibble(ville = villes_manquantes), by = "ville") %>%
    arrange(ville)
  
  return(rapport)
  
}

# --- 3. Exécution de l'analyse ---
cat("Lancement de l'analyse de complétude des données...\n\n")

# Chemin vers votre fichier de données
chemin_fichier <- here("data", "era5_temperatures_france.rds")

# Appel de la fonction
rapport_final <- analyser_completude_donnees(chemin_fichier, villes)

cat("\n--- RAPPORT DE COMPLÉTUDE DES DONNÉES ---\n\n")
print(rapport_final, n = nrow(rapport_final))
cat("\nAnalyse terminée.\n")