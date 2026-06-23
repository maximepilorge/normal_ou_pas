# utils/render_partage.R
#
# Génération de la « carte de résultat partageable » du quiz, au format Open
# Graph (1200×630). Fonction PURE (ggplot2 uniquement, aucune BDD) afin d'être
# réutilisable à la fois :
#   - dans l'app Shiny (downloadHandler du module quiz) ;
#   - par le sidecar plumber facultatif (utils/share_api.R) pour l'aperçu social.
#
# Le message met en avant le fait climatique marquant : « il a fait X°C, soit
# Z°C au-dessus/en-dessous de la normale de saison », cœur de l'ambition de l'app.

library(ggplot2)

# Construit le ggplot de la carte de partage.
# params : list(ville, date (Date|chr), temp, normale_moy, periode_ref,
#               categorie ("Au-dessus des normales"/"En-dessous…"/"Dans…"))
dessiner_carte_partage <- function(params) {
  mois_fr_loc <- c("janvier", "février", "mars", "avril", "mai", "juin",
                   "juillet", "août", "septembre", "octobre", "novembre", "décembre")
  d <- as.Date(params$date)
  date_txt <- paste0(as.integer(format(d, "%d")), " ", mois_fr_loc[as.integer(format(d, "%m"))])

  temp <- round(as.numeric(params$temp), 1)
  normale <- round(as.numeric(params$normale_moy), 1)
  ecart <- round(temp - normale, 1)
  sens <- if (ecart > 0) "au-dessus" else if (ecart < 0) "en-dessous" else "dans"

  categorie <- params$categorie %||% "Dans les normales de saison"
  couleur_accent <- if (grepl("Au-dessus", categorie)) "#E41A1C"
                    else if (grepl("En-dessous", categorie)) "#1f77b4"
                    else "#2E8B57"

  phrase_ecart <- if (ecart == 0) {
    "exactement dans la normale de saison"
  } else {
    paste0(sprintf("%+.1f", ecart), " °C ", sens, " de la normale de saison")
  }

  ggplot() +
    # Fond et cadre d'accent.
    annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "#f8f9fa") +
    annotate("rect", xmin = 0, xmax = 2.2, ymin = 0, ymax = 100, fill = couleur_accent) +
    # En-tête.
    annotate("text", x = 6, y = 90, hjust = 0, vjust = 1, size = 7.5,
             color = "#343a40", fontface = "bold", label = "Climat : Normal ou pas ?") +
    # Lieu et date.
    annotate("text", x = 6, y = 76, hjust = 0, size = 9, color = "#343a40",
             label = paste0("Le ", date_txt, ", à ", params$ville)) +
    # Température en grand.
    annotate("text", x = 6, y = 55, hjust = 0, size = 26, fontface = "bold",
             color = couleur_accent, label = paste0(temp, " °C")) +
    # Écart à la normale.
    annotate("text", x = 6, y = 33, hjust = 0, size = 10, color = "#343a40",
             label = phrase_ecart) +
    annotate("text", x = 6, y = 22, hjust = 0, size = 7, color = "#6c757d",
             label = paste0("Normale ", params$periode_ref, " : ", normale, " °C")) +
    # Pied de page.
    annotate("text", x = 6, y = 8, hjust = 0, size = 6.5, color = "#6c757d",
             label = "normal-ou-pas.com — testez vos repères de température") +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_void()
}

# Opérateur de repli (évite une dépendance à rlang pour %||%).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Sauvegarde la carte en PNG 1200×630. Utilise ragg si disponible (rendu plus
# net), sinon le device png de base.
sauver_carte_partage <- function(params, chemin, largeur = 1200, hauteur = 630) {
  p <- dessiner_carte_partage(params)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(chemin, width = largeur, height = hauteur, units = "px", res = 96)
    print(p); dev.off()
  } else {
    ggsave(chemin, plot = p, width = largeur / 96, height = hauteur / 96,
           dpi = 96, device = "png")
  }
  chemin
}
