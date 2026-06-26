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

# « Autour de <ville> » avec élision française. On dit « autour de » (et non « à »)
# car les températures viennent de la maille ERA5-Land (zone, périphérie incluse),
# moins marquée par l'îlot de chaleur urbain que le centre-ville. Défini ici car ce
# fichier est sourcé à la fois par l'app (global.R) et par le sidecar share_api.R.
autour_de <- function(ville) {
  if (ville == "Le Havre") return("autour du Havre")
  if (grepl("^[AEIOUYÀÂÄÉÈÊËÎÏÔÖÙÛÜaeiouy]", ville)) return(paste0("autour d'", ville))
  paste0("autour de ", ville)
}

# Construit le ggplot de la carte de partage (résultat de quiz).
# params : list(ville, date (Date|chr), temp, normale_moy, periode_ref,
#               categorie ("Au-dessus des normales"/"En-dessous…"/"Dans…"),
#               juste (logique : réponse correcte ; optionnel/NA -> pas de verdict))
dessiner_carte_partage <- function(params) {
  mois_fr_loc <- c("janvier", "février", "mars", "avril", "mai", "juin",
                   "juillet", "août", "septembre", "octobre", "novembre", "décembre")
  d <- as.Date(params$date)
  date_txt <- paste0(as.integer(format(d, "%d")), " ", mois_fr_loc[as.integer(format(d, "%m"))])

  fmt <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",")
  temp <- as.numeric(params$temp)
  normale <- as.numeric(params$normale_moy)
  ecart <- round(temp - normale, 1)

  categorie <- params$categorie %||% "Dans les normales de saison"
  couleur_accent <- if (grepl("Au-dessus", categorie)) "#E41A1C"
                    else if (grepl("En-dessous", categorie)) "#1f77b4"
                    else "#2E8B57"
  verdict_label <- if (grepl("Au-dessus", categorie)) "au-dessus des normales"
                   else if (grepl("En-dessous", categorie)) "en-dessous des normales"
                   else "dans les normales de saison"

  ecart_txt <- if (ecart == 0) "pile dans la normale de saison"
               else paste0(format(abs(ecart), nsmall = 1, decimal.mark = ","),
                           " °C ", if (ecart > 0) "au-dessus" else "en-dessous",
                           " de la normale de saison")

  juste <- params$juste
  a_verdict <- !is.null(juste) && !is.na(juste)

  p <- ggplot() +
    # Fond + barre d'accent (couleur = catégorie de la température).
    annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "#f8f9fa") +
    annotate("rect", xmin = 0, xmax = 2.2, ymin = 0, ymax = 100, fill = couleur_accent) +
    # Bandeau d'en-tête : on comprend tout de suite que c'est un quiz.
    annotate("text", x = 6, y = 92, hjust = 0, vjust = 1, size = 7,
             color = "#6c757d", fontface = "bold",
             label = "QUIZ — CLIMAT : NORMAL OU PAS ?")

  # Pastille de verdict (bonne / mauvaise réponse), en haut à droite.
  if (a_verdict) {
    p <- p + annotate("label", x = 94, y = 90, hjust = 1, vjust = 1,
                      label = if (isTRUE(juste)) "BONNE RÉPONSE" else "MAUVAISE RÉPONSE",
                      fill = if (isTRUE(juste)) "#2E8B57" else "#C0392B",
                      color = "white", fontface = "bold", size = 8,
                      label.size = 0, label.r = grid::unit(0.3, "lines"),
                      label.padding = grid::unit(0.45, "lines"))
  }

  p +
    # Scénario du quiz : un jour de l'année (sans prétendre à une date précise)
    # et une température maximale PROPOSÉE (tirée dans la plage historique).
    annotate("text", x = 6, y = 74, hjust = 0, size = 8.5, color = "#343a40",
             label = paste0("Un ", date_txt, " ", autour_de(params$ville))) +
    annotate("text", x = 6, y = 58, hjust = 0, size = 24, fontface = "bold",
             color = couleur_accent, label = paste0(fmt(temp), " °C")) +
    annotate("text", x = 6, y = 47, hjust = 0, size = 5.5, color = "#6c757d",
             fontface = "italic", label = "température maximale proposée par le quiz") +
    # Verdict climatique : la bonne réponse, explicitée.
    annotate("text", x = 6, y = 37, hjust = 0, size = 9, fontface = "bold",
             color = couleur_accent, label = paste0("Réponse : ", verdict_label)) +
    annotate("text", x = 6, y = 27, hjust = 0, size = 6.5, color = "#343a40",
             label = paste0("soit ", ecart_txt)) +
    annotate("text", x = 6, y = 19, hjust = 0, size = 6, color = "#6c757d",
             label = paste0("Normale ", params$periode_ref, " : ", fmt(normale), " °C")) +
    # Pied de page : appel à l'action.
    annotate("text", x = 6, y = 9, hjust = 0, size = 6.5, color = "#6c757d",
             label = "Et vous ? Testez vos repères sur normal-ou-pas.com") +
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
