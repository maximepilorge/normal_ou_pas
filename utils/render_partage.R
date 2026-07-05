# utils/render_partage.R
#
# Génération des « cartes de résultat partageables » (format Open Graph 1200×630),
# en ggplot2 PUR (aucune BDD) afin d'être réutilisables par l'app Shiny (quiz +
# onglet « Une journée ») et par le sidecar plumber facultatif (utils/share_api.R).
#
# Chaque carte combine : un bloc texte (le fait marquant) + une JAUGE thermomètre
# (.ajouter_jauge) qui situe visuellement la température sur une échelle colorée,
# avec la zone « normale » (p10–p90) et le repère du jour — cœur du message
# « vos repères sont décalés ».

library(ggplot2)

# Opérateur de repli (évite une dépendance à rlang pour %||%). Le test is.na()
# n'est appliqué qu'à un scalaire : sur un vecteur de longueur > 1 il renverrait un
# vecteur et ferait planter le ||. Un tel vecteur est donc renvoyé tel quel.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

# « Autour de <ville> » avec élision française. On dit « autour de » (et non « à »)
# car les températures viennent de la maille ERA5-Land (zone, périphérie incluse),
# moins marquée par l'îlot de chaleur urbain que le centre-ville.
autour_de <- function(ville) {
  if (ville == "Le Havre") return("autour du Havre")
  if (grepl("^[AEIOUYÀÂÄÉÈÊËÎÏÔÖÙÛÜaeiouy]", ville)) return(paste0("autour d'", ville))
  paste0("autour de ", ville)
}

# Ajoute la JAUGE thermomètre en bas de la carte (coordonnées 0..100) : barre en
# dégradé bleu→rouge le long d'une échelle de température, encoche de la moyenne,
# crochet de la zone normale (p10–p90) et triangle-repère sur `temp`.
#   p10/p90/normale peuvent être NA (élément simplement omis).
#   dy : décalage vertical de toute la jauge (négatif = vers le bas), pour libérer
#        de la place au-dessus (ex. bandeau de phrase sur 2 lignes).
.ajouter_jauge <- function(p, temp, normale, p10, p90, accent,
                           marqueur_label = "ce jour", periode = NULL, dy = 0) {
  fmt1 <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",")
  vals <- c(temp, normale, p10, p90); vals <- vals[is.finite(vals)]
  lo <- floor(min(vals) - 3); hi <- ceiling(max(vals) + 3)
  xg0 <- 6; xg1 <- 94; y0 <- 31 + dy; y1 <- 37 + dy
  sx <- function(t) xg0 + (t - lo) / (hi - lo) * (xg1 - xg0)

  # Barre en dégradé (tuiles fines : ggplot ne fait pas de remplissage continu).
  n <- 100
  xs <- seq(xg0, xg1, length.out = n + 1)
  cols <- grDevices::colorRampPalette(
    c("#2166ac", "#4393c3", "#4daf4a", "#fdae61", "#d73027"))(n)
  tiles <- data.frame(xmin = xs[-(n + 1)], xmax = xs[-1], fill = cols)
  p <- p +
    geom_rect(data = tiles, aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1, fill = fill),
              inherit.aes = FALSE) +
    scale_fill_identity()

  # Moyenne (encoche blanche + libellé).
  if (is.finite(normale)) {
    p <- p +
      annotate("segment", x = sx(normale), xend = sx(normale), y = y0, yend = y1,
               color = "white", linewidth = 1.1) +
      annotate("text", x = sx(normale), y = 28 + dy, size = 3.4, color = "#495057",
               label = paste0("normale ", fmt1(normale), " °C"))
  }
  if (is.finite(p10) && is.finite(p90)) {
    libelle_zone <- if (is.null(periode)) "zone normale" else paste0("zone normale ", periode)
    p <- p +
      annotate("segment", x = sx(p10), xend = sx(p90), y = 24 + dy, yend = 24 + dy,
               color = "#343a40", linewidth = 0.7) +
      annotate("segment", x = sx(p10), xend = sx(p10), y = 23.2 + dy, yend = 24.8 + dy,
               color = "#343a40", linewidth = 0.7) +
      annotate("segment", x = sx(p90), xend = sx(p90), y = 23.2 + dy, yend = 24.8 + dy,
               color = "#343a40", linewidth = 0.7) +
      annotate("text", x = (sx(p10) + sx(p90)) / 2, y = 20 + dy, size = 3.8, color = "#343a40",
               label = libelle_zone)
  }
  xt <- sx(temp)
  tri <- data.frame(x = c(xt - 1.2, xt + 1.2, xt), y = c(41 + dy, 41 + dy, 37.2 + dy))
  p +
    geom_polygon(data = tri, aes(x = x, y = y), fill = accent, inherit.aes = FALSE) +
    annotate("text", x = xt, y = 43.5 + dy, size = 4, fontface = "bold", color = accent,
             label = marqueur_label) +
    annotate("text", x = xg0, y = 14 + dy, size = 3.6, color = "#6c757d", hjust = 0,
             label = paste0(lo, " °C")) +
    annotate("text", x = xg1, y = 14 + dy, size = 3.6, color = "#6c757d", hjust = 1,
             label = paste0(hi, " °C"))
}

# Couleur d'accent selon la catégorie (au-dessus / en-dessous / dans les normales).
.couleur_categorie <- function(categorie) {
  if (grepl("Au-dessus", categorie)) "#E41A1C"
  else if (grepl("En-dessous", categorie)) "#1f77b4"
  else "#2E8B57"
}

.mois_fr_min <- c("janvier", "février", "mars", "avril", "mai", "juin",
                  "juillet", "août", "septembre", "octobre", "novembre", "décembre")

# Carte de partage du QUIZ.
# params : list(ville, date, temp, normale_moy, periode_ref, categorie,
#               juste (logique|NA), p10, p90,
#               projection_txt (optionnel : phrase « nouvelle normale » du futur),
#               projection_couleur (optionnel : couleur de cette phrase))
dessiner_carte_partage <- function(params) {
  d <- as.Date(params$date)
  date_txt <- paste0(as.integer(format(d, "%d")), " ", .mois_fr_min[as.integer(format(d, "%m"))])
  fmt <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",")
  temp <- as.numeric(params$temp)
  normale <- as.numeric(params$normale_moy)
  a_normale <- is.finite(normale)
  ecart <- round(temp - normale, 1)

  categorie <- params$categorie %||% "Dans les normales de saison"
  couleur_accent <- .couleur_categorie(categorie)
  verdict_label <- if (grepl("Au-dessus", categorie)) "au-dessus des normales"
                   else if (grepl("En-dessous", categorie)) "en-dessous des normales"
                   else "dans les normales de saison"
  ecart_txt <- if (!a_normale) NULL
               else if (ecart == 0) "pile dans la normale de saison"
               else paste0(format(abs(ecart), nsmall = 1, decimal.mark = ","),
                           " °C ", if (ecart > 0) "au-dessus" else "en-dessous",
                           " de la normale de saison")
  p10 <- suppressWarnings(as.numeric(params$p10 %||% NA))
  p90 <- suppressWarnings(as.numeric(params$p90 %||% NA))
  juste <- params$juste
  a_verdict <- !is.null(juste) && !is.na(juste)
  proj_txt <- params$projection_txt
  proj_couleur <- params$projection_couleur %||% "#495057"

  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "#f8f9fa") +
    annotate("rect", xmin = 0, xmax = 2.2, ymin = 0, ymax = 100, fill = couleur_accent) +
    annotate("text", x = 6, y = 95, hjust = 0, vjust = 1, size = 6.5,
             color = "#6c757d", fontface = "bold", label = "QUIZ — CLIMAT : NORMAL OU PAS ?")
  if (a_verdict) {
    p <- p + annotate("label", x = 94, y = 95, hjust = 1, vjust = 1,
                      label = if (isTRUE(juste)) "BONNE RÉPONSE" else "MAUVAISE RÉPONSE",
                      fill = if (isTRUE(juste)) "#2E8B57" else "#C0392B",
                      color = "white", fontface = "bold", size = 7,
                      label.size = 0, label.r = grid::unit(0.3, "lines"),
                      label.padding = grid::unit(0.4, "lines"))
  }
  p <- p +
    annotate("text", x = 6, y = 86, hjust = 0, size = 7, color = "#343a40",
             label = paste0("Un ", date_txt, " ", autour_de(params$ville))) +
    annotate("text", x = 6, y = 74, hjust = 0, size = 18, fontface = "bold",
             color = couleur_accent, label = paste0(fmt(temp), " °C")) +
    annotate("text", x = 6, y = 64, hjust = 0, size = 6, fontface = "bold",
             color = couleur_accent, label = paste0("Réponse : ", verdict_label))
  if (!is.null(ecart_txt))
    p <- p + annotate("text", x = 6, y = 57, hjust = 0, size = 4.8, color = "#343a40",
                      label = paste0("soit ", ecart_txt))

  # Phrase « nouvelle normale » du futur (projection TRACC), si disponible : bandeau
  # sur 2 lignes — le verdict (coloré selon l'issue) PUIS la condition « trajectoire
  # actuelle ». Cette condition est essentielle : elle évite de présenter le futur
  # comme une fatalité (le réchauffement dépend de nos choix). La jauge est décalée
  # vers le bas (dy) pour laisser la place à ces deux lignes.
  dy_jauge <- 0
  if (!is.null(proj_txt)) {
    dy_jauge <- -4
    p <- p +
      annotate("rect", xmin = 4, xmax = 96, ymin = 44, ymax = 54, fill = "#eef1f4") +
      annotate("text", x = 6, y = 50.3, hjust = 0, size = 4.6, fontface = "bold",
               color = proj_couleur, label = proj_txt) +
      annotate("text", x = 6, y = 45.8, hjust = 0, size = 3.5, fontface = "italic",
               color = "#6c757d",
               label = "… si le réchauffement suit la trajectoire actuelle (réf. TRACC)")
  }

  p <- .ajouter_jauge(p, temp, normale, p10, p90, couleur_accent,
                      marqueur_label = "proposé", periode = params$periode_ref,
                      dy = dy_jauge)

  p +
    annotate("text", x = 6, y = 6, hjust = 0, size = 5.5, color = "#6c757d",
             label = "Et vous ? Testez vos repères sur normal-ou-pas.com") +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_void()
}

# Carte de partage pour l'analyse d'UN JOUR PRÉCIS (onglet « Une journée »).
# params : list(ville, date, temp, normale_moy, periode_ref, categorie,
#               rang_txt (phrase de rang), p10, p90)
dessiner_carte_jour <- function(params) {
  d <- as.Date(params$date)
  date_txt <- paste0(as.integer(format(d, "%d")), " ",
                     .mois_fr_min[as.integer(format(d, "%m"))], " ", format(d, "%Y"))
  fmt <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",")
  temp <- as.numeric(params$temp)
  normale <- as.numeric(params$normale_moy)
  a_normale <- is.finite(normale)
  ecart <- round(temp - normale, 1)

  categorie <- params$categorie %||% "Dans les normales de saison"
  couleur_accent <- .couleur_categorie(categorie)
  ecart_txt <- if (!a_normale) NULL
               else if (ecart == 0) "pile dans la normale de saison"
               else paste0(format(abs(ecart), nsmall = 1, decimal.mark = ","),
                           " °C ", if (ecart > 0) "au-dessus" else "en-dessous",
                           " de la normale de saison")
  rang_txt <- params$rang_txt %||% ""
  p10 <- suppressWarnings(as.numeric(params$p10 %||% NA))
  p90 <- suppressWarnings(as.numeric(params$p90 %||% NA))

  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "#f8f9fa") +
    annotate("rect", xmin = 0, xmax = 2.2, ymin = 0, ymax = 100, fill = couleur_accent) +
    annotate("text", x = 6, y = 94, hjust = 0, vjust = 1, size = 6.5,
             color = "#6c757d", fontface = "bold", label = "CLIMAT : NORMAL OU PAS ?") +
    annotate("text", x = 6, y = 85, hjust = 0, size = 7, color = "#343a40",
             label = paste0("Le ", date_txt, " ", autour_de(params$ville))) +
    annotate("text", x = 6, y = 72, hjust = 0, size = 19, fontface = "bold",
             color = couleur_accent, label = paste0(fmt(temp), " °C")) +
    annotate("text", x = 6, y = 63, hjust = 0, size = 4, color = "#6c757d",
             fontface = "italic", label = "température maximale observée ce jour-là") +
    annotate("text", x = 6, y = 55, hjust = 0, size = 5.8, fontface = "bold",
             color = couleur_accent, label = rang_txt)
  if (!is.null(ecart_txt))
    p <- p + annotate("text", x = 6, y = 49, hjust = 0, size = 5, color = "#343a40",
                      label = paste0("soit ", ecart_txt))

  p <- .ajouter_jauge(p, temp, normale, p10, p90, couleur_accent,
                      marqueur_label = "ce jour", periode = params$periode_ref)

  p +
    annotate("text", x = 6, y = 6, hjust = 0, size = 5.5, color = "#6c757d",
             label = "Et vous ? Explorez votre ville sur normal-ou-pas.com") +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_void()
}

# Carte de partage « RAYURES CLIMATIQUES » d'une ville (façon warming stripes) :
# une bande verticale par année, du bleu (année plus fraîche que la normale de
# référence) au rouge (plus chaude). Le visuel climatique le plus reconnu — ici
# décliné sur les températures maximales de la ville.
# params : list(ville, annees (entiers), anomalies (°C, alignées sur annees),
#               periode_ref (libellé de la normale de référence),
#               rechauffement (optionnel : °C depuis le début de la série))
dessiner_carte_stripes <- function(params) {
  annees <- as.integer(params$annees)
  anom   <- as.numeric(params$anomalies)
  ok <- is.finite(annees) & is.finite(anom)
  annees <- annees[ok]; anom <- anom[ok]
  stopifnot(length(annees) > 0)
  o <- order(annees); annees <- annees[o]; anom <- anom[o]

  # Palette divergente RdBu (ColorBrewer 11 classes, codée en dur : ce fichier
  # reste sans dépendance au-delà de ggplot2), bornes symétriques autour de 0
  # pour que le blanc soit exactement « à la normale ».
  pal <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
           "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
  M <- max(abs(anom))
  if (!is.finite(M) || M <= 0) M <- 1
  rampe <- grDevices::colorRampPalette(pal)(256)
  couleur <- rampe[pmin(256L, pmax(1L, as.integer(round((anom + M) / (2 * M) * 255)) + 1L))]

  x0 <- 4; x1 <- 96; y0 <- 28; y1 <- 70
  n <- length(annees)
  xs <- seq(x0, x1, length.out = n + 1)
  bandes <- data.frame(xmin = xs[-(n + 1)], xmax = xs[-1], fill = couleur)

  # « de Nantes » / « d'Avignon » / « du Havre » : on réutilise l'élision
  # d'autour_de en retirant le mot « autour ».
  de_ville <- sub("^autour ", "", autour_de(params$ville))
  rech <- suppressWarnings(as.numeric(params$rechauffement %||% NA))
  fmt1 <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",")

  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "#f8f9fa") +
    annotate("text", x = 4, y = 95, hjust = 0, vjust = 1, size = 6.5,
             color = "#6c757d", fontface = "bold", label = "CLIMAT : NORMAL OU PAS ?") +
    annotate("text", x = 4, y = 86, hjust = 0, size = 10, fontface = "bold",
             color = "#343a40", label = paste0("Les rayures climatiques ", de_ville)) +
    annotate("text", x = 4, y = 78, hjust = 0, size = 4.4, color = "#6c757d",
             label = paste0("Écart de chaque année à la normale ", params$periode_ref,
                            " — températures maximales (ERA5-Land)"))
  if (is.finite(rech) && rech > 0) {
    p <- p + annotate("text", x = 96, y = 78, hjust = 1, size = 6.5, fontface = "bold",
                      color = "#B2182B",
                      label = paste0("≈ +", fmt1(rech), " °C depuis ", annees[1]))
  }
  p +
    geom_rect(data = bandes, aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1, fill = fill),
              inherit.aes = FALSE) +
    scale_fill_identity() +
    annotate("text", x = x0, y = 23.5, hjust = 0, size = 5, fontface = "bold",
             color = "#495057", label = annees[1]) +
    annotate("text", x = x1, y = 23.5, hjust = 1, size = 5, fontface = "bold",
             color = "#495057", label = annees[n]) +
    annotate("text", x = 4, y = 8, hjust = 0, size = 5.5, color = "#6c757d",
             label = "Chaque bande est une année. Et chez vous ? normal-ou-pas.com") +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_void()
}

# Ligne de contexte d'une série (« Série : Toutes les villes · Été · normale … »).
.contexte_serie <- function(ville, saison, periode) {
  bits <- ville %||% "Toutes les villes"
  s <- saison %||% "Toutes les saisons"
  bits <- c(bits, if (identical(s, "Toutes les saisons")) "toutes saisons" else s)
  if (!is.null(periode)) bits <- c(bits, paste0("normale ", periode))
  paste0("Série : ", paste(bits, collapse = " · "))
}

# Sauvegarde un ggplot en PNG (1200×630 par défaut). Utilise ragg si disponible
# (rendu plus net), sinon le device png de base.
.sauver_png <- function(p, chemin, largeur = 1200, hauteur = 630) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(chemin, width = largeur, height = hauteur, units = "px", res = 96)
    print(p); dev.off()
  } else {
    ggsave(chemin, plot = p, width = largeur / 96, height = hauteur / 96,
           dpi = 96, device = "png")
  }
  chemin
}

sauver_carte_partage <- function(params, chemin, largeur = 1200, hauteur = 630) {
  .sauver_png(dessiner_carte_partage(params), chemin, largeur, hauteur)
}

sauver_carte_jour <- function(params, chemin, largeur = 1200, hauteur = 630) {
  .sauver_png(dessiner_carte_jour(params), chemin, largeur, hauteur)
}

sauver_carte_stripes <- function(params, chemin, largeur = 1200, hauteur = 630) {
  .sauver_png(dessiner_carte_stripes(params), chemin, largeur, hauteur)
}
