# modules/mod_memoire.R
#
# Onglet « Ma référence » — ancrage mémoriel personnel.
# L'utilisateur choisit une ville et une année repère (son année de naissance,
# un été dont il se souvient...). On lui montre concrètement le « glissement de
# la baseline » : ce qui était une saison normale à l'époque est aujourd'hui
# plus frais que la normale actuelle. Répond frontalement à « c'est juste l'été ».
#
# N'utilise que des tables existantes (temperatures_max, stats_normales) :
# fonctionne sans la mise à jour tmin du pipeline.

mod_memoire_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Votre repère de température a changé",
      fillable = FALSE,

      sidebar = sidebar(
        width = "350px",
        card(
          card_header("Votre repère"),
          pickerInput(ns("ville_memoire"),
                      "Votre ville :",
                      choices = villes_triees,
                      selected = villes_triees[1],
                      options = list('live-search' = FALSE)),
          sliderInput(ns("annee_repere"),
                      "Une année qui vous parle (naissance, été marquant…) :",
                      min = an_min_data,
                      max = an_max_data,
                      value = max(an_min_data, an_max_data - 45),
                      sep = ""),
          helpText("On compare la saison telle qu'elle était 'normale' autour de cette année avec la normale d'aujourd'hui.")
        )
      ),

      uiOutput(ns("message_memoire")),

      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("titre_graphique"))),
        plotlyOutput(ns("graphique_memoire"), height = "520px")
      )
    )
  )
}

mod_memoire_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Normales (jour calendaire) d'une ville pour une période de référence donnée.
    normales_periode <- function(ville, periode) {
      tbl(db_pool, "stats_normales") %>%
        filter(ville == !!ville, periode_ref == !!periode) %>%
        select(mois, jour_mois, t_moy, t_min, t_q1, t_q3, t_max) %>%
        collect()
    }

    # Données de l'année repère (températures max journalières observées).
    donnees_annee_repere <- reactive({
      req(input$ville_memoire, input$annee_repere)
      an <- input$annee_repere
      tbl(db_pool, "temperatures_max") %>%
        filter(
          ville == !!input$ville_memoire,
          date >= !!as.Date(sprintf("%d-01-01", an)),
          date <  !!as.Date(sprintf("%d-01-01", an + 1))
        ) %>%
        select(date, temperature_max) %>%
        collect() %>%
        rename(tmax_celsius = temperature_max)
    }) %>% bindCache(input$ville_memoire, input$annee_repere)

    # Écart d'été (JJA) entre la normale de l'époque et la normale récente.
    glissement <- reactive({
      req(input$ville_memoire, input$annee_repere)
      periode_repere <- periode_pour_annee(input$annee_repere)

      norm_repere <- normales_periode(input$ville_memoire, periode_repere)
      norm_recent <- normales_periode(input$ville_memoire, periode_recente)
      req(nrow(norm_repere) > 0, nrow(norm_recent) > 0)

      ete <- function(df) mean(df$t_moy[df$mois %in% 6:8], na.rm = TRUE)
      ete_repere <- ete(norm_repere)
      ete_recent <- ete(norm_recent)

      list(
        periode_repere = periode_repere,
        periode_recente = periode_recente,
        ete_repere = ete_repere,
        ete_recent = ete_recent,
        ecart = ete_recent - ete_repere,
        # Même période que l'époque : si repère == récente, pas de glissement à montrer.
        identique = identical(periode_repere, periode_recente)
      )
    }) %>% bindCache(input$ville_memoire, input$annee_repere)

    output$titre_graphique <- renderUI({
      req(input$ville_memoire, input$annee_repere)
      paste0("L'année ", input$annee_repere, " à ", input$ville_memoire,
             " face à la normale d'aujourd'hui (", periode_recente, ")")
    })

    output$message_memoire <- renderUI({
      g <- glissement()
      ecart <- g$ecart

      if (g$identique) {
        contenu <- p(HTML(paste0(
          "Pour ", strong(input$annee_repere), " à ", strong(input$ville_memoire),
          ", la période de référence la plus représentative est déjà la plus récente (",
          g$periode_recente, ") : choisissez une année plus ancienne pour visualiser le glissement du climat."
        )))
        classe <- "border-secondary"
      } else if (ecart >= 0) {
        contenu <- p(HTML(paste0(
          "Autour de ", strong(input$annee_repere), " à ", strong(input$ville_memoire),
          ", un été 'normal' était en moyenne de <b>", sprintf("%.1f", g$ete_repere), "°C</b> ",
          "(normale ", g$periode_repere, "). Aujourd'hui, la normale d'été (", g$periode_recente,
          ") atteint <b>", sprintf("%.1f", g$ete_recent), "°C</b>, soit <b>+", sprintf("%.1f", ecart), "°C</b>.<br>",
          "Autrement dit : ce que vous avez connu comme un été normal serait aujourd'hui considéré comme ",
          strong("plus frais que la moyenne"), "."
        )))
        classe <- "border-danger"
      } else {
        contenu <- p(HTML(paste0(
          "Autour de ", strong(input$annee_repere), " à ", strong(input$ville_memoire),
          ", un été 'normal' était de <b>", sprintf("%.1f", g$ete_repere), "°C</b> ",
          "(normale ", g$periode_repere, "), contre <b>", sprintf("%.1f", g$ete_recent), "°C</b> aujourd'hui (",
          g$periode_recente, "), soit <b>", sprintf("%.1f", ecart), "°C</b>."
        )))
        classe <- "border-secondary"
      }

      card(class = classe, card_body(contenu))
    })

    output$graphique_memoire <- renderPlotly({
      req(input$ville_memoire, input$annee_repere)
      an <- input$annee_repere

      data_repere <- donnees_annee_repere()
      norm_recent <- normales_periode(input$ville_memoire, periode_recente)
      req(nrow(norm_recent) > 0)

      # On projette la normale récente sur l'axe de l'année repère (gestion 29/02).
      norm_recent <- norm_recent %>%
        filter(!(mois == 2 & jour_mois == 29 & !leap_year(as.Date(sprintf("%d-01-01", an))))) %>%
        mutate(date = as.Date(paste(an, mois, jour_mois, sep = "-")))

      p <- ggplot() +
        geom_ribbon(data = norm_recent, aes(x = date, ymin = t_min, ymax = t_max),
                    fill = "lightblue", alpha = 0.45) +
        geom_ribbon(data = norm_recent, aes(x = date, ymin = t_q1, ymax = t_q3),
                    fill = "skyblue", alpha = 0.55) +
        geom_line(data = norm_recent, aes(x = date, y = t_moy,
                                          color = paste0("Normale actuelle (", periode_recente, ")")),
                  linetype = "dashed", linewidth = 0.6) +
        geom_point(data = norm_recent, aes(x = date, y = t_moy,
                   text = paste("Date:", format(date, "%d %b"), "<br>Normale actuelle:", round(t_moy, 1), "°C")),
                   alpha = 0)

      if (nrow(data_repere) > 0) {
        p <- p +
          geom_line(data = data_repere, aes(x = date, y = tmax_celsius,
                                            color = paste0("Année ", an)),
                    linewidth = 0.8) +
          geom_point(data = data_repere, aes(x = date, y = tmax_celsius,
                     text = paste("Date:", format(date, "%d %b"), "<br>Température max:", round(tmax_celsius, 1), "°C")),
                     alpha = 0)
      }

      couleurs <- c("darkblue", "#E41A1C")
      names(couleurs) <- c(paste0("Normale actuelle (", periode_recente, ")"), paste0("Année ", an))

      p <- p +
        scale_color_manual(values = couleurs) +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        labs(y = "Température maximale (°C)", x = NULL, color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    }) %>% bindCache(input$ville_memoire, input$annee_repere)

  })
}
