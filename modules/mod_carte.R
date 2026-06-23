# modules/mod_carte.R
#
# Onglet « Carte » — comparaison géographique du réchauffement entre les villes.
# Chaque ville est une pastille dont la couleur/taille reflète son réchauffement
# (écart de tmax annuelle moyenne entre 1961-1990 et 1991-2020). Un filtre permet
# de choisir les villes affichées. Donne la « vue d'ensemble » réclamée, tout en
# restant concret (villes nommées) plutôt qu'un indice national abstrait.
#
# S'appuie sur villes_rechauffement / villes_coords (pré-calculés dans global.R).
# Les colonnes d'enrichissement (nb_canicules, nuits_trop_recent) sont
# optionnelles : absentes tant que le pipeline tmin n'a pas été ré-exécuté.

mod_carte_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      title = "Le réchauffement, ville par ville",
      fillable = TRUE,

      sidebar = sidebar(
        width = "320px",
        card(
          card_header("Villes affichées"),
          pickerInput(ns("villes_carte"),
                      "Sélectionnez les villes :",
                      choices = villes_coords$ville,
                      selected = villes_coords$ville,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 3",
                        `count-selected-text` = "{0} villes sélectionnées",
                        `live-search` = TRUE,
                        `none-selected-text` = "Aucune ville"
                      )),
          helpText("Couleur et taille des pastilles = ampleur du réchauffement (écart de la température maximale annuelle moyenne entre 1961-1990 et 1991-2020).")
        )
      ),

      card(
        full_screen = TRUE,
        card_header("Réchauffement par ville (1961-1990 → 1991-2020)"),
        leafletOutput(ns("carte"), height = "650px")
      )
    )
  )
}

mod_carte_server <- function(id, db_pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Palette commune (sur l'ensemble des villes, pour une échelle stable quel
    # que soit le filtre). On gère le cas où le réchauffement n'a pas pu être
    # calculé (valeurs NA) en repliant sur un domaine neutre.
    domaine <- range(villes_rechauffement$rechauffement, na.rm = TRUE)
    if (!all(is.finite(domaine))) domaine <- c(0, 1)
    pal <- colorNumeric("YlOrRd", domain = domaine, na.color = "#9aa0a6")

    construire_popups <- function(df) {
      vapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        rech <- if (is.finite(r$rechauffement)) sprintf("+%.1f °C", r$rechauffement) else "n/d"
        html <- paste0("<b>", r$ville, "</b><br>Réchauffement : <b>", rech, "</b>")
        if (!is.null(df$nb_canicules) && !is.na(r$nb_canicules)) {
          html <- paste0(html, "<br>Canicules recensées : ", r$nb_canicules)
        }
        if (!is.null(df$nuits_trop_recent) && is.finite(r$nuits_trop_recent)) {
          html <- paste0(html, "<br>Nuits tropicales/an (2011+) : ", round(r$nuits_trop_recent, 1))
        }
        html
      }, character(1))
    }

    # Ajoute les pastilles des villes à une carte (leaflet) ou à un proxy
    # (leafletProxy). Factorisé pour servir au 1er rendu ET aux mises à jour.
    ajouter_marqueurs <- function(carte, df) {
      if (nrow(df) == 0) return(carte)
      rayon <- if (all(is.na(df$rechauffement))) rep(9, nrow(df)) else
        scales::rescale(df$rechauffement, to = c(8, 20), from = domaine)
      addCircleMarkers(
        carte, data = df, lng = ~longitude, lat = ~latitude, group = "villes",
        radius = rayon, stroke = TRUE, color = "#444444", weight = 1,
        fillColor = ~pal(rechauffement), fillOpacity = 0.85,
        label = ~ville, popup = construire_popups(df)
      )
    }

    selection_courante <- function() {
      sel <- input$villes_carte
      if (is.null(sel)) villes_coords$ville else sel
    }

    # Carte de base + pastilles de la sélection initiale. Dessiner les marqueurs
    # ICI (et non seulement via le proxy) évite la race condition où le proxy se
    # déclenche avant que la carte de l'onglet ne soit rendue : sans cela, seul
    # le fond s'affichait. On lit la sélection en isolate pour ne pas re-rendre
    # toute la carte à chaque changement de filtre (géré par l'observeEvent).
    output$carte <- renderLeaflet({
      df0 <- villes_rechauffement %>% filter(ville %in% isolate(selection_courante()))
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(-5.5, 41.0, 9.8, 51.5) %>%
        addLegend("bottomright", pal = pal, values = domaine,
                  title = "Réchauffement (°C)", opacity = 0.9) %>%
        ajouter_marqueurs(df0)
    })

    # Mise à jour des pastilles selon le filtre, sans redessiner la carte.
    observeEvent(input$villes_carte, {
      df <- villes_rechauffement %>% filter(ville %in% input$villes_carte)
      leafletProxy("carte", session) %>%
        clearGroup("villes") %>%
        ajouter_marqueurs(df)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

  })
}
