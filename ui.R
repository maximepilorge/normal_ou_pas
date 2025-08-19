# ui.R
library(shiny)
library(shinyjs)
library(plotly)
library(bslib)

# On charge le code UI de chaque module
source("modules/mod_quiz.R")
source("modules/mod_visualisation.R")
source("modules/mod_analyse.R")

ui <- navbarPage(
  "Climat : Normal ou pas ?",
  theme = bs_theme(version = 5),
  header = useShinyjs(),
  
  tags$head(
    # Balises Open Graph (pour LinkedIn, Facebook, etc.)
    tags$meta(property = "og:title", content = "Climat : Normal ou pas ?"),
    tags$meta(property = "og:description", content = "Explorez le changement climatique. Jouez au quiz, comparez les années et analysez si la météo est vraiment 'normale'."),
    tags$meta(property = "og:image", content = "https://normal-ou-pas.com/social_preview.png"),
    tags$meta(property = "og:url", content = "https://normal-ou-pas.com"),
    tags$meta(property = "og:type", content = "website"),
    
    # Balises Twitter Card (pour Twitter)
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Climat : Normal ou pas ?"),
    tags$meta(name = "twitter:description", content = "Explorez le changement climatique. Jouez au quiz, comparez les années et analysez si la météo est vraiment 'normale'."),
    tags$meta(name = "twitter:image", content = "https://normal-ou-pas.com/social_preview.png") # URL à personnaliser !
  ),
  
  # -- Onglet 1 : Le Quiz --
  tabPanel("Le Quiz 🧐",
           mod_quiz_ui("quiz_1")
  ),
  
  # -- Onglet 2 : L'Explorateur --
  tabPanel("Comparer les années 📊",
           mod_visualisation_ui("visu_1")
  ),
  
  # -- Onglet 3 : Analyse d'un événement --
  tabPanel("Analyse d'un événement 🔍",
           mod_analyse_ui("analyse_1")
  ),
  
  # -- Onglet 4 : Méthodologie --
  tabPanel("Méthodologie 📝", 
           fluidPage( 
             titlePanel("Choix méthodologiques"), 
             fluidRow( 
               column(10, offset = 1, 
                      
                      h3("Source et attribution des données 🌡️"), 
                      p("La méthodologie de l'application repose sur des données publiques et des techniques de traitement géospatial standards."), 
                      tags$ul( 
                        tags$li(strong("Source des données : "), "Les températures proviennent du jeu de données ERA5-Land, accessible via le Copernicus Climate Change Service (C3S). C'est une base de données de 'réanalyse' climatique qui combine des observations passées avec des modèles météorologiques pour créer un enregistrement climatique complet et cohérent."), 
                        tags$li(strong("Granularité : "), "Les données sont initialement téléchargées à une fréquence horaire puis agrégées pour ne conserver que la température maximale de chaque journée. La période couverte s'étend de 1950 à aujourd'hui."), 
                        tags$li(strong("Attribution des données à une ville : "), "Les données ERA5-Land sont fournies sur une grille géographique avec des mailles de 9km x 9km. Pour chaque ville, l'application identifie la maille la plus proche et toutes les données de température proviennent ensuite exclusivement de cette maille."), 
                        tags$li(strong("Important - Ce que cette température représente : "), "La valeur affichée correspond à la température moyenne sur l'ensemble d'une maille de 81 km² (9x9 km). Elle peut donc différer de la température que vous lisez sur un thermomètre chez vous ou de celle annoncée dans les prévisions météo, qui correspondent souvent à des mesures plus locales (aéroport, station météo spécifique).")
                      ), 
                      
                      hr(), 
                      
                      h3("Calcul et définition des 'Normales de saison' 📊"),
                      p("L'application se base sur le calcul de normales climatiques, conformément aux standards internationaux."),
                      tags$ul(
                        tags$li(strong("Périodes de référence : "), "Les normales sont calculées pour différentes périodes de 30 ans (ex: 1961-1990, 1991-2020), comme le recommande l'Organisation Météorologique Mondiale (OMM), afin de permettre la comparaison et de visualiser l'évolution du climat."),
                        tags$li(strong("Définition du 'normal' : "), "Pour définir ce qui est 'normal', l'application utilise la méthode des percentiles.",
                                tags$ol(
                                  tags$li("Pour un jour donné (ex: le 15 août) et une période de référence, l'application analyse la distribution de toutes les températures maximales observées les 15 août de cette période."),
                                  tags$li("Elle calcule ensuite le 10e percentile (la valeur en dessous de laquelle se trouvent les 10% des jours les plus froids) et le 90e percentile (la valeur au-dessus de laquelle se trouvent les 10% des jours les plus chauds)."),
                                  tags$li("Une température est jugée ", strong("'Dans les normales de saison'"), " si elle se situe entre ces deux bornes (le 10e et le 90e percentile)."),
                                  tags$li("Si elle est en-dehors de cette plage, elle est considérée comme 'En-dessous' ou 'Au-dessus' des normales.")
                                )
                        ),
                        p("Cette méthode signifie qu'environ 80% des températures de la période de référence sont considérées comme 'normales'.")
                      ),
                      
                      hr(), 
                      
                      h3("Code Source 💻"), 
                      p("Pour les plus curieux, le code source complet de cette application est disponible sur GitHub. N'hésitez pas à le consulter, à le réutiliser ou à proposer des améliorations !"), 
                      p(style = "text-align: center; margin-top: 20px;", 
                        tags$a(href = "https://github.com/maximepilorge/normal_ou_pas", 
                               target = "_blank",
                               class = "btn btn-primary btn-lg", 
                               icon("github"),
                               "Voir le code sur GitHub" 
                        ) 
                      ) 
               ) 
             ) 
           ) 
  ) 
)