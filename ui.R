# ui.R
library(shiny)
library(shinyjs)
library(plotly)

# On charge le code UI de chaque module
source("modules/mod_quiz.R")
source("modules/mod_visualisation.R")
source("modules/mod_analyse.R")

ui <- navbarPage(
  "Climat : Normal ou pas normal ?",
  header = useShinyjs(),
  
  # -- Onglet 1 : Le Quiz (Appel du module) --
  tabPanel("Le Quiz 🧐",
           # On appelle la fonction UI du module en lui donnant un ID unique
           mod_quiz_ui("quiz_1")
  ),
  
  # -- Onglet 2 : L'Explorateur (Appel du module) --
  tabPanel("Comparer les années 📊",
           mod_visualisation_ui("visu_1")
  ),
  
  # -- Onglet 3 : Analyse d'un événement (Appel du module) --
  tabPanel("Analyse d'un Événement 🔍",
           mod_analyse_ui("analyse_1")
  ),
  
  # -- Onglet 4 : Méthodologie (Reste dans le UI principal) --
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
                        tags$li(strong("Attribution des données à une ville : "), "Les données ERA5-Land sont fournies sur une grille géographique avec des mailles de 9km x 9km. Pour chaque ville, le script identifie la maille la plus proche. Toutes les données de température pour une ville donnée proviennent exclusivement de cette maille."), 
                        tags$li(strong("Important - Ce que cette température représente : "), "La valeur affichée correspond à la température moyenne sur l'ensemble d'une maille de 81 km² (9x9 km). Elle peut donc différer de la température que vous lisez sur un thermomètre chez vous ou de celle annoncée dans les prévisions météo, qui correspondent souvent à des mesures très locales (aéroport, station météo spécifique). C'est une représentation du climat à une échelle plus large, pas une mesure ponctuelle.")
                      ), 
                      
                      hr(), 
                      
                      h3("Calcul et définition des 'Normales de saison' 📊"), 
                      p("L'application se base sur le calcul de normales climatiques."), 
                      tags$ul( 
                        tags$li(strong("Périodes de référence : "), "Les normales sont calculées pour différentes périodes (ex: 1961-1990, 1991-2020) afin de permettre la comparaison et de visualiser l'évolution du climat."), 
                        tags$li(strong("Définition statistique du 'normal' : "), "Pour le quiz, une température n'est pas simplement comparée à la moyenne. L'application utilise la méthode de l'écart interquartile (IQR) pour détecter les valeurs atypiques.", 
                                tags$ol( 
                                  tags$li("Pour un jour donné (ex: le 15 août) et une période de référence, l'application analyse la distribution de toutes les températures maximales observées les 15 août de cette période."), 
                                  tags$li("Elle calcule le premier quartile (Q1) et le troisième quartile (Q3)."), 
                                  tags$li("Une température est jugée ", strong("'Dans les normales de saison'"), " si elle se situe entre les bornes [Q1 - 1.5 * IQR] et [Q3 + 1.5 * IQR]."), 
                                  tags$li("Si elle est en dehors de ces bornes, elle est considérée comme 'En-dessous' ou 'Au-dessus' des normales.") 
                                ) 
                        ), 
                        p("Cette méthode est plus robuste qu'une simple comparaison à la moyenne car elle tient compte de la variabilité habituelle des températures pour un jour donné.") 
                      ), 
                      
                      hr(), 
                      
                      h3("Code Source 💻"), 
                      p("Pour les plus curieux, le code source complet de cette application est disponible sur GitHub. N'hésitez pas à le consulter, à le réutiliser ou à proposer des améliorations !"), 
                      p(style = "text-align: center; margin-top: 20px;", 
                        tags$a(href = "https://github.com/maximepilorge/guess_climate", 
                               target = "_blank", # Ouvre le lien dans un nouvel onglet 
                               class = "btn btn-primary btn-lg", # Style de bouton pour le rendre plus visible 
                               icon("github"), # Ajoute l'icône GitHub 
                               "Voir le code sur GitHub" 
                        ) 
                      ) 
               ) 
             ) 
           ) 
  ) 
)