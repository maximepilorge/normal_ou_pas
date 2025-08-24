# ui.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(bslib)

# On charge le code UI de chaque module
source("modules/mod_quiz.R")
source("modules/mod_visualisation.R")
source("modules/mod_analyse.R")

ui <- tagList( # On utilise tagList comme conteneur principal
  
  # --- HEADER ---
  tags$head(
    # 1. Balises meta
    # Open Graph (pour LinkedIn, Facebook, etc.)
    tags$meta(property = "og:title", content = "Climat : Normal ou pas ?"),
    tags$meta(property = "og:description", content = "Explorez le changement climatique. Jouez au quiz, comparez les années et analysez si la météo est vraiment 'normale'."),
    tags$meta(property = "og:image", content = "https://normal-ou-pas.com/social_preview.png"),
    tags$meta(property = "og:url", content = "https://normal-ou-pas.com"),
    tags$meta(property = "og:type", content = "website"),
    
    # Twitter Card (pour Twitter)
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Climat : Normal ou pas ?"),
    tags$meta(name = "twitter:description", content = "Explorez le changement climatique. Jouez au quiz, comparez les années et analysez si la météo est vraiment 'normale'."),
    tags$meta(name = "twitter:image", content = "https://normal-ou-pas.com/social_preview.png"),
    
    # 2. CSS pour styliser le bandeau
    tags$style(HTML("
      #cookie-banner {
        position: fixed; bottom: 0; left: 0; width: 100%;
        background-color: #343a40; color: white; padding: 15px 25px;
        display: flex; justify-content: space-between; align-items: center;
        z-index: 1050; font-size: 0.9rem;
      }
      #cookie-banner p { margin: 0; }
      #accept-cookie-btn { margin-left: 20px; white-space: nowrap; }
    ")),
    
    # 3. Logique des cookies
    tags$script(HTML(r'(
      // --- Fonctions utilitaires pour les cookies ---
      function generateUUID() {
        return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, function(c) {
          var r = Math.random() * 16 | 0, v = c == "x" ? r : (r & 0x3 | 0x8);
          return v.toString(16);
        });
      }
      function getCookie(name) {
        let matches = document.cookie.match(new RegExp(
          "(?:^|; )" + name.replace(/([.$?*|{}()[]\/+^])/g, "\$1") + "=([^;]*)"
        ));
        return matches ? decodeURIComponent(matches[1]) : undefined;
      }
      function setCookie(name, value, days) {
        var expires = "";
        if (days) {
          var date = new Date();
          date.setTime(date.getTime() + (days*24*60*60*1000));
          expires = "; expires=" + date.toUTCString();
        }
        document.cookie = name + "=" + (value || "")  + expires + "; path=/; SameSite=Lax";
      }
      
      // Détecte le type d'appareil
      function getDeviceType() {
        const ua = navigator.userAgent;
        if (/(tablet|ipad|playbook|silk)|(android(?!.*mobi))/i.test(ua)) {
          return "Tablette";
        }
        if (/Mobile|iP(hone|od)|Android|BlackBerry|IEMobile|Kindle|Silk-Accelerated|(hpw|web)OS|Opera M(obi|ini)/.test(ua)) {
          return "Mobile";
        }
        return "Ordinateur";
      };

      // --- Logique exécutée au démarrage de la session Shiny ---
      $(document).on("shiny:connected", function(event) {
        // Logique pour l'ID visiteur (existante)
        let visitorId = getCookie("visitor_id");
        if (!visitorId) {
          visitorId = generateUUID();
          setCookie("visitor_id", visitorId, 365);
        }
        Shiny.setInputValue("visitor_id", visitorId, {priority: "event"});
    
        // NOUVELLE LIGNE : On envoie le type d'appareil au serveur
        Shiny.setInputValue("device_type", getDeviceType(), {priority: "event"});
        
        // Logique pour l'affichage du bandeau (existante)
        if (!getCookie("cookie_consent")) {
          $("#cookie-banner").show();
        }
      });
    
      // --- Logique pour le clic sur le bouton d'acceptation (existante) ---
      $(document).on("click", "#accept-cookie-btn", function() {
        setCookie("cookie_consent", "true", 365);
        $("#cookie-banner").hide();
      });
    )'))
  ),
  
  
  # --- INTERFACE PRINCIPALE DE L'APPLICATION ---
  navbarPage(
    "Climat : Normal ou pas ?",
    theme = bs_theme(version = 5),
    header = tagList(
      useShinyjs(),
      extendShinyjs(text = "
        shinyjs.disablePicker = function(id) { $('button[data-id=\"' + id + '\"]').addClass('disabled').prop('disabled', true); };
        shinyjs.enablePicker = function(id) { $('button[data-id=\"' + id + '\"]').removeClass('disabled').prop('disabled', false); };
      ", functions = c("disablePicker", "enablePicker"))
    ),
    
    # -- Onglets de l'application --
    tabPanel("Le Quiz 🧐", mod_quiz_ui("quiz_1")),
    tabPanel("Comparer les années 📊", mod_visualisation_ui("visu_1")),
    tabPanel("Evolution globale 🔍", mod_analyse_ui("analyse_1")),
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
                          tags$li(strong("Attribution des données à une ville : "), "Les données ERA5-Land sont fournies sur une grille géographique avec des mailles de 9x9km. Pour chaque ville, l'application identifie la maille la plus proche et toutes les données de température proviennent ensuite exclusivement de cette maille."),
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
                                    tags$li("Elle calcule ensuite le 10e percentile (la valeur en-dessous de laquelle se trouvent les 10% des jours les plus froids) et le 90e percentile (la valeur au-dessus de laquelle se trouvent les 10% des jours les plus chauds)."),
                                    tags$li("Une température est jugée ", strong("'Dans les normales de saison'"), " si elle se situe entre ces deux bornes (le 10e et le 90e percentile)."),
                                    tags$li("Si elle est en-dehors de cette plage, elle est considérée comme 'En-dessous' ou 'Au-dessus' des normales.")
                                  )
                          )
                        ),
                        
                        # Le paragraphe est maintenant à l'extérieur de la liste
                        p("Cette méthode signifie qu'environ 80% des températures de la période de référence sont considérées comme 'normales'."),
                        
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
  ),
  
  # --- BANDEAU ---
  tags$div(
    id = "cookie-banner",
    style = "display: none;",
    tags$p("Ce site utilise un cookie pour réaliser des statistiques de visites anonymes afin d'améliorer l'application."),
    tags$button(
      id = "accept-cookie-btn",
      type = "button",
      class = "btn btn-primary btn-sm",
      "J'ai compris"
    )
  )
)