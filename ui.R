# ui.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(bslib)

# On charge le code UI de chaque module
source("modules/mod_quiz.R")
source("modules/mod_analyse.R")
# Onglet « Comparer » : fusion des anciens Comparaison + Carte (mod_comparer).
source("modules/mod_comparer.R")
# Onglet « Une journée » : analyse d'un jour précis (rang, fréquence, partage).
source("modules/mod_jour.R")

ui <- fluidPage( # On utilise tagList comme conteneur principal
  
  theme = bs_theme(version = 5),
  
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
      
      /* Met en valeur l'onglet actif */
      .navbar .nav-item .nav-link.active {
        color: #007bff !important; /* Couleur bleue vive pour l'icône et le texte */
      }
      
          /* Règles pour les écrans de petite taille (max 767px). */
      @media (max-width: 767px) {
        /* 1. On cache le bouton hamburger (Bootstrap 3 ET 5) */
        .navbar .navbar-toggle,
        .navbar .navbar-toggler {
          display: none !important;
        }

        /* 2. On force l'affichage du conteneur des liens */
        .navbar-collapse.collapse,
        .navbar-collapse {
          display: block !important;
          height: auto !important;
          overflow: visible !important;
        }

        /* 3. Barre d'onglets sur une seule ligne, défilable horizontalement.
              Avec 6 onglets, on évite que les libellés ne s'écrasent : chaque
              onglet garde une largeur lisible et l'utilisateur fait défiler. */
        .navbar-nav {
          display: flex;
          flex-direction: row !important;
          flex-wrap: nowrap !important;
          justify-content: flex-start;
          width: 100%;
          margin: 0;
          overflow-x: auto;
          -webkit-overflow-scrolling: touch;
          scrollbar-width: none; /* Firefox : barre de défilement masquée */
        }
        .navbar-nav::-webkit-scrollbar { display: none; } /* WebKit : idem */
        .navbar-nav > li {
            float: none;
        }

        /* 4. On cache le titre de l'app */
        .navbar-brand {
          display: none;
        }

        /* 5. Onglets compacts : icône au-dessus du libellé, sans rétrécissement */
        .navbar .nav-item .nav-link {
          display: flex;
          flex-direction: column;
          align-items: center;
          text-align: center;
          flex: 0 0 auto;
          min-width: 60px;
          font-size: 0.7rem;
          line-height: 1.1;
          padding: 6px 8px !important;
        }
        .navbar .nav-item .nav-link i {
          font-size: 1rem;
          margin-bottom: 2px;
        }

        /* 6. Confort tactile : barre latérale moins large, graphiques contenus */
        .bslib-sidebar-layout > .sidebar {
          --_sidebar-width: min(85vw, 320px);
        }
        .leaflet-container {
          max-height: 70vh;
        }
        .card { margin-bottom: 0.75rem; }
      }
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
    )')),

    # 4. Logique de partage de la carte de résultat du quiz
    tags$script(src = "partage.js"),

    # 5. Habillage du quiz « Série de 10 » (carte, progression, anneau, révélation)
    tags$link(rel = "stylesheet", href = "quiz_serie.css?v=5")
  ),
  
  
  # --- INTERFACE PRINCIPALE DE L'APPLICATION ---
  page_navbar(
    title = "Climat : Normal ou pas ?",
    # id + values des nav_panel : pilotables par nav_select (boucle inter-onglets)
    # et reflétés dans l'URL (?onglet=...) pour les permaliens (cf. server.R).
    id = "nav_principal",
    #position = "fixed-top",
    header = tagList(
      useShinyjs(),
      extendShinyjs(text = "
        shinyjs.disablePicker = function(id) { $('button[data-id=\"' + id + '\"]').addClass('disabled').prop('disabled', true); };
        shinyjs.enablePicker = function(id) { $('button[data-id=\"' + id + '\"]').removeClass('disabled').prop('disabled', false); };
      ", functions = c("disablePicker", "enablePicker"))
    ),
    
    # -- Onglets de l'application --
    nav_panel(
      "Le Quiz",
      mod_quiz_ui("quiz_1"),
      icon = icon("question-circle"),
      value = "quiz"
      ),
    nav_panel(
      "Comparaison",
      mod_comparer_ui("comparer_1"),
      icon = icon("chart-bar"),
      value = "comparer"
      ),
    nav_panel(
      "Une journée",
      mod_jour_ui("jour_1"),
      icon = icon("calendar-day"),
      value = "jour"
      ),
    nav_panel(
      "Évolution",
      mod_analyse_ui("analyse_1"),
      icon = icon("chart-line"),
      value = "evolution"
      ),
    nav_panel(
      "Méthodo",
      value = "methodo",
      fluidPage(
        titlePanel("Choix méthodologiques"),
        fluidRow(
          column(10, offset = 1,
                 
                 h3("Source et attribution des données 🌡️"),
                 p("La méthodologie de l'application repose sur des données publiques et des techniques de traitement géospatial standards."),
                 tags$ul(
                   tags$li(strong("Source des données : "), "Les températures proviennent du jeu de données ERA5-Land, accessible via le Copernicus Climate Change Service (C3S). C'est une base de données de 'réanalyse' climatique qui combine des observations passées avec des modèles météorologiques pour créer un enregistrement climatique complet et cohérent."),
                   tags$li(strong("Granularité : "), "Les données sont initialement téléchargées à une fréquence horaire puis agrégées pour conserver, pour chaque journée, la température maximale (max des 24 heures) et la température minimale (min des 24 heures). La période couverte s'étend de 1950 à aujourd'hui."),
                   tags$li(strong("Attribution des données à une ville : "), "Les données ERA5-Land sont fournies sur une grille géographique avec des mailles d'environ 9x9 km. Plutôt que de retenir une seule maille, l'application sélectionne ", strong("toutes les mailles qui recouvrent l'emprise de la commune"), " et les combine par une moyenne ", strong("pondérée par la part de surface communale couverte par chaque maille"), ". Une grande commune (Paris, Marseille…) est ainsi représentée par plusieurs mailles, ce qui reflète mieux son étendue réelle qu'un point unique."),
                   tags$li(strong("Important - Ce que cette température représente : "), "La valeur affichée est la température maximale journalière, ", strong("moyennée spatialement (pondérée par les surfaces) sur les mailles couvrant la commune"), ". Chaque maille ERA5-Land représentant déjà une moyenne sur environ 81 km² (9x9 km), cette valeur peut différer de la température que vous lisez sur un thermomètre chez vous ou de celle annoncée dans les prévisions météo, qui correspondent souvent à des mesures plus locales (aéroport, station météo spécifique).")
                 ),

                 hr(),

                 h3("Calcul et définition des 'Normales de saison' 📊"),
                 p("L'application se base sur le calcul de normales climatiques, conformément aux standards internationaux."),
                 tags$ul(
                   tags$li(strong("Périodes de référence : "), "Les normales sont calculées pour différentes périodes de 30 ans (ex: 1961-1990, 1991-2020), comme le recommande l'Organisation Météorologique Mondiale (OMM), afin de permettre la comparaison et de visualiser l'évolution du climat."),
                   tags$li(strong("Définition du 'normal' : "), "Pour définir ce qui est 'normal', l'application utilise la méthode des percentiles.",
                           tags$ol(
                             tags$li("Pour un jour donné (ex: le 15 août) et une période de référence, l'application analyse la distribution des températures maximales observées dans une ", strong("fenêtre de ±7 jours autour de cette date"), " (du 8 au 22 août), sur toutes les années de la période. Cette fenêtre glissante élargit l'échantillon (~450 valeurs au lieu d'une trentaine) et rend les seuils plus stables."),
                             tags$li("Elle calcule ensuite le 10e percentile (la valeur en-dessous de laquelle se trouvent les 10% des jours les plus froids) et le 90e percentile (la valeur au-dessus de laquelle se trouvent les 10% des jours les plus chauds)."),
                             tags$li("Une température est jugée ", strong("'Dans les normales de saison'"), " si elle se situe entre ces deux bornes (le 10e et le 90e percentile)."),
                             tags$li("Si elle est en-dehors de cette plage, elle est considérée comme 'En-dessous' ou 'Au-dessus' des normales.")
                           )
                   )
                 ),
                 
                 # Le paragraphe est maintenant à l'extérieur de la liste
                 p("Cette méthode signifie qu'environ 80% des températures de la période de référence sont considérées comme 'normales'."),
                 
                 hr(),

                 h3("Projections futures : « 2050 » et « 2100 » 🔮"),
                 p("Les repères « 2050 » et « 2100 » (boutons du quiz, barres projetées du graphique des jours de forte chaleur) s'appuient sur les projections climatiques de référence ", strong("Explore2"), " — le projet piloté par l'INRAE avec Météo-France et l'Office français de la biodiversité — alignées sur la ", strong("Trajectoire de Réchauffement de référence pour l'Adaptation au Changement Climatique (TRACC)"), " : ", strong("+2,7 °C en 2050"), " et ", strong("+4 °C en 2100"), " pour la France métropolitaine, par rapport à l'ère préindustrielle."),
                 tags$ul(
                   tags$li(strong("17 simulations climatiques : "), "Explore2 rassemble ", strong("17 chaînes de modèles"), " (6 modèles climatiques mondiaux croisés avec 9 modèles régionaux, issus de la base européenne EURO-CORDEX), corrigées de leurs biais sur les observations françaises (méthode ADAMONT, grille 8 km de Météo-France). Multiplier les modèles couvre l'éventail des sensibilités climatiques ; l'application affiche la ", strong("médiane des 17 simulations"), " (l'estimation centrale)."),
                   tags$li(strong("On projette le réchauffement, pas la température brute : "), "on calcule l'", strong("écart de réchauffement"), " entre la période future et le présent (1991-2020), sur la même fenêtre glissante de ±7 jours, et on l'", strong("ajoute aux normales ERA5"), " de l'application. Tout reste ainsi sur la même échelle que l'observé, et directement comparable avec lui."),
                   tags$li(strong("La distribution évolue, pas seulement la moyenne : "), "sur le graphique du quiz, le décalage est calculé séparément pour la moyenne, le 10e et le 90e percentile. Les extrêmes chauds montant plus vite, la zone « normale » ne fait pas que se translater : elle s'", strong("élargit vers le haut"), "."),
                   tags$li(strong("Des tendances, pas des prévisions datées : "), "ces valeurs décrivent le climat ", strong("moyen d'un horizon"), " (la « nouvelle normale » d'une époque), pas la météo d'une année précise. Elles donnent une direction et un ordre de grandeur, cohérents avec la trajectoire officielle d'adaptation de la France.")
                 ),

                 hr(),

                 h3("Aller plus loin 🔗"),
                 p("Cette application teste vos repères de température. Pour explorer plus largement le changement climatique en France et ses indicateurs, d'autres ressources de référence complètent cette approche :"),
                 tags$ul(
                   tags$li(
                     tags$a(href = "https://dataclimat.fr/", target = "_blank", rel = "noopener", "dataclimat.fr"),
                     " — visualisations et indicateurs du changement climatique en France."
                   ),
                   tags$li(
                     tags$a(href = "https://meteofrance.com/climathd", target = "_blank", rel = "noopener", "Météo-France — Climat HD"),
                     " — bilan du climat passé et futur en France (températures, vagues de chaleur…)."
                   ),
                   tags$li(
                     tags$a(href = "https://climate.copernicus.eu/", target = "_blank", rel = "noopener", "Copernicus Climate Change Service (C3S)"),
                     " — fournisseur du jeu de données ERA5-Land à l'origine de toutes les températures de l'application."
                   )
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
      ),
      icon = icon("book")
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