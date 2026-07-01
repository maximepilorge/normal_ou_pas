FROM rocker/shiny:4.4
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libpq-dev \
    libtiff6 \
    libjpeg-turbo8 \
    libpng16-16 \
    libfreetype6 \
    libfontconfig1
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinyjs", \
              "shinyWidgets", \
              "bslib", \
              "ggplot2", \
              "plotly", \
              "lubridate", \
              "dplyr", \
              "here", \
              "tidyr", \
              "glue", \
              "DBI", \
              "RPostgres", \
              "pool", \
              "dbplyr", \
              "leaflet", \
              "ragg", \
              "plumber"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/noble/2025-08-11"\
          )'
WORKDIR /home/shinyusr
COPY global.R global.R
COPY server.R server.R
COPY ui.R ui.R
COPY data data
COPY modules modules
COPY www www
# Fichiers de utils/ nécessaires au RUNTIME (sourcés par global.R) : carte de
# partage, utilitaires partagés et table de référence des villes. Le reste de
# utils/ est le pipeline de données, exécuté hors conteneur.
COPY utils/render_partage.R utils/render_partage.R
COPY utils/helpers.R utils/helpers.R
COPY utils/villes_reference.R utils/villes_reference.R
CMD Rscript -e "shiny::runApp(appDir = '.', host = '0.0.0.0', port = 5000)"