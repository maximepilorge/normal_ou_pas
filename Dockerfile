FROM rocker/shiny:4.4
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libpq-dev 
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinyjs", \
              "shinyWidgets", \
              "ggplot2", \
              "plotly", \
              "lubridate", \
              "dplyr", \
              "here", \
              "tidyr", \
              "DBI", \
              "RPostgres", \
              "pool", \
              "dbplyr"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2025-08-11"\
          )'
WORKDIR /home/shinyusr
COPY global.R global.R
COPY server.R server.R
COPY ui.R ui.R
COPY .Renviron .Renviron
COPY normal_ou_pas.Rproj normal_ou_pas.Rproj
COPY data data
COPY modules modules
COPY www www
CMD Rscript -e "shiny::runApp(appDir = '.', host = '0.0.0.0', port = 5000)"