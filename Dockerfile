FROM rocker/shiny:4.4
RUN R -e 'install.packages(c(\
              "shiny", \
              "ggplot2", \
              "plotly", \
              "lubridate", \
              "dplyr", \
              "shinyjs", \
              "here", \
              "tidyr"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2025-08-11"\
          )'
WORKDIR /home/shinyusr
COPY global.R global.R
COPY server.R server.R
COPY ui.R ui.R
COPY .Renviron .Renviron
COPY guess_climate.Rproj guess_climate.Rproj
COPY data data
COPY modules modules
CMD Rscript -e "shiny::runApp(appDir = '.', host = '0.0.0.0', port = 5000)"