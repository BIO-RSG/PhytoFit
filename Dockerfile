FROM rocker/geospatial:latest

COPY . /srv/shiny-server/phytofit
WORKDIR /srv/shiny-server/phytofit

# Install R libraries
RUN Rscript -e 'install.packages(c("remotes", "fst", "shiny", "leaflet", "leafem", "quantreg", "sp", "ggplot2", "dplyr", "tidyr", "raster", "RCurl", "sf", "fs"))'
RUN Rscript -e 'remotes::install_version("shinyWidgets", upgrade = "never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("shinyjs", upgrade = "never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy", upgrade = "never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("leafpm", upgrade = "never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("minpack.lm", upgrade = "never", version = "1.2.4")'
RUN Rscript -e 'remotes::install_version("ggpp", upgrade = "never", version = "0.5.8")'
RUN Rscript -e 'remotes::install_github("BIO-RSG/oceancolouR")'

# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/phytofit/data

# expose container port 3838
EXPOSE 3838

# serve the application on startup
CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');shiny::runApp('app.R');"]
