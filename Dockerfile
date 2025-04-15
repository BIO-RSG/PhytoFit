
FROM rocker/shiny
COPY . /srv/shiny-server/phytofit
WORKDIR /srv/shiny-server/phytofit

# From https://github.com/r-spatial/sf/issues/1377#issuecomment-739456122
RUN apt update && apt install -y \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev \
    r-base-dev r-cran-sf r-cran-raster r-cran-rjava

# Install R libraries
RUN Rscript -e 'install.packages(c("remotes", "fst", "shiny", "shinyWidgets", "shinyjs", "shinybusy", "leaflet", "stars", "leafem", "leafpm", "quantreg", "minpack.lm", "sp", "ggplot2", "ggpp", "dplyr", "tidyr", "raster", "RCurl", "sf", "fs"))'
RUN Rscript -e 'remotes::install_github("BIO-RSG/oceancolouR")'

# prep data directory; when deploy in k8s this folder will be backended to a NAS containing all fst files
RUN mkdir /data
RUN chmod 0777 /data
RUN ln -s /data /srv/shiny-server/phytofit/data

# expose container port 3838
EXPOSE 3838

# serve the application on startup
CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');shiny::runApp('app.R');"]
