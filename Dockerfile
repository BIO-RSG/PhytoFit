
FROM rocker/shiny
COPY . /srv/shiny-server/phytofit
WORKDIR /srv/shiny-server/phytofit

# From https://github.com/r-spatial/sf/issues/1377#issuecomment-739456122
RUN apt update && apt install -y \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev \
    r-base-dev r-cran-sf r-cran-raster r-cran-rjava

# Install R libraries
RUN Rscript -e 'install.packages(c("fst", "shiny", "shinyWidgets", "shinyjs", "shinybusy", "leaflet", "stars", "leafem", "leafpm", "quantreg", "minpack.lm", "sp", "ggplot2", "ggpp", "dplyr", "tidyr", "raster", "RCurl", "sf", "fs"))'
RUN Rscript -e 'remotes::install_github("BIO-RSG/oceancolouR")'
