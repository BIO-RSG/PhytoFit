
FROM rocker/shiny
COPY . /PhytoFit
WORKDIR /PhytoFit

# From https://github.com/r-spatial/sf/issues/1377#issuecomment-739456122
RUN apt update && apt install -y \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev \
    r-base-dev r-cran-sf r-cran-raster r-cran-rjava

EXPOSE 7520

# Install R libraries
RUN Rscript -e 'install.packages(c("sf", "remotes", "dplyr", "lubridate", "fst", "shiny", "shinyWidgets", "shinyjs", "shinybusy", "htmlwidgets", "leaflet", "leaflet.extras", "leafem", "quantreg", "minpack.lm", "rgdal", "sp", "ggplot2", "grid", "gridExtra", "dplyr", "tidyr", "geometry", "raster", "proj4", "curl"))'
RUN Rscript -e 'remotes::install_github("bhaskarvk/leaflet.extras", ref = remotes::github_pull("184"))'
RUN Rscript -e 'remotes::install_github("BIO-RSG/oceancolouR")'

# Link in datasets directory
# RUN ln -s /datasets /PhytoFit/data

# Download data
# RUN Rscript download_new_datasets.R

# info on how to set host, port etc.:
# https://shiny.rstudio.com/reference/shiny/1.0.1/runApp.html
# RUN R -e "shiny::runApp('app.R', port = 7520, host = '0.0.0.0')"


