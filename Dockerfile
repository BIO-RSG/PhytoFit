
FROM rocker/shiny
COPY . /srv/shiny-server/phytofit
WORKDIR /srv/shiny-server/phytofit

# From https://github.com/r-spatial/sf/issues/1377#issuecomment-739456122
RUN apt update && apt install -y \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev \
    r-base-dev r-cran-sf r-cran-raster r-cran-rjava cron

# Install R libraries
RUN Rscript -e 'install.packages(c("remotes", "fst", "shiny", "shinyWidgets", "shinyjs", "shinybusy", "leaflet", "stars", "leafem", "leafpm", "quantreg", "minpack.lm", "sp", "ggplot2", "ggpp", "dplyr", "tidyr", "raster", "RCurl", "sf", "fs"))'
RUN Rscript -e 'remotes::install_github("BIO-RSG/oceancolouR")'

# Install the script to fetch the data
COPY shell_scripts/fetch_data.sh /usr/local/bin/
RUN chmod u+x /usr/local/bin/fetch_data.sh
RUN /usr/local/bin/fetch_data.sh

# Install the script to fetch the data and install as cronjob
#COPY shell_scripts/crontab /etc/cron.d/fetch-data
#RUN #chmod 0744 /etc/cron.d/fetch-data

RUN ln -s /srv/shiny-server/phytofit/app.R /srv/shiny-server/app.R

#CMD ["Rscript", "-e", "options('shiny.port' = 3838, shiny.host = '0.0.0.0');"]

EXPOSE 3838
