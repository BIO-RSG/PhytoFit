#!/bin/bash

cd /srv/shiny-server/phytofit
Rscript /srv/shiny-server/phytofit/scripts/00_download_new_datasets.R 'false'
Rscript /srv/shiny-server/phytofit/scripts/00_update_datasets.R
chown -R shiny:shiny /srv/shiny-server/
sudo service shiny-server restart
