
# Link in existing datasets
ln -s /data /PhytoFit/data
# Download any new datasets
yes | Rscript download_new_datasets.R
# Run the app
R -e "shiny::runApp('app.R', port = 7520, host = '0.0.0.0')"
