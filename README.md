# PhytoFit

This app can be used to display satellite chlorophyll concentration, and calculate statistics and model phytoplankton blooms for regions within custom polygons.

## Prerequisites

1. Install the latest versions of R and RStudio.

2. Run this command within RStudio to install the necessary libraries:
install.packages(c("shiny", "shinyWidgets", "shinyjs", "shinybusy", "htmlwidgets", "leaflet", "leaflet.extras", "oce", "quantreg", "minpack.lm", "sp", "ggplot2", "grid", "gridExtra", "dplyr", "geometry", "raster", "proj4"))

3. Run this command within RStudio to install a necessary fix for the leaflet.extras package:
remotes::install_github("bhaskarvk/leaflet.extras", ref = remotes::github_pull("184"))

4. Download this repository.

5. Download the data files from ftp://ftp.dfo-mpo.gc.ca/bometrics/PhytoFit/ and store them in the appropriate subdirectories inside your local copy of the repository (data/atlantic and data/pacific).


## Running

Open app.R within RStudio, and click "Run app"


## Authors

* **Chantelle Layton** - *Initial concept, design, and coding*

* **Stephanie Clay** - *Significant modifications - extra features, formatting, new datasets*

## Acknowledgments

* **Andrea Hilborn** for many valuable suggestions
