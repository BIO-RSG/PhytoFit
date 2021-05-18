# PhytoFit

[![DOI](https://zenodo.org/badge/277295931.svg)](https://zenodo.org/badge/latestdoi/277295931)

This app can be used to display satellite chlorophyll concentration, and calculate statistics and model phytoplankton blooms for regions within custom polygons. See below for example in screen capture.

<a target="_blank" href="images/screencap01.png">
<img src="images/screencap01.png" alt="screencap" width="200"/>
</a>


## Prerequisites

1. Install the latest versions of R and RStudio.

2. Install the necessary libraries:
`fst, shiny, shinyWidgets, shinyjs, shinybusy, htmlwidgets, leaflet, leaflet.extras, leafem, quantreg, minpack.lm, rgdal, sp, ggplot2, grid, gridExtra, dplyr, geometry, raster, proj4`

3. Install a necessary fix for the leaflet.extras package, and a custom package (oceancolouR):
``` r
# install.packages("remotes")
remotes::install_github("bhaskarvk/leaflet.extras", ref = remotes::github_pull("184"))
remotes::install_github("BIO-RSG/oceancolouR")
```

4. Restart R after the packages and fix have been installed.

5. Download this repository one of two ways:  

- Code --> Download ZIP  
- Using git (this will make it easier to download updates in the future): Open git bash terminal, navigate to the folder where you want to download the repository, and type: `git clone https://github.com/BIO-RSG/PhytoFit.git`  

**WARNING: individual data files are generally < 50mb, but the entire dataset is several GB. Check if you have enough space on your drive.**


## Running

Open app.R within RStudio, and click "Run app"


## Authors

* **Chantelle Layton** - *Initial concept, design, and coding*

* **Stephanie Clay** - *Significant modifications - extra features, formatting, new datasets*

## Acknowledgments

* **Andrea Hilborn** for many valuable suggestions

## Links

[User guide](https://github.com/BIO-RSG/PhytoFit/blob/master/USERGUIDE.md) (In progress)  
[References and data sources](https://github.com/BIO-RSG/PhytoFit/blob/master/USERGUIDE.md#references-and-data-sources)  
[Using the raw (binned) data](https://github.com/BIO-RSG/PhytoFit/blob/master/fst_tutorial.md) (This is a quick tutorial explaining how the raw satellite chlorophyll data used in PhytoFit can be read into R and manipulated for other purposes)  
[Code updates affecting the algorithms](https://github.com/BIO-RSG/PhytoFit/blob/master/updates.md) (Summary of updates that affect the way the bloom metrics are calculated)  

