# PhytoFit

[![DOI](https://zenodo.org/badge/277295931.svg)](https://zenodo.org/badge/latestdoi/277295931)

This app can be used to display satellite chlorophyll concentration, and calculate statistics and model phytoplankton blooms for regions within custom polygons. See below for example in screen capture.  

<a target="_blank" href="images/screencap01.png">
<img src="images/screencap01.png" alt="screencap" width="200"/>
</a>

**Public host:** https://cioosatlantic.ca/phytofit/  


### How to cite

In publications, please include acknowledgements to [NASA OBPG](https://oceancolor.gsfc.nasa.gov) for the satellite data and the [BIO remote sensing group](https://github.com/BIO-RSG) for the application, and use this citation in the references:  

*Stephanie Clay, Chantelle Layton, & Emmanuel Devred. (2021). BIO-RSG/PhytoFit: First release (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.4770754*  

BibTeX format:  

    @misc{clay21,
      author       = {Clay, Stephanie and Layton, Chantelle and Devred, Emmanuel},
      title        = "PhytoFit",
      howpublished = "\url{https://github.com/BIO-RSG/PhytoFit}",
      year         = 2021
    }


## Prerequisites

1. Install the latest versions of R and RStudio.

2. Install the necessary packages:
```r
install.packages(c("fst", "shiny", "shinyWidgets", "shinyjs", "shinybusy", "leaflet", "leafpm", "quantreg", "minpack.lm", "sp", "ggplot2", "ggpp", "dplyr", "tidyr", "terra", "stringr", "RCurl", "sf", "fs"))
```

3. Restart R after the packages have been installed.


## Getting started

1. Download this repository one of two ways:  

- Option 1: Code --> Download ZIP  

- Option 2: Using git (this will make it easier to download updates in the future, by simply using the `git pull` command): Open git bash terminal, navigate to the folder where you want to download the repository, and type: `git clone https://github.com/BIO-RSG/PhytoFit.git`  

2. Open the PhytoFit repository in RStudio:  

- File --> Open Project --> Navigate to the PhytoFit folder and open "PhytoFit.Rproj"  

3. Download the datasets of your choice:  

- Open `00_download_new_datasets.R` from the PhytoFit folder. Set *ask_user=FALSE* to download all available datasets, or *ask_user=TRUE* to ask before downloading each one. Alternatively, you can run the script from the command line like:  `Rscript [script directory]/00_download_new_datasets.R 'false'`, filling in the [script directory] with the location where you stored the script. *'false'* is the ask_user argument, set to *'true'* for prompts.    

4. To update existing datasets:  

- Similar to the download script in step 3, open `00_update_datasets.R` and set the *ask_user* argument, or run from the command line (e.g. `Rscript [script directory]/00_update_datasets.R 'false'`. This will update the datasets you have already downloaded with the most recent copies (and download any years of data missing from your local directory).  


**WARNINGS:**  
- Data files will be downloaded to `data/[region]/` subfolders of the PhytoFit repository - Do NOT move them from there or the app will not be able to read them.  
- If possible, please keep the data files if you intend to use them in the future, rather than re-downloading them later, to avoid excessive traffic on the ftp server.  
- Any data that is < 3 months old is "Near Real Time" (NRT) quality. NRT data is replaced with "Science quality" data after it becomes available, following the 3-month lag. More info <a href="https://lance.modaps.eosdis.nasa.gov/data/difference.php">here</a>.  


## Running

Open app.R within RStudio, and click "Run app"


## Authors

* **Chantelle Layton** - *Initial concept, preliminary design, coding, and algorithm development/improvements*  
* **Stephanie Clay** - *Final app design and modifications, feature addition, new datasets, maintenance, and algorithm improvements*  
* **Emmanuel Devred** - *Scientific support, algorithm development/improvements, review and feature recommendations*  

## Acknowledgments

* **Andrea Hilborn** for many valuable suggestions


## Tips and recommendations

- **Selecting the region from the drop-down menu before loading data:**  
If your area of interest is contained within one of the smaller regions (e.g. Bay of Fundy or Gulf of Saint Lawrence), choose this in the "Region" drop-down menu to get the satellite/variable selections available for that region.  
Recommended satellite sensor and chl-a models:  
  - **Atlantic:** OC-CCI v6.0 POLY4 chl-a  
  - **Gulf of Saint Lawrence, 4km:** OC-CCI v6.0 POLY4 chl-a (**<font color="red">IMPORTANT</font>**: This is a different version of POLY4 than the Atlantic, trained with data from the GoSL only. To use the GoSL-POLY4 instead of the Atlantic-POLY4, you must select "Gulf of Saint Lawrence (4km)" from the region drop-down menu).  
Note that the "Gulf of Saint Lawrence, 1km" option in the region drop-down menu is an out-of-date product that was used in a single study, and should no longer be used.  
  - **Bay of Fundy (BoF):** OC-CCI v6.0 OCxSPM-cor chl-a  

- **Selecting a temporal composite length:**  
If you're concerned about low data coverage, you can use 4- or 8-day average composites, but you risk missing short-lived peaks in chl-a concentration. **Typically we use daily data**.  

- **Custom polygons:**  
  - Ensure your polygon overlaps at least several pixels. Chl-a can be highly variable over small spatial scales and outliers or erroneous pixels (particularly near the coast) could have a more negative effect on the results in polygons with fewer pixels. If your area of interest is very small, consider expanding your polygon to take an average of the surrounding area.  
  - If your region of interest it too close to the coast, it might not overlap any valid data pixels. Pixels near the coast should also be used with caution as they can be affected by land runoff or shallow water where the bottom is visible and interferes with the reflected light detected by the satellite.  

- **csv table of statistics:**  
The columns with "_log10" suffixes are the calculations performed on the log10-transformed data (i.e. chl-a pixel values are logged, the statistics are calculated, then the result is transformed back to linear space). Since chl-a is lognormally distributed, **we recommend that you use these statistics** - e.g. if you want a time series of daily average chl-a within the polygon, use the "mean_log10" column.  


## Links

[User guide](https://github.com/BIO-RSG/PhytoFit/blob/master/USERGUIDE.md) (In progress)  
[Chl-a model performance evaluation](https://bio-rsg.github.io/chla_model_performance_summary.html)  
[References and data sources](docs/references.md)  
[Using the raw (binned) data](https://github.com/BIO-RSG/PhytoFit/blob/master/fst_tutorial.md) (This is a quick tutorial explaining how the raw satellite chlorophyll data used in PhytoFit can be read into R and manipulated for other purposes)  
[Code updates affecting the algorithms](https://github.com/BIO-RSG/PhytoFit/blob/master/updates.md) (Summary of updates that affected the way the bloom metrics are calculated)  

