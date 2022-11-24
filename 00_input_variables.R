# Variable options in the sidebar
# Note: Regions and polygons are created using 00_create_new_region.R, and stored in reginfo.rds

#*******************************************************************************
# VARIABLES THAT CAN BE CHANGED - vectors of potential sensors and variables

# The vector names (i.e. modisaquar2018, seawifsr2018, oci, poly4...) must match the "sensor" and "variable"
# in the fst filenames.
# The vector values (i.e. MODIS-Aqua v2018, OCI chl-a...) are the full names of the sensor and variable,
# which will appear in the drop-down menus in the app.
# Note: if you don't have any data files for these stored in your "data" folder, they won't
# appear in the drop-down menus when you load the app.
# Note: SENSOR NAMES INCLUDE THE VERSION DATA (i.e. the reprocessing)

sensor_names <- c("MODIS-Aqua R2018.1" = "modisaquar2018",
                  "SeaWiFS R2018.0" = "seawifsr2018",
                  "VIIRS-SNPP R2018.0" = "viirssnppr2018",
                  "OLCI-A R2022.0" = "olcis3ar2022",
                  "OLCI-B R2022.0" = "olcis3br2022",
                  "SeaWiFS-modelled MODIS R2018.1" = "mseawifsr2018",
                  "VIIRS-modelled MODIS R2018.1" = "mviirssnppr2018",
                  "Multi-sensor R2018.1" = "multisensorr2018")

variable_names <- c("OCI chl-a (global, band ratio)" = "oci",
                    "POLY4 chl-a (regional, band ratio)" = "poly4",
                    "GSM_GS chl-a (regional, semi-analytical)" = "gsmgs",
                    "EOF chl-a (regional, empirical)" = "eof")


#*******************************************************************************
# VARIABLES THAT DON'T CHANGE

concentration_types <- list("Full chlorophyll-a concentration"="full",
                            "Small/Large cell concentrations"="model1",
                            "Small/Medium/Large cell concentrations"="model2")
cell_sizes_model1 <- list("Small"="small",
                          "Large"="large")
cell_sizes_model2 <- list("Small"="small",
                          "Medium"="medium",
                          "Large"="large")

intervals <- c("Daily"="daily",
               "Weekly (8day)"="weekly")

latlon_methods <- c("Draw polygon on map" = "drawPoly",
                    "Type coordinates" = "typeCoords",
                    "Load shapefile" = "loadShapefile")

outliers <- c('None' = 'none',
              '+/- 2 SD' = 'sd2',
              '+/- 3 SD' = 'sd3',
              '1.5 IQR' = 'iqr15',
              'Outer 0.01%' = 'q0001',
              'Outer 0.05%' = 'q0005',
              'Outer 0.1%' = 'q0010',
              'Outer 0.5%' = 'q0050',
              'Outer 1%' = 'q0100',
              'Outer 5%' = 'q0500',
              'Outer 10%' = 'q1000',
              'Outer 15%' = 'q1500')

dailystats <- c('Arithmetic mean' = 'average',
                'Median' = 'median')

fitmethods <- c("Shifted Gaussian" = "gauss",
                "Rate of Change" = "roc",
                "Threshold" = "thresh")

bloomShapes <- c("Symmetric" = "symmetric",
                 "Asymmetric" = "asymmetric")

smoothMethods <- c("No smoothing" = "nofit",
                   "LOESS" = "loess")

ti_threshold_types <- c("20% amplitude" = "percent_thresh",
                        "Constant threshold" = "constant_thresh")

# bloom fit table parameter names, depending on fitmethod, bloomShape, beta (code \u03B2 to get the symbol)
pnlist <- list("gauss"=list("symmetric"=c("Mean", "Median", "StDev", "t[start]", "t[max]", "t[end]", "t[duration]",
                                          "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags",
                                          "B0", "h", "sigma", "beta", "failure_code", "RMSE"),
                            "asymmetric"=c("Mean", "Median", "StDev", "t[start]", "t[max]", "t[end]", "t[duration]",
                                           "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags",
                                           "B0[left]", "h[left]", "sigma[left]", "beta[left]",
                                           "B0[right]", "h[right]", "sigma[right]", "beta[right]", "failure_code", "RMSE")),
               "roc"=c("Mean", "Median", "StDev", "t[start]", "t[max]", "t[end]", "t[duration]", "Magnitude", "Amplitude"),
               "thresh"=c("Mean", "Median", "StDev", "t[start]", "t[max]", "t[end]", "t[duration]", "Magnitude", "Amplitude", "Threshold"))


#*******************************************************************************
# Variables in settings.csv file (each vector must be in the same order)

# inputId for each widget to save in the settings
# (note that polylon and polylat will be manually added at the end because they are actually part of the "state" reactive list, not "input")
input_ids_to_save <- c("region", "sat_alg", "concentration_type", "cell_size_model1", "cell_size_model2", "year", "interval", "log_chla",
                       "yearday_slide", "percent", "outlier", "dailystat", "pixrange1", "pixrange2",
                       "fitmethod", "bloomShape", "smoothMethod", "loessSpan",
                       "t_range", "ti_limits", "tm_limits",
                       "ti_threshold_type", "ti_threshold_constant",
                       "tm", "beta", "use_weights", "rm_bkrnd",
                       "flag1_lim1", "flag1_lim2", "flag2_lim1", "flag2_lim2", "threshcoef",
                       "fullrunoutput_png", "fullrunoutput_statcsv",
                       "fullrunyears", "fullrunboxes", "box", "custom_name", "polylon", "polylat")

# how should the input be coerced when reloaded? 1 = numeric, 2 = character, 3 = logical
# note that some number inputs are actually character-type to get the textInput formatting
input_ids_variable_type <- c(2,2,2,2,2,1,2,3,1,1,2,2,2,2,2,2,2,1,1,1,1,2,1,3,3,3,3,2,2,2,2,1,3,3,1,2,2,2,1,1)

# types of widgets used for each input (need this to update them properly)
# selectInput=1, sliderInput=2, numericInput=3, textInput=4, radioButtons=5,
# checkboxInput=6, switchInput=7, pickerInput=8, radioGroupButtons=9
input_ids_widget_type <- c(1,1,5,9,9,1,1,7,2,3,1,1,4,4,1,1,1,3,2,2,2,5,3,7,7,7,6,4,4,4,4,3,6,6,2,8,1,0,0,0)

# longer description of each inputId
input_ids_description <- c("Region", "Sensor and Chlorophyll-a Algorithm",
                           "Full chl-a concentration or subset based on cell size using one of two models",
                           "Cell size using model 1", "Cell size using model 2",
                           "Year", "Temporal binning", "Chlorophyll-a logged",
                         "Day of year (or first day of 8day period if weekly data selected)",
                         "Minimum daily (or weekly) percent coverage",
                         "Outlier detection method", "Daily (or weekly) statistic",
                         "Minimum value used in statistics and fit",
                         "Maximum value used in statistics and fit",
                         "Fit method", "Bloom fit shape", "Smoothing method", "LOESS span",
                         "Allowed range of days for bloom fitting",
                         "Allowed range of days for bloom initiation",
                         "Allowed range of days for maximum concentration of bloom",
                         "Gaussian t[start] threshold method",
                         "Gaussian t[start] constant threshold",
                         "Set t[max] as variable parameter in Gaussian",
                         "Use beta parameter for linear background in Gaussian",
                         "Weight fit points by daily (or weekly) percent coverage for Gaussian",
                         "Remove background of Gaussian",
                         "Gaussian fit flag 1 lower limit",
                         "Gaussian fit flag 1 upper limit",
                         "Gaussian fit flag 2 lower limit",
                         "Gaussian fit flag 2 upper limit",
                         "Coefficient for threshold method",
                         "For full run, create png of each fit?",
                         "For full run, create csv of the daily (or weekly) stats for each year?",
                         "For full run, earliest and latest years in the list of years to process",
                         "For full run, list of boxes to process",
                         "Polygon name", "Custom polygon name", "Custom polygon longitudes", "Custom polygon latitudes")




#*******************************************************************************
# EXTRA VARIABLES ####

# colors used in the map
# from "oceColorsJet" in "oce" package
# map_cols <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
# "yellow", "#FF7F00", "red", "#7F0000"))(100)
# from "roma" palette in "scico" package (colorblind-friendly)
map_cols <- colorRampPalette(c("#1A3399", "#2F5FAC", "#428FC0", "#5FC2D4", "#AEE8D5", "#E3EBB1", "#D9D26A", "#BA9532", "#9E5B19", "#7E1900"))(100)

# variables for using weekly data rather than daily
doy_week_start <- as.integer(8*(0:45)+1) # note: this is the same for leap years, using NASA's system
doy_week_end <- c(doy_week_start[2:length(doy_week_start)] - 1, 365)
doys_per_week <- lapply(1:length(doy_week_start), function(i) {doy_week_start[i]:doy_week_end[i]})

# Load region data (name, existing polygons, lat/lon vectors, resolution)
reginfo <- readRDS("reginfo.rds")

# Extract region and polygon info
regions <- names(reginfo)
names(regions) <- sapply(reginfo, "[[", "name")
poly_choices <- lapply(reginfo, function(x) {v=c("custom",names(x$poly)); names(v)=c("Custom",sapply(x$poly,"[[","name")); v})
multipoly_choices <- lapply(reginfo, function(x) {v=c("custom",names(x$poly)); names(v)=c("Custom",names(x$poly)); v})
names(poly_choices) <- names(multipoly_choices) <- regions

# variables from the original 00_regionBoxes.R, for easier transition to new code
all_regions <- lapply(reginfo, function(x) lapply(x$poly, function(y) y[c("lat","lon")]))
full_names <- lapply(reginfo, function(x) sapply(x$poly, "[[", "name"))
poly_ID <- lapply(reginfo, function(x) names(x$poly))
abbrev <- lapply(reginfo, function(x) sapply(x$poly, "[[", "label"))
names(all_regions) <- names(full_names) <- names(poly_ID) <- names(abbrev) <- regions

# Make polygon objects to plot on the leaflet map for each region
original_polylist <- lapply(regions, function(reg) {
  poly_coord_list <- all_regions[[reg]]
  if (length(poly_coord_list) > 0) {
    original_polys <- lapply(1:length(poly_coord_list), function(k) {
      Polygon(coords=cbind(poly_coord_list[[k]]$lon, poly_coord_list[[k]]$lat), hole=TRUE)
    })
    original_polyIDs <- lapply(1:length(original_polys), function(k) {
      Polygons(list(original_polys[[k]]), toupper(poly_ID[[reg]][k]))
    })
    return(SpatialPolygons(original_polyIDs, 1:length(poly_coord_list)))
  } else {
    return(NULL)
  }
}) %>% setNames(regions)

# Get a list of data files in your "data" folder and extract region/sensor/variable/year info from them
datasets <- data.frame(filename = list.files("data", recursive=TRUE, full.names=TRUE), stringsAsFactors = FALSE) %>%
  dplyr::mutate(basename = basename(filename)) %>%
  dplyr::filter(endsWith(filename,".fst")) %>%
  tidyr::separate(col=basename, into=c("region","sensor","variable","year"), sep="_") %>%
  tidyr::drop_na() %>% # remove rows with missing values
  dplyr::filter(region %in% regions & sensor %in% sensor_names & variable %in% variable_names)

if (nrow(datasets)==0) {
  
  # user has no datasets to view
  sat_algs <- c(" " = "")
  years <- list(" " = "")
  data_last_updated <- "NEVER"
  
} else {
  
  # get the date that the data was last updated
  data_last_updated <- file.info(datasets$filename)$mtime
  most_recent <- which.max(as.numeric(data_last_updated))
  data_last_updated <- data_last_updated[most_recent]
  
  # for ordering sensor and variable names
  sensor_num <- 1:length(sensor_names) %>% pad0(len=3) %>% setNames(sensor_names)
  variable_num <- 1:length(variable_names) %>% pad0(len=3) %>% setNames(variable_names)
  datasets <- datasets %>%
    dplyr::mutate(year = as.numeric(gsub(".fst","",year)),
                  region_name = sapply(reginfo, "[[", "name")[match(region,names(reginfo))],
                  sensor_name = names(sensor_names)[match(sensor,sensor_names)],
                  variable_name = names(variable_names)[match(variable,variable_names)],
                  sensor_num = sensor_num[match(sensor,names(sensor_num))],
                  variable_num = variable_num[match(variable,names(variable_num))]) %>%
    tidyr::unite(col="sat_alg", sensor, variable, sep="_", remove=FALSE) %>%
    tidyr::unite(col="sat_alg_name", sensor_name, variable_name, sep=", ", remove=FALSE)
  
  # extract the sensor/variables for each region
  sat_algs <- lapply(regions, function(x) {
    tmp <- datasets %>%
      dplyr::filter(region==x) %>%
      dplyr::arrange(sensor_num, variable_num) %>%
      dplyr::distinct(sat_alg, sat_alg_name)
    tmpv <- tmp$sat_alg
    names(tmpv) <- tmp$sat_alg_name
    return(tmpv)
  }) %>% setNames(regions)
  
  # extract the years of data for each region/sensor/variable combination
  years <- lapply(regions, function(x) {
    tmp_sat_algs <- sat_algs[[x]]
    tmpx <- datasets %>% dplyr::filter(region==x) %>% dplyr::arrange(sensor_num, variable_num)
    tmpv <- lapply(tmp_sat_algs, function(y) {
      tmpy <- tmpx %>% dplyr::filter(sat_alg==y) %>% dplyr::distinct(year)
      ys <- sort(as.numeric(tmpy$year))
      names(ys) <- as.character(ys)
      return(ys)
    }) %>% setNames(tmp_sat_algs)
  }) %>% setNames(regions)
  
}

# set up defaults
default_region <- regions[1]
default_sat_algs <- sat_algs[[default_region]]
default_sensor <- strsplit(default_sat_algs[1], split="_")[[1]][1]
default_algorithm <- strsplit(default_sat_algs[1], split="_")[[1]][2]
default_years <- years[[default_region]][[default_sat_algs[1]]]


#*******************************************************************************
# HTML STYLING ####

# Default sidebar widget width in pixels
widget_width <- NULL#"180px"

# Style for leaflet map date marker
tag.map.title <- tags$style(HTML("
                          .leaflet-control.map-title {
                            position: absolute;
                            white-space: nowrap;
                            left: 100%;
                            text-align: left;
                            padding-left: 10px; 
                            padding-right: 10px; 
                            background: rgba(255,255,255,0.75);
                            font-weight: bold;
                            font-size: 32px;
                          }"))

# "Accordion" button styles
button_style <- "background-image: linear-gradient(#ddd, #eee);
                 color: #182;
                 padding: 5px 0px;
                 width: 100%;
                 text-align: center;
                 font-size: 18px;
                 border: none;
                 outline-style: solid;
                 outline-color: #fff;
                 outline-width: 1px;"

help_text_style <- "white-space: normal; font-size: 10px;"
label_text_style <- "white-space: normal; font-size: 14px; color: #555555; font-weight: bold; margin-bottom: 1px; margin-top: -10px;"
label_text_style_main_options <- "white-space: normal; font-size: 10px; color: #555555; margin-bottom: 1px; margin-top: -10px;"
error_text_style <- "white-space: normal; font-size: 10px; color: #ee0022; font-weight: bold;"

# Remove polygon programmatically (instead of making the user manually delete it
# with the draw toolbar) -- this is used when, for example, the user selects a
# different polygon, to make the old polygon disappear.
# This variable is called at the top of the UI
remove_custom_poly <- tags$script(HTML(
  "Shiny.addCustomMessageHandler(
          'removeleaflet',
          function(x){
            console.log('deleting',x)
            // get leaflet map
            var map = HTMLWidgets.find('#' + x.elid).getMap();
            // remove
            map.removeLayer(map._layers[x.layerid])
          })
        "
))

# Called at the top of the UI wrapped in HTML(), this variable:
#       - styles the horizontal bar in the sidebar,
#       - reduces padding inside widget boxes
#       - reduces padding between widget boxes
#       - adjusts padding between inline radioButton options
sidebar_tags_style <- "hr {border-top: 1px solid #bbbbbb;}
                       .form-control { padding:3px 3px;}
                       .radio-inline {margin-right: -5px;}"


#*******************************************************************************
# START SCREEN POPUP MESSAGE ####

startup_popup <- paste0("This app can be used to display satellite chlorophyll concentration and model phytoplankton blooms. Use the controls in the left panel to visualize statistics for DFO regions of interest or draw your own, and export data and graphs.<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit\">Github repository</a> (All code and data can be accessed here)<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/USERGUIDE.md\">User guide</a> (In progress)<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/fst_tutorial.md\">Using the raw (binned) data</a><br>This is a quick tutorial explaining how the raw satellite chlorophyll data used in PhytoFit can be read into R and manipulated for other purposes.<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/updates.md\">Code updates affecting the algorithms</a><br>Summary of updates that affect the way the bloom metrics are calculated.<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/USERGUIDE.md#references-and-data-sources\">References and data sources</a><br><br>",
                      "<b>How to cite:</b><br>",
                      "In publications, please include acknowledgments to <a href=\"https://oceancolor.gsfc.nasa.gov/\">NASA OBPG</a> for the raw satellite data and the <a href=\"https://github.com/BIO-RSG\">BIO remote sensing group</a> for the application, and use this citation in the references:<br>",
                      "<i>Stephanie Clay, Chantelle Layton, & Emmanuel Devred. (2021). BIO-RSG/PhytoFit: First release (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.4770754</i><br><br>",
                      "<b>Contact:</b><br>",
                      "Stephanie.Clay@dfo-mpo.gc.ca<br><br>",
                      "<b>Dataset last updated:</b><br>", data_last_updated)


