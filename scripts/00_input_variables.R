

#*******************************************************************************
# VARIABLES THAT CAN BE CHANGED ####

# Note: if you don't have any data files for these sensors/variables stored in your "data" folder, they won't
# appear in the drop-down menus when you load the app.

# The vector values (i.e. modisaquar2018.1, seawifsr2022.0...) must match the "sensor"
# in the fst filenames, and must NOT contain any dashes or underscores (only letters, numbers, or periods).
# The vector names (i.e. MODIS-Aqua R2018.1...) are the full names of the sensor,
# which will appear in the drop-down menu in the app.
# INCLUDE THE DATA VERSION (i.e. the reprocessing, for example R2022.0).
sensor_names <- c("OC-CCI v6.0" = "occciv6.0",
                  "MODIS-Aqua R2022.0" = "modisaquar2022.0",
                  "OLCI-S3A R2022.0" = "olcis3ar2022.0",
                  "OLCI-S3B R2022.0" = "olcis3br2022.0",
                  "CZCS R2014.0" = "czcsnimbus7r2014.0")

# List your variables here.
# The list element name (e.g. chloci, chlpoly4, sst...) must match the "variable" in your fst
# filenames, and must NOT contain any dashes or underscores (only letters, numbers, or periods).
#   - name_dropdown is the name that appears in the drop-down menu of the app.
#   - name_plottitle is the variable name appearing in the density plot and time series plot titles
#   - map_legend_title is a string formatted with HTML
#   - timeseries_ytitle is the y-axis title on the time series plot, formatted using bquote()
#   - colscale is a vector of values on the adjustable color scale below the map.
#   - colscale_log is the vector used in the case of a logged color scale. Values should be LINEAR, but spaced as though they are on a log scale.
#   - If log=FALSE, the data logging switches are set to FALSE and disabled when that variable is selected. If it's set to TRUE, the switches are enabled and you can change back and forth, with the scale defaulting to the log scale option (i.e. color scale and plot axes are displayed in log format, AND the data are logged to fit a time series model). Note that if log=TRUE, values <=0 are filtered out when the data is initially loaded.
#   - If cell_model_option=TRUE (e.g. for chlorophyll) you have the option of splitting the value into small, medium, or large cell sizes

chla_colscale <- seq(0,100,by=0.5)
chla_colscale_log <- c(0.005, seq(0.01, 0.05, by=0.01), seq(0.1, 0.5, by=0.1), seq(1, 5, by=1), seq(10, 50, by=10), 100)
variables <- list("chlpoly4"=list(name_dropdown="POLY4 chl-a (regional, band ratio)",
                                  name_plottitle="Chlorophyll-a",
                                  abbrev="Chl-a",
                                  map_legend_title="<center>Chl-a</br>[ mg/m<sup>3</sup> ]</center>",
                                  timeseries_ytitle=bquote("Chl-a [" * mg/m^3 * "]"),
                                  colscale=chla_colscale,
                                  colscale_log=chla_colscale_log,
                                  log=TRUE,
                                  cell_model_option=TRUE),
                  "chloci"=list(name_dropdown="OCI chl-a (global, band ratio)",
                                  name_plottitle="Chlorophyll-a",
                                  abbrev="Chl-a",
                                  map_legend_title="<center>Chl-a</br>[ mg/m<sup>3</sup> ]</center>",
                                  timeseries_ytitle=bquote("Chl-a [" * mg/m^3 * "]"),
                                  colscale=chla_colscale,
                                  colscale_log=chla_colscale_log,
                                  log=TRUE,
                                  cell_model_option=TRUE),
                  "chloccci"=list(name_dropdown="OC-CCI chl-a (global, band ratio)",
                                  name_plottitle="Chlorophyll-a",
                                  abbrev="Chl-a",
                                  map_legend_title="<center>Chl-a</br>[ mg/m<sup>3</sup> ]</center>",
                                  timeseries_ytitle=bquote("Chl-a [" * mg/m^3 * "]"),
                                  colscale=chla_colscale,
                                  colscale_log=chla_colscale_log,
                                  log=TRUE,
                                  cell_model_option=TRUE),
                  "chlgsmgs"=list(name_dropdown="GSM_GS chl-a (regional, semi-analytical)",
                                  name_plottitle="Chlorophyll-a",
                                  abbrev="Chl-a",
                                  map_legend_title="<center>Chl-a</br>[ mg/m<sup>3</sup> ]</center>",
                                  timeseries_ytitle=bquote("Chl-a [" * mg/m^3 * "]"),
                                  colscale=chla_colscale,
                                  colscale_log=chla_colscale_log,
                                  log=TRUE,
                                  cell_model_option=TRUE),
                  "chleof"=list(name_dropdown="EOF chl-a (regional, empirical)",
                                name_plottitle="Chlorophyll-a",
                                abbrev="Chl-a",
                                map_legend_title="<center>Chl-a</br>[ mg/m<sup>3</sup> ]</center>",
                                timeseries_ytitle=bquote("Chl-a [" * mg/m^3 * "]"),
                                colscale=chla_colscale,
                                colscale_log=chla_colscale_log,
                                log=TRUE,
                                cell_model_option=TRUE),
                  "sst"=list(name_dropdown="Sea Surface Temperature",
                             name_plottitle="Sea Surface Temperature",
                             abbrev="SST",
                             map_legend_title="<center>SST</br>[ &deg;C ]</center>",
                             timeseries_ytitle=bquote("SST [°C]"),
                             colscale=-3:35,
                             log=FALSE,
                             cell_model_option=FALSE))


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

composites <- c("Daily"="1",
                "4-day"="4",
                "8-day"="8")

latlon_methods <- c("Draw polygon on map" = "drawPoly",
                    "Type coordinates" = "typeCoords",
                    "Load shapefile" = "loadShapefile")

outliers <- c('None' = 'none',
              '+/- 2 StDev' = 'sd2',
              '+/- 3 StDev' = 'sd3',
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

ti_threshold_types <- c("% amplitude" = "percent_thresh",
                        "Constant threshold" = "constant_thresh")

# model fit table parameter names, depending on fitmethod, bloomShape, beta (code \u03B2 to get the symbol)
pnlist <- list("gauss"=list("symmetric"=c("Annual_Mean", "Annual_Median", "Annual_StDev", "t[start]", "t[max_real]", "t[max_fit]", "t[end]", "t[duration]", "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags", "B0", "h", "sigma", "beta", "failure_code", "RMSE", "RMSLE"),
                            "asymmetric"=c("Annual_Mean", "Annual_Median", "Annual_StDev", "t[start]", "t[max_real]", "t[max_fit]", "t[end]", "t[duration]", "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags", "B0[left]", "h[left]", "sigma[left]", "beta[left]", "B0[right]", "h[right]", "sigma[right]", "beta[right]", "failure_code", "RMSE", "RMSLE")),
               "roc"=c("Annual_Mean", "Annual_Median", "Annual_StDev", "t[start]", "t[max_real]", "t[end]", "t[duration]", "Magnitude", "Amplitude"),
               "thresh"=c("Annual_Mean", "Annual_Median", "Annual_StDev", "t[start]", "t[max_real]", "t[end]", "t[duration]", "Magnitude", "Amplitude", "Threshold"))

# formatting for tables overlaid on density and model fit plots when they are exported to png
tab_theme <- ttheme_gtminimal(core=list(fg_params=list(parse=TRUE,hjust=0,x=0.01),
                                        bg_params=list(fill="white",alpha=0.6)),
                              base_size=10,padding=unit(c(1,1), "mm"))


#*******************************************************************************
# Variables in settings.txt file (each vector must be in the same order)

# inputId for each widget to save in the settings
# (note that polystr will be manually added at the end because it's actually part of the "state" reactive list, not "input")
input_ids_to_save <- c("region", "sat_alg", "concentration_type", "cell_size_model1", "cell_size_model2", "year", "composite", 
                       "yearday_slide", "percent", "outlier", "dailystat", "pixrange1", "pixrange2",
                       "fitmethod", "bloomShape", "smoothMethod", "log_chla", "loessSpan", "t_range", "ti_limits", "tm_limits",
                       "ti_threshold_type", "ti_threshold_percent", "ti_threshold_constant", "tm", "beta", "use_weights", "rm_bkrnd",
                       "flag1_lim1", "flag1_lim2", "flag2_lim1", "flag2_lim2", "threshcoef",
                       "fullrunoutput_png", "fullrunyears", "fullrunboxes", "box", "custom_name", "polystr")

# how should the input be coerced when reloaded? 1 = numeric, 2 = character, 3 = logical
# note that some number inputs are actually character-type to get the textInput formatting
input_ids_variable_type <- c(2,2,2,2,2,1,2,
                             1,1,2,2,2,2,
                             2,2,2,3,1,1,1,1,
                             2,1,1,3,3,3,3,
                             2,2,2,2,1,
                             3,1,2,2,2,2)

# longer description of each inputId
input_ids_description <- c("Region", "Sensor and variable",
                           "Full chl-a concentration or subset based on cell size using one of two models",
                           "Cell size using model 1", "Cell size using model 2",
                           "Year", "Temporal binning", 
                         "Day of year (or first day of composite period)",
                         "Minimum composite percent coverage",
                         "Outlier detection method", "Composite statistic",
                         "Minimum value used in statistics and fit",
                         "Maximum value used in statistics and fit",
                         "Fit method", "Model fit shape", "Smoothing method", "Was the data log10-transformed?", "LOESS span",
                         "Allowed range of days for model fitting",
                         "Allowed range of days for model initiation",
                         "Allowed range of days for peak amplitude of model",
                         "Gaussian t[start] threshold method",
                         "Gaussian t[start] percent threshold",
                         "Gaussian t[start] constant threshold",
                         "Set t[max_fit] as variable parameter in Gaussian",
                         "Use beta parameter for linear background in Gaussian",
                         "Weight fit points by composite percent coverage for Gaussian",
                         "Remove background of Gaussian",
                         "Gaussian fit flag 1 lower limit",
                         "Gaussian fit flag 1 upper limit",
                         "Gaussian fit flag 2 lower limit",
                         "Gaussian fit flag 2 upper limit",
                         "Coefficient for threshold method",
                         "For full run, create png of each fit?",
                         "For full run, earliest and latest years in the list of years to process",
                         "For full run, list of boxes to process",
                         "Polygon name", "Custom polygon name", "Custom polygon string")


#*******************************************************************************
# DEFINE EXTRA VARIABLES ####

# maximum number of pixels allowed in a polygon, to prevent overloading memory
# or significantly slowing down calculations
max_pixels <- 5e5

# colors used in the map
# from "oceColorsJet" in "oce" package
# map_cols <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(100)
# from "roma" palette in "scico" package (colorblind-friendly)
map_cols <- colorRampPalette(c("#1A3399", "#2F5FAC", "#428FC0", "#5FC2D4", "#AEE8D5", "#E3EBB1", "#D9D26A", "#BA9532", "#9E5B19", "#7E1900"))(100)

# variable for using longer composites (4 or 8-day) data rather than daily
dvecs <- lapply(composites, function(x) {
  x <- as.numeric(x)
  sdoy_vec <- as.integer(seq(1,365,by=x))
  edoy_vec <- sdoy_vec+(x-1)
  sdoy_vec_len <- length(sdoy_vec)
  edoy_vec[sdoy_vec_len] <- min(edoy_vec[sdoy_vec_len],365)
  dlist <- lapply(1:length(sdoy_vec), function(i) {sdoy_vec[i]:edoy_vec[i]})
  list(sdoy_vec=sdoy_vec,dlist=dlist)
}) %>% setNames(paste0("X",composites))

# Load region data and predefined polygon data
reginfo <- readRDS("reginfo.rds")
predefined_polys <- sf::st_read("polygons.shp")
# restructure polygons into a list of simple features objects
allregs <- unique(predefined_polys$region)
predefined_polys <- lapply(1:length(allregs), function(i) {
  ptmp <- predefined_polys %>% dplyr::filter(region==allregs[i]) %>% dplyr::select(-region)
  if (is.na(ptmp$poly_id[1])) return(list())
  allgroups <- unique(ptmp$group)
  lapply(1:length(allgroups), function(j) {
    ptmp %>% dplyr::filter(group==allgroups[j])
  }) %>% setNames(allgroups)
}) %>% setNames(allregs)

# Choices for main region
regions <- names(reginfo)
names(regions) <- sapply(reginfo, "[[", "name")

# Make sure predefined_polys is in the same order as reginfo, and that each region
# has either a list of polygons or an empty list associated with it.
predefined_polys <- lapply(1:length(reginfo), function(i) {
  rname <- names(reginfo)[i]
  polys <- predefined_polys[[rname]]
  if (is.null(polys)) return(list())
  return(polys)
}) %>% setNames(names(reginfo))
# Make a separate variable with subregions merged within each region, for easier access to polygons.
ppolys_merged <- lapply(predefined_polys, FUN=do.call, what=rbind) %>% setNames(regions)

# Choices for polygon and "full run" set of multiple polygons
poly_choices <- multipoly_choices <- lapply(predefined_polys, function(x) {
  c(list(Custom=c(Custom="custom")),
    lapply(x, function(y) {v=y$poly_id; names(v)=y$name; v}))
}) %>% setNames(regions)

# Get a list of data files in your "data" folder and extract region/sensor/variable/year info from them
datasets <- data.frame(filename = list.files("data", recursive=TRUE, full.names=TRUE), stringsAsFactors = FALSE) %>%
  dplyr::mutate(basename = basename(filename)) %>%
  dplyr::filter(endsWith(filename,".fst") | endsWith(filename,".nc")) %>%
  tidyr::separate(col=basename, into=c("region","sensor","variable","year"), sep="_") %>%
  tidyr::drop_na() %>% # remove rows with missing values
  # list of possible datasets is restricted to the user-defined sensors and variables at the top of this script, and regions in reginfo
  dplyr::filter(region %in% regions & sensor %in% sensor_names & variable %in% names(variables))

if (nrow(datasets)==0) {
  
  # user has no datasets to view - set up some defaults so app doesn't crash
  data_last_updated <- "NEVER"
  default_region <- regions[1]
  sat_algs <- list(list("No datasets in storage"="empty_sat_alg")) %>% setNames(default_region)
  default_sat_algs <- sat_algs[[default_region]]
  years <- list(list(empty_sat_alg=c(0) %>% setNames(0))) %>% setNames(regions[1])
  default_years <- years[[regions[1]]]$empty_sat_alg
  default_variable <- variables$chloci
  
} else {
  
  # get the date that the data was last updated
  data_last_updated <- fs::file_info(datasets$filename)$modification_time
  most_recent <- which.max(as.numeric(data_last_updated))
  data_last_updated <- data_last_updated[most_recent]
  
  # for ordering sensor and variable names
  sensor_num <- 1:length(sensor_names) %>% pad0(len=3) %>% setNames(sensor_names)
  variable_num <- 1:length(variables) %>% pad0(len=3) %>% setNames(names(variables))
  datasets <- datasets %>%
    dplyr::mutate(year = as.numeric(gsub(".fst|.nc","",year)),
                  region_name = sapply(reginfo, "[[", "name")[match(region,names(reginfo))],
                  sensor_name = names(sensor_names)[match(sensor,sensor_names)],
                  variable_name = sapply(variables,FUN="[[","name_dropdown")[match(variable,names(variables))],
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
  
  # set up defaults
  default_region <- regions[1]
  default_sat_algs <- sat_algs[[default_region]]
  default_years <- years[[default_region]][[default_sat_algs[1]]]
  default_variable <- variables[[strsplit(default_sat_algs[1],split="_")[[1]][2]]]
  
}

if (default_variable$log) {
  default_colscale <- default_variable$colscale_log
} else {
  default_colscale <- default_variable$colscale
}



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
# BUTTON/WIDGET HELP TEXT ####

# helptext messages that appear above some of the buttons
bhelp <- list(
  settings_file = paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">Option 1: </font><br>",
                         "Load a file with predefined settings (.txt, created in PhytoFit): Browse to select, then click \"Apply settings\" and \"Load data\" below."),
  main_settings = paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">Option 2: </font><br>",
                         "Start selecting your settings below, then click \"Load data\" and adjust remaining settings as needed."),
  concentration_type = "View full satellite [chla], or use one of two models to separate satellite [chla] into concentrations of different phytoplankton cell sizes, and choose the cell size to view:",
  box_custom = "(Optional) Enter a name (use only alphanumeric characters, underscores, or periods) and click \"Apply\".",
  box_method = "Choose the method to create your polygon.",
  box_draw = "Draw polygon using the toolbar at the top left corner of the map.",
  box_type = paste0("Enter decimal latitudes and longitudes for vertices of polygon, separated by commas, then click \"Create polygon\". ",
                    "Use lon/lat < 0 for west/south. Lists must be the same length, with >2 values each, in the same order so that each latitude is paired with longitude.</br>"),
  box_shp = "Click \"Browse\" to find a shapefile. Select the \"shp\" file and all files with the same name but different extensions (e.g. dbf, prj, sbx...), then \"Open\". Shapefile must contain a Simple Features (sf) object. If the sf contains multiple polygons, a button will appear below to select the polygon you want to use. <b>WARNING: polygons with a large number of vertices may take several seconds to load.</b>",
  percent = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Minimum % coverage.</font></br>",
                   "Composites with lower coverage in the selected polygon will not be plotted on the density plot or time series, or used in the model fit."),
  outlier = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Outlying pixel detection method</font></br>",
                   "Remove outlying pixels before calculating statistics for the selected composite and polygon.</br>",
                   "SD = standard deviation</br>",
                   "IQR = interquartile range"),
  dailystat = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Statistic</font></br>",
                     "Calculate either the mean or median value of each composite, to be used in the time series and model fit."),
  pixrange = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Range of pixel values</font></br>",
                    "Choose the range of values allowed in the calculation of the statistics and model fit (pixels outside this range will be omitted).</br>",
                    "If a limit is left blank, it will be ignored."),
  bf_desc = "Fit a model to the points on the time series.",
  bf = "Choose fit method, model shape, and point smoothing method.",
  loessSpan = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">LOESS span</font></br>",
                     "Controls the degree of smoothing."),
  log_chla = "Fit the model to log10-transformed data?",
  fit_days = "Set the range of days to use in the model fit, and the limits of the day of peak amplitude and the first day of the bloom.",
  fit_tstart_method = "Select the method used to calculate t<sub>start</sub> :<br>Either a percentage of the curve amplitude between 0.01 and 90 (peak minus background), or a constant threshold between 0.01 and 5 (difference between the fitted curve and background value, calculated in linear space).",
  fit_ti_threshold_percent = "Set % curve amplitude to mark start of bloom.",
  fit_ti_threshold_constant = "Set increase over background to mark start of bloom.",
  fit_tmax = "Switch to ON to consider t<sub>max_fit</sub> a parameter in the regression.<br><b>WARNING: This will disable t<sub>start</sub> limits.</b>",
  fit_beta = "Switch to ON to allow background values to vary linearly as a function of day.",
  fit_weights = "Switch to ON to weight each point in the time series by percent coverage.",
  fit_bkrnd = "When calculating magnitude (area under the curve between t<sub>start</sub> and t<sub>end</sub>) and amplitude of the curve, should the background be removed first?",
  fit_flags = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flags</font></br>",
                     "Fits will be flagged if they meet certain criteria that indicate potential problems with the fit (NOTE: this does not affect the fit itself). Combinations of flags will be written as a single number (for example, 13 for flags 1 and 3). Click below for details. Optionally adjust the parameters of some flags."),
  fit_flag1 = "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 1: Amplitude ratio limits</font>",
  fit_flag2 = "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 2: Magnitude ratio limits</font>",
  fit_threshcoef = paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Threshold coefficient</font></br>",
                          "The start of the phytoplankton bloom is considered to be the point before t<sub>max</sub> ",
                          "when [chla] drops below a threshold for > 14 days.</br>",
                          "Threshold = chla<sub>median</sub> * threshold coefficient</br>",
                          "chla<sub>median</sub> is the median of days with sufficient % coverage within t<sub>range</sub>."),
  fit_threshcoefnote = "NOTE: If you opted to log the input data, chla<sub>median</sub> is calculated by taking the median of the logged data and transforming it back to linear space to compute the threshold. The threshold itself is then logged again to determine the bloom initiation.",
  fullrun = paste0("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Time series</font></br>",
                    "Select a series of years and the polygons you would like to process using the current settings, ",
                    "then click \"Run time series\" to generate the following:</br><ul><li>tables of statistics (.csv),</li><li>a single .csv file containing the fitted parameters for all selected years and polygons, and</li><li>a .csv file containing the settings used for the time series.</li></ul>",
                    "Files will be zipped to a folder following the naming convention ",
                    "<i>satellite_ region_ compositeLength_ years_ cellSizes_ variable_ fitmethod_ timecreated</i>.</br>",
                    "Make sure at least one polygon is selected.<br>",
                    "<b>When processing is complete and the new filename appears over the download button, click \"Download results (.zip)\".</b>")
)



#*******************************************************************************
# POPUP MESSAGES ####

# start screen popup
startup_popup <- paste0("This app can be used to display satellite data such as Chlorophyll-a and Sea Surface Temperature (SST), and model phytoplankton blooms. Use the controls in the left panel to visualize statistics for regions of interest or draw your own, and export data and graphs.<br><br>",
                      "<b>WARNING:</b> Any data that is < 3 months old is \"Near Real Time\" (NRT) quality. NRT data is replaced with \"Science quality\" data after it becomes available, following the 3-month lag. More info <a href=\"https://lance.modaps.eosdis.nasa.gov/data/difference.php\">here</a>.<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit\">Github repository</a> (All code and data can be accessed here)<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/docs/Userguide.pdf\">User guide</a> (In progress)<br><br>",
                      "<a href=\"https://bio-rsg.github.io/chla_model_performance_summary.html\">Chl-a model performance evaluation</a><br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/docs/fst_tutorial.md\">Using the raw data (.fst format, for R users only)</a><br>This is a quick tutorial explaining how the raw satellite chlorophyll data used in PhytoFit can be read into R and manipulated for other purposes.<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/docs/updates.md\">Code updates affecting the algorithms</a><br>Summary of past updates that affected the way the bloom metrics were calculated.<br><br>",
                      "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/docs/Userguide.pdf\">References and data sources</a><br><br>",
                      "<b>How to cite:</b><br>",
                      "In publications, please include acknowledgments to <a href=\"https://oceancolor.gsfc.nasa.gov/\">NASA OBPG</a> for the raw satellite data and the <a href=\"https://github.com/BIO-RSG\">BIO remote sensing group</a> for the application, and use this citation in the references:<br>",
                      "<i>Stephanie Clay, Chantelle Layton, & Emmanuel Devred. (2021). BIO-RSG/PhytoFit: First release (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.4770754</i><br><br>",
                      "<b>Contact:</b><br>",
                      "Stephanie.Clay@dfo-mpo.gc.ca<br><br>",
                      "<b>Dataset last updated:</b><br>", data_last_updated)

# gaussian bloom fit flag descriptions
gauss_flag_popup <- paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 1: Amplitude ratio</font></br>",
                           "Flagged if (amplitude<sub>fit</sub> / amplitude<sub>real</sub>) is outside the selected range (default 0.75-1.25).</br></br>",
                           "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 2: Magnitude ratio</font></br>",
                           "Flagged if (magnitude<sub>fit</sub> / magnitude<sub>real</sub>) is outside the selected range (default 0.85-1.15).</br></br>",
                           "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 3: Small sigma</font></br>",
                           "Flagged if sigma <= time resolution (time resolution = number of days in composite).</br></br>",
                           "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 4: t<sub>start</sub> on boundary</font></br>",
                           "Flagged if the calculated t<sub>start</sub> is on the boundary of the t<sub>start</sub> slider.</br></br>",
                           "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 5: t<sub>max</sub> on boundary</font></br>",
                           "Flagged if the calculated t<sub>max</sub> is on the boundary of the t<sub>max</sub> slider.</br></br>",
                           "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 6: t<sub>end</sub> on boundary</font></br>",
                           "Flagged if the calculated t<sub>end</sub> is on the boundary of the t<sub>range</sub> slider.")


#*******************************************************************************
# MONTH LABEL BAR AT TOP ####

# make a simple plot of month abbreviations for the top of the day of year slider
# just use a regular year, ignore the change in leap years because it's small
# this is just to make it easier to see where each month starts along the slider
pydays <- as.numeric(format(as.Date(paste0("2019",pad0(1:12,2),"01"),format="%Y%m%d"),"%j"))
pmonth <- ggplot() +
  # geom_hline(yintercept=0, color="grey") +
  geom_vline(xintercept=pydays, color="grey") +
  scale_x_continuous(limits=c(1,365), breaks=pydays, labels=month.abb, expand=c(0,0), position="top") +
  scale_y_continuous(limits=c(-1,0.2)) +
  theme(panel.background=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey"),
        axis.title=element_blank(),
        axis.text.x=element_text(angle=45, hjust=0, vjust=1, size=12),
        axis.text.y=element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm"))

