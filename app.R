# clear memory to free up space
gc()

# LIBRARIES ####

# suppress some repetitive warnings when using addRasterImage in leafletProxy
options("rgdal_show_exportToProj4_warnings"="none")
library(fst)            # for speedier data file loading
library(shiny)
library(shinyWidgets)   # for updating buttons
library(shinyjs)        # for enabling/disabling download buttons
library(shinybusy)      # to show progress when processing a full time series
library(htmlwidgets)    # to use saveWidget to save the map
library(leaflet)        # for creating map
library(leaflet.extras) # for custom polygon drawings on map
library(leafem)         # mouse coordinates at top of map
library(quantreg)       # used in the fitting models
library(minpack.lm)     # to use nlsLM for asymmetric gaussian fit
library(rgdal)          # needed for some other packages
library(sp)             # to use point.in.polygon to extract points within lat/lon boundaries
library(ggplot2)        # for the density plot and bloom fit plots
library(grid)           # for formatting tableGrobs overlaid on ggplots
library(gridExtra)      # for creating tableGrobs overlaid on ggplots
library(dplyr)          # for formatting data tables
library(geometry)       # to check if user-entered lat/lons make a polygon with area too large (degrees^2)
library(raster)         # to use rasters on the map instead of binned points (faster, less accurate)
library(proj4)          # to use custom projection with the map
library(oceancolouR)
# library(htmlTable)      # for making tables in popups
# library(geosphere)      # for calculating accurate distances between single point click and data point plotted on the map
# library(RSQLite)        # for reading satellite data from databases instead of .rda files
# library(dbplyr)         # also for databases

source("rateOfChange.R")    # rate of change (ROC) function for bloom fit
source("threshold.R")       # threshold function for bloom fit
source("gaussFit.R")        # gaussian function for bloom fit
source("00_regionBoxes.R")  # contains coordinates of boxes/polygons
source("full_run.R")        # contains function to run full time series with current settings
source("functions.R")       # extra functions


#*******************************************************************************
# VARIABLES ####

sensors <- c("MODIS 4km" = "modis",
             "VIIRS 4km" = "viirs",
             "SeaWiFS 4km" = "seawifs")

regions <- c("Atlantic"="atlantic",
             "Pacific"="pacific")

algorithms <- c("OCx (global, band ratio)"="ocx",
                "POLY4 (regional, band ratio)"="poly4",
                "GSM_GS (regional, semi-analytical)"="gsmgs",
                "EOF (regional, empirical)"="eof")

# years with available data for each sensor
years <- list("modis"=2003:2021,
              "viirs"=2012:2021,
              "seawifs"=1997:2010)
for (i in 1:length(years)) {names(years[[i]]) <- years[[i]]}
default_years <- years[["modis"]]

intervals <- c("Daily"="daily",
               "Weekly"="weekly")

fitmethods <- c("Shifted Gaussian" = "gauss",
                "Rate of Change" = "roc",
                "Threshold" = "thresh")

bloomShapes <- c("Symmetric" = "symmetric",
                 "Asymmetric" = "asymmetric")

smoothMethods <- c("No smoothing" = "nofit",
                   "LOESS" = "loess")

# bloom fit table parameter names, depending on fitmethod, bloomShape, beta (code \u03B2 to get the symbol)
pnlist <- list("gauss"=list("symmetric"=list("beta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                      "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]",
                                                      "B0", "h", "sigma", "beta", "Flags", "RMSE"),
                                             "nonbeta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                         "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]",
                                                         "B0", "h", "sigma", "Flags", "RMSE")),
                            "asymmetric"=list("beta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                       "Magnitude[real_left]", "Magnitude[fit_left]", "Amplitude[real_left]", "Amplitude[fit_left]",
                                                       "B0[left]", "h[left]", "sigma[left]", "beta[left]", "Flags[left]",
                                                       "Magnitude[real_right]", "Magnitude[fit_right]", "Amplitude[real_right]", "Amplitude[fit_right]",
                                                       "B0[right]", "h[right]", "sigma[right]", "beta[right]", "Flags[right]", "RMSE"),
                                              "nonbeta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                          "Magnitude[real_left]", "Magnitude[fit_left]", "Amplitude[real_left]", "Amplitude[fit_left]",
                                                          "B0[left]", "h[left]", "sigma[left]", "Flags[left]",
                                                          "Magnitude[real_right]", "Magnitude[fit_right]", "Amplitude[real_right]", "Amplitude[fit_right]",
                                                          "B0[right]", "h[right]", "sigma[right]", "Flags[right]", "RMSE"))),
               "roc"=c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                       "t[duration]", "Magnitude", "Amplitude"),
               "thresh"=c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                          "t[duration]", "Magnitude", "Amplitude", "Threshold"))


# variables for using weekly data rather than daily
doy_week_start <- as.integer(8*(0:45)+1) # note: this is the same for leap years, using NASA's system
doy_week_end <- c(doy_week_start[2:length(doy_week_start)] - 1, 365)
doys_per_week <- lapply(1:length(doy_week_start), function(i) {doy_week_start[i]:doy_week_end[i]})


# get the date that the data was last updated
data_last_updated <- file.info(list.files("data", pattern=".fst", full.names = TRUE, recursive = TRUE))$mtime
most_recent <- which.max(as.numeric(data_last_updated))
data_last_updated <- data_last_updated[most_recent]



#*******************************************************************************
# STYLING ####

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

# Called at the top of the UI, this variable:
#       - styles the horizontal bar in the sidebar,
#       - creates the multi-column styling in the "full run" polygon selection,
#       - reduces padding inside widget boxes
#       - reduces padding between widget boxes
sidebar_tags_style <- tags$style(HTML(
            "hr {border-top: 1px solid #bbbbbb;}
            .multicol {
                       font-size: 14px;
                       -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                       -moz-column-count: 2;    /* Firefox */ 
                       column-count: 2; 
                       -moz-column-fill: auto;
                       -column-fill: auto;
                     }
            .form-control { height:auto; padding:3px 3px;}"
))




#*******************************************************************************
# UI  ####

ui <- fluidPage(
    
    # For hiding download buttons if data hasn't been loaded yet
    useShinyjs(),
    
    # styling
    tags$head(sidebar_tags_style),
    
    # old polygon removal code
    tags$div(remove_custom_poly),
    
    chooseSliderSkin("Modern"),
    
    # App title
    titlePanel("Satellite Chlorophyll Data Visualization"),
    
    fluidRow(
        
        sidebarPanel(width = 3,
            
            # UI LOAD OPTIONS ####
            
            helpText("Satellite / spatial resolution",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "satellite",
                        label = NULL,
                        choices = sensors,
                        width = widget_width),
            helpText("Region",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "region",
                        label = NULL,
                        choices = regions,
                        width = widget_width),
            helpText("Chlorophyll algorithm",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "algorithm",
                        label = NULL,
                        choices = algorithms,
                        width = widget_width),
            helpText("**EOF data only exists in a box encompassing the Gulf of Saint Lawrence (41-53 N, 49-71 W)",
                     width = widget_width,
                     style = paste(help_text_style, "margin-bottom: 20px; margin-top: -15px;")),
            helpText("Year",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "year",
                        label = NULL,
                        choices = default_years,
                        selected = default_years[length(default_years)],
                        width = widget_width),
            helpText("Data composite length",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "interval",
                        label = NULL,
                        choices = intervals,
                        selected = "daily",
                        width = widget_width),
            helpText("Log chlorophyll",
                     width = widget_width,
                     style = label_text_style_main_options),
            switchInput(inputId = "log_chla",
                        label = HTML("log<sub>10</sub><i>chla</i>"),
                        value = TRUE,
                        onStatus = "success"),
            # this will be enabled if data is available for the
            # combination of variables selected above
            disabled(actionButton(inputId = "load",
                                  label = "Load data",
                                  style = button_style)),
            uiOutput("help_load",
                     width = widget_width,
                     style = "white-space: normal;"),
            br(),
            
            shinyjs::hidden(div(id="hiddenPanel",
            
            # UI MAP COLOUR SCALE ####
            
            helpText("Adjust map colour scale",
                     width = widget_width,
                     style = label_text_style),
            div(style="display: inline-block; vertical-align:top; width: 50px;",
                textInput(inputId = "zlim1",
                          label = NULL,
                          value = round(log10(0.05),2))),
            div(style="display: inline-block; vertical-align:top; width: 10px;",
                helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
            div(style="display: inline-block; vertical-align:top; width: 50px;",
                textInput(inputId = "zlim2",
                          label = NULL,
                          value = round(log10(20),2))),
            div(style="display: inline-block;vertical-align:top; width: 60px;",
                actionButton(inputId="applyzlim",
                             label="Apply",
                             style=button_style)),
            
            
            # UI YEAR DAY ####
            
            helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Choose day of year</font></br>",
                                 "Enter the day of year and click \"Go\", or drag the slider to view the map for that day. ",
                                 "Use the \"play/pause\" button on the slider to move through a sequence of daily/weekly chlorophyll maps automatically.<br>",
                                 "NOTE: If you are viewing weekly (8-day) data, the day of year will reset to the first day of the selected week.")),
                     width = widget_width,
                     style = help_text_style),
            # Enter numerically
            div(style="display: inline-block; vertical-align:top; align:center; width: 80px;",
                numericInput(inputId = "yearday_num",
                             label = NULL,
                             min = 1,
                             max = 365,
                             value = 1)),
            div(style="display: inline-block;vertical-align:top; width: 50px;",
                actionButton(inputId="daygo",
                             label="Go",
                             style=button_style)),
            # Slider input
            sliderInput(inputId = "yearday_slide",
                        label = NULL,
                        min = 1,
                        max = 365,
                        value = 1,
                        animate = animationOptions(interval=4000),
                        ticks = FALSE),
            
            
            # UI POLYGON ####
            
            actionButton(inputId="polygonButton",
                         label="Polygon",
                         "data-toggle"='collapse',
                         "data-target"='#polygonDiv',
                         style=button_style),
            
            div(id = 'polygonDiv',
                class="collapse",
                
            br(),
            
            uiOutput(outputId = "box"),
            
            # If custom polygon selected, enter a name for the polygon (optional),
            # and choose whether to draw polygons on the map or enter a list of
            # lat/lons manually
            conditionalPanel(condition = "input.box =='custom'",
                             helpText("(Optional) Enter a name and click \"Apply\".",
                                      width = widget_width,
                                      style = help_text_style),
                             div(style="display: inline-block; vertical-align:top; align:center; width: 110px;",
                                 textInput(inputId = "custom_name",
                                           label = NULL,
                                           value = NULL)),
                             div(style="display: inline-block; width: 60px",
                                 actionButton(inputId="applyname",
                                              label="Apply",
                                              style = button_style)),
                             helpText("Choose the method to create your polygon.",
                                      width = widget_width,
                                      style = help_text_style),
                             radioButtons(inputId = "latlon_method",
                                          label = NULL,
                                          choiceNames = c("Draw polygon on map",
                                                          "Type coordinates"),
                                          choiceValues = c("drawPoly", "typeCoords"),
                                          selected = "drawPoly",
                                          width = widget_width)),
            
            # Give instructions if user opts to draw polygon.
            conditionalPanel(condition = "input.box =='custom' && input.latlon_method =='drawPoly'",
                             helpText("Draw polygon using the toolbar at the top left corner of the map.",
                                      width = widget_width,
                                      style = help_text_style)),
            # If user selected the option to type lat/lon manually, two
            # numericInput boxes will appear, one for lats and one for lons
            conditionalPanel(condition = "input.box =='custom' && input.latlon_method =='typeCoords'",
                             helpText(HTML(paste0("Enter decimal latitudes and longitudes for vertices of polygon, separated by commas, then click \"Create polygon\". ",
                                                  "Use lon/lat < 0 for west/south. Lists must be the same length, with >2 values each, in the same order so that each latitude is paired with longitude. Example:</br>",
                                                  "List of latitudes: 42.6, 43, 42, 40.4, 40, 42.6</br>",
                                                  "List of longitudes: -61, -59, -55, -57, -60.4, -61")),
                                      width = widget_width,
                                      style = help_text_style),
                             textInput(inputId = "manual_lats",
                                       label = HTML("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">List of latitudes:</font>"),
                                       value = "",
                                       width = widget_width),
                             textInput(inputId = "manual_lons",
                                       label = HTML("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">List of longitudes:</font>"),
                                       value = "",
                                       width = widget_width),
                             actionButton(inputId = 'draw',
                                          label = 'Create polygon',
                                          width = widget_width,
                                          style = button_style),
                             uiOutput("help_latlon",
                                      width = widget_width,
                                      style = "white-space: normal;"),
                             br()),
            
            br()
            ),
            
            
            # UI STATISTICS ####
            
            actionButton(inputId="statsButton",
                         label="Statistics",
                         "data-toggle"='collapse',
                         "data-target"='#statsDiv',
                         style=button_style),
            
            div(id = 'statsDiv',
                class="collapse",
            
            br(),
            
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Minimum daily/weekly % coverage</font></br>",
                                 "Days with less than the minimum percent coverage in the selected polygon will not be plotted on the density plot or time series, or used in the bloom fit.")),
                     width = widget_width,
                     style = help_text_style),
            numericInput(inputId = 'percent',
                         label = NULL,
                         value = 10,
                         min = 0,
                         max = 100,
                         width = widget_width),
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Outlier detection method</font></br>",
                                 "SD = standard deviation</br>",
                                 "IQR = interquartile range")),
                     width = widget_width,
                     style = help_text_style),
            selectInput(inputId='outlier',
                        label = NULL,
                        choices = c('None' = 'none',
                                    '+/- 2 SD' = 'sd2',
                                    '+/- 3 SD' = 'sd3',
                                    '1.5 IQR' = 'iqr15'),
                        selected = 'none',
                        width = widget_width),
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Daily/weekly statistic</font></br>",
                                 "Choose to use either daily/weekly mean or median chlorophyll in the time series and bloom fit.")),
                     width = widget_width,
                     style = help_text_style),
            selectInput(inputId = 'dailystat',
                        label = NULL,
                        choices = c('Mean' = 'average',
                                    'Median' = 'median'),
                        selected = 'average',
                        width = widget_width),
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Range of pixel values</font></br>",
                                 "Choose the range of values allowed in the calculation of the statistics and bloom fit (pixels outside this range will be omitted).</br>",
                                 "If a limit is left blank, it will be ignored.")),
                     width = widget_width,
                     style = help_text_style),
            
            
            
            div(style="display: inline-block; vertical-align:top; width: 50px;",
                textInput(inputId = "pixrange1",
                          label = NULL,
                          value = NA)),
            div(style="display: inline-block; vertical-align:top; width: 10px;",
                helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
            div(style="display: inline-block; vertical-align:top; width: 50px;",
                textInput(inputId = "pixrange2",
                          label = NULL,
                          value = NA)),
            div(style="display: inline-block;vertical-align:top; width: 60px;",
                actionButton(inputId="applypixrange",
                             label="Apply",
                             style=button_style)),
            
            br(),
            br()
            ),
            
            
            # UI BLOOM FIT ####
            
            actionButton(inputId="bfButton",
                         label="Bloom fit",
                         "data-toggle"='collapse',
                         "data-target"='#bfDiv',
                         style=button_style),
            
            div(id = 'bfDiv',
                class="collapse",
            
            br(),
            helpText("Choose fit method, bloom shape, and point smoothing method.",
                     width = widget_width,
                     style = help_text_style),
            selectInput(inputId = 'fitmethod',
                        label = NULL,
                        choices = fitmethods,
                        selected = 'gauss',
                        width = widget_width),
            selectInput(inputId = 'bloomShape',
                        label = NULL,
                        choices = bloomShapes,
                        selected = 'symmetric',
                        width = widget_width),
            selectInput(inputId = 'smoothMethod',
                        label = NULL,
                        choices = smoothMethods,
                        selected = 'nofit',
                        width = widget_width),
            conditionalPanel(condition = "input.smoothMethod == 'loess'",
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">LOESS span</font></br>",
                                                  "Controls the degree of smoothing.")),
                                      width = widget_width,
                                      style = help_text_style),
                             numericInput(inputId = 'loessSpan',
                                          label = NULL,
                                          value = 0.3,
                                          min = 0.04,
                                          max = 1,
                                          step = 0.02,
                                          width = widget_width)),
            helpText("Set the range of days to use in the bloom fit, and the limits of the day of maximum concentration and the first day of the bloom.",
                     width = widget_width,
                     style = help_text_style),
            sliderInput(inputId = 't_range',
                        label = HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">t<sub>range</sub></font>"),
                        min = 1,
                        max = 365,
                        value = c(31,274),
                        ticks = FALSE),
            sliderInput(inputId = 'tm_limits',
                        label = HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">t<sub>max</sub></font>"),
                        min = 1,
                        max = 365,
                        value = c(91,181),
                        ticks = FALSE),
            sliderInput(inputId = 'ti_limits',
                        label = HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">t<sub>start</sub></font>"),
                        min = 1,
                        max = 365,
                        value = c(60,151),
                        ticks = FALSE),
            conditionalPanel(condition = "input.fitmethod == 'gauss'",
                             # helpText(HTML("Set the threshold for defining the start of the bloom (percentage of the amplitude of the curve).</br>Possible values: 1 - 50"),
                             #          width = widget_width,
                             #          style = help_text_style),
                             # numericInput(inputId = 'ti_threshold',
                             #              label = NULL,
                             #              value = 20,
                             #              min = 1,
                             #              max = 50,
                             #              step = 1,
                             #              width = widget_width),
                             # conditionalPanel(condition = "input.bloomShape == 'asymmetric'",
                             #                  helpText(HTML("Similarly, set the threshold for defining the end of the bloom."),
                             #                           width = widget_width,
                             #                           style = help_text_style),
                             #                  numericInput(inputId = 'tt_threshold',
                             #                               label = NULL,
                             #                               value = 20,
                             #                               min = 1,
                             #                               max = 50,
                             #                               step = 1,
                             #                               width = widget_width)),
                             helpText(HTML("Switch to ON to consider t<sub>max</sub> a parameter in the regression."),
                                      width = widget_width,
                                      style = help_text_style),
                             switchInput(inputId = 'tm',
                                         label = HTML('t<sub>max</sub>'),
                                         value = FALSE,
                                         onStatus = "success"),
                             helpText(HTML("Switch to ON to allow background chlorophyll B<sub>0</sub> to vary as a function of day."),
                                      width = widget_width,
                                      style = help_text_style),
                             switchInput(inputId = 'beta',
                                         label = '\u03B2t',
                                         value = FALSE,
                                         onStatus = "success"),
                             helpText("Switch to ON to weight each daily/weekly point in the fit by percent coverage.",
                                      width = widget_width,
                                      style = help_text_style),
                             switchInput(inputId = 'use_weights',
                                         label = 'weights',
                                         value = FALSE,
                                         onStatus = "success"),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flags</font></br>",
                                                  "Fits will be flagged if they meet the criteria below, which indicate potential problems with the fit (NOTE: this does not affect the fit itself). Combinations of flags will be written as a single number (so the possible values are 1, 2, 3, 12, 13, 23, or 123).")),
                                      width = widget_width,
                                      style = help_text_style),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 1: Amplitude ratio</font></br>",
                                                  "Flag 1 will occur if the ratio of amplitudes (amplitude of the fitted curve at t<sub>max</sub>",
                                                  " over the amplitude of the real values at t<sub>max</sub>) is outside the range selected below.")),
                                      width = widget_width,
                                      style = help_text_style),
                             div(style="display: inline-block; vertical-align:top; width: 50px;",
                                 textInput(inputId = "flag1_lim1",
                                           label = NULL,
                                           value = 0.75)),
                             div(style="display: inline-block; vertical-align:top; width: 10px;",
                                 helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
                             div(style="display: inline-block; vertical-align:top; width: 50px;",
                                 textInput(inputId = "flag1_lim2",
                                           label = NULL,
                                           value = 1.25)),
                             div(style="display: inline-block;vertical-align:top; width: 60px;",
                                 actionButton(inputId="apply_flag1_lim",
                                              label="Apply",
                                              style=button_style)),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 2: Magnitude ratio</font></br>",
                                                  "Flag 2 will occur if the ratio of magnitudes (area under the fitted curve over the area under the real values)",
                                                  " is outside the range selected below.")),
                                      width = widget_width,
                                      style = help_text_style),
                             div(style="display: inline-block; vertical-align:top; width: 50px;",
                                 textInput(inputId = "flag2_lim1",
                                           label = NULL,
                                           value = 0.85)),
                             div(style="display: inline-block; vertical-align:top; width: 10px;",
                                 helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
                             div(style="display: inline-block; vertical-align:top; width: 50px;",
                                 textInput(inputId = "flag2_lim2",
                                           label = NULL,
                                           value = 1.15)),
                             div(style="display: inline-block;vertical-align:top; width: 60px;",
                                 actionButton(inputId="apply_flag2_lim",
                                              label="Apply",
                                              style=button_style)),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 3: Small sigma</font></br>",
                                                  "Flag 3 will occur if sigma (the parameter controlling the width of the curve)",
                                                  " is the same size or smaller than the time resolution (1 for daily data,",
                                                  " 8 for weekly data).")),
                                      width = widget_width,
                                      style = help_text_style)),
            conditionalPanel(condition = "input.fitmethod == 'thresh'",
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Threshold coefficient</font></br>",
                                                  "The start of the bloom is considered to be the point before t<sub>max</sub> ",
                                                  "when chlorophyll concentration drops below a threshold for > 14 days.</br>",
                                                  "Threshold = chla<sub>med</sub> * threshold coefficient</br>",
                                                  "chla<sub>med</sub> = UNLOGGED median chlorophyll</br>",
                                                  "NOTE: If log<sub>10</sub><i>chla</i> is ON, the resulting theshold will be logged.")),
                                      width = widget_width,
                                      style = help_text_style),
                             numericInput(inputId = 'threshcoef',
                                          label = NULL,
                                          value = 1.05,
                                          min = 1.00,
                                          width = widget_width)),
            
            ),
            
            br(),
            br(),
            br(),
            
            # UI SAVE OPTIONS ####
            
            disabled(downloadButton(outputId = "savesettings",
                                    label = "Save settings (.txt)",
                                    style = button_style)),
            
            hr(),
            
            helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Time series</font></br>",
                                 "Select a series of years and the polygons you would like to process, ",
                                 "then click \"Run time series\" to generate the following:</br>",
                                 "<ul>",
                                    "<li>time series plots (.png),</li>",
                                    "<li>tables of statistics (.csv),</li>",
                                    "<li>a single .csv file containing the fitted parameters for all selected years and polygons, and</li>",
                                    "<li>a .txt file containing the settings used for the time series, for reference.</li>",
                                 "</ul>",
                                 "The settings used in the time series will be the current selections for satellite, ",
                                 "region, algorithm, interval, log<sub>10</sub><i>chla</i> ON/OFF, statistics, and bloom fit. Files will be zipped",
                                 " to a folder following the naming convention <i>satellite_ region_ algorithm_ years_ interval_ (un)loggedChla_ fitmethod_ timecreated</i>.</br>",
                                 "Make sure at least one polygon is selected.<br>",
                                 "When processing is complete and the new filename appears over the download button, click \"Download results (.zip)\".")),
                     width = widget_width,
                     style = help_text_style),
            sliderInput(inputId = "fullrunyears",
                        label = NULL,
                        min = default_years[1],
                        max = default_years[length(default_years)],
                        value = c(default_years[1],default_years[length(default_years)]),
                        ticks = FALSE,
                        step = 1,
                        sep = ""),
            
            radioButtons(inputId = "fullrunallboxes",
                         label = NULL,
                         choices = list("Process all polygons"=TRUE,
                                        "Select polygons to process"=FALSE),
                         selected = TRUE),
            conditionalPanel(condition = "input.fullrunallboxes == 'FALSE'",
                             uiOutput(outputId = "fullrunboxes")),
            br(),
            actionButton(inputId = "fullrun_process",
                         label = "Run time series",
                         style = button_style),
            uiOutput("fullrun_fname",
                     width = widget_width,
                     style = "white-space: normal;"),
            disabled(downloadButton(outputId = "fullrun_download",
                                    label = "Download results (.zip)",
                                    style = button_style)),
            br()
            
            ))
            
        ), # close column
        
        # UI DISPLAY ####
        
        mainPanel(width = 9,
               leafletOutput(outputId = 'fullmap',
                             height = '800px'),
               disabled(downloadButton(outputId = "savemap",
                                       label = "Download map (.html)",
                                       style = button_style)),
               br(),
               br(),
               htmlOutput(outputId = "poly_title"),
               br(),
               plotOutput(outputId = 'chla_hist',
                          height = '360px'),
               disabled(downloadButton(outputId = "savedensplot",
                                       label = "Download density plot (.png)",
                                       style = button_style)),
               br(),
               br(),
               plotOutput(outputId = 'bloomfit',
                          height = '440px',
                          click = 'bloomfit_click'),
               disabled(downloadButton(outputId = "savebloomfit",
                                       label = "Download time series plot of daily/weekly chlorophyll (.png)",
                                       style = button_style)),
               disabled(downloadButton(outputId = "saveannualstats",
                                       label = "Download time series table of statistics (.csv)",
                                       style = button_style)),
               disabled(downloadButton(outputId = "savebloomparams",
                                       label = "Download fitted bloom parameters (.csv)",
                                       style = button_style)),
               br(),
               br()
        ) # closes column for plots
    )
    
)

#*******************************************************************************
# SERVER ####

server <- function(input, output, session) {
    
    # Create a list of reactive values to collect widget input.
    state <- reactiveValues()
    
    # Set this variable to TRUE so the plots know that there is no custom
    # box/polygon data available yet.
    state$null_rchla <- TRUE
    
    # Set these to FALSE - they will switch to TRUE if the user enters coordinates
    # to draw a polygon that is too large (>500 degrees square) or if there is
    # a problem with the format of the coordinates.
    state$latlon_invalid <- FALSE
    state$latlon_toolarge <- FALSE
    
    # initialize some defaults so the code doesn't break
    state$data_loaded <- FALSE
    state$satellite <- "modis"
    state$algorithm <- "ocx"
    state$year <- 2020
    state$interval <- "daily"
    state$log_chla <- TRUE
    state$doy_vec <- 1:365
    state$sschla <- matrix(nrow=1,ncol=365)
    state$zlim1 <- log10(0.05)
    state$zlim2 <- log10(20)
    state$flag1_lim1 <- 0.75
    state$flag1_lim2 <- 1.25
    state$flag2_lim1 <- 0.85
    state$flag2_lim2 <- 1.15
    state$pixrange1 <- -Inf
    state$pixrange2 <- Inf
    state$latlon_method <- "drawPoly"
    state$draw_toolbar <- TRUE
    state$newpoly <- NULL
    state$editedpoly <- NULL
    state$fullrunboxes <- "custom"
    state$fullrun_fname <- NULL
    state$ti_threshold <- 0.2
    state$tt_threshold <- 0.2
    # default box - need this so when everything first evaluates, some functions
    # dependent on it know what to do (since the box option doesn't appear until
    # the box UI renders, then evaluates AFTER that)
    state$box <- "custom"
    state$custom_name <- ""
    state$fullrunboxes <- "custom"
    state$help_load_txt <- ""
    
    # if "tm" is on (i.e. day of max. concentration is a parameter in the bloom fit),
    # then the start of the bloom can't be restricted, so turn off that slider button
    observe({
        toggleState("ti_limits", condition = !input$tm)
    })
    
    
    # START SCREEN POPUP ####
    
    observe({
        showModal(modalDialog(
                    title = "Satellite Chlorophyll Data Visualization",
                    HTML(paste0("This app can be used to display satellite chlorophyll concentration and model phytoplankton blooms. Use the controls in the left panel to visualize statistics for DFO regions of interest or draw your own, and export data and graphs.<br><br>",
                                "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/USERGUIDE.md\">USER GUIDE</a> (In progress)<br><br>",
                                "<a href=\"https://github.com/BIO-RSG/PhytoFit/blob/master/fst_tutorial.md\">Using the raw (binned) data</a><br>This is a quick tutorial explaining how the raw satellite chlorophyll data used in PhytoFit can be read into R and manipulated for other purposes.<br><br>",
                                "<b>Sources:</b><br>",
                                "Bloom fitting models (Shifted Gaussian, Rate of Change, and Threshold methods):<br>",
                                "<p style=\"margin-left: 30px;\"><i>TECH REPORT IN PROGRESS</i></p>",
                                "Chlorophyll-a algorithms OCx, POLY4, and GSM_GS:<br>",
                                "<p style=\"margin-left: 30px;\"><a href=\"https://www.mdpi.com/2072-4292/11/22/2609\"><i>Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.</i></a></p>",
                                "Chlorophyll-a algorithm EOF:<br>",
                                "<p style=\"margin-left: 30px;\"><a href=\"https://www.mdpi.com/2072-4292/10/2/265\"><i>Laliberté, J.; Larouche, P.; Devred, E.; Craig, S. Chlorophyll-a Concentration Retrieval in the Optically Complex Waters of the St. Lawrence Estuary and Gulf Using Principal Component Analysis. Remote Sens. 2018, 10, 265.</i></a></p>",
                                "Raw data:<br>",
                                "<p style=\"margin-left: 30px;\">Daily level-3 binned files are downloaded from <a href=\"https://oceancolor.gsfc.nasa.gov/\">NASA OBPG</a>, and weekly composites are generated by taking the average of each pixel over an 8-day period (46 weeks/year), following the same system as NASA OBPG. The binned data is used for statistics and bloom fitting, and rasterized and projected onto the map using <a href=\"https://spatialreference.org/ref/sr-org/7483/\">EPSG:3857</a> (the Web Mercator projection) for faster image loading.<br>",
                                "<a href=\"https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/\">NASA OCx chlorophyll-a algorithm</a><br>",
                                "<a href=\"https://oceancolor.gsfc.nasa.gov/products/\">Level-3 binned files</a><br>",
                                "<a href=\"https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/\">Binning scheme</a><br>",
                                "<a href=\"https://oceancolor.gsfc.nasa.gov/atbd/ocl2flags/\">Level-3 binned default flags</a></p><br>",
                                "<b>Contact:</b><br>",
                                "Stephanie.Clay@dfo-mpo.gc.ca<br><br>",
                                "<b>Dataset last updated:</b><br>", data_last_updated)),
                    easyClose = TRUE,
                    footer = modalButton("OK")
        ))
    })
    
    # Load coordinates
    state$coord_list <- readRDS("coords.rds")
    
    
    #***************************************************************************
    # COLLECT USER INPUT ####
    
    # Hide the settings panel if main options have changed but "load" has
    # not been clicked yet.
    observeEvent({
        input$satellite
        input$region
        input$algorithm
        input$year
        input$interval
        input$log_chla
    }, {
        hideElement(id = "hiddenPanel", anim = FALSE)
        disable("savemap")
        disable("savedensplot")
        disable("savebloomfit")
        disable("savebloomparams")
        disable("saveannualstats")
        state$data_loaded <- FALSE
        
        # check if the selected combination of satellite, region,
        # algorithm, and year has available data
        data_exists <- file.exists(paste0("./data/", input$region, "/", input$region, "_", input$satellite, "_", input$algorithm, "_", input$year, ".fst"))
        
        # block/unblock the "load" button depending on whether data
        # is available or not, and update the help text beneath it
        if (data_exists) {
            enable("load")
            state$help_load_txt <- ""
        } else {
            disable("load")
            state$help_load_txt <- "No data available for the selected options."
        }
        
    })
    
    output$help_load <- renderUI({
        
        helpText(state$help_load_txt,
                 width = widget_width,
                 style = help_text_style)
        
    })
    
    
    # GET SATELLITE ####
    
    # Update region and year drop-down menus based on satellite
    observeEvent(input$satellite, {
        
        tmp_years <- years[[input$satellite]]
        updateSelectInput(session,
                          inputId = "year",
                          label = NULL,
                          choices = tmp_years,
                          selected = tmp_years[length(tmp_years)])
        
        # Update full_run slider input
        tmp_years <- as.numeric(tmp_years)
        updateSliderInput(session,
                          inputId = 'fullrunyears',
                          min = tmp_years[1],
                          max = tmp_years[length(tmp_years)],
                          value = c(tmp_years[1],tmp_years[length(tmp_years)]))
        
    })
    
    
    # GET REGION ####
    
    # Get newly-selected region and set the lat/lon ranges, and create the list
    # of polygon objects for the leaflet map
    observeEvent(input$region, {
        
        reg <- input$region
        state$region <- reg
        
        if (reg=="atlantic") {
            state$center_lon <- -55
            state$center_lat <- 53
            state$zoom_level <- 5
        } else if (reg=="pacific") {
            state$center_lon <- -132.5
            state$center_lat <- 51.5
            state$zoom_level <- 6
        }
        
        
        poly_coord_list <- all_regions[[reg]]
        
        
        # # split a disjoint polygon into separate polygons, each with the same label
        # reg_list <- all_regions[[reg]]
        # poly_coord_list <- list()
        # poly_coord_list_names <- c()
        # for (i in 1:length(reg_list)) {
        #     if (anyNA(reg_list[[i]]$lat)) {
        # 
        #         tmp_lats <- reg_list[[i]]$lat
        #         tmp_lons <- reg_list[[i]]$lon
        # 
        #         NA_inds <- c(0, which(is.na(tmp_lats)))
        # 
        #         for (j in 1:(length(NA_inds)-1)) {
        #             coord_inds <- (NA_inds[j]+1):(NA_inds[j+1]-1)
        #             tmp_box <- list(lat=reg_list[[i]]$lat[coord_inds],
        #                             lon=reg_list[[i]]$lon[coord_inds])
        #             poly_coord_list <- c(poly_coord_list, list(tmp_box))
        #             poly_coord_list_names <- c(poly_coord_list_names, paste0(names(reg_list)[i], j))
        # 
        #         }
        # 
        #     } else {
        #         poly_coord_list <- c(poly_coord_list, list(reg_list[[i]]))
        #         poly_coord_list_names <- c(poly_coord_list_names, names(reg_list)[i])
        #     }
        # }
        # names(poly_coord_list) <- poly_coord_list_names
        
        
        # Make polygons for existing boxes, to add to base leaflet map
        original_polys <- lapply(1:length(poly_coord_list), function(k) {
            Polygon(coords=cbind(poly_coord_list[[k]]$lon, poly_coord_list[[k]]$lat), hole=TRUE)
        })
        original_polyIDs <- lapply(1:length(original_polys), function(k) {
            Polygons(list(original_polys[[k]]), toupper(poly_ID[[isolate(state$region)]][k]))
        })
        state$original_polylist <- SpatialPolygons(original_polyIDs, 1:length(poly_coord_list))
        
    })
    
    
    
    # GET BOX/POLYGON ####
    
    output$box <- renderUI({
        
        choices <- c("custom", poly_ID[[state$region]])
        names(choices) <- c("Custom polygon", full_names[[state$region]])
        
        selectInput(inputId = 'box',
                    label = HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Choose a polygon</font>"),
                    choices = choices,
                    selected = 'custom',
                    width = widget_width)
        
    })
    
    observeEvent(input$box,{
        # If the "draw polygon" method is selected for custom, it will want to keep
        # the draw toolbar on the map. In this case, if you switch from a custom box
        # to a pre-made one, the map should be triggered to remove the toolbar. If you
        # switch from a pre-made box to a custom one, the map should trigger to add the toolbar.
        if (state$latlon_method == "drawPoly") {
            if (state$box != "custom" & input$box == "custom") {
                state$draw_toolbar <- TRUE
            } else if (state$box == "custom" & input$box != "custom") {
                state$draw_toolbar <- FALSE
            }
        }
        state$box <- input$box
    })
    
    observeEvent(input$latlon_method, {
        state$latlon_method <- input$latlon_method
        if (state$latlon_method == "drawPoly") {
            state$draw_toolbar <- TRUE
        } else {
            state$draw_toolbar <- FALSE
        }
    })
    
    observeEvent(input$applyname, {
        state$custom_name <- input$custom_name
    })
    
    observe({
        state$poly_name <- ifelse(state$box=='custom',
                                  ifelse(nchar(state$custom_name)==0, "Custom polygon", state$custom_name),
                                  paste0(full_names[[isolate(state$region)]][which(state$box==poly_ID[[isolate(state$region)]])]))
    })
    
    
    # GET MAP COLOUR SCALE ####
    
    observeEvent(input$applyzlim, {
        
        zlim1 <- as.numeric(input$zlim1)
        zlim2 <- as.numeric(input$zlim2)
        
        # Check if values are valid (not NA, NaN, Inf, -Inf, or zlim1 > zlim2),
        # and if so, apply them to the reactive state variables
        if (is.finite(zlim1) & is.finite(zlim2) & zlim2 >= zlim1) {
            state$zlim1 <- zlim1
            state$zlim2 <- zlim2
        } else {
            # if values are invalid, reset them to the previous values
            updateTextInput(session, inputId = "zlim1", value = round(state$zlim1,2))
            updateTextInput(session, inputId = "zlim2", value = round(state$zlim2,2))
        }
        
    })
    
    
    # GET YEAR DAY ####
    
    observeEvent(input$yearday_slide, {
        
        # Get the day entered on the slider
        state$yearday <- input$yearday_slide
        
        # Get some time variables for later use
        time_variables <- get_time_vars(interval=state$interval,
                                        year=state$year,
                                        yearday=state$yearday,
                                        doys_per_week=doys_per_week)
        state$day_label <- time_variables$day_label
        state$time_ind <- time_variables$time_ind
        
    })
    
    observeEvent(input$daygo, {
        
        # Get the day entered manually
        yearday_num <- input$yearday_num
        
        # Check if it's numeric
        isnum <- is.numeric(yearday_num)

        if (!isnum) {
            
            if (is.null(state$yearday)) {numinputday <- 1
            } else {numinputday <- state$yearday}
            updateNumericInput(session, inputId = 'yearday_num', value = numinputday)

        } else {

            # Check if it's an integer
            isint <- yearday_num%%1==0
            if (!isint) {yearday_num <- floor(yearday_num)}
            
            # Check if it's within range
            if (yearday_num < 1) {yearday_num <- 1
            } else if (yearday_num > 365) {yearday_num <- 365}
            
            # If using weekly data, set yearday_num to start day of the week
            if (state$interval=="weekly") {
                yearday_num <- doy_week_start[tail(which(doy_week_start <= yearday_num),1)]
            }
            
            # Assign it to state reactiveValues
            state$yearday <- yearday_num
            
            # Get some time variables for later use
            time_variables <- get_time_vars(interval=state$interval,
                                            year=state$year,
                                            yearday=yearday_num,
                                            doys_per_week=doys_per_week)
            state$day_label <- time_variables$day_label
            state$time_ind <- time_variables$time_ind
            
            # Update the input widgets with the final value
            updateNumericInput(session, inputId = "yearday_num", value = yearday_num)
            updateSliderInput(session, inputId = 'yearday_slide', value = yearday_num)
            
        }
        
    })
    
    
    # GET STATISTICS ####
    
    observeEvent(input$outlier, {
        state$outlier <- input$outlier
    })
    observeEvent(input$dailystat,{
        state$dailystat <- input$dailystat
    })
    observeEvent(input$percent, {
        state$percent <- input$percent / 100
    })
    observeEvent(input$applypixrange, {
        state$pixrange1 <- as.numeric(input$pixrange1)
        state$pixrange2 <- as.numeric(input$pixrange2)
    })
    
    
    # GET BLOOM FIT VARIABLES ####
    
    observeEvent(input$fitmethod, {
        state$fitmethod <- input$fitmethod
        # Make sure tmax option starts out as "off" when fit method is changed.
        # Needs to be "off" for non-gauss methods so that ti_limits can be adjusted,
        # and need to reset it for gauss method anyway, otherwise the whole switch
        # disappears when you switch from another method to gauss method.
        updateSwitchInput(session, "tm", value = FALSE, label = HTML('t<sub>max</sub>'))
    })
    observeEvent(input$bloomShape, {
        state$bloomShape <- input$bloomShape
    })
    observeEvent(input$smoothMethod,{
        state$smoothMethod <- input$smoothMethod
    })
    observeEvent(input$loessSpan,{
        state$loessSpan <- input$loessSpan
    })
    
    # THRESHOLD METHOD SPECIFICALLY
    observeEvent(input$threshcoef, {
        state$threshcoef <- input$threshcoef
    })
    
    # GAUSS METHOD SPECIFICALLY
    # observeEvent(input$ti_threshold,{
    #     # Check if value is valid and within the 0-50 range,
    #     # and if so, apply it to the reactive state variable
    #     ti_threshold <- input$ti_threshold
    #     if (!is.finite(ti_threshold)) {
    #         updateNumericInput(session, inputId = "ti_threshold", value = (state$ti_threshold * 100))
    #     } else if (ti_threshold < 1) {
    #         updateNumericInput(session, inputId = "ti_threshold", value = 1)
    #     } else if (ti_threshold > 50) {
    #         updateNumericInput(session, inputId = "ti_threshold", value = 50)
    #     } else {
    #         state$ti_threshold <- ti_threshold/100
    #     }
    # })
    # observeEvent(input$tt_threshold,{
    #     # Check if value is valid and within the 0-50 range,
    #     # and if so, apply it to the reactive state variable
    #     tt_threshold <- input$tt_threshold
    #     if (!is.finite(tt_threshold)) {
    #         updateNumericInput(session, inputId = "tt_threshold", value = (state$tt_threshold * 100))
    #     } else if (tt_threshold < 1) {
    #         updateNumericInput(session, inputId = "tt_threshold", value = 1)
    #     } else if (tt_threshold > 50) {
    #         updateNumericInput(session, inputId = "tt_threshold", value = 50)
    #     } else {
    #         state$tt_threshold <- tt_threshold/100
    #     }
    # })
    observeEvent(input$tm,{
        state$tm <- input$tm
    })
    observeEvent(input$beta,{
        state$beta <- input$beta
    })
    observeEvent(input$use_weights,{
        state$use_weights <- input$use_weights
    })
    observeEvent(input$apply_flag1_lim, {
        flag1_lim1 <- as.numeric(input$flag1_lim1)
        flag1_lim2 <- as.numeric(input$flag1_lim2)
        # Check if values are valid (not NA, NaN, Inf, -Inf, or lim1 > lim2),
        # and if so, apply them to the reactive state variables
        if (is.finite(flag1_lim1) & is.finite(flag1_lim2) & flag1_lim2 >= flag1_lim1) {
            state$flag1_lim1 <- flag1_lim1
            state$flag1_lim2 <- flag1_lim2
        } else {
            # if values are invalid, reset them to the previous values
            updateTextInput(session, inputId = "flag1_lim1", value = state$flag1_lim1)
            updateTextInput(session, inputId = "flag1_lim2", value = state$flag1_lim2)
        }
    })
    observeEvent(input$apply_flag2_lim, {
        flag2_lim1 <- as.numeric(input$flag2_lim1)
        flag2_lim2 <- as.numeric(input$flag2_lim2)
        # Check if values are valid (not NA, NaN, Inf, -Inf, or zlim1 > zlim2),
        # and if so, apply them to the reactive state variables
        if (is.finite(flag2_lim1) & is.finite(flag2_lim2) & flag2_lim2 >= flag2_lim1) {
            state$flag2_lim1 <- flag2_lim1
            state$flag2_lim2 <- flag2_lim2
        } else {
            # if values are invalid, reset them to the previous values
            updateTextInput(session, inputId = "flag2_lim1", value = state$flag2_lim1)
            updateTextInput(session, inputId = "flag2_lim2", value = state$flag2_lim2)
        }
    })
    
    # get ranges of days used for fitting and certain parameters
    observeEvent(input$t_range, {
        
        tr1 <- input$t_range[1]
        tr2 <- input$t_range[2]
        
        update_t_range <- FALSE
        
        # make sure the range of days used in the fit is > the theoretical minimum
        # range for a curve (3 days - start, maximum, end)
        if (tr1 > 363) {
            tr1 <- 363
            tr2 <- 365
            update_t_range <- TRUE
        }
        
        if (abs(tr1 - tr2) < 2) {
            tr2 <- tr1 + 2
            update_t_range <- TRUE
        }
        
        if (update_t_range) {
            updateSliderInput(session, inputId = 't_range', value = c(tr1, tr2))
        }
        
        state$t_range <- c(tr1, tr2)
        
    })
    
    # adjust tm_limits based on t_range
    observeEvent({
        state$t_range
        input$tm_limits
    }, {
        
        tr1 <- state$t_range[1]
        tr2 <- state$t_range[2]
        
        tm1 <- input$tm_limits[1]
        tm2 <- input$tm_limits[2]
        
        update_tm_limits <- FALSE
        
        if (tm2 >= tr2) {
            tm2 <- tr2 - 1
            tm1 <- min(tm1, tm2)
            update_tm_limits <- TRUE
        }
        if (tm1 <= tr1) {
            tm1 <- tr1 + 1
            tm2 <- max(tm1, tm2)
            update_tm_limits <- TRUE
        }
        
        state$tm_limits <- c(tm1, tm2)
        
        if (update_tm_limits) {
            updateSliderInput(session, inputId = 'tm_limits', value = c(tm1, tm2))
        }
        
    })
    
    # adjust ti_limits based on t_range and tm_limits
    observeEvent({
        state$t_range
        state$tm_limits
        input$ti_limits
    }, {
        
        tr1 <- state$t_range[1]
        tr2 <- state$t_range[2]
        
        tm1 <- state$tm_limits[1]
        tm2 <- state$tm_limits[2]
        
        ti1 <- input$ti_limits[1]
        ti2 <- input$ti_limits[2]
        
        update_ti_limits <- FALSE
        
        if (ti1 >= tm1) {
            ti1 <- tm1 - 1
            update_ti_limits <- TRUE
        }
        if (ti1 < tr1) {
            ti1 <- tr1
            ti2 <- max(ti1, ti2)
            update_ti_limits <- TRUE
        }
        if (ti2 >= tm2) {
            ti2 <- tm2 - 1
            update_ti_limits <- TRUE
        }
        
        state$ti_limits <- c(ti1, ti2)
        
        if (update_ti_limits) {
            updateSliderInput(session, inputId = 'ti_limits', value = c(ti1, ti2))
        }
        
    })
    
    
    # GET "FULL RUN" CHECKBOXES ####
    
    output$fullrunboxes <- renderUI({
        
        choices <- c("custom", poly_ID[[input$region]])
        names(choices) <- c("Custom", poly_ID[[input$region]])
        
        tags$div(class = "multicol",
        checkboxGroupInput(inputId = "fullrunboxes",
                           label = NULL,
                           choices = choices,
                           selected = state$box,
                           width = widget_width)
        )
        
    })
    
    observeEvent(input$fullrunallboxes, {
        
        ifrab <- input$fullrunallboxes
        
        choices <- c("custom", poly_ID[[input$region]])
        names(choices) <- c("Custom", toupper(poly_ID[[input$region]]))
        
        if (ifrab) {
            state$fullrunboxes <- choices
            enable("fullrun_process")
        } else {
            state$fullrunboxes <- input$fullrunboxes
        }
        
    })
    
    observeEvent(input$fullrunboxes, {
        
        ifrb <- input$fullrunboxes
        
        if (length(ifrb)==1 & ifrb=="custom" & is.null(state$newpoly) & is.null(state$editedpoly)) {
            disable("fullrun_process")
        } else {
            enable("fullrun_process")
        }
        
        state$fullrunboxes <- ifrb
        
    })
    
    
    
    #***************************************************************************
    # LOAD DATASET ####
    # for the selected satellite/year, logged or unlogged
    
    full_data <- eventReactive(input$load, {
        
        show_modal_spinner(spin = "atom",
                           color = "#112446",
                           text = paste0("Loading data for ", input$year, "..."))
        
        showElement(id = "hiddenPanel", anim = FALSE)
        
        state$data_loaded <- TRUE
        
        # If log_chla changes, map colour scale defaults should change
        if (state$log_chla != input$log_chla) {
            new_zlim1 <- 0.05
            new_zlim2 <- 20
            
            if (input$log_chla) {
                new_zlim1 <- round(log10(new_zlim1),2)
                new_zlim2 <- round(log10(new_zlim2),2)
            }
            
            updateTextInput(session, inputId="zlim1", value = new_zlim1)
            updateTextInput(session, inputId="zlim2", value = new_zlim2)
            
            state$zlim1 <- new_zlim1
            state$zlim2 <- new_zlim2
        }
        
        # Now that "load" has been pressed, load the full dataset for this year,
        # check to see if the values should be logged, and then assign the new
        # year to the reactive values that will be used in the date label on the
        # map, and other output
        state$satellite <- input$satellite
        state$algorithm <- input$algorithm
        state$year <- input$year
        state$interval <- input$interval
        state$log_chla <- input$log_chla
        
        enable("savesettings")
        
        # Load full map data
        sslat <- state$coord_list[[state$region]]$lat
        sslon <- state$coord_list[[state$region]]$lon
        all_data <- get_data(state$region, state$satellite, state$algorithm, state$year,
                             state$yearday, state$interval, state$log_chla, length(sslat),
                             doys_per_week, doy_week_start, doy_week_end)
        sschla <- all_data$sschla
        state$available_days <- all_data$available_days
        state$doy_vec <- all_data$doy_vec # days of the year, whether you're using daily or weekly data
        state$day_label <- all_data$day_label
        state$time_ind <- all_data$time_ind
        
        # Update the yearday slider so if using weekly data, it only allows the user
        # to select the starting day of each 8-day week.
        # Note: Don't update the actual value in the slider, otherwise the map will update
        # with the newly loaded values, update the slider based on the new yearday, and then
        # reload the map again with the same data.
        if (state$interval=="daily") {
            updateSliderInput(session, inputId = "yearday_slide", step = 1)
        } else if (state$interval=="weekly") {
            updateSliderInput(session, inputId = "yearday_slide", step = 8)
        }
        
        remove_modal_spinner()
        
        gc()
        
        return(list(sschla=sschla,
                    sslon=sslon,
                    sslat=sslat))
        
    })
    
    
    #***************************************************************************
    # MAP BASE ####
    
    # Raster data will be overlaid after this
    map_reactive <- reactive({
    
        # Use leaflet() here, and only include aspects of the map that won't need
        # to change dynamically unless the entire map is torn down and recreated.
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles("Esri.WorldGrayCanvas",
                             options = providerTileOptions(minZoom = 4,
                                                           maxZoom = 10,
                                                           updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
                                                           updateWhenIdle = TRUE)) %>% # map won't load new tiles when panning
            setView(lng = state$center_lon,
                    lat = state$center_lat,
                    zoom = state$zoom_level) %>%
            # Add mouse coordinates to top of map
            # Note: need to "remove" first, otherwise it gets stuck if you try
            # to reload the base map (for example, switching from Atlantic to Pacific)
            removeMouseCoordinates() %>%
            addMouseCoordinates() %>%
            # Add boxes based on the current AZMP statistic boxes
            addPolygons(group = "Stats boxes",
                        data = state$original_polylist,
                        stroke = TRUE,
                        color = "darkgrey",
                        weight = 2,
                        opacity = 1,
                        fill = FALSE,
                        label = abbrev[[isolate(state$region)]],
                        labelOptions = labelOptions(noHide = TRUE,
                                                    textOnly = TRUE,
                                                    textsize = '13px',
                                                    direction = "center",
                                                    style = list(
                                                        'color'='white',
                                                        'text-shadow' = '0px 0px 4px #000000'))) %>%
            # Add gridlines
            addSimpleGraticule(group = "Gridlines",
                               interval = 5,
                               showOriginLabel = FALSE) %>%
            # Add option to remove the gridlines or existing statistic boxes
            addLayersControl(overlayGroups = c("Gridlines", "Stats boxes"),
                             options = layersControlOptions(collapsed = FALSE))
        
    })
    
    # MAP UPDATE ####
    # based on user input
    observe({
        
        # Get the selected annual dataset
        ssfull <- full_data()
        sschla <- ssfull$sschla
        sslat <- ssfull$sslat
        sslon <- ssfull$sslon
        
        # Get the selected day of year
        yearday <- state$yearday
        day_label <- state$day_label
        time_ind <- state$time_ind
        
        # check if data is available in the file for the selected day
        if (yearday > isolate(state$available_days)) {
            
            # Update map based on choices of year day
            lfp <- leafletProxy("fullmap", session) %>%
                clearPopups() %>%
                clearControls() %>%
                clearImages() %>%
                # Label map with current year and day of year
                addControl(tags$div(tag.map.title, HTML(paste0(day_label, "<br>NO DATA AVAILABLE YET"))),
                           position = "topleft",
                           className = "map-title")
            
            disable("savemap")
            disable("savedensplot")
            
        } else {
            
            # Subset chla, lat, lon by day, and remove NA cells
            chla_ind <- !is.na(sschla[,time_ind])
            
            if (sum(chla_ind)==0) {
                
                # Update map based on choices of year day
                lfp <- leafletProxy("fullmap", session) %>%
                    clearPopups() %>%
                    clearControls() %>%
                    clearImages() %>%
                    # Label map with current year and day of year
                    addControl(tags$div(tag.map.title, HTML(paste0(day_label, "<br>NO DATA"))),
                               position = "topleft",
                               className = "map-title")
                
                disable("savemap")
                disable("savedensplot")
                
            } else {
                
                pts <- data.frame(lon = sslon[chla_ind],
                                  lat = sslat[chla_ind],
                                  chl = sschla[chla_ind,time_ind],
                                  stringsAsFactors = FALSE)
                state$pts <- pts
                
                coordinates(pts) = ~lon+lat
                
                # create an empty raster object to the extent of the points
                tr <- raster(ext=extent(pts), resolution = c(0.065,0.04333333))
                
                # rasterize your irregular points
                tr <- rasterize(pts, tr, pts$chl, fun = mean, na.rm = T) # we use a mean function here to regularly grid the irregular input points
                # state$tr <- tr # only used for input$fullmap_click, currently disabled
                
                # Get colour scale for leaflet map
                zlim <- c(state$zlim1, state$zlim2)
                state$zlim <- zlim
                
                # Get legend title
                leg_title <- paste0("<center>Chlorophyll-a</br>[ ",
                                    ifelse(isolate(state$log_chla), "log<sub>10</sub> ", ""),
                                    "mg m<sup>-3</sup> ]</center>")
                state$leg_title <- leg_title
                
                cm <- colorNumeric(
                    palette = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(100),
                    domain = zlim,
                    na.color = "#00000000"# transparent
                )
                state$cm <- cm
                
                # Use a raster with values above/below the color scale set to the limits.
                tr_coloradj <- calc(tr, function(x) ifelse(x <= zlim[1], zlim[1]+(1e-10), ifelse(x >= zlim[2], zlim[2]-(1e-10), x)))
                state$tr_coloradj <- tr_coloradj
                
                # Update map based on choices of year day
                lfp <- leafletProxy("fullmap", session) %>%
                    clearPopups() %>%
                    clearControls() %>%
                    clearImages() %>%
                    addRasterImage(x = tr_coloradj, colors = cm) %>%
                    addLegend(position = 'topright',
                              pal = cm,
                              values = zlim,#c(getValues(tr_coloradj),zlim),
                              title = leg_title,
                              bins = 10,
                              opacity = 1) %>%
                    # Label map with current year and day of year
                    addControl(tags$div(tag.map.title, HTML(day_label)),
                               position = "topleft",
                               className = "map-title")
                # }))
                # now that data has been loaded, make the download button visible
                enable("savemap")
                
            }
            
        }
        
        if (state$draw_toolbar) {
            lfp <- lfp %>%
                addDrawToolbar(
                    # remove options to draw lines, circles, or single markers
                    polylineOptions=FALSE,
                    circleOptions=FALSE,
                    markerOptions=FALSE,
                    circleMarkerOptions=FALSE,
                    # only one custom polygon at a time
                    singleFeature=TRUE,
                    # adjust custom polygon colors
                    polygonOptions=drawPolygonOptions(shapeOptions = drawShapeOptions(color="yellow", fill=FALSE, weight=2.5)),
                    rectangleOptions=drawRectangleOptions(shapeOptions = drawShapeOptions(color="yellow", fill=FALSE, weight=2.5)),
                    # custom polygons can be edited
                    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
        } else {
            lfp <- lfp %>%
                removeDrawToolbar(clearFeatures = TRUE)
        }
        
        gc()
        
    })
    
    
    # RENDER MAP (print to screen)
    output$fullmap <- renderLeaflet({
        
        map_reactive()
        
    })
    
    
    
    #***************************************************************************
    # CUSTOM POLYGON/POINT FUNCTIONS ####
    
    # Get user input for new, edited, or deleted polygons
    observeEvent(input$fullmap_draw_new_feature, {
        state$newpoly <- input$fullmap_draw_new_feature
        state$editedpoly <- NULL
        enable("fullrun_process")
    })
    observeEvent(input$fullmap_draw_edited_features, {
        state$newpoly <- NULL
        state$editedpoly <- input$fullmap_draw_edited_features
    })
    observeEvent(input$fullmap_draw_deleted_features, {
        state$newpoly <- NULL
        state$editedpoly <- NULL
        state$custom_name <- ""
        updateTextInput(session,
                        inputId="custom_name",
                        value="")
        if (length(state$fullrunboxes)==1 & state$fullrunboxes=="custom") {
            disable("fullrun_process")
        }
    })
    
    # If new polygons are created, edited, or deleted, this block of code
    # will trigger to get the coordinates of the polygon and send them to annual
    # stats to update the density plot and bloom fit scatterplot
    draw_polygon <- reactive({
        
        # Reset this, assume acceptable polygon size
        state$latlon_toolarge <- FALSE
        
        # Check for new coordinates
        coords <- try(unlist(state$newpoly$geometry$coordinates))
        
        # Check for edited coordinates
        if (is.null(coords)) {
            coords <- try(unlist(state$editedpoly$features[[1]]$geometry$coordinates))
        }
        
        
        # Check if polygon area is too large
        if (!is.null(coords)) {
          
          # Extract lat/lon from coordinates
          Longitude <- coords[seq(1,length(coords), 2)]
          Latitude <- coords[seq(2,length(coords), 2)]
          
          # check area of polygon in case it's too large
          polygon_area <- polyarea(x=Longitude, y=Latitude)
          
          if (polygon_area > 500) {
            coords <- NULL
            state$latlon_toolarge <- TRUE
          }
          
        }
        
        return(coords)
        
    })
    
    
    # Instead of getting coordinates of a polygon drawn directly on the map,
    # get coordinates entered in a box by the user.
    observeEvent(input$draw, {
        
        coords <- NULL
        
        # Reset these
        state$latlon_invalid <- FALSE
        state$latlon_toolarge <- FALSE
        
        manual_lats <- input$manual_lats
        manual_lons <- input$manual_lons
        
        # split by commas, trim white space from each, and convert to numeric
        manual_lats <- as.numeric(sapply(strsplit(manual_lats, ",")[[1]], trimws))
        manual_lons <- as.numeric(sapply(strsplit(manual_lons, ",")[[1]], trimws))
        
        # check if any lat/lons are NA or NULL or INF
        if (any(!is.finite(manual_lats)) | any(!is.finite(manual_lons)) | any(is.null(manual_lats)) | any(is.null(manual_lons))) {
            
            state$latlon_invalid <- TRUE
            state$latlon_toolarge <- FALSE
            
            # check if lat/lons are not numeric, or not the same length, or empty
        } else if (!(all(is.numeric(manual_lats)) & all(is.numeric(manual_lons))) | (length(manual_lats) != length(manual_lons)) | length(manual_lats)==0) {
            
            state$latlon_invalid <- TRUE
            state$latlon_toolarge <- FALSE
            
        } else {
            
            # if user forgets to enter first point to close the polygon, fix that here
            if (!(manual_lats[1]==manual_lats[length(manual_lats)] & manual_lons[1]==manual_lons[length(manual_lons)])) {
                
                manual_lats <- c(manual_lats, manual_lats[1])
                manual_lons <- c(manual_lons, manual_lons[1])
                
            }
            
            # check area of polygon in case it's too large
            polygon_area <- polyarea(x=manual_lons, y=manual_lats)
            
            if (polygon_area > 500) {
                
                state$latlon_invalid <- FALSE
                state$latlon_toolarge <- TRUE
                
            } else {
                
                state$latlon_invalid <- FALSE
                state$latlon_toolarge <- FALSE
                coords <- c(rbind(manual_lons, manual_lats))
                
            }
            
        }
        
        state$typed_coords <- coords
        
    })
    
    type_polygon <- reactive({
        
        return(state$typed_coords)
        
    })
    
    output$help_latlon <- renderUI({
        
        help_msg <- ""
        if (state$latlon_invalid) {
            help_msg <- "Invalid latitude/longitude."
        } else if (state$latlon_toolarge) {
            help_msg <- "Polygon is too large (max allowed area = 500 degrees^2)."
        }
        helpText(help_msg,
                 width = widget_width,
                 style = help_text_style)
        
    })
    
    
    # THE COMMENTED BLOCK OF CODE BELOW WAS WORKING WHEN IT WAS WRITTEN (~ EARLY 2020)
    # BUT HAS SOME FRUSTRATING CONSEQUENCES
    # Note: if uncommented, you must also uncomment the following lines at the
    # top of the script:
    #library(htmlTable)      # for making tables in popups
    #library(geosphere)      # for calculating accurate distances between single point click and data point plotted on the map
    
    # This allows the user to click on any point on the map and get a popup
    # showing the latitude, longitude, and chlorophyll value at that point.
    # THE PROBLEM: it pops up whenever you're clicking on the map to draw/edit
    # a polygon and gets in the way.
    # My stackoverflow post for help that no one has noticed :(
    # https://stackoverflow.com/questions/60840223/r-shiny-leaflet-map-single-point-click-popup-interferes-when-drawing-or-editi
    
    # # Add a popup with latitude, longitude, and chla data (if available at that
    # # point) if a user clicks a single point on the map.
    # observeEvent(input$fullmap_click, {
    # 
    #     # Get the coordinates of a single clicked point
    #     pt_coords <- input$fullmap_click
    #     ptlat <- pt_coords$lat
    #     ptlon <- pt_coords$lng
    # 
    #     df <- data.frame("Latitude"=round(pt_coords$lat, 3),
    #                      "Longitude"=round(pt_coords$lng, 3),
    #                      "Chlorophyll.a"=NA,
    #                      "Rasterized.Chlorophyll.a"=NA)
    # 
    #     # GET CLOSEST BINNED VALUES
    #     #*****************
    #     # Get binned values
    #     pts <- state$pts
    # 
    #     if (!is.null(pts)) {
    # 
    #         # Reduce the size of the points dataset to check for matches
    #         lat_diff <- abs(pts$lat - ptlat)
    #         lon_diff <- abs(pts$lon - ptlon)
    #         hypotenuse <- sqrt(lat_diff^2 + lon_diff^2)
    #         closest <- which(hypotenuse < 0.5)
    # 
    #         # Check if any chl values are < 0.5 degrees from the click
    #         if (length(closest) > 0) {
    # 
    #             # Get the accurate distance between those points and the point click
    #             distGeo_pts <- as.numeric(sapply(1:length(closest),function(i) {distGeo(c(ptlon, ptlat),c(pts[closest[i],"lon"],pts[closest[i],"lat"]))}))
    # 
    #             # Check again if any chl values are < 2.3km of the click (i.e. the
    #             # radius of the circle markers)
    #             min_distGeo <- min(distGeo_pts, na.rm=TRUE)
    # 
    #             if (min_distGeo <= 2300) {
    #                 ind <- which.min(distGeo_pts)
    #                 df[1,"Chlorophyll.a"] <- round(pts[closest[ind],"chl"], 3)
    #             }
    # 
    #         }
    # 
    #         # GET CLOSEST RASTER VALUES
    #         #*****************
    #         # For comparison, get rasterized chla
    #         tr <- state$tr
    #         pt_extent <- extent(ptlon-0.2,ptlon+0.2,ptlat-0.2,ptlat+0.2)
    #         # Check if the small box around the point overlaps the raster plotted on the map
    #         ext_check <- raster::intersect(extent(tr),pt_extent)
    #         if (!is.null(ext_check)) {
    #             tr_cropped <- crop(tr, pt_extent)
    #             tr_values <- rasterToPoints(tr_cropped)
    #             if (nrow(tr_values) > 0) {
    #                 distGeo_rast <- as.numeric(sapply(1:nrow(tr_values),function(i) {distGeo(c(ptlon, ptlat),as.numeric(tr_values[i,1:2]))}))
    #                 min_distGeo <- which.min(distGeo_rast)
    #                 df[1,"Rasterized.Chlorophyll.a"] <- round(tr_values[min_distGeo,3], 3)
    #             }
    #         }
    # 
    #     }
    # 
    #     df <- t(df)
    # 
    #     # Add a popup to the map to display lat, lon, and chla data
    #     leafletProxy("fullmap", session) %>%
    #         clearPopups() %>%
    #         addPopups(lng=pt_coords$lng,
    #                   lat=pt_coords$lat,
    #                   htmlTable(df, align=c("l","r")))
    # 
    # })
    
    
    # POLYGON TITLE PANEL
    output$poly_title <- renderUI({
        
        if (state$data_loaded) {
            
            str1 <- paste0(state$year, " ", ifelse(state$region=="atlantic", "Atlantic", "Pacific"), ", ", state$poly_name)
            str2a <- "Latitudes:"
            str2b <- paste0(state$polylat, collapse=", ")
            str3a <- "Longitudes:"
            str3b <- paste0(state$polylon, collapse=", ")

            HTML(paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">", str1, "</font><br/>",
                        "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">", str2a, "</font> ", str2b, "<br/>",
                        "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">", str3a, "</font> ", str3b))
            
        }
        
    })
    
    
    #***************************************************************************
    # ANNUAL STATS FOR BOX/POLYGON ####
    
    # If box or latlon_method have changed, reset/delete old highlighted boxes
    delete_boxes <- eventReactive({
        state$box
        state$latlon_method
    }, {
        
        # Remove popups, previously selected existing AZMP boxes, or previously
        # typed polygons
        leafletProxy("fullmap", session) %>%
            clearPopups() %>%
            removeShape("highlighted_box") %>%
            removeShape("typedpoly")
        
        snp <- isolate(state$newpoly)
        sep <- isolate(state$editedpoly)
        
        # Remove drawn polygons
        if (!is.null(snp)) {
            drawnshapes <- snp$properties$`_leaflet_id`
            session$sendCustomMessage(
                "removeleaflet",
                list(elid="fullmap", layerid=drawnshapes)
            )
        } else if (!is.null(sep)) {
            drawnshapes <- sep$features[[1]]$properties$`_leaflet_id`
            session$sendCustomMessage(
                "removeleaflet",
                list(elid="fullmap", layerid=drawnshapes)
            )
        }
        
        # Reset drawn/typed polygon variables to NULL
        state$newpoly <- NULL
        state$editedpoly <- NULL
        state$typed_coords <- NULL
        
    })
    
    annual_stats <- reactive({
        
        # Make sure you have the updated satellite/year data
        ssfull <- full_data()
        
        # Reset/erase boxes if state$box or state$latlon_method have changed
        delete_boxes()
        
        coords <- NULL
        add_poly <- FALSE # add new polygons using leafletProxy?
        
        # Get coordinates from a custom polygon
        if (state$box=="custom" & state$latlon_method=="drawPoly") {
            
            coords <- draw_polygon()
            
        } else if (state$box=="custom" & state$latlon_method=="typeCoords") {
            
            coords <- type_polygon()
            
            # for highlighted box in leaflet map
            highlight_ID <- "typedpoly"
            add_poly <- TRUE
            
        }
        
        if (state$box == "custom" & is.null(coords)) {
            
            Longitude <- Latitude <- rchla <- NULL
            
        } else {
            
            if (state$box == "custom") {
                
                # Extract lat/lon from coordinates
                Longitude <- coords[seq(1,length(coords), 2)]
                Latitude <- coords[seq(2,length(coords), 2)]
                
            } else {
                
                # Use point.in.polygon to extract AZMP stat boxes based on their
                # lat/lon boundaries.
                Longitude <- as.numeric(all_regions[[isolate(state$region)]][[which(state$box==poly_ID[[isolate(state$region)]])]]$lon)
                Latitude <- as.numeric(all_regions[[isolate(state$region)]][[which(state$box==poly_ID[[isolate(state$region)]])]]$lat)
                
                # for highlighted box in leaflet map
                highlight_ID <- "highlighted_box"
                add_poly <- TRUE
                
            }
            
            if (add_poly) {
                leafletProxy("fullmap", session) %>%
                    addPolygons(layerId = highlight_ID,
                                lng = Longitude,
                                lat = Latitude,
                                stroke = TRUE,
                                color = "yellow",
                                weight = 2.5,
                                opacity = 1,
                                fill = FALSE)
            }
            
            # Create new rchla based on selected coordinates
            sschla <- ssfull$sschla
            sslon <- ssfull$sslon
            sslat <- ssfull$sslat
            
            # Create the mask for the subregion
            mask <- point.in.polygon(sslon, sslat, Longitude, Latitude)
            mask <- which(as.logical(mask))
            
            if (length(mask)==0) {
                rchla <- NULL
            } else if (sum(mask)==1) {
                # make sure rchla is in matrix format
                rchla <- matrix(sschla[mask,], nrow=1)
            } else {
                rchla <- sschla[mask,]
            }
            
            # Should the range of pixel values used in the stats be restricted?
            # (i.e. set pixels beyond a threshold to NA?)
            pixrange1 <- state$pixrange1
            pixrange2 <- state$pixrange2
            if (!is.na(pixrange1)) {
                rchla[rchla < pixrange1] <- NA
            }
            if (!is.na(pixrange2)) {
                rchla[rchla > pixrange2] <- NA
            }
            if (all(is.na(rchla))) {rchla <- NULL}
            
        }
        
        state$polylon <- Longitude
        state$polylat <- Latitude
        state$null_rchla <- is.null(rchla)
        
        # If there is valid data in this region for any day, compute statistics for it
        if (!is.null(rchla)) {
            
            all_stats <- get_stats(rchla=rchla, outlier=state$outlier)
            
            # update state with statistics for this region
            state$limits <- all_stats$limits
            state$lenok <- all_stats$lenok
            state$chl_mean <- all_stats$chl_mean
            state$chl_median <- all_stats$chl_median
            state$chl_sd <- all_stats$chl_sd
            state$chl_min <- all_stats$chl_min
            state$chl_max <- all_stats$chl_max
            state$nobs <- all_stats$nobs
            state$percent_coverage <- all_stats$percent_coverage
            
        }
        
        gc()
        
        return(rchla)
        
    })
    
    
    #***************************************************************************
    # DAILY/WEEKLY STATS, DENSITY PLOT ####
    
    make_density_plot <- reactive({
        
        # get the most recent annual data
        rchla <- annual_stats()
        
        # get the latest day of year
        yearday <- state$yearday
        day_label <- state$day_label
        time_ind <- state$time_ind
        
        plot_title <- paste0('Density plot of chlorophyll concentration for ', day_label)
        
        # create base plot
        p <- ggplot() + theme_bw()
        
        
        #***********************************************************************
        # CHECK IF THERE IS ENOUGH DATA
        
        # Reset error message to NULL, then check if it should be changed and
        # printed instead of doing the density plot
        em <- NULL
        
        if (state$data_loaded) {
            
            if (yearday > state$available_days) {
                
                em <- paste0("No data available yet for ", day_label)
                
            } else if (state$box=="custom" & state$latlon_toolarge) {
              
                em <- "Polygon is too large (max allowed area = 500 degrees^2)."
            
            } else if (state$null_rchla) {
                
                if (state$box=="custom" & is.null(state$newpoly) & is.null(state$editedpoly) & is.null(state$typedpoly)) {
                    em <- "Create your custom polygon in a region with sufficient data"
                } else {
                    em <- "No data in the selected region"
                }
            
            } else {
                
                # check if % coverage for this day and region is high enough to
                # create a density plot (default = 10%)
                lenok <- state$lenok
                ok <- lenok[time_ind] / nrow(rchla) > state$percent
                if (!ok) {
                    em <- paste0("Insufficient data, coverage < ", (state$percent*100), "%")
                } else if (lenok[time_ind]==1) {
                    em <- "Only one valid point selected"
                }
                
            }
            
        } else {
            em <- "Load data to begin"
        }
        
        
        #***********************************************************************
        # IF THERE IS ENOUGH DATA, PLOT IT
        
        if (is.null(em)) {
            
            chl_mean <- state$chl_mean
            chl_median <- state$chl_median
            
            # Create density plot and add vertical lines for mean, median
            p <- p +
                ggtitle(plot_title) +
                geom_density(data=data.frame(x=rchla[,time_ind]), aes(x=x), fill="grey", alpha = 0.7) + 
                geom_vline(aes(xintercept=chl_mean[time_ind], col="chl_mean"), size=1.2) +
                geom_vline(aes(xintercept=chl_median[time_ind], col="chl_median"), size=1)
            
            # Color codes for mean/median lines, to use in legend
            leg_col_labels <- c("mean", "median")
            leg_col_scale <- c(chl_mean = "dodgerblue2", chl_median = "red2")
            
            # Plot outlier boundaries
            if (state$outlier != "none") {
                limits <- state$limits[time_ind,]
                p <- p + geom_vline(aes(xintercept=limits, col="stdev"), linetype="dotted", size=1)
                # Update color code vector for legend, to include outlier bars
                leg_col_labels[3] <- ifelse(state$outlier=="sd2", "+/- 2 standard deviations",
                                            ifelse(state$outlier=="sd3", "+/- 3 standard deviations", "+/- 1.5 IQR (interquartile range)"))
                leg_col_scale[3] <- "springgreen3"
                names(leg_col_scale) <- c(names(leg_col_scale)[1:2], "stdev")
            }
            
            # create table of stats to print on the plot
            stat_df <- cbind(c("mean", "median", "stdev",
                               "min", "max", "n[obs]", "% cov"),
                             round(c(chl_mean[time_ind],
                                     chl_median[time_ind],
                                     state$chl_sd[time_ind],
                                     state$chl_min[time_ind],
                                     state$chl_max[time_ind],
                                     state$nobs[time_ind],
                                     state$percent_coverage[time_ind]), 2))
            # convert stats table to tableGrob and format it
            stat_df <- tableGrob(d = stat_df, rows = NULL, cols = NULL,
                                 # Define theme to parse plotmath expressions
                                 theme = ttheme_minimal(core=list(fg_params=list(parse=TRUE,
                                                                                 hjust=0, x=0.01),
                                                                  bg_params = list(fill="white",
                                                                                   alpha=0.6)),
                                                        base_size=10,
                                                        padding=unit(c(1,1), "mm")))
            
            # get x, y values to use in placement of stats table
            density_data <- ggplot_build(p)[["data"]][[1]]
            minx <- min(density_data$x, na.rm=TRUE)
            maxx <- max(density_data$x, na.rm=TRUE)
            miny <- min(density_data$y, na.rm=TRUE)
            maxy <- max(density_data$y, na.rm=TRUE)
            
            # add table of statistics for this day to the top right corner of the density plot
            table_xminloc <- maxx - (1/10)*(maxx - minx)
            table_yminloc <- maxy - (2/7)*(maxy - miny)
            p <- p +
                scale_y_continuous(limits=c(0,maxy), expand = expansion(mult = c(0, .05))) +
                annotation_custom(stat_df, xmin=table_xminloc, ymin=table_yminloc) +
                # final formatting and legend
                scale_color_manual(labels=leg_col_labels, values = leg_col_scale) +
                theme(legend.position="bottom",
                      legend.title=element_blank(),
                      legend.margin=margin(0,0,0,0),
                      axis.title.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(size=12),
                      axis.text.y=element_text(size=12),
                      panel.border = element_rect(colour="black", fill=NA, size=0.4))
            
            # make the download button visible since there are data available
            enable("savedensplot")
            
        } else {
            
            p <- p +
                ggtitle(plot_title) +
                annotation_custom(grobTree(textGrob(em)))
            
            # make the download button invisible
            disable("savedensplot")
            
        }
        
        # output the graph
        return(p)
        
    })
    
    
    # RENDER DENSITY PLOT (print to screen)
    output$chla_hist <- renderPlot({
        
        make_density_plot()
        
    })
    
    
    #***************************************************************************
    # BLOOMFIT SCATTERPLOT ####
    
    make_bloom_fit <- reactive({
        
        # get the most recent annual data
        rchla <- annual_stats()
        
        plot_title <- paste0("Time series of ", isolate(state$interval), " ", isolate(state$dailystat),
                             " chlorophyll concentration for ", isolate(state$year))
        
        # Get the vector of dataframe names
        pnames <- pnlist[[state$fitmethod]]
        if (state$fitmethod=="gauss") {
            pnames <- pnames[[state$bloomShape]]
            if (state$beta) {pnames <- pnames[["beta"]]
            } else {pnames <- pnames[["nonbeta"]]}
        }
        
        state$dfbloomparms <- NULL
        
        # create base plot
        p <- ggplot() + theme_bw()
        
        
        #***********************************************************************
        # CHECK IF VALUES CAN BE FITTED
        
        # Reset error message to NULL, then check if it should be changed and
        # printed instead of doing the density plot
        em <- NULL
        
        if (state$data_loaded) {
            
            if (state$box=="custom" & state$latlon_toolarge) {
            
              em <- "Polygon is too large (max allowed area = 500 degrees^2)."
            
            } else if (state$null_rchla) {
                
                if (state$box=="custom" & is.null(state$newpoly) & is.null(state$editedpoly) & is.null(state$typedpoly)) {
                    em <- "Create your custom polygon in a region with sufficient data"
                } else {
                    em <- "No data in the selected region"
                }
                
            } else {
                
                lenok <- state$lenok
                chl_mean <- state$chl_mean
                chl_median <- state$chl_median
                
                first_day <- state$t_range[1]
                last_day <- state$t_range[2]
                
                doy_vec <- state$doy_vec
                available_days <- state$available_days
                
                daily_percov <- lenok / nrow(rchla)
                ind_percov <- daily_percov > state$percent
                ind_dayrange <- doy_vec >= first_day & doy_vec <= min(last_day, available_days)
                ind_dayrange_percov <- ind_percov & ind_dayrange
                ydays_percov <- doy_vec[ind_percov] # all days with high enough percent coverage
                ydays_dayrange <- doy_vec[ind_dayrange]
                ydays_dayrange_percov <- doy_vec[ind_dayrange_percov] # subset of days used for fit
                
                # If there is no data available for the fit after removing days outside
                # the day range and with insufficient data, print an error message instead.
                if (sum(ydays_dayrange_percov)==0) {
                
                    em <- paste0("No data available between day ", first_day, " and ",
                                 last_day, " with >= ", (state$percent*100), "% coverage")
                    
                }
                
            }
            
        } else {
            em <- "Load data to begin"
        }
        
        
        #***********************************************************************
        # IF VALUES CAN BE FITTED, FIT THEM
        
        if (is.null(em)) {
            
            bf_data <- get_bloom_fit_data(interval=isolate(state$interval),
                                          p=p,
                                          pnames = pnames,
                                          dailystat = state$dailystat,
                                          chl_mean = chl_mean,
                                          chl_median = chl_median,
                                          lenok = lenok,
                                          ind_dayrange_percov = ind_dayrange_percov,
                                          ind_percov = ind_percov,
                                          ydays_dayrange_percov = ydays_dayrange_percov,
                                          ydays_percov = ydays_percov,
                                          ydays_dayrange = ydays_dayrange,
                                          rchla_nrow = nrow(rchla),
                                          use_weights = state$use_weights,
                                          smoothMethod = state$smoothMethod,
                                          loessSpan = state$loessSpan,
                                          fitmethod = state$fitmethod,
                                          bloomShape = state$bloomShape,
                                          daily_percov = daily_percov,
                                          tm = state$tm,
                                          beta = state$beta,
                                          tm_limits = state$tm_limits,
                                          ti_limits = state$ti_limits,
                                          log_chla = isolate(state$log_chla),
                                          threshcoef = state$threshcoef,
                                          doy_vec = doy_vec,
                                          plot_title = plot_title,
                                          flag1_lim1 = state$flag1_lim1,
                                          flag1_lim2 = state$flag1_lim2,
                                          flag2_lim1 = state$flag2_lim1,
                                          flag2_lim2 = state$flag2_lim2,
                                          ti_threshold = state$ti_threshold,
                                          tt_threshold = state$tt_threshold)
            
            p <- bf_data$p
            
            state$fitparams <- bf_data$fitparams
            
            # dataframe for nearPoints
            state$dfbloomparms <- data.frame(y = bf_data$chlall, x = ydays_percov)
            
            enable("saveannualstats")
            enable("savebloomfit")
            enable("savebloomparams")
            
        
        } else {
            
            p <- p +
                ggtitle(plot_title) +
                annotation_custom(grobTree(textGrob(em)))
            
            # make the download bloom fit buttons invisible
            disable("savebloomfit")
            disable("savebloomparams")
            disable("saveannualstats")
            
            state$dfbloomparms <- NULL
            
        }
        
        # output the graph
        # note: do not use "print(p)" or the bloomfit_click function will not work
        return(p)
        
    }) # closes "observe"
    
    
    # RENDER BLOOM FIT PLOT (print to screen)
    output$bloomfit <- renderPlot({
        
        make_bloom_fit()
        
    })
    
    
    # BLOOMFIT POINT CLICK ####
    observeEvent(input$bloomfit_click, {
        
        if (!is.null(state$dfbloomparms)) {
            
            state$bloomfit_click <- input$bloomfit_click
            
            npyday <- nearPoints(state$dfbloomparms,
                                 coordinfo = state$bloomfit_click,
                                 xvar = "x",
                                 yvar = "y")$x
            updateSliderInput(session, inputId = 'yearday_slide', value = npyday)
            updateNumericInput(session, inputId = "yearday_num", value = npyday)
            
        }
        
    })
    
    
    
    # RUN FULL TIME SERIES ####
    
    # Take a vector of years selected by the user, and compute annual statistics
    # and bloom fits for each, using the current settings
    observeEvent(input$fullrun_process, {
        
        regs <- isolate(state$fullrunboxes)
        
        # if "custom" box is selected but no polygon is drawn, unselect it
        if (is.null(isolate(state$newpoly)) & is.null(isolate(state$editedpoly))) {
            regs <- regs[regs != "custom"]
        }
        
        # Get variables
        isolate({
            satellite <- state$satellite
            region <- state$region
            algorithm <- state$algorithm
            interval <- state$interval
            log_chla <- state$log_chla
            yearday <- state$yearday
            fitmethod <- state$fitmethod
            bloomShape <- state$bloomShape
            tm <- state$tm
            beta <- state$beta
            custom_name <- state$custom_name
            polylat <- state$polylat
            polylon <- state$polylon
            coord_list <- state$coord_list
            latlon_method <- state$latlon_method
            dailystat <- state$dailystat
            pixrange1 <- state$pixrange1
            pixrange2 <- state$pixrange2
            outlier <- state$outlier
            percent <- state$percent
            smoothMethod <- state$smoothMethod
            loessSpan <- state$loessSpan
            use_weights <- state$use_weights
            threshcoef <- state$threshcoef
            t_range <- state$t_range
            tm_limits <- state$tm_limits
            ti_limits <- state$ti_limits
            flag1_lim1 <- state$flag1_lim1
            flag1_lim2 <- state$flag1_lim2
            flag2_lim1 <- state$flag2_lim1
            flag2_lim2 <- state$flag2_lim2
            ti_threshold = state$ti_threshold
            tt_threshold = state$tt_threshold
        })
        
        # create column names for parameter table
        pnames <- pnlist[[fitmethod]]
        if (fitmethod=="gauss") {
            pnames <- pnames[[bloomShape]]
            if (beta) {pnames <- pnames[["beta"]]
            } else {pnames <- pnames[["nonbeta"]]}
        }
        
        year_bounds <- isolate(input$fullrunyears)
        year_list <- (year_bounds[1]):(year_bounds[2])
        
        # grey out the screen while processing, and show progress bar
        show_modal_progress_line(text = paste0("Computing fits for ", year_list[1], "..."))
        
        # Create output subfolders
        output_dir <- file.path(tempdir(),
                                output_str(satellite=satellite,
                                           region=region,
                                           algorithm=algorithm,
                                           year=year_bounds,
                                           interval=interval,
                                           log_chla=log_chla,
                                           fitmethod=fitmethod,
                                           custom_end="fulltimeseries"))
        dir.create(output_dir)
        
        steps <- 100/length(year_list)
        progress_updates <- round(seq(steps[1], 100, by=steps),1)
        
        poly_names <- sapply(1:length(regs), function(r) ifelse(regs[r]=='custom',
                                                                ifelse(nchar(custom_name)==0, "Custom polygon", custom_name),
                                                                paste0(full_names[[region]][which(regs[r]==poly_ID[[region]])])))
        
        boxes <- all_regions[[region]]
        names(boxes) <- poly_ID[[region]]
        if ("custom" %in% regs) {
            boxes[["custom"]] <- list()
            boxes[["custom"]]$lat <- polylat
            boxes[["custom"]]$lon <- polylon
        }
        boxes <- boxes[regs]
        
        total_params_df <- data.frame(matrix(nrow=(length(year_list)*length(regs)), ncol=(length(pnames)+2)), stringsAsFactors = FALSE)
        colnames(total_params_df) <- c("Region", "Year", pnames)
        
        for (x in 1:length(year_list)) {
            
            tmp_par <- full_run(
                year = year_list[x],
                satellite = satellite,
                region = region,
                algorithm = algorithm,
                interval = interval,
                sslat = coord_list[[region]]$lat,
                sslon = coord_list[[region]]$lon,
                boxes = boxes,
                latlon_method = latlon_method,
                pnames = pnames,
                yearday = yearday,
                doys_per_week = doys_per_week,
                doy_week_start = doy_week_start,
                doy_week_end = doy_week_end,
                dailystat = dailystat,
                pixrange1 = pixrange1,
                pixrange2 = pixrange2,
                outlier = outlier,
                percent = percent,
                log_chla = log_chla,
                poly_names = poly_names,
                fitmethod = fitmethod,
                bloomShape = bloomShape,
                smoothMethod = smoothMethod,
                loessSpan = loessSpan,
                use_weights = use_weights,
                threshcoef = threshcoef,
                tm = tm,
                beta = beta,
                t_range = t_range,
                tm_limits = tm_limits,
                ti_limits = ti_limits,
                dir_name = output_dir,
                flag1_lim1 = flag1_lim1,
                flag1_lim2 = flag1_lim2,
                flag2_lim1 = flag2_lim1,
                flag2_lim2 = flag2_lim2,
                ti_threshold = ti_threshold,
                tt_threshold = tt_threshold)
            
            # add to final output dataframe
            total_params_df[((x-1)*length(regs)+1):(x*length(regs)),] <- tmp_par
            
            # update progress bar
            if (x==length(year_list)) {update_text <- "Zipping output files..."
            } else {update_text <- paste0("Computing fits for ", year_list[x+1], "...")}
            update_modal_progress(value = (progress_updates[x]/100), text = update_text)
            
            gc()
            
        }
        
        write.csv(total_params_df %>% dplyr::arrange(., Region, Year),
                  file=file.path(output_dir, "bloom_fit_params.csv"),
                  quote=FALSE,
                  na=" ",
                  row.names=FALSE)
        
        
        # SAVE SETTINGS
        
        info <- settings_str(satellite = names(sensors)[sensors==satellite],
                             region = names(regions)[regions==region],
                             algorithm = names(algorithms)[algorithms==algorithm],
                             year_list = year_bounds,
                             date_var = NA,
                             interval = interval,
                             log_chla = log_chla,
                             polygon_name_list = poly_names,
                             polygon_coord_list = boxes,
                             percent = percent,
                             outlier = outlier,
                             dailystat = dailystat,
                             pixrange1 = pixrange1,
                             pixrange2 = pixrange2,
                             fitmethod = names(fitmethods)[fitmethods==fitmethod],
                             bloomShape = names(bloomShapes)[bloomShapes==bloomShape],
                             smoothMethod = names(smoothMethods)[smoothMethods==smoothMethod],
                             loessSpan = loessSpan,
                             t_range = t_range,
                             ti_limits = ti_limits,
                             tm_limits = tm_limits,
                             tm = tm,
                             beta = beta,
                             use_weights = use_weights,
                             threshcoef = threshcoef)
        
        if (year_bounds[1]==year_bounds[2]) {
            year_bounds <- year_bounds[1]
        } else {
            year_bounds <- paste(year_bounds, collapse="-")
        }
        
        fileConn <- file(file.path(output_dir, "settings.txt"))
        writeLines(info, fileConn)
        close(fileConn)
        
        gc()
        
        fname <- output_str(satellite=satellite,
                            region=region,
                            algorithm=algorithm,
                            year=isolate(input$fullrunyears),
                            interval=interval,
                            log_chla=log_chla,
                            fitmethod=fitmethod,
                            custom_end="fulltimeseries.zip")
        
        # zip files up to be downloaded
        # j flag prevents files from being sorted into subdirectories inside the zip file (the other flags are defaults)
        zip(file.path(output_dir, fname), list.files(output_dir, full.names=TRUE), flags = "-r9Xj")
        
        # remove progress bar and return to normal screen
        remove_modal_progress()
        
        state$fullrun_outputdir <- output_dir
        state$fullrun_fname <- fname
        
        enable("fullrun_download")
        
    })
    
    output$fullrun_fname <- renderUI({
        if (is.null(state$fullrun_fname)) {
            helpText("",
                     width = widget_width,
                     style = help_text_style)
        } else {
            helpText(HTML(paste0("File ready for download:<br>", gsub("_", "_ ", state$fullrun_fname))),
                     width = widget_width,
                     style = help_text_style)
        }
    })
    
    # Download the results from "fullrun_process"
    output$fullrun_download <- downloadHandler(
        filename <- function() {
            isolate(state$fullrun_fname)
        },
        content <- function(file) {
            file.copy(file.path(isolate(state$fullrun_outputdir), isolate(state$fullrun_fname)), file)
        },
        contentType = "application/zip"
    )
    
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    # DOWNLOAD OUTPUT ####
    #
    # map:                              .html
    # density plot, bloom fit plot:     .png
    # annual stats, bloom parameters:   .csv
    # info/current settings:            .txt
    #
    # Each has a shiny download button that will download the objects to the
    # browser's default downloads folder.
    
    
    # SAVE MAP
    output$savemap <- downloadHandler(
        filename <- function() {
            output_str(satellite=isolate(state$satellite),
                       region=isolate(state$region),
                       algorithm=isolate(state$algorithm),
                       year=isolate(state$year),
                       interval=isolate(state$interval),
                       log_chla=isolate(state$log_chla),
                       day_label=gsub(" ", "", strsplit(isolate(state$day_label), "[()]+")[[1]][2]),
                       polygon=gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                       fitmethod=isolate(state$fitmethod),
                       custom_end="map.html")
            },
        content <- function(file) {
            isolate({
                pc <- state$tr_coloradj
                cm <- state$cm
                lt <- state$leg_title
                dl <- state$day_label
                zl <- state$zlim
            })
            saveWidget(widget = map_reactive() %>%
                           clearControls() %>%
                           clearImages() %>%
                           addRasterImage(x = pc, colors = cm) %>%
                           addLegend(position = 'topright',
                                     pal = cm,
                                     values = c(getValues(pc), zl),
                                     title = lt,
                                     bins = 10,
                                     opacity = 1) %>%
                           # Label map with current year and day of year
                           addControl(tags$div(tag.map.title, HTML(dl)),
                                      position = "topleft",
                                      className = "map-title"),
                       file = file)
        }
    )
    
    
    # SAVE DENSITY PLOT
    output$savedensplot <- downloadHandler(
        filename <- function() {
            output_str(satellite=isolate(state$satellite),
                       region=isolate(state$region),
                       algorithm=isolate(state$algorithm),
                       year=isolate(state$year),
                       interval=isolate(state$interval),
                       log_chla=isolate(state$log_chla),
                       day_label=gsub(" ", "", strsplit(isolate(state$day_label), "[()]+")[[1]][2]),
                       polygon=gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                       fitmethod=isolate(state$fitmethod),
                       custom_end="density_plot.png")
            },
        content <- function(file) {
            ggsave(file=file,
                   plot=isolate(make_density_plot()),
                   width=12,
                   height=5,
                   units="in")
        }
    )
    
    
    # SAVE BLOOM FIT PLOT
    output$savebloomfit <- downloadHandler(
        filename <- function() {
            output_str(satellite=isolate(state$satellite),
                       region=isolate(state$region),
                       algorithm=isolate(state$algorithm),
                       year=isolate(state$year),
                       interval=isolate(state$interval),
                       log_chla=isolate(state$log_chla),
                       polygon=gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                       fitmethod=isolate(state$fitmethod),
                       custom_end="bloom_fit.png")
            },
        content <- function(file) {
            ggsave(file=file,
                   plot=isolate(make_bloom_fit()),
                   width=12,
                   height=5,
                   units="in")
        }
    )
    
    
    # SAVE ANNUAL STATS
    output$saveannualstats <- downloadHandler(
        filename <- function() {
            output_str(satellite=isolate(state$satellite),
                       region=isolate(state$region),
                       algorithm=isolate(state$algorithm),
                       year=isolate(state$year),
                       interval=isolate(state$interval),
                       log_chla=isolate(state$log_chla),
                       polygon=gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                       fitmethod=isolate(state$fitmethod),
                       custom_end="annual_stats.csv")
            },
        content <- function(file) {
            write.csv(data.frame(doy=isolate(state$doy_vec),
                                 mean_chl=isolate(state$chl_mean),
                                 median_chl=isolate(state$chl_median),
                                 stdev_chl=isolate(state$chl_sd),
                                 min_chl=isolate(state$chl_min),
                                 max_chl=isolate(state$chl_max),
                                 nobs=isolate(state$nobs),
                                 percent_coverage=isolate(state$percent_coverage),
                                 stringsAsFactors=FALSE),
                      file=file,
                      quote=FALSE,
                      na=" ",
                      row.names=FALSE)
        }
    )
    
    # SAVE BLOOM FIT PARAMETERS
    output$savebloomparams <- downloadHandler(
        filename <- function() {
            output_str(satellite=isolate(state$satellite),
                       region=isolate(state$region),
                       algorithm=isolate(state$algorithm),
                       year=isolate(state$year),
                       interval=isolate(state$interval),
                       log_chla=isolate(state$log_chla),
                       polygon=gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                       fitmethod=isolate(state$fitmethod),
                       custom_end="bloom_parameters.csv")
            },
        content <- function(file) {
            write.csv(isolate(state$fitparams),
                      file=file,
                      quote=FALSE,
                      na=" ",
                      row.names=FALSE)
        }
    )
    
    # SAVE CURRENT INFO/SETTINGS (if annual data has been loaded)
    output$savesettings <- downloadHandler(
        filename <- function() {
            output_str(satellite=isolate(state$satellite),
                       region=isolate(state$region),
                       algorithm=isolate(state$algorithm),
                       year=isolate(state$year),
                       interval=isolate(state$interval),
                       log_chla=isolate(state$log_chla),
                       day_label=gsub(" ", "", strsplit(isolate(state$day_label), "[()]+")[[1]][2]),
                       polygon=gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                       fitmethod=isolate(state$fitmethod),
                       custom_end="settings.txt")
            },
        content <- function(file) {
            info <- settings_str(satellite = names(sensors)[sensors==isolate(state$satellite)],
                                 region = names(regions)[regions==isolate(state$region)],
                                 algorithm = names(algorithms)[algorithms==isolate(state$algorithm)],
                                 year_list = isolate(state$year),
                                 date_var = gsub(" \\d{4} ", " ", isolate(state$day_label)), # remove the year
                                 interval = isolate(state$interval),
                                 log_chla = isolate(state$log_chla),
                                 polygon_name_list = isolate(state$poly_name),
                                 polygon_coord_list = list(box=list(lon=isolate(state$polylon),
                                                                    lat=isolate(state$polylat))),
                                 percent = isolate(state$percent),
                                 outlier = isolate(state$outlier),
                                 dailystat = isolate(state$dailystat),
                                 pixrange1 = isolate(state$pixrange1),
                                 pixrange2 = isolate(state$pixrange2),
                                 fitmethod = names(fitmethods)[fitmethods==isolate(state$fitmethod)],
                                 bloomShape = names(bloomShapes)[bloomShapes==isolate(state$bloomShape)],
                                 smoothMethod = names(smoothMethods)[smoothMethods==isolate(state$smoothMethod)],
                                 loessSpan = isolate(state$loessSpan),
                                 t_range = isolate(state$t_range),
                                 ti_limits = isolate(state$ti_limits),
                                 tm_limits = isolate(state$tm_limits),
                                 tm = isolate(state$tm),
                                 beta = isolate(state$beta),
                                 use_weights = isolate(state$use_weights),
                                 threshcoef = isolate(state$threshcoef))
            fileConn <- file(file)
            writeLines(info, fileConn)
            close(fileConn)
        }
    )
    
}

# RUN APPLICATION ####
shinyApp(ui = ui, server = server)
