# clear memory to free up space
gc()

# LIBRARIES ####

library(fst)            # for speedier data file loading
library(shiny)
library(shinyWidgets)   # for updating buttons
library(shinyjs)        # for enabling/disabling download buttons
library(shinybusy)      # to show progress when processing a full time series
library(htmlwidgets)    # to use saveWidget to save the map
library(leaflet)        # for creating map
library(leaflet.extras) # for custom polygon drawings on map
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
# library(htmlTable)      # for making tables in popups
# library(geosphere)      # for calculating accurate distances between single point click and data point plotted on the map
# library(RSQLite)        # for reading satellite data from databases instead of .rda files
# library(dbplyr)         # also for databases

# suppress some repetitive warnings when using addRasterImage in leafletProxy
options("rgdal_show_exportToProj4_warnings"="none")

source("rateOfChange.R")    # rate of change (ROC) function for bloom fit
source("threshold.R")       # threshold function for bloom fit
source("gaussFit.R")        # gaussian function for bloom fit
source("00_regionBoxes.R")  # contains coordinates of boxes/polygons
source("full_run.R")        # contains function to run full time series with current settings
source("functions.R")       # extra functions


#*******************************************************************************
# VARIABLES ####

default_sensors <- c("MODIS 4km" = "modis",
                     "VIIRS 4km" = "viirs")

# regions with available data for each sensor
regions <- list("modis"=c("Atlantic"="atlantic",
                          "Pacific"="pacific"),
                "viirs"=c("Atlantic"="atlantic",
                          "Pacific"="pacific"))
default_regions <- regions[["modis"]]

# chlorophyll algorithms with available data for each sensor
algorithms <- list("modis"=c("OCx"="ocx",
                             "POLY4"="poly4"),
                   "viirs"=c("OCx"="ocx",
                             "POLY4"="poly4"))
default_algorithms <- algorithms[["modis"]]

# years with available data for each sensor
years <- list("modis"=2003:2020,
              "viirs"=2012:2020)
for (i in 1:length(years)) {names(years[[i]]) <- years[[i]]}
default_years <- years[["modis"]]

default_intervals <- c("Daily"="daily",
                       "Weekly"="weekly")

default_fitmethods <- c('Shifted Gaussian' = 'gauss',
                        'Rate of Change' = 'roc',
                        'Threshold' = 'thresh')

default_bloomShapes <- c('Symmetric' = 'symmetric',
                         'Asymmetric' = 'asymmetric')

default_smoothMethods <- c('LOESS smooth' = 'loess',
                           'No smooth' = 'nofit')

# bloom fit table parameter names, depending on fitmethod, bloomShape, beta (code \u03B2 to get the symbol)
pnlist <- list("gauss"=list("symmetric"=list("beta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                      "Magnitude", "Amplitude", "B0", "h", "sigma", "\u03B2"),
                                             "nonbeta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                         "Magnitude", "Amplitude", "B0", "h", "sigma")),
                            "asymmetric"=list("beta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                       "Magnitude[left]", "Amplitude[left]",
                                                       "B0[left]", "h[left]", "sigma[left]",
                                                       "Magnitude[right]", "Amplitude[right]",
                                                       "B0[right]", "h[right]", "sigma[right]",
                                                       "\u03B2[left]", "\u03B2[right]"),
                                              "nonbeta"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                                          "Magnitude[left]", "Amplitude[left]",
                                                          "B0[left]", "h[left]", "sigma[left]",
                                                          "Magnitude[right]", "Amplitude[right]",
                                                          "B0[right]", "h[right]", "sigma[right]"))),
               "roc"=c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                       "t[duration]", "Magnitude", "Amplitude"),
               "thresh"=c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                          "t[duration]", "Magnitude", "Amplitude", "Threshold"))


# variables for using weekly data rather than daily
doy_week_start <- as.integer(8*(0:45)+1) # note: this is the same for leap years, using NASA's system
doy_week_end <- c(doy_week_start[2:length(doy_week_start)] - 1, 365)
doys_per_week <- lapply(1:length(doy_week_start), function(i) {doy_week_start[i]:doy_week_end[i]})



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

# Style for the horizontal bar in the sidebar, and the multi-column styling in 
# the "full run" polygon selection.
# This variable is called at the top of the UI
hr_multicol_style <- tags$style(HTML(
            "hr {border-top: 1px solid #bbbbbb;}
            .multicol {
                       font-size: 14px;
                       -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                       -moz-column-count: 2;    /* Firefox */ 
                       column-count: 2; 
                       -moz-column-fill: auto;
                       -column-fill: auto;
                     }"
))


#*******************************************************************************
# UI  ####

ui <- fluidPage(
    
    # For hiding download buttons if data hasn't been loaded yet
    useShinyjs(),
    
    # hr() and multi-column styling, and old polygon removal code
    tags$head(hr_multicol_style),
    tags$div(remove_custom_poly),
    
    chooseSliderSkin("Modern"),
    
    # App title
    titlePanel("Satellite Chlorophyll Data Visualization"),
    
    fluidRow(
        
        sidebarPanel(width = 3,
            
            # UI LOAD OPTIONS ####
            
            helpText("Select satellite, region, chlorophyll algorithm, year, composite length (daily or weekly images), and logged or unlogged chlorophyll, then click \"Load data\".",
                      width = widget_width,
                      style = help_text_style),
            selectInput(inputId = "satellite",
                        label = NULL,
                        choices = default_sensors,
                        width = widget_width),
            selectInput(inputId = "region",
                        label = NULL,
                        choices = default_regions,
                        width = widget_width),
            selectInput(inputId = "algorithm",
                        label = NULL,
                        choices = default_algorithms,
                        width = widget_width),
            selectInput(inputId = "year",
                        label = NULL,
                        choices = default_years,
                        selected = default_years[length(default_years)],
                        width = widget_width),
            selectInput(inputId = "interval",
                        label = NULL,
                        choices = default_intervals,
                        selected = "daily",
                        width = widget_width),
            switchInput(inputId = "log_chla",
                        label = HTML("log<sub>chla</sub>"),
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
            
            # UI YEAR DAY ####
            
            br(),
            
            helpText(HTML(paste0("<font style=\"font-size: 14px; color: #404040; font-weight: bold;\">Choose day of year</font></br>",
                                 "Enter the day of year and click \"Go\", or drag the slider to view the map for that day. ",
                                 "Use the \"play/pause\" button on the slider to move through a sequence of daily chlorophyll maps automatically.")),
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
                                       label = HTML("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">List of latitudes:</font>"),
                                       value = "",
                                       width = widget_width),
                             textInput(inputId = "manual_lons",
                                       label = HTML("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">List of longitudes:</font>"),
                                       value = "",
                                       width = widget_width),
                             actionButton(inputId = 'draw',
                                          label = 'Create polygon',
                                          width = widget_width,
                                          style = button_style),
                             uiOutput("help_latlon",
                                      width = widget_width,
                                      style = "white-space: normal;"),
                             br())
            
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
            
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">Minimum daily % coverage</font></br>",
                                 "Days with less than the minimum percent coverage in the selected polygon will not be plotted on the density plot or time series, or used in the bloom fit.")),
                     width = widget_width,
                     style = help_text_style),
            numericInput(inputId = 'percent',
                         label = NULL,
                         value = 10,
                         min = 0,
                         max = 100,
                         width = widget_width),
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">Outlier detection method</font></br>",
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
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">Daily statistic</font></br>",
                                 "Choose to use either daily mean or median chlorophyll in the time series and bloom fit.")),
                     width = widget_width,
                     style = help_text_style),
            selectInput(inputId = 'dailystat',
                        label = NULL,
                        choices = c('Mean' = 'avg',
                                    'Median' = 'med'),
                        selected = 'avg',
                        width = widget_width),
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">Maximum pixel value</font></br>",
                                 "Choose the maximum value allowed in the calculation of the statistics and bloom fit (pixels above this value will be omitted).</br>",
                                 "Leave blank to use all values.")),
                     width = widget_width,
                     style = help_text_style),
            numericInput(inputId = "maxpixval",
                         label = NULL,
                         min = -Inf,
                         max = Inf,
                         value = NA,
                         width = widget_width),
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
                        choices = default_fitmethods,
                        selected = 'gauss',
                        width = widget_width),
            selectInput(inputId = 'bloomShape',
                        label = NULL,
                        choices = default_bloomShapes,
                        selected = 'symmetric',
                        width = widget_width),
            selectInput(inputId = 'smoothMethod',
                        label = NULL,
                        choices = default_smoothMethods,
                        selected = 'nofit',
                        width = widget_width),
            conditionalPanel(condition = "input.smoothMethod == 'loess'",
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">LOESS span</font></br>",
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
                        label = HTML("<font style=\"font-size: 14px; color: #404040; font-weight: bold;\">t<sub>range</sub></font>"),
                        min = 1,
                        max = 365,
                        value = c(31,274),
                        ticks = FALSE),
            sliderInput(inputId = 'tm_limits',
                        label = HTML("<font style=\"font-size: 14px; color: #404040; font-weight: bold;\">t<sub>max</sub></font>"),
                        min = 1,
                        max = 365,
                        value = c(91,181),
                        ticks = FALSE),
            sliderInput(inputId = 'ti_limits',
                        label = HTML("<font style=\"font-size: 14px; color: #404040; font-weight: bold;\">t<sub>start</sub></font>"),
                        min = 1,
                        max = 365,
                        value = c(60,151),
                        ticks = FALSE),
            conditionalPanel(condition = "input.fitmethod == 'gauss'",
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
                             helpText("Switch to ON to weight each daily point in the fit by percent coverage.",
                                      width = widget_width,
                                      style = help_text_style),
                             switchInput(inputId = 'use_weights',
                                         label = 'weights',
                                         value = FALSE,
                                         onStatus = "success")),
            conditionalPanel(condition = "input.fitmethod == 'thresh'",
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">Threshold coefficient</font></br>",
                                                  "The start of the bloom is considered to be the point before t<sub>max</sub> ",
                                                  "when chlorophyll concentration drops below a threshold for > 14 days.</br>",
                                                  "Threshold = chla<sub>med</sub> * threshold coefficient</br>",
                                                  "chla<sub>med</sub> = UNLOGGED median chlorophyll</br>",
                                                  "NOTE: If log<sub>chla</sub> is ON, the resulting theshold will be logged.")),
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
            
            helpText(HTML(paste0("<font style=\"font-size: 14px; color: #404040; font-weight: bold;\">Time series</font></br>",
                                 "Select a series of years and the polygons you would like to process, ",
                                 "then click \"Run time series\" to generate the following:</br>",
                                 "<ul>",
                                    "<li>time series plots (.png),</li>",
                                    "<li>tables of statistics (.csv),</li>",
                                    "<li>a single .csv file containing the fitted parameters for all selected years and polygons, and</li>",
                                    "<li>a .txt file containing the settings used for the time series, for reference.</li>",
                                 "</ul>",
                                 "The settings used in the time series will be the current selections for satellite, ",
                                 "region, algorithm, interval, log<sub>chla</sub> ON/OFF, statistics, and bloom fit. Files will be written",
                                 " to a folder following the naming convention satellite_region_algorithm_years_interval_(un)loggedChla_fitmethod_timecreated.</br>",
                                 "Make sure at least one polygon is selected.")),
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
            actionButton(inputId = 'fullrun',
                         label = 'Run time series',
                         style = button_style),
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
                          height = '360px',
                          click = 'bloomfit_click'),
               disabled(downloadButton(outputId = "savebloomfit",
                                       label = "Download time series plot of daily chlorophyll (.png)",
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
    
    # Initial popup description
    observe({
        showModal(modalDialog(
                    title = "Satellite Chlorophyll Data Visualization",
                    "This app can be used to display satellite chlorophyll concentration and model phytoplankton blooms. Use the controls in the left panel to visualize statistics for DFO regions of interest or draw your own, and export data and graphs.",
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
        
        tmp_regions <- regions[[input$satellite]]
        updateSelectInput(session,
                          inputId = "region",
                          label = NULL,
                          choices = tmp_regions)
        
        tmp_algorithms <- algorithms[[input$satellite]]
        updateSelectInput(session,
                          inputId = "algorithm",
                          label = NULL,
                          choices = tmp_algorithms)
        
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
            state$lon_range <- c(-71, -42)
            state$lat_range <- c(39, 63)
        } else if (reg=="pacific") {
            state$lon_range <- c(-140, -122)
            state$lat_range <- c(46, 60)
        }
        
        # Make polygons for existing boxes, to add to base leaflet map
        original_polys <- lapply(1:length(all_regions[[reg]]), function(k) {Polygon(coords=cbind(all_regions[[reg]][[k]]$lon, all_regions[[reg]][[k]]$lat), hole=TRUE)})
        original_polyIDs <- lapply(1:length(original_polys), function(k) {Polygons(list(original_polys[[k]]), toupper(names(all_regions[[reg]])[k]))})
        state$original_polylist <- SpatialPolygons(original_polyIDs, 1:length(all_regions[[reg]]))
        
    })
    
    
    
    # GET BOX/POLYGON ####
    
    output$box <- renderUI({
        
        # default choices, used for atlantic
        choices <- c("custom", names(all_regions[["atlantic"]]))
        names(choices) <- c("Custom polygon", full_names[["atlantic"]])
        
        if (state$region == "pacific") {
            
            choices <- c("custom", names(all_regions[["pacific"]]))
            names(choices) <- c("Custom polygon", full_names[["pacific"]])
            
        }
        
        selectInput(inputId = 'box',
                    label = HTML("<font style=\"font-size: 14px; color: #404040; font-weight: bold;\">Choose a polygon</font>"),
                    choices = choices,
                    selected = 'custom',
                    width = widget_width)
        
    })
    
    observeEvent(input$box,{
        state$box <- input$box
    })
    observeEvent(input$latlon_method, {
        state$latlon_method <- input$latlon_method
    })
    observeEvent(input$applyname, {
        state$custom_name <- input$custom_name
    })
    
    observe({
        state$poly_name <- ifelse(state$box=='custom',
                                  ifelse(nchar(state$custom_name)==0, "Custom polygon", state$custom_name),
                                  paste0(full_names[[isolate(state$region)]][which(state$box==names(all_regions[[isolate(state$region)]]))]))
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
    observeEvent(input$maxpixval, {
        state$maxpixval <- as.numeric(input$maxpixval)
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
    observeEvent(input$tm,{
        state$tm <- input$tm
    })
    observeEvent(input$beta,{
        state$beta <- input$beta
    })
    observeEvent(input$use_weights,{
        state$use_weights <- input$use_weights
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
        
        choices <- c("custom", names(all_regions[[input$region]]))
        names(choices) <- c("Custom", toupper(names(all_regions[[input$region]])))
        
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
        
        choices <- c("custom", names(all_regions[[input$region]]))
        names(choices) <- c("Custom", toupper(names(all_regions[[input$region]])))
        
        if (ifrab) {
            state$fullrunboxes <- choices
        } else {
            state$fullrunboxes <- input$fullrunboxes
        }
        
    })
    
    observeEvent(input$fullrunboxes, {
        
        state$fullrunboxes <- input$fullrunboxes
        
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
        if (state$interval=="daily") {
            updateSliderInput(session,
                              inputId = 'yearday_slide',
                              value = as.numeric(state$yearday),
                              step = 1)
        } else if (state$interval=="weekly") {
            updateSliderInput(session,
                              inputId = 'yearday_slide',
                              value = as.numeric(state$doy_vec[state$time_ind]),
                              step = 8)
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
    
        abbrev <- sapply(strsplit(full_names[[isolate(state$region)]], "[()]+"), "[[", 2)
        abbrev[duplicated(abbrev)] <- NA
        
        # Use leaflet() here, and only include aspects of the map that won't need
        # to change dynamically unless the entire map is torn down and recreated.
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles("Esri.WorldGrayCanvas",
                             options = providerTileOptions(minZoom = 4,
                                                           maxZoom = 10,
                                                           updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
                                                           updateWhenIdle = TRUE)) %>% # map won't load new tiles when panning
            fitBounds(lng1 = state$lon_range[1],
                      lat1 = state$lat_range[1],
                      lng2 = state$lon_range[2],
                      lat2 = state$lat_range[2]) %>%
            # Add boxes based on the current AZMP statistic boxes
            addPolygons(group = "Stats boxes",
                        data = state$original_polylist,
                        stroke = TRUE,
                        color = "magenta",
                        weight = 2,
                        opacity = 1,
                        fill = FALSE,
                        label = abbrev,#names(state$original_polylist),
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
                #clearGroup("Points") %>%
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
                    #clearGroup("Points") %>%
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
                xres <- 0.065
                yres <- (2/3) * xres
                tr <- raster(ext=extent(pts),
                             resolution = c(xres,yres))
                # rasterize your irregular points
                tr <- rasterize(pts, tr, pts$chl, fun = mean, na.rm = T) # we use a mean function here to regularly grid the irregular input points
                # state$tr <- tr # only used for input$fullmap_click, currently disabled
                
                # Create colour scale for leaflet map
                # Note: getting values from the raster is slower due to the "getValues" function
                if (state$log_chla) {
                    
                    # # pre-defined scale:
                    # zlim <- c(0, log(20))
                    
                    # getting values from the raster itself:
                    zlim <- c(min(getValues(tr), na.rm=TRUE),
                              min(log(20), max(getValues(tr), na.rm=TRUE)))
                    
                    leg_title <- "<center>Chlorophyll-a</br>[ log mg m<sup>-3</sup> ]</center>"
                
                } else {
                    
                    # # pre-defined scale:
                    # zlim <- c(0, 20)
                    
                    # getting values from the raster itself:
                    zlim <- c(min(getValues(tr), na.rm=TRUE),
                              min(20, max(getValues(tr), na.rm=TRUE)))
                    
                    leg_title <- "<center>Chlorophyll-a</br>[ mg m<sup>-3</sup> ]</center>"
                
                }
                
                cm <- colorNumeric(
                    palette = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(100),
                    domain = zlim,
                    na.color = "#00000000"# transparent
                )
                
                state$zlim <- zlim
                state$leg_title <- leg_title
                state$cm <- cm
                
                # # For plotting points, use an adjusted table of lat/lon/chl where the values
                # # above/below the color scale are set to the color scale limits.
                # pts_coloradj <- pts %>%
                #     dplyr::mutate(., chl=if_else(chl < zlim[1], zlim[1],
                #                                  if_else(chl > zlim[2], zlim[2], chl)))
                # state$pts_coloradj <- pts_coloradj
                
                # Same as above, for raster instead
                tr_coloradj <- calc(tr, function(x) ifelse(x <= zlim[1], zlim[1]+(1e-10), ifelse(x >= zlim[2], zlim[2]-(1e-10), x)))
                state$tr_coloradj <- tr_coloradj
                
                # Update map based on choices of year day
                lfp <- leafletProxy("fullmap", session) %>%
                    clearPopups() %>%
                    clearControls() %>%
                    clearImages() %>%
                    addRasterImage(x = tr_coloradj, colors = cm) %>%
                    # clearGroup("Points") %>%
                    # addCircles(data = pts_coloradj,
                    #            ~lon, ~lat,
                    #            color = ~cm(chl),
                    #            radius = 2300,
                    #            fillOpacity = 0.7,
                    #            stroke = FALSE,
                    #            group = "Points") %>%
                    addLegend(position = 'topright',
                              pal = cm,
                              values = getValues(tr_coloradj),#pts_coloradj$chl,
                              title = leg_title,
                              bins = 10,
                              opacity = 1) %>%
                    # Label map with current year and day of year
                    addControl(tags$div(tag.map.title, HTML(day_label)),
                               position = "topleft",
                               className = "map-title")
                
                # now that data has been loaded, make the download button visible
                enable("savemap")
                
            }
            
        }
        
        if (state$box=="custom" & state$latlon_method=="drawPoly") {
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

            HTML(paste0("<font style=\"font-size: 18px; color: #404040; font-weight: bold;\">", str1, "</font><br/>",
                        "<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">", str2a, "</font> ", str2b, "<br/>",
                        "<font style=\"font-size: 12px; color: #404040; font-weight: bold;\">", str3a, "</font> ", str3b))
            
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
                Longitude <- as.numeric(all_regions[[isolate(state$region)]][[state$box]]$lon)
                Latitude <- as.numeric(all_regions[[isolate(state$region)]][[state$box]]$lat)
                
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
            pixthreshold <- state$maxpixval
            if (!is.na(pixthreshold)) {
                rchla[rchla > pixthreshold] <- NA
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
    # DAILY STATS, DENSITY PLOT ####
    
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
                    em <- "Only one point selected"
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
        
        dailystat_name <- ifelse(state$dailystat == 'avg', "average", "median")
        plot_title <- paste0('Time series of daily ', dailystat_name,
                             ' chlorophyll concentration for ', isolate(state$year))
        
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
                ind_dayrange <- doy_vec > first_day & doy_vec <= min(last_day-1, available_days)
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
            
            bf_data <- get_bloom_fit_data(p=p,
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
                                          plot_title = plot_title)
            
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
    observeEvent(input$fullrun, {
        
        regs <- isolate(state$fullrunboxes)
        
        # if "custom" box is selected but no polygon is drawn, unselect it
        if (is.null(isolate(state$newpoly)) & is.null(isolate(state$editedpoly))) {
            regs <- regs[regs != "custom"]
        }
        
        
        if (length(regs) > 0) {
            
            if (is.null(isolate(state$satellite))) {
                satellite <- isolate(input$satellite)
            } else {
                satellite <- isolate(state$satellite)
            }
            if (is.null(isolate(state$log_chla))) {
                log_chla <- isolate(input$log_chla)
            } else {
                log_chla <- isolate(state$log_chla)
            }
            
            # create column names for parameter table
            fitmethod <- isolate(state$fitmethod)
            bloomShape <- isolate(state$bloomShape)
            beta <- isolate(state$beta)
            
            # Get the vector of dataframe names
            pnames <- pnlist[[fitmethod]]
            if (fitmethod=="gauss") {
                pnames <- pnames[[bloomShape]]
                if (beta) {pnames <- pnames[["beta"]]
                } else {pnames <- pnames[["nonbeta"]]}
            }
            
            # grey out the screen while processing, and show progress bar
            show_modal_progress_line()
            
            year_bounds <- input$fullrunyears
            year_list <- (year_bounds[1]):(year_bounds[2])
            
            # Create output subfolders
            tmp_odir <- get_dir("output/") # make sure "output" subfolder exists
            output_dir <- get_dir(paste0("output/",
                                         output_str(satellite=isolate(state$satellite),
                                                    region=isolate(state$region),
                                                    algorithm=isolate(state$algorithm),
                                                    year=year_bounds,
                                                    interval=isolate(state$interval),
                                                    log_chla=isolate(state$log_chla),
                                                    fitmethod=isolate(state$fitmethod),
                                                    custom_end="fulltimeseries")))
            get_dir(paste0(output_dir, "/stats_csv"))
            get_dir(paste0(output_dir, "/bloom_fit_pngs"))
            
            steps <- 100/length(year_list)
            progress_updates <- round(seq(steps[1], 100, by=steps),1)
            
            poly_names <- sapply(1:length(regs), function(r) ifelse(regs[r]=='custom',
                                                                    ifelse(nchar(isolate(state$custom_name))==0, "Custom polygon", isolate(state$custom_name)),
                                                                    paste0(full_names[[isolate(state$region)]][which(regs[r]==names(all_regions[[isolate(state$region)]]))])))
            
            boxes <- isolate(all_regions[[input$region]])
            if ("custom" %in% regs) {
                boxes[["custom"]] <- list()
                boxes[["custom"]]$lat <- isolate(state$polylat)
                boxes[["custom"]]$lon <- isolate(state$polylon)
            }
            boxes <- boxes[regs]
            
            
            total_params_df <- data.frame(matrix(nrow=(length(year_list)*length(regs)), ncol=(length(pnames)+2)), stringsAsFactors = FALSE)
            colnames(total_params_df) <- c("Region", "Year", pnames)
            
            for (x in 1:length(year_list)) {
                
                tmp_par <- full_run(
                    year = year_list[x],
                    satellite = satellite,
                    region = isolate(state$region),
                    algorithm = isolate(state$algorithm),
                    interval = isolate(state$interval),
                    sslat = state$coord_list[[isolate(state$region)]]$lat,
                    sslon = state$coord_list[[isolate(state$region)]]$lon,
                    boxes = boxes,
                    latlon_method = isolate(state$latlon_method),
                    pnames = pnames,
                    yearday = isolate(state$yearday),
                    doys_per_week = doys_per_week,
                    doy_week_start = doy_week_start,
                    doy_week_end = doy_week_end,
                    dailystat = isolate(state$dailystat),
                    maxpixval = isolate(state$maxpixval),
                    outlier = isolate(state$outlier),
                    percent = isolate(state$percent),
                    log_chla = log_chla,
                    poly_names = poly_names,
                    fitmethod = fitmethod,
                    bloomShape = bloomShape,
                    smoothMethod = isolate(state$smoothMethod),
                    loessSpan = isolate(state$loessSpan),
                    use_weights = isolate(state$use_weights),
                    threshcoef = isolate(state$threshcoef),
                    tm = isolate(state$tm),
                    beta = beta,
                    t_range = isolate(state$t_range),
                    tm_limits = isolate(state$tm_limits),
                    ti_limits = isolate(state$ti_limits),
                    dir_name = output_dir)
                
                # add to final output dataframe
                total_params_df[((x-1)*length(regs)+1):(x*length(regs)),] <- tmp_par
                
                # update progress bar
                update_modal_progress(value = (progress_updates[x]/100))
                
                gc()
                
            }
            
            write.csv(total_params_df %>% dplyr::arrange(., Region, Year),
                      file=paste0(output_dir, "/bloomfit_params.csv"),
                      quote=FALSE,
                      na="",
                      row.names=FALSE)
            
            
            # SAVE SETTINGS
            
            info <- settings_str(satellite = names(default_sensors)[default_sensors==satellite],
                                 region = names(regions[[satellite]])[regions[[satellite]]==isolate(state$region)],
                                 algorithm = names(algorithms[[satellite]])[algorithms[[satellite]]==isolate(state$algorithm)],
                                 year_list = year_bounds,
                                 date_var = NA,
                                 interval = names(default_intervals)[default_intervals==isolate(state$interval)],
                                 log_chla = isolate(state$log_chla),
                                 polygon_name_list = poly_names,
                                 polygon_coord_list = boxes,
                                 percent = isolate(state$percent),
                                 outlier = isolate(state$outlier),
                                 dailystat = isolate(state$dailystat),
                                 maxpixval = isolate(state$maxpixval),
                                 fitmethod = names(default_fitmethods)[default_fitmethods==isolate(state$fitmethod)],
                                 bloomShape = names(default_bloomShapes)[default_bloomShapes==isolate(state$bloomShape)],
                                 smoothMethod = names(default_smoothMethods)[default_smoothMethods==isolate(state$smoothMethod)],
                                 loessSpan = isolate(state$loessSpan),
                                 t_range = isolate(state$t_range),
                                 ti_limits = isolate(state$ti_limits),
                                 tm_limits = isolate(state$tm_limits),
                                 tm = isolate(state$tm),
                                 beta = isolate(state$beta),
                                 use_weights = isolate(state$use_weights),
                                 threshcoef = isolate(state$threshcoef))
            
            fileConn <- file(paste0(output_dir, "/", paste(year_bounds, collapse="-"), "_settings.txt"))
            writeLines(info, fileConn)
            close(fileConn)
            
            # remove progress bar and return to normal screen
            remove_modal_progress()
            
        }
        
        gc()
        
    })
    
    
    
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
            pc <- isolate(state$tr_coloradj)
            cm <- isolate(state$cm)
            lt <- isolate(state$leg_title)
            dl <- isolate(state$day_label)
            saveWidget(widget = map_reactive() %>%
                           clearControls() %>%
                           clearImages() %>%
                           addRasterImage(x = pc, colors = cm) %>%
                           # clearGroup("Points") %>%
                           # addCircles(data = pc, ~lon, ~lat,
                           #            color = ~cm(chl),
                           #            radius = 2300,
                           #            fillOpacity = 0.7,
                           #            stroke = FALSE,
                           #            group = "Points") %>%
                           addLegend(position = 'topright',
                                     pal = cm,
                                     values = getValues(pc),#pc$chl,
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
            write.csv(data.frame(mean_chl=isolate(state$chl_mean),
                                 median_chl=isolate(state$chl_median),
                                 stdev_chl=isolate(state$chl_sd),
                                 min_chl=isolate(state$chl_min),
                                 max_chl=isolate(state$chl_max),
                                 nobs=isolate(state$nobs),
                                 percent_coverage=isolate(state$percent_coverage),
                                 stringsAsFactors=FALSE),
                      file=file,
                      quote=FALSE,
                      na="",
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
            tmp_params <- isolate(state$fitparams)
            tmp_params[,"parameter"] <- sapply(1:nrow(tmp_params), function(i) {sub("\u03B2", "beta", tmp_params[i,"parameter"])})
            write.csv(tmp_params,
                      file=file,
                      quote=FALSE,
                      na="",
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
            info <- settings_str(satellite = names(default_sensors)[default_sensors==isolate(state$satellite)],
                                 region = names(regions[[isolate(state$satellite)]])[regions[[isolate(state$satellite)]]==isolate(state$region)],
                                 algorithm = names(algorithms[[isolate(state$satellite)]])[algorithms[[isolate(state$satellite)]]==isolate(state$algorithm)],
                                 year_list = isolate(state$year),
                                 date_var = gsub(" (\\d{4}) ", " ", isolate(state$day_label)), # remove the year
                                 interval = names(default_intervals)[default_intervals==isolate(state$interval)],
                                 log_chla = isolate(state$log_chla),
                                 polygon_name_list = isolate(state$poly_name),
                                 polygon_coord_list = list(box=list(lon=isolate(state$polylon),
                                                                    lat=isolate(state$polylat))),
                                 percent = isolate(state$percent),
                                 outlier = isolate(state$outlier),
                                 dailystat = isolate(state$dailystat),
                                 maxpixval = isolate(state$maxpixval),
                                 fitmethod = names(default_fitmethods)[default_fitmethods==isolate(state$fitmethod)],
                                 bloomShape = names(default_bloomShapes)[default_bloomShapes==isolate(state$bloomShape)],
                                 smoothMethod = names(default_smoothMethods)[default_smoothMethods==isolate(state$smoothMethod)],
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
