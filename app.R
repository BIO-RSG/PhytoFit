# clear memory to free up space
gc()

# LIBRARIES/FUNCTIONS ####

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

source("rateOfChange.R")    # rate of change (ROC) function for bloom fit
source("threshold.R")       # threshold function for bloom fit
source("gaussFit.R")        # gaussian function for bloom fit
source("00_regionBoxes.R")  # contains coordinates of boxes/polygons
source("full_run.R")        # contains function to run full time series with current settings

# Pad a number with leading zeroes to make it length "len".
pad_num <- function(num, len) {
    num_len <- nchar(as.character(floor(num)))
    if (num_len > len) {len <- num_len}
    paste0(paste(replicate(len - num_len, '0'), collapse=''), num)
}

# Check for existence of a folder, and create it if necessary.
get_dir <- function(path) {
    if (!dir.exists(path)) {dir.create(path, showWarnings=F)}
    path
}


#*******************************************************************************
# VARIABLES ####

# years with available data for each sensor
years <- list("modis"=2003:2020,
              "viirs"=2012:2020)
for (i in 1:length(years)) {names(years[[i]]) <- years[[i]]}
default_years <- years[["modis"]]

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
            
            helpText("Select satellite, region, chlorophyll algorithm, and year, choose logged or unlogged chlorophyll, then click \"Load data\".",
                      width = widget_width,
                      style = help_text_style),
            selectInput(inputId = "satellite",
                        label = NULL,
                        choices = c("MODIS 4km" = "modis",
                                    "VIIRS 4km" = "viirs"),
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
            switchInput(inputId = "log_chla",
                        label = HTML("log<sub>chla</sub>"),
                        value = TRUE,
                        onStatus = "success"),
            actionButton(inputId = "load",
                         label = "Load data",
                         style = button_style),
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
                        animate = animationOptions(interval=3000),
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
                        choices = list('Shifted Gaussian' = 'gauss',
                                       'Rate Of Change' = 'roc',
                                       'Threshold' = 'thresh'),
                        selected = 'gauss',
                        width = widget_width),
            selectInput(inputId = 'bloomShape',
                        label = NULL,
                        choices = list('Symmetric' = 'symmetric',
                                       'Asymmetric' = 'asymmetric'),
                        selected = 'symmetric',
                        width = widget_width),
            selectInput(inputId = 'smoothMethod',
                        label = NULL,
                        choices = list('LOESS smooth' = 'loess',
                                       'No smooth' = 'nofit'),
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
                                 "log<sub>chla</sub> ON/OFF, statistics, and bloom fit. Files will be written",
                                 " to a folder following the naming convention satellite_region_years_fitmethod_timecreated.</br>",
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
    
    # default if data hasn't been loaded yet
    state$data_loaded <- FALSE
    
    # default box - need this so when everything first evaluates, some functions
    # dependent on it know what to do (since the box option doesn't appear until
    # the box UI renders, then evaluates AFTER that)
    state$box <- "custom"
    state$custom_name <- ""
    state$fullrunboxes <- "custom"
    
    # if "tm" is on (i.e. day of max. concentration is a parameter in the bloom fit),
    # then the start of the bloom can't be restricted, so turn off that slider button
    observe({
        toggleState("ti_limits",
                    condition = !input$tm)
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
    load("coords.rda")
    state$coord_list <- list("atlantic"=list("lon"=atlantic_lon, "lat"=atlantic_lat),
                             "pacific"=list("lon"=pacific_lon, "lat"=pacific_lat))
    
    
    #***************************************************************************
    # COLLECT USER INPUT ####
    
    # Hide the settings panel if main options have changed but "load" has
    # not been clicked yet.
    observeEvent({
        input$satellite
        input$region
        input$algorithm
        input$year
        input$log_chla
    }, {
        hideElement(id = "hiddenPanel", anim = FALSE)
        disable("savemap")
        disable("savedensplot")
        disable("savebloomfit")
        disable("savebloomparams")
        disable("saveannualstats")
        state$data_loaded <- FALSE
    })
    
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
    
    # Get newly-selected region and set the lat/lon ranges, and create the list
    # of polygon objects for the leaflet map
    observeEvent(input$region, {
        
        state$region <- input$region
        
        if (input$region=="atlantic") {
            state$lon_range <- c(-71, -42)
            state$lat_range <- c(39, 63)
        } else if (input$region=="pacific") {
            state$lon_range <- c(-140, -122)
            state$lat_range <- c(46, 60)
        }
        
        # Make polygons for existing boxes, to add to base leaflet map
        original_polys <- lapply(1:length(all_regions[[input$region]]), function(k) {Polygon(coords=cbind(all_regions[[input$region]][[k]]$lon, all_regions[[input$region]][[k]]$lat), hole=TRUE)})
        original_polyIDs <- lapply(1:length(original_polys), function(k) {Polygons(list(original_polys[[k]]), toupper(names(all_regions[[input$region]])[k]))})
        state$original_polylist <- SpatialPolygons(original_polyIDs, 1:length(all_regions[[input$region]]))
        
    })
    
    
    #************************
    # BOX/POLYGON
    
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
    
    
    #************************
    # DAY OF YEAR
    
    observeEvent(input$yearday_slide, {
        
        # Get the day entered on the slider
        state$yearday <- input$yearday_slide
        
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
            
            # Assign it to state reactiveValues
            state$yearday <- yearday_num
            
            # Update the input widgets with the final value
            updateNumericInput(session, inputId = "yearday_num", value = yearday_num)
            updateSliderInput(session, inputId = 'yearday_slide', value = yearday_num)
            
        }
        
    })
    
    
    #************************
    # DATA PROCESSING OPTIONS
    
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
    
    # BLOOM FIT METHODS
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
    
    
    # CHECKBOXES FOR FULL RUN
    
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
        state$log_chla <- input$log_chla
        
        enable("savesettings")
        
        # Load full map data
        
        # Previous methods:
        # load(paste0('./data/', state$region, '/', state$satellite, state$year, 'ss.rda'))
        # sschla <- readRDS(paste0('./data/', state$region, '/', state$satellite, state$year, 'ss.rds'))
        
        sslat <- state$coord_list[[state$region]]$lat
        sslon <- state$coord_list[[state$region]]$lon
        
        sschla <- read_fst(paste0("./data/", state$region, "/", state$region, "_", state$satellite, "_", state$algorithm, "_", state$year, ".fst"))
        
        if (state$log_chla) {
            sschla <- log(sschla)
        }
        
        state$available_days <- 1:(nrow(sschla)/length(sslat))
        
        remove_modal_spinner()
        
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
                        label = names(state$original_polylist),
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
        yday <- state$yearday
        
        
        # # USING DATABASE INSTEAD (incomplete*************)
        # #************************
        # db_sat_day <- sat_data %>% filter(Doy_stamp==day)
        # sslat_day <- db_sat_day %>% dplyr::select(Latitude)
        # sslon_day <- db_sat_day %>% dplyr::select(Longitude)
        # sschla_day <- db_sat_day %>% dplyr::select(Pix_Val)
        
        
        # check if data is available in the .rda file for the selected day
        if (yday > max(isolate(state$available_days))) {
            
            # Update map based on choices of year day
            lfp <- leafletProxy("fullmap", session) %>%
                clearPopups() %>%
                clearControls() %>%
                clearImages() %>%
                #clearGroup("Points") %>%
                # Label map with current year and day of year
                addControl(tags$div(tag.map.title,
                                    HTML(paste0(format(as.Date((yday-1), origin = paste0(isolate(state$year), "-01-01")), "%d %b %Y"), " (day ", yday, ")<br>NO DATA AVAILABLE YET"))),
                           position = "topleft",
                           className = "map-title")
            
            disable("savemap")
            disable("savedensplot")
            
        } else {
            
            # Subset chla, lat, lon by day, and remove NA cells
            # chla_ind <- !is.na(sschla[yday,])
            pix_num <- length(sslat)
            day_ind <- ((yday-1)*pix_num + 1):(yday*pix_num)
            pts <- sschla %>%
              dplyr::slice(., day_ind) %>%
              dplyr::mutate(., lat=sslat,
                               lon=sslon) %>%
              tidyr::drop_na(., chl)
            
            if (nrow(pts)==0) {
                
                # Update map based on choices of year day
                lfp <- leafletProxy("fullmap", session) %>%
                    clearPopups() %>%
                    clearControls() %>%
                    clearImages() %>%
                    #clearGroup("Points") %>%
                    # Label map with current year and day of year
                    addControl(tags$div(tag.map.title,
                                        HTML(paste0(format(as.Date((yday-1), origin = paste0(isolate(state$year), "-01-01")), "%d %b %Y"), " (day ", yday, ")<br>NO DATA"))),
                               position = "topleft",
                               className = "map-title")
                
                disable("savemap")
                disable("savedensplot")
                
            } else {
                
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
                    addRasterImage(x = tr_coloradj,
                                   colors = cm) %>%
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
                    addControl(tags$div(tag.map.title,
                                        HTML(paste0(format(as.Date((yday-1), origin = paste0(state$year, "-01-01")), "%d %b %Y"), " (day ", yday, ")"))),
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
    
    
    # THE COMMENTED BLOCK OF CODE BELOW CURRENTLY WORKS, BUT HAS SOME FRUSTRATING CONSEQUENCES
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
            } else {
                # make sure rchla is in matrix format if mask length = 1
                pix_num <- length(sslat)
                avd <- max(isolate(state$available_days))
                mask <- pix_num * rep(0:(avd-1),each=length(mask)) + mask
                sschla <- sschla %>% dplyr::slice(., mask)
                rchla <- matrix(sschla$chl, nrow=avd, byrow=TRUE)
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
            
            # Outlier method
            chl_mean <- apply(rchla, 1, mean, na.rm = TRUE)
            chl_sd <- apply(rchla, 1, sd, na.rm = TRUE)  
            chl_median <- apply(rchla, 1, median, na.rm = TRUE)
            iqr <- apply(rchla, 1, IQR, na.rm = TRUE) 
            limits <- matrix(nrow = length(sd), ncol = 2)
            if(state$outlier == 'sd2'){
                limits <- matrix(nrow = length(chl_sd), ncol = 2)
                limits[,1] <- -1 * 2 * chl_sd + chl_mean
                limits[,2] <- 1 * 2 * chl_sd + chl_mean
            } else if (state$outlier == 'sd3'){
                limits <- matrix(nrow = length(chl_sd), ncol = 2)
                limits[,1] <- -1 * 3 * chl_sd + chl_mean
                limits[,2] <- 1 * 3 * chl_sd + chl_mean
            } else if (state$outlier == 'iqr15'){
                limits <- matrix(nrow = length(iqr), ncol = 2)
                limits[,1] <- -1 * 1.5 * iqr + chl_median
                limits[,2] <- 1 * 1.5 * iqr + chl_median
            }
            
            # Extra stats
            # Note: can't use "min" and "max" functions alone because unlike the
            # mean and median functions, which return NA if all their input is
            # NA, min and max return Inf
            chl_min <- sapply(1:nrow(rchla), function(i) {ifelse(all(is.na(rchla[i,])), NaN, min(rchla[i,], na.rm = TRUE))})
            chl_max <- sapply(1:nrow(rchla), function(i) {ifelse(all(is.na(rchla[i,])), NaN, max(rchla[i,], na.rm = TRUE))})
            nobs <- as.numeric(sapply(1:nrow(rchla), function(i) {sum(!is.na(rchla[i,]))}))
            percent_coverage <- (nobs/ncol(rchla))*100
            
            # OUTLIERS
            # remove outliers based on selected method and obtain indices where
            # data coverage is greater than defined percentage
            lenok <- vector(mode = 'logical', length = nrow(rchla))
            for (i in 1:nrow(rchla)) {
                d <- rchla[i,]
                ok <- which(!is.na(d))
                if(state$outlier != 'none'){
                    ok <- which(d >= limits[i,1] & d <= limits[i,2])
                    # update stats for this day after removing outliers
                    if (length(ok)==0) {
                        chl_mean[i] <- chl_median[i] <- chl_sd[i] <- chl_min[i] <- chl_max[i] <- NA
                        nobs[i] <- percent_coverage[i] <- 0
                    } else {
                        chl_mean[i] <- mean(d[ok])
                        chl_median[i] <- median(d[ok])
                        chl_sd[i] <- sd(d[ok])
                        chl_min[i] <- min(d[ok])
                        chl_max[i] <- max(d[ok])
                        nobs[i] <- length(ok)
                        percent_coverage[i] <- (nobs[i]/length(d))*100
                    }
                    
                }
                lenok[i] <- length(ok) 
            }
            
            # update state with statistics for this region
            state$limits <- limits
            state$chl_lenok <- lenok
            state$chl_mean <- chl_mean
            state$chl_median <- chl_median
            state$chl_stdev <- chl_sd
            state$chl_min <- chl_min
            state$chl_max <- chl_max
            state$chl_nobs <- nobs
            state$chl_percent_coverage <- percent_coverage
            
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
        yday <- state$yearday
        
        plot_title <- paste0('Density plot of chlorophyll concentration for ', state$year, ', day ', yday)
        
        # create base plot
        p <- ggplot() + theme_bw()
        
        available_days <- state$available_days
        
        # Reset error message to NULL, then check if it should be changed and
        # printed instead of doing the density plot
        em <- NULL
        
        if (state$data_loaded) {
            
            if (yday > max(available_days)) {
                
                em <- paste0("No data available yet for day ", yday)
                
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
                lenok <- state$chl_lenok
                ok <- lenok[yday] / ncol(rchla) > state$percent
                if (!ok) {
                    em <- paste0("Insufficient data, coverage < ", (state$percent*100), "%")
                } else if (lenok[yday]==1) {
                    em <- "Only one point selected"
                }
                
            }
            
        } else {
            em <- "Load data to begin"
        }
        
        
        # MAKE THE PLOT, unless there's an error message
        if (is.null(em)) {
            
            chl_mean <- state$chl_mean
            chl_median <- state$chl_median
            
            # Create density plot and add vertical lines for mean, median
            p <- p +
                ggtitle(plot_title) +
                geom_density(data=data.frame(x=rchla[yday,]), aes(x=x), fill="grey", alpha = 0.7) + 
                geom_vline(aes(xintercept=chl_mean[yday], col="chl_mean"), size=1.2) +
                geom_vline(aes(xintercept=chl_median[yday], col="chl_median"), size=1)
            
            # Color codes for mean/median lines, to use in legend
            leg_col_labels <- c("mean", "median")
            leg_col_scale <- c(chl_mean = "dodgerblue2", chl_median = "red2")
            
            # Plot outlier boundaries
            if (state$outlier != "none") {
                limits <- state$limits[yday,]
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
                             c(round(state$chl_mean[yday],2),
                               round(state$chl_median[yday],2),
                               round(state$chl_stdev[yday], 2),
                               round(state$chl_min[yday], 2),
                               round(state$chl_max[yday], 2),
                               round(state$chl_nobs[yday]),
                               round(state$chl_percent_coverage[yday], 2)))
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
                             ' chlorophyll concentration for ', state$year)
        
        state$dfbloomparms <- NULL
        
        # create base plot
        p <- ggplot() + theme_bw()
        
        
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
                
                lenok <- state$chl_lenok
                chl_mean <- state$chl_mean
                chl_median <- state$chl_median
                
                first_day <- state$t_range[1]
                last_day <- state$t_range[2]
                
                ydays <- state$available_days
                daily_percov <- lenok / ncol(rchla)
                ind_percov <- daily_percov > state$percent
                ind_dayrange <- ydays > first_day & ydays < last_day
                ind_dayrange_percov <- ind_percov & ind_dayrange
                ydays_percov <- ydays[ind_percov] # all days with high enough percent coverage
                ydays_dayrange <- ydays[ind_dayrange]
                ydays_dayrange_percov <- ydays[ind_dayrange_percov] # subset of days used for fit
                
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
        
        
        # DO FIT AND MAKE PLOT, unless there's an error message
        if (is.null(em)) {
        
            # Get vector of chlorophyll based on daily statistic and valid indices
            if(state$dailystat == 'avg'){
                chlorophyll <- chl_mean[ind_dayrange_percov]
                chlall <- chl_mean[ind_percov]
            } else if(state$dailystat == 'med'){
                chlorophyll <- chl_median[ind_dayrange_percov]
                chlall <- chl_median[ind_percov]
            }
            
            # Create final day/chlorophyll vectors for the fit, smoothed or not
            t <- ydays_dayrange_percov
            if (state$smoothMethod == 'loess'){
                mod <- try(loess(chlorophyll ~ ydays_dayrange_percov, span = state$loessSpan, degree = 2), silent=TRUE)
                bad_loess <- class(mod)=="try-error" | is.null(mod)
                if (bad_loess) {y <- chlorophyll
                } else {y <- fitted(mod)}
            } else if (state$smoothMethod == 'nofit'){
                y <- chlorophyll
            }
            
            if(state$fitmethod == 'gauss'){
                
                if (state$use_weights) {
                    weights <- daily_percov * 100
                    weights <- weights[ind_dayrange_percov]
                } else {
                    weights <- rep(1,length(chlorophyll))
                }
                
                if (state$tm) {
                    tmp_ti_lim <- c(1,365)
                } else {
                    tmp_ti_lim <- state$ti_limits
                }
                
                gauss_res <- gaussFit(t = t, y = y, w = weights,
                                       bloomShape = state$bloomShape,
                                       tm = state$tm,
                                       beta = state$beta,
                                       tm_limits = state$tm_limits,
                                       ti_limits = tmp_ti_lim,
                                       logchla = state$log_chla)
                
                # collect parameters from fit
                bf_results <- gauss_res$values
                
                # get parameter names for the output dataframe
                if (state$bloomShape=="symmetric") {
                    
                    pnames <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                "Magnitude", "Amplitude", "B0", "h", "sigma")
                    if (state$beta) {
                        pnames <- c(pnames, "\u03B2")
                    }
                    
                } else if (state$bloomShape=="asymmetric") {
                    
                    pnames <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                "Magnitude[left]", "Amplitude[left]",
                                "B0[left]", "h[left]", "sigma[left]",
                                "Magnitude[right]", "Amplitude[right]",
                                "B0[right]", "h[right]", "sigma[right]")
                    if (state$beta) {
                        pnames <- c(pnames, "\u03B2[left]", "\u03B2[right]")
                    }
                    
                }
                
            } else if (state$fitmethod=="roc" | state$fitmethod=="thresh") {
                
                pnames <- c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                            "t[duration]", "Magnitude", "Amplitude")
                
                if (state$fitmethod == 'roc') {
                    bf_results <- rateOfChange(y = y, t = t,
                                               yall = chlall, tall = ydays_percov,
                                               bloomShape = state$bloomShape,
                                               tm_limits = state$tm_limits,
                                               ti_limits = state$ti_limits)
                } else if (state$fitmethod == "thresh") {
                    bf_results <- threshold(t = t, y = y, 
                                            tall = ydays_percov, yall = chlall,
                                            threshcoef = state$threshcoef, 
                                            bloomShape = state$bloomShape,
                                            tm_limits = state$tm_limits,
                                            ti_limits = state$ti_limits,
                                            logchla = state$log_chla)
                    pnames <- c(pnames, "Threshold")
                }
                
                # collect parameters from "rate of change" or "threshold" fit
                bf_results <- bf_results$values
                
            }
            
            # Format parameters in a table to print on bloom fit plot
            # and (optionally) to output csv file
            tmp_v <- as.numeric(bf_results)
            tmp_v[1:2] <- round(tmp_v[1:2], 3)
            tmp_v[3:6] <- round(tmp_v[3:6])
            tmp_v[7:length(tmp_v)] <- round(tmp_v[7:length(tmp_v)], 3)
            
            fitparams <- data.frame(parameter=pnames,
                                    value=tmp_v,
                                    stringsAsFactors = FALSE)
            
            # convert parameter table to tableGrob to overlay on ggplot
            values_df <- tableGrob(d = fitparams, rows = NULL, cols = NULL,
                                   # Define theme to parse plotmath expressions
                                   theme = ttheme_minimal(core=list(fg_params=list(parse=TRUE, hjust=0, x=0.02),
                                                                    bg_params = list(fill="white", alpha=0.6)),
                                                          base_size=10,
                                                          padding=unit(c(1,1), "mm")))
            
            state$fitparams <- fitparams
            
            
            #*******************************************************************
            # PLOT FIT
            
            alldays <- (1:nrow(rchla))
            
            # get actual data (all days with good percent coverage)
            allchl <- rep(NA,nrow(rchla))
            allchl[ind_percov] <- chlall
            allpercov <- rep(NA,nrow(rchla))
            allpercov[ind_percov] <- (lenok * 100 / ncol(rchla))[ind_percov]
            
            # dataframe with x, y, fitted y, and percent coverage for plotting
            # (points are sized by percent coverage)
            data_df <- data.frame(x=alldays,
                                  y=allchl,
                                  percov=allpercov,
                                  stringsAsFactors = FALSE)
            
            # initialize and format base plot
            p <- p +
                geom_point(data=data_df, aes(x=x, y=y, size=percov), alpha=0.6) +
                ggtitle(plot_title) +
                labs(x='Day number',
                     y=ifelse(state$dailystat == 'avg',
                              expression('Daily average chlorophyll ' * '[' * mg/m^3 * ']'),
                              expression('Daily median chlorophyll' * '[' * mg/m^3 * ']'))) +
                scale_x_continuous(limits=c(0,365), breaks=seq(0,365,by=50)) +
                scale_size_continuous(name = "Percent coverage",
                                      breaks = c(25, 50, 75, 100),
                                      limits = c(10, 100),
                                      labels = c(25, 50, 75, 100),
                                      range = c(1, 6)) +
                theme(legend.position="bottom",#c(0.93, 0.79),
                      legend.title=element_text(size=12),
                      axis.title.y=element_text(size=10),
                      axis.title.x=element_text(size=10),
                      axis.text.x=element_text(size=12),
                      axis.text.y=element_text(size=12),
                      panel.border = element_rect(colour="black", fill=NA, size=0.4))
            
            # color of fit line, based on choice of mean/median daily statistic,
            # matched with the mean/median vertical bar coloring in the density plot
            fit_col <- ifelse(state$dailystat=="avg","dodgerblue2","red2")
            
            # color and transparency of vertical bars marking indices of fit
            ind_col <- "black"
            ind_alpha <- 0.6
            
            # parameter table location variables
            miny <- min(allchl, na.rm=TRUE)
            maxy <- max(allchl, na.rm=TRUE)
            table_ydiff <- maxy - miny
            
            # add line and statistics based on user-selected fit method
            if (state$fitmethod == 'gauss') {
                
                if (is.null(gauss_res$fit)) {
                    
                    p <- p + annotation_custom(grobTree(textGrob("unable to fit",
                                                                 x=0.018, y=0.93,hjust=0,
                                                                 gp=gpar(fontsize=16, col="red", fontface="bold"))))
                    
                } else {
                    
                    # fitted data (everything in the day range)
                    if (state$bloomShape=="symmetric") {
                        
                        tmp_beta <- ifelse(state$beta, as.numeric(bf_results$beta_value), 0)
                        yfit <- as.numeric(bf_results$B0) + tmp_beta * ydays_dayrange + as.numeric(bf_results$h) / (sqrt(2*pi) * as.numeric(bf_results$sigma)) * exp(- (ydays_dayrange - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigma)^2))
                        
                        # adjust vertical location of parameter table
                        table_yminloc <- maxy - (3/5) * table_ydiff
                        table_ymaxloc <- maxy
                        
                    } else if (state$bloomShape=="asymmetric") {
                        
                        tmp_betaL <- ifelse(state$beta, as.numeric(bf_results$beta_valueL), 0)
                        tmp_betaR <- ifelse(state$beta, as.numeric(bf_results$beta_valueR), 0)
                        Lidx <- ydays_dayrange <= as.numeric(bf_results$tm)
                        Ridx <- ydays_dayrange > as.numeric(bf_results$tm)
                        yfitL <- as.numeric(bf_results$B0L) + tmp_betaL * ydays_dayrange[Lidx] + as.numeric(bf_results$hL) / (sqrt(2*pi) * as.numeric(bf_results$sigmaL)) * exp(- (ydays_dayrange[Lidx] - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigmaL)^2))
                        yfitR <- as.numeric(bf_results$B0R) + tmp_betaR * ydays_dayrange[Ridx] + as.numeric(bf_results$hR) / (sqrt(2*pi) * as.numeric(bf_results$sigmaR)) * exp(- (ydays_dayrange[Ridx] - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigmaR)^2))
                        yfit <- c(yfitL, yfitR)
                        
                        table_yminloc <- -Inf
                        table_ymaxloc <- Inf
                        
                    }
                    
                    # add data to base plot
                    p <- p +
                        geom_line(data=data.frame(x=ydays_dayrange, yfit=yfit, stringsAsFactors = FALSE),
                                  aes(x=x, y=yfit), color=fit_col) +
                        geom_vline(xintercept=as.numeric(bf_results$ti), col=ind_col, alpha=ind_alpha) +
                        geom_vline(xintercept=as.numeric(bf_results$tm), col=ind_col, alpha=ind_alpha) +
                        geom_vline(xintercept=as.numeric(bf_results$tt), col=ind_col, alpha=ind_alpha) +
                        annotation_custom(values_df, xmin=330, xmax=360,
                                          ymin=table_yminloc, ymax=table_ymaxloc)
                    
                }
                
            } else if (state$fitmethod=="roc" | state$fitmethod=="thresh") {
                
                table_ymaxloc <- maxy
                table_yminloc <- maxy - (2/5) * table_ydiff
                
                # If you use LOESS smoothing, add the line to the data
                # (otherwise just add vertical lines for indices below)
                if (state$smoothMethod == 'loess') {
                    if (!bad_loess) {
                        p <- p +
                            geom_line(data=data.frame(x=t, yfit=y, stringsAsFactors = FALSE),
                                      aes(x=x, y=yfit), color=fit_col)
                    }
                }
                
                if (state$fitmethod=="thresh") {
                    p <- p + geom_hline(yintercept=as.numeric(bf_results$thresh), col="red", alpha=0.4)
                }
                
                p <- p +
                    geom_vline(xintercept=as.numeric(bf_results$ti), col=ind_col, alpha=ind_alpha) +
                    geom_vline(xintercept=as.numeric(bf_results$tm), col=ind_col, alpha=ind_alpha) +
                    geom_vline(xintercept=as.numeric(bf_results$tt), col=ind_col, alpha=ind_alpha) +
                    annotation_custom(values_df, xmin=330, xmax=360,
                                      ymin=table_yminloc, ymax=table_ymaxloc)
                
            }
            
            # dataframe for nearPoints
            state$dfbloomparms <- data.frame(y = chlall, x = ydays_percov)
            
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
            
            if (fitmethod=="gauss") {
                
                if (bloomShape=="symmetric") {
                    
                    pnames <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                "Magnitude", "Amplitude", "B0", "h", "sigma")
                    if (beta) {pnames <- c(pnames, "beta")}
                    
                } else if (bloomShape=="asymmetric") {
                    
                    pnames <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                "Magnitude[left]", "Amplitude[left]",
                                "B0[left]", "h[left]", "sigma[left]",
                                "Magnitude[right]", "Amplitude[right]",
                                "B0[right]", "h[right]", "sigma[right]")
                    if (beta) {pnames <- c(pnames, "beta[left]", "beta[right]")}
                    
                }
                
            } else if (fitmethod=="roc" | fitmethod=="thresh") {
                
                pnames <- c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                            "t[duration]", "Magnitude", "Amplitude")
                if (fitmethod == "thresh") {pnames <- c(pnames, "Threshold")}
                
            }
            
            # grey out the screen while processing, and show progress bar
            show_modal_progress_line()
            
            year_bounds <- input$fullrunyears
            year_list <- (year_bounds[1]):(year_bounds[2])
            
            # Create output subfolders
            tmp_odir <- get_dir("output/") # make sure "output" subfolder exists
            output_dir <- get_dir(paste0("output/",
                                         satellite, "4km_",
                                         isolate(state$region), "_",
                                         paste(year_bounds, collapse=" - "), "_",
                                         #gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                                         "_", ifelse(isolate(state$fitmethod)=="gauss", "Gaussian",
                                                     ifelse(isolate(state$fitmethod)=="roc", "RateOfChange", "Threshold")),
                                         "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S")))
            
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
                    boxes = boxes,
                    latlon_method = isolate(state$latlon_method),
                    pnames = pnames,
                    dailystat = isolate(state$dailystat),
                    maxpixval = isolate(state$maxpixval),
                    outlier = isolate(state$outlier),
                    percent = isolate(state$percent),
                    log_chla = log_chla,
                    poly_names = poly_names,
                    ydays = isolate(state$available_days),
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
            
            
            # Save settings
            
            info <- c("Satellite:", paste(satellite, "4km"), "",
                      "Region:", isolate(state$region), "",
                      "Years:", paste(year_bounds, collapse=" - "), "",
                      "Chlorophyll-a logged:", isolate(state$log_chla), "",
                      "Polygon:", paste0(poly_names, collapse=", "), "",
                      #"Latitudes:", paste0(isolate(state$polylat), collapse=", "), "",
                      #"Longitudes:", paste0(isolate(state$polylon), collapse=", "), "",
                      "Minimum daily percent coverage:", (isolate(state$percent) * 100), "",
                      "Outlier detection method:", ifelse(isolate(state$outlier)=="none",
                                                          "None",
                                                          ifelse(isolate(state$outlier)=="sd2",
                                                                 "+/- 2 sd",
                                                                 ifelse(isolate(state$outlier)=="sd3",
                                                                        "+/- 3 sd",
                                                                        "1.5 IQR"))), "",
                      "Daily statistic:", ifelse(isolate(state$dailystat)=="avg",
                                                 "Average",
                                                 "Median"), "",
                      "Maximum pixel value used in statistics and fit:", isolate(state$maxpixval), "",
                      "Fit method:", ifelse(isolate(state$fitmethod)=="gauss",
                                            "Shifted Gaussian curve",
                                            ifelse(isolate(state$fitmethod)=="roc",
                                                   "Rate of change",
                                                   "Threshold")), "",
                      "Bloom fit shape:", isolate(state$bloomShape), "",
                      "Smoothing method:", isolate(state$smoothMethod), "")
            
            if (isolate(state$smoothMethod)=="loess") {
                info <- c(info, "LOESS span:", isolate(state$loessSpan), "")
            }
            
            info <- c(info,
                      "Allowed range of days for bloom fitting:", paste(isolate(state$t_range), collapse="-"), "",
                      "Allowed range of days for bloom initiation:", paste(isolate(state$ti_limits), collapse="-"), "",
                      "Allowed range of days for maximum concentration of bloom:", paste(isolate(state$tm_limits), collapse="-"), "")
            
            if (isolate(state$fitmethod) == "gauss") {
                
                info <- c(info,
                          "Use t[max] parameter:", isolate(state$tm), "",
                          "Use beta parameter:", isolate(state$beta), "",
                          "Weight fit points by daily percent coverage:", isolate(state$use_weights), "")
                
            } else if (isolate(state$fitmethod) == "thresh") {
                
                info <- c(info, "Threshold coefficient:", isolate(state$threshcoef), "")
                
            }
            
            fileConn <- file(paste0(output_dir, "/", paste(year_bounds, collapse="-"),
                                    "_settings_full_time_series.txt"))
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
            paste0(isolate(state$satellite), "4km_",
                   isolate(state$year), pad_num(isolate(state$yearday),3), "_",
                   gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                   "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S"),
                   "_map.html")
        },
        content <- function(file) {
            pc <- isolate(state$tr_coloradj)
            cm <- isolate(state$cm)
            lt <- isolate(state$leg_title)
            yd <- isolate(state$yearday)
            y <- isolate(state$year)
            saveWidget(widget = map_reactive() %>%
                           clearControls() %>%
                           clearImages() %>%
                           addRasterImage(x = pc,
                                          colors = cm) %>%
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
                           addControl(tags$div(tag.map.title,
                                               HTML(paste0(format(as.Date((yd-1), origin = paste0(y, "-01-01")), "%d %b %Y"), " (day ", yd, ")"))),
                                      position = "topleft",
                                      className = "map-title"),
                       file = file)
        }
    )
    
    
    # SAVE DENSITY PLOT
    output$savedensplot <- downloadHandler(
        filename <- function() {
            paste0(isolate(state$satellite), "4km_",
                   isolate(state$year), pad_num(isolate(state$yearday),3), "_",
                   gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                   "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S"),
                   "_density_plot.png")
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
            paste0(isolate(state$satellite), "4km_",
                   isolate(state$year), "_",
                   gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                   "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S"),
                   "_bloom_fit.png")
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
            paste0(isolate(state$satellite), "4km_",
                   isolate(state$year), "_",
                   gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                   "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S"),
                   "_annual_stats.csv")
        },
        content <- function(file) {
            write.csv(data.frame(mean_chl=isolate(state$chl_mean),
                                 median_chl=isolate(state$chl_median),
                                 stdev_chl=isolate(state$chl_stdev),
                                 min_chl=isolate(state$chl_min),
                                 max_chl=isolate(state$chl_max),
                                 nobs=isolate(state$chl_nobs),
                                 percent_coverage=isolate(state$chl_percent_coverage),
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
            paste0(isolate(state$satellite), "4km_",
                   isolate(state$year), "_",
                   gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                   "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S"),
                   "_bloom_parameters.csv")
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
            paste0(isolate(state$satellite), "4km_",
                   isolate(state$year), "_", pad_num(isolate(state$yearday),3), "_",
                   gsub(pattern=" ", replacement="_", x=isolate(state$poly_name)),
                   "_created_", format(Sys.time(),"%Y-%m-%d-%H%M%S"),
                   "_settings.txt")
        },
        
        content <- function(file) {
            
            info <- c("Satellite:", paste(isolate(state$satellite), "4km"), "",
                      "Region:", isolate(state$region), "",
                      "Date:", format(as.Date((isolate(state$yearday)-1), origin = paste0(isolate(state$year), "-01-01")), "%d %b %Y"), "",
                      "Day of year:", isolate(state$yearday), "",
                      "Chlorophyll-a logged:", isolate(state$log_chla), "",
                      "Polygon:", isolate(state$poly_name), "",
                      "Latitudes:", paste0(isolate(state$polylat), collapse=", "), "",
                      "Longitudes:", paste0(isolate(state$polylon), collapse=", "), "",
                      "Minimum daily percent coverage:", (isolate(state$percent) * 100), "",
                      "Outlier detection method:", ifelse(isolate(state$outlier)=="none",
                                                          "None",
                                                          ifelse(isolate(state$outlier)=="sd2",
                                                                 "+/- 2 sd",
                                                                 ifelse(isolate(state$outlier)=="sd3",
                                                                        "+/- 3 sd",
                                                                        "1.5 IQR"))), "",
                      "Daily statistic:", ifelse(isolate(state$dailystat)=="avg",
                                                 "Average",
                                                 "Median"), "",
                      "Maximum pixel value used in statistics and fit:", isolate(state$maxpixval), "",
                      "Fit method:", ifelse(isolate(state$fitmethod)=="gauss",
                                            "Shifted Gaussian curve",
                                            ifelse(isolate(state$fitmethod)=="roc",
                                                   "Rate of change",
                                                   "Threshold")), "",
                      "Bloom fit shape:", isolate(state$bloomShape), "",
                      "Smoothing method:", isolate(state$smoothMethod), "")
                      
            if (isolate(state$smoothMethod)=="loess") {
                info <- c(info, "LOESS span:", isolate(state$loessSpan), "")
            }
            
            info <- c(info,
                      "Allowed range of days for bloom fitting:", paste(isolate(state$t_range), collapse="-"), "",
                      "Allowed range of days for bloom initiation:", paste(isolate(state$ti_limits), collapse="-"), "",
                      "Allowed range of days for maximum concentration of bloom:", paste(isolate(state$tm_limits), collapse="-"), "")
            
            if (isolate(state$fitmethod) == "gauss") {
                
                info <- c(info,
                          "Use t[max] parameter:", isolate(state$tm), "",
                          "Use beta parameter:", isolate(state$beta), "",
                          "Weight fit points by daily percent coverage:", isolate(state$use_weights), "")
                
            } else if (isolate(state$fitmethod) == "thresh") {
                
                info <- c(info, "Threshold coefficient:", isolate(state$threshcoef), "")
                
            }
            
            fileConn <- file(file)
            writeLines(info, fileConn)
            close(fileConn)
            
        }
    )
    
}

# RUN APPLICATION ####
shinyApp(ui = ui, server = server)
