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
library(leafem)         # to add raster to map, and mouse coordinates at top of map
library(quantreg)       # used in the fitting models
library(minpack.lm)     # to use nlsLM for asymmetric gaussian fit
library(sp)             # to use point.in.polygon to extract points within lat/lon boundaries
library(ggplot2)        # for the density plot and bloom fit plots
library(grid)           # for formatting tableGrobs overlaid on ggplots
library(gridExtra)      # for creating tableGrobs overlaid on ggplots
library(dplyr)          # for formatting data tables
library(geometry)       # to check if user-entered lat/lons make a polygon with area too large (degrees^2)
library(raster)         # to use rasters on the map instead of binned points (faster, but less accurate)
library(oceancolouR)    # for shifted_gaussian() and sparkle_fill()
library(stringr)        # for reading and formatting dataset lists
# library(htmlTable)      # for making tables in popups
# library(geosphere)      # for calculating accurate distances between single point click and data point plotted on the map

source("rateOfChange.R")        # rate of change (ROC) function for bloom fit
source("threshold.R")           # threshold function for bloom fit
source("gaussFit.R")            # gaussian function for bloom fit
source("full_run.R")            # contains function to run full time series with current settings
source("functions.R")           # extra functions
source("00_input_variables.R")  # load pre-defined variables
source("widget_download_funcs.R")


#*******************************************************************************
# UI  ####

ui <- fluidPage(
    
    # For hiding download buttons if data hasn't been loaded yet
    useShinyjs(),
    
    # styling
    tags$head(tags$style(HTML(sidebar_tags_style))),
    inlineCSS(list("#applysettings" = "margin-top: -15px")),
    tags$style(".shiny-file-input-progress {display: none}
               .irs-grid-text {font-size: 14px; color: white; bottom: -10px; top: 12px;}
               .irs-from, .irs-to, .irs-min, .irs-max {visibility: hidden !important;"),
    
    # old polygon removal code
    tags$div(remove_custom_poly),
    
    # color and style for day/year sliders
    chooseSliderSkin("Flat", color="#00cc00"),
    
    # App title
    titlePanel("Satellite Chlorophyll Data Visualization"),
    
    fluidRow(
        
        sidebarPanel(width = 3,
            
            # UI LOAD OPTIONS ####
            
            helpText(HTML(paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">Option 1: </font><br>",
                                 "Load a file with predefined settings (.txt, created in PhytoFit): Browse to select, then click \"Apply settings\" and \"Load data\" below.")),
                     width = widget_width,
                     style = label_text_style_main_options),
            br(),
            fileInput(inputId = "settings_file",
                      label = NULL,
                      multiple = FALSE,
                      accept = ".txt",
                      width = widget_width),
            actionButton(inputId="applysettings",
                         label="Apply settings",
                         style=button_style),
            uiOutput("help_settings_file",
                     width = widget_width,
                     style = "white-space: normal;"),
            hr(),
            helpText(HTML(paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">Option 2: </font><br>",
                                 "Start selecting your settings below, then click \"Load data\" and adjust remaining settings as needed.")),
                     width = widget_width,
                     style = label_text_style_main_options),
            br(),br(),
            helpText("Region",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "region",
                        label = NULL,
                        choices = regions,
                        selected = default_region,
                        width = widget_width),
            helpText("Satellite and variable",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "sat_alg",
                        label = NULL,
                        choices = default_sat_algs,
                        selected = default_sat_algs[1],
                        width = widget_width),
            helpText("View full satellite [chla], or use one of two models to separate satellite [chla] into concentrations of different phytoplankton cell sizes, and choose the cell size to view:",
                     width = widget_width,
                     style = paste(label_text_style_main_options, "margin-bottom: 20px;")),
            radioButtons(inputId = "concentration_type",
                         label = NULL,
                         choices = concentration_types,
                         selected = "full",
                         width = widget_width),
            conditionalPanel(condition = "input.concentration_type == 'model1'",
                             radioGroupButtons(inputId = "cell_size_model1",
                                               label = NULL,
                                               choices = cell_sizes_model1,
                                               selected = "small",
                                               width = widget_width)),
            conditionalPanel(condition = "input.concentration_type == 'model2'",
                             radioGroupButtons(inputId = "cell_size_model2",
                                               label = NULL,
                                               choices = cell_sizes_model2,
                                               selected = "small",
                                               width = widget_width)),
            br(),
            helpText("Year",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "year",
                        label = NULL,
                        choices = rev(default_years),
                        width = widget_width),
            helpText("Data composite length",
                     width = widget_width,
                     style = label_text_style_main_options),
            selectInput(inputId = "interval",
                        label = NULL,
                        choices = intervals,
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
            
            br(),br(),
            
            # UI POLYGON ####
            
            actionButton(inputId="polygonButton",
                         label="Polygon",
                         "data-toggle"='collapse',
                         "data-target"='#polygonDiv',
                         style=button_style),
            
            div(id = 'polygonDiv',
                class="collapse",
                
            br(),
            selectInput(inputId = 'box',
                        label = HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Choose a polygon</font>"),
                        choices = poly_choices[[default_region]],
                        selected = 'custom',
                        width = widget_width),
            # If custom polygon selected, enter a name for the polygon (optional),
            # and choose whether to draw polygons on the map or enter a list of
            # lat/lons manually
            conditionalPanel(condition = "input.box =='custom'",
                             helpText("(Optional) Enter a name (use only alphanumeric characters, underscores, or periods) and click \"Apply\".",
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
                                          choices = latlon_methods,
                                          selected = "drawPoly",
                                          width = widget_width),
                            conditionalPanel(condition = "input.latlon_method =='drawPoly'",
                                             helpText("Draw polygon using the toolbar at the top left corner of the map.",
                                                      width = widget_width,
                                                      style = help_text_style)),
                            conditionalPanel(condition = "input.latlon_method =='typeCoords'",
                                             helpText(HTML(paste0("Enter decimal latitudes and longitudes for vertices of polygon, separated by commas, then click \"Create polygon\". ",
                                                                  "Use lon/lat < 0 for west/south. Lists must be the same length, with >2 values each, in the same order so that each latitude is paired with longitude.</br>")),
                                                      width = widget_width,
                                                      style = help_text_style)),
                            conditionalPanel(condition = "input.latlon_method =='loadShapefile'",
                                             helpText(HTML("Click \"Browse\" to find a shapefile. Select the \"shp\" file and all files with the same name but different extensions (e.g. dbf, prj, sbx...), then \"Open\". If the polygon does not load automatically, click \"Create polygon\" <b>(NOTE: polygons with a large number of vertices may take several seconds to load). Warning: This will only use the first polygon in the shapefile, and disjoint polygons are NOT allowed.</b>"),
                                                      width = widget_width,
                                                      style = help_text_style),
                                             fileInput(inputId = "shapefile",
                                                       label = NULL,
                                                       multiple = TRUE,
                                                       accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qix'),
                                                       width = widget_width)),
                            conditionalPanel(condition = "input.latlon_method =='typeCoords' || input.latlon_method =='loadShapefile'",
                                             helpText(HTML("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">List of latitudes:</font>"),
                                                      width = widget_width,
                                                      style = paste(help_text_style, "margin-bottom: -2px; margin-top: -5px;")),
                                             textInput(inputId = "manual_lats",
                                                       label = NULL,
                                                       value = "",
                                                       width = widget_width,
                                                       placeholder = "42.6, 43, 42, 40.4, 40, 42.6"),
                                             helpText(HTML("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">List of longitudes:</font>"),
                                                      width = widget_width,
                                                      style = paste(help_text_style, "margin-bottom: -2px; margin-top: -5px;")),
                                             textInput(inputId = "manual_lons",
                                                       label = NULL,
                                                       value = "",
                                                       width = widget_width,
                                                       placeholder = "-61, -59, -55, -57, -60.4, -61"),
                                             actionButton(inputId = 'createTypedPoly',
                                                          label = 'Create polygon',
                                                          width = widget_width,
                                                          style = button_style)),
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
                        choices = outliers,
                        selected = 'none',
                        width = widget_width),
            helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Daily/weekly statistic</font></br>",
                                 "Choose to use either daily/weekly mean or median chlorophyll in the time series and bloom fit.")),
                     width = widget_width,
                     style = help_text_style),
            selectInput(inputId = 'dailystat',
                        label = NULL,
                        choices = dailystats,
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
            
            br(),br()
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
                             helpText(HTML("Select the method used to calculate t<sub>start</sub> :<br>Either 20% of the curve amplitude (peak minus background), or a constant threshold between 0.05 and 1 (difference between the fitted curve and background chla, calculated in linear space)."),
                                      width = widget_width,
                                      style = help_text_style),
                             radioButtons(inputId = "ti_threshold_type",
                                          label = NULL,
                                          choices = ti_threshold_types,
                                          selected = "percent_thresh",
                                          width = widget_width),
                             div(style="display: inline-block; margin-left: 20px; margin-top: -12px; vertical-align:top; width: 80px;",
                                 numericInput(inputId = "ti_threshold_constant",
                                              label = NULL,
                                              value = 0.1,
                                              min = 0.01,
                                              max = 1,
                                              step = 0.05,
                                              width = widget_width)),
                             helpText(HTML("Switch to ON to consider t<sub>max</sub> a parameter in the regression.<br><b>WARNING: This will disable t<sub>start</sub> limits.</b>"),
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
                             helpText(HTML("When calculating magnitude (area under the curve between t<sub>start</sub> and t<sub>end</sub>) and amplitude of the bloom, should the background be removed first?"),
                                      width = widget_width,
                                      style = help_text_style),
                             checkboxInput(inputId = "rm_bkrnd",
                                           label = "Remove background",
                                           value = TRUE,
                                           width = widget_width),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flags</font></br>",
                                                  "Fits will be flagged if they meet certain criteria that indicate potential problems with the fit (NOTE: this does not affect the fit itself). Combinations of flags will be written as a single number (for example, 13 for flags 1 and 3). Click below for details. Optionally adjust the parameters of some flags.")),
                                      width = widget_width,
                                      style = help_text_style),
                             actionButton(inputId = "flagdescriptions",
                                          label = "Flag descriptions",
                                          style = button_style),
                             br(),br(),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 1: Amplitude ratio limits</font>")),
                                      width = widget_width,
                                      style = help_text_style),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId = "flag1_lim1",
                                           label = NULL,
                                           value = 0.75)),
                             div(style="display: inline-block; vertical-align:top; width: 10px; margin-top: -10px;",
                                 helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId = "flag1_lim2",
                                           label = NULL,
                                           value = 1.25)),
                             div(style="display: inline-block;vertical-align:top; width: 60px; margin-top: -10px;",
                                 actionButton(inputId="apply_flag1_lim",
                                              label="Apply",
                                              style=button_style)),
                             helpText(HTML(paste0("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Flag 2: Magnitude ratio limits</font>")),
                                      width = widget_width,
                                      style = help_text_style),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId = "flag2_lim1",
                                           label = NULL,
                                           value = 0.85)),
                             div(style="display: inline-block; vertical-align:top; width: 10px; margin-top: -10px;",
                                 helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId = "flag2_lim2",
                                           label = NULL,
                                           value = 1.15)),
                             div(style="display: inline-block;vertical-align:top; width: 60px; margin-top: -10px;",
                                 actionButton(inputId="apply_flag2_lim",
                                              label="Apply",
                                              style=button_style))),
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
            
            br(),br(),hr(),
            
            # UI SAVE OPTIONS ####
            
            helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Time series</font></br>",
                                 "Select a series of years and the polygons you would like to process using the current settings, ",
                                 "then click \"Run time series\" to generate the following:</br>")),
                     width = widget_width,
                     style = help_text_style),
            checkboxInput(inputId = "fullrunoutput_png",
                          label = HTML("<font style=\"white-space: normal; font-size: 10px;\">time series plots (.png),</font>"),
                          value = TRUE,
                          width = widget_width),
            div(style="margin-top: -15px; line-height: 90%;",
            checkboxInput(inputId = "fullrunoutput_statcsv",
                          label = HTML("<font style=\"white-space: normal; font-size: 10px;\">tables of statistics (.csv),</font>"),
                          value = TRUE,
                          width = widget_width)),
            div(style="margin-top: -15px; line-height: 90%;",
            disabled(checkboxInput(inputId = "fullrunoutput_paramcsv",
                          label = HTML("<font style=\"white-space: normal; font-size: 10px;\">a single .csv file containing the fitted parameters for all selected years and polygons, and</font>"),
                          value = TRUE,
                          width = widget_width))),
            div(style="margin-top: -15px; line-height: 90%;",
            disabled(checkboxInput(inputId = "fullrunoutput_settingcsv",
                          label = HTML("<font style=\"white-space: normal; font-size: 10px;\">a .csv file containing the settings used for the time series, for reference.</font>"),
                          value = TRUE,
                          width = widget_width))),
            helpText(HTML(paste0("Files will be zipped to a folder following the naming convention ",
                                 "<i>satellite_ region_ compositeLength_ years_ cellSizes_ chlaAlgorithm_ fitmethod_ timecreated</i>.</br>",
                                 "Make sure at least one polygon is selected.<br>",
                                 "<b>When processing is complete and the new filename appears over the download button, click \"Download results (.zip)\".</b>")),
                     width = widget_width,
                     style = help_text_style),
            sliderInput(inputId = "fullrunyears",
                        label = NULL,
                        min = min(default_years),
                        max = max(default_years),
                        value = range(default_years),
                        ticks = FALSE,
                        step = 1,
                        sep = ""),
            pickerInput(inputId = "fullrunboxes",
                        label = "Select your polygons",
                        choices = multipoly_choices[[default_region]],
                        selected = "input.box",
                        options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                        multiple = TRUE,
                        width = widget_width),
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
            br(),br(),
            disabled(downloadButton(outputId = "savesettings",
                                    label = "Save settings (.csv)",
                                    style = button_style))
            
            ))
            
        ), # close column
        
        # UI DISPLAY ####
        
        mainPanel(width = 9,
              
              # UI YEAR DAY ####
              div(style="margin-bottom: -30px; margin-left: 4px; margin-right: 7px;",
                  plotOutput(outputId = 'datebar',height="40px",width = '100%')),
              sliderInput(inputId = "yearday_slide",
                          label = NULL,
                          min = 1,
                          max = 365,
                          value = 1,
                          animate = animationOptions(interval=4000),
                          ticks = TRUE,
                          width = '100%'),
              
               leafletOutput(outputId = 'fullmap',
                             height = '800px'),
               disabled(downloadButton(outputId = "savemap",
                                       label = "Download map (.html)",
                                       style = button_style)),
               br(),br(),
               htmlOutput(outputId = "poly_title"),
               br(),
               plotOutput(outputId = 'chla_hist',
                          height = '360px'),
               disabled(downloadButton(outputId = "savedensplot",
                                       label = "Download density plot (.png)",
                                       style = button_style)),
               br(),br(),
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
               br(),br()
        ) # closes column for plots
    )
    
)




#*******************************************************************************
# SERVER ####

server <- function(input, output, session) {
    
    # Create a list of reactive values to collect widget input.
    state <- reactiveValues()
    
    # initialize some defaults so the code doesn't break
    state$null_rchla <- TRUE # used to tell the plots that there's no custom polygon data yet
    state$latlon_invalid <- FALSE # used to prevent custom polygons with invalid coordinates
    state$latlon_toolarge <- FALSE # used to prevent custom polygons that are too large
    state$nbad_polys <- 0
    state$data_loaded <- FALSE
    state$sat_alg <- default_sat_algs[1]
    state$satellite <- default_sensor
    state$algorithm <- default_algorithm
    state$year <- as.numeric(format(Sys.Date(),"%Y"))
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
    state$typedpoly <- NULL
    state$fullrun_fname <- NULL
    state$ti_threshold <- 0.2
    state$tt_threshold <- 0.2
    state$box <- "custom"
    state$custom_name <- ""
    state$fullrunboxes <- "custom"
    state$help_load_txt <- ""
    state$help_latlon_txt <- ""
    state$secondary_settings <- NULL
    state$draw_programmatically <- FALSE
    state$applyname_programmatically <- FALSE
    state$num_sfile_no_main_change <- 0 # number of settings files loaded that changed the main inputs
    state$max_area <- reginfo[[default_region]]$max_area
    state$loess_smooth <- NA
    
    # These are used to check which specific inputs have been updated in the code block
    # below that hides the left panel if any of the main inputs have changed.
    # years are dependant on sat_algs, which is dependent on region
    state$current_region <- default_region
    state$current_sat_algs <- default_sat_algs
    state$current_years <- default_years
    
    # START SCREEN POPUP
    observe({
        showModal(modalDialog(
            title = "Satellite Chlorophyll Data Visualization",
            HTML(startup_popup),
            easyClose = TRUE,
            footer = modalButton("OK")
        ))
    })
    
    
    # APPLY SETTINGS FILE ####
    
    # If user uploads an existing settings file, read the settings and apply them.
    # Here is the order of operations:
    #       1. Apply checks on the input file
    #       2. If the data is good:
    #               a. load the main settings (i.e. before the "load data" button)
    #               b. store the secondary settings
    #       3. When the main settings change, a block of code is executed to check if there is data
    #          available for the selected settings. If there is, and there are secondary settings stored
    #          in the "state" variable, the "load data" button is automatically clicked.
    #       4. At the end of the "load data" code chunk:
    #               a. the secondary settings are updated
    #               b. if the settings file contains box=="custom" and valid polylons/polylats:
    #                   - input$box and state$box are updated to "custom"
    #                   - latlon_method is changed to typeCoords (i.e. the method of creating a custom polygon)
    #                   - manual_lats/manual_lons are updated with the custom coordinates
    #                   - another state variable (draw_programmatically) is set to TRUE
    #               c. state$secondary_settings is reset to NULL
    #       5. If 4b is TRUE:
    #               a. the "create polygon" button is clicked automatically
    #               b. state$draw_programmatically is reset to FALSE
    observeEvent(input$applysettings, {
        file <- input$settings_file
        ext <- tools::file_ext(file$datapath)
        # check the extension, try to load the file, and check file contents
        if (ext == "txt") {
            # try to load the file
            file_contents <- try(read.table(file$datapath, header = TRUE, sep="\\"), silent=TRUE)
            if (class(file_contents)=="try-error") {
                help_settings_file_txt <- "Invalid input file."
            } else {
                # check the file contents
                if (all(colnames(file_contents)==c("setting_id", "value", "setting_description",
                                                   "value_description", "setting_id_variable_type",
                                                   "setting_id_widget_type"))) {
                    help_settings_file_txt <- ""
                    main_ids <- c("region", "sat_alg", "concentration_type",
                                  "cell_size_model1", "cell_size_model2", "year", "interval", "log_chla")
                    main_inds <- file_contents$setting_id %in% main_ids
                    primary_settings <- file_contents[main_inds,]
                    state$secondary_settings <- file_contents[!main_inds,]
                    # update main input buttons
                    formatted_settings <- format_settings_to_load(primary_settings)
                    tmp_ids <- formatted_settings$ids
                    tmp_values <- formatted_settings$values
                    tmp_widgets <- formatted_settings$widgets
                    # test if the new main inputs are all the same as the existing ones
                    current_inputs <- reactiveValuesToList(input)
                    proper_order <- match(main_ids[main_ids %in% names(current_inputs)], names(current_inputs))
                    current_inputs <- unlist(current_inputs[proper_order])
                    names(current_inputs) <- NULL
                    new_inputs <- unlist(tmp_values)
                    names(new_inputs) <- NULL
                    # if necessary, update region, satellite/algorithm, concentration type, year, interval, and log_chla
                    if (identical(current_inputs, new_inputs)) {
                        state$num_sfile_no_main_change <- state$num_sfile_no_main_change + 1
                    } else {
                        updateSelectInput(session, inputId = tmp_ids[1], selected = tmp_values[[1]])
                        updateSelectInput(session, inputId = tmp_ids[2], selected = tmp_values[[2]])
                        updateRadioButtons(session, inputId = tmp_ids[3], selected = tmp_values[[3]])
                        updateRadioGroupButtons(session, inputId = tmp_ids[4], selected = tmp_values[[4]])
                        updateRadioGroupButtons(session, inputId = tmp_ids[5], selected = tmp_values[[5]])
                        updateSelectInput(session, inputId = tmp_ids[6], selected = tmp_values[[6]], choices = rev(years[[tmp_values[[1]]]]))
                        updateSelectInput(session, inputId = tmp_ids[7], selected = tmp_values[[7]])
                        updateSwitchInput(session, inputId = tmp_ids[8], value = tmp_values[[8]])
                    }
                } else {
                    help_settings_file_txt <- "Invalid file contents."
                }
            }
        } else {
            help_settings_file_txt <- "Please select a settings file with extension .csv."
        }
        state$help_settings_file_txt <- help_settings_file_txt
    })
    
    output$help_settings_file <- renderUI({
        helpText(state$help_settings_file_txt,
                 width = widget_width,
                 style = error_text_style)
    })
    
    
    # DISABLE/TOGGLE BUTTONS ####
    
    # if "tm" is on (i.e. day of max. concentration is a parameter in the bloom fit),
    # then the start of the bloom can't be restricted, so turn off that slider button
    observe({
        toggleState("ti_limits", condition = !input$tm | input$ti_threshold_type=="constant_thresh")
    })
    
    # for shifted gaussian, if using 20% threshold, grey out the option for a constant threshold value
    observe({
        toggleState("ti_threshold_constant", condition = input$ti_threshold_type=="constant_thresh")
    })
    
    # enable/disable "Run time series" button depending on which boxes are selected and if there are custom coordinates
    observe({
        frb <- state$fullrunboxes
        if (length(frb)==0) {
            disable("fullrun_process")
        } else if (length(frb)==1) {
            if (frb[1] == "custom" & is.null(state$newpoly) & is.null(state$editedpoly) & is.null(state$typedpoly)) {
                disable("fullrun_process")
            } else {
                enable("fullrun_process")
            }
        } else {
            enable("fullrun_process")
        }
    })
    
    # if a file with predefined settings has not been loaded, disable the "apply settings" button
    observe({
        if (is.null(input$settings_file)) {
            disable("applysettings")
        } else {
            enable("applysettings")
        }
    })
    
    # Hide the settings panel if main options have changed but "load" has not been clicked yet.
    observeEvent({
        input$region
        input$sat_alg
        input$concentration_type
        input$cell_size_model1
        input$cell_size_model2
        input$year
        input$interval
        input$log_chla
        state$num_sfile_no_main_change
    }, {
        
        hideElement(id = "hiddenPanel", anim = FALSE)
        disable("savemap")
        disable("savedensplot")
        disable("savebloomfit")
        disable("savebloomparams")
        disable("saveannualstats")
        
        # if the satellite has changed, update the sat_alg and year dropdown menus
        # if they don't need to be updated, then continue checking if data exists

        new_sat_algs <- sat_algs[[input$region]]
        if (!identical(new_sat_algs, state$current_sat_algs)) {
            if (state$sat_alg %in% new_sat_algs) {
                selected_sat_alg <- state$sat_alg
            } else {
                selected_sat_alg <- new_sat_algs[1]
            }
            updateSelectInput(session, inputId = "sat_alg", choices = new_sat_algs, selected = selected_sat_alg)
            state$current_sat_algs <- new_sat_algs
            sat_alg_file <- selected_sat_alg
        } else {
            sat_alg_file <- input$sat_alg
        }
        new_years <- years[[input$region]][[sat_alg_file]]
        if (!identical(new_years, state$current_years)) {
            if (state$year %in% new_years) {
                selected_year <- state$year
            } else {
                selected_year <- max(new_years)
            }
            updateSelectInput(session, inputId = "year", choices = rev(new_years), selected = selected_year)
            state$current_years <- new_years
            year_file <- selected_year
        } else {
            year_file <- input$year
        }
        
        # enable/disable load button depending on whether or not data exists for these settings
        state$data_loaded <- FALSE
        data_exists <- file.exists(paste0("./data/", input$region, "/", input$region, "_", sat_alg_file, "_", year_file, ".fst"))
        
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
    
    
    # GET REGION ####
    
    # Get newly-selected region and set the lat/lon ranges, and create the list
    # of polygon objects for the leaflet map
    observeEvent(input$region, {
        state$region <- input$region
        state$original_polylist <- original_polylist[[state$region]]
        # Update polygon dropdown menu and fullrunboxes choices
        updateSelectInput(session, inputId = "box", choices = poly_choices[[state$region]], selected = "custom")
        updatePickerInput(session, inputId = "fullrunboxes", choices = multipoly_choices[[state$region]], selected = "custom")
    })
    
    
    # GET BOX/POLYGON ####
    
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
        state$help_latlon_txt <- ""
        if (state$latlon_method == "drawPoly") {
            state$draw_toolbar <- TRUE
        } else {
            state$draw_toolbar <- FALSE
        }
    })
    
    observeEvent({
        input$manual_lats
        input$manual_lons
    }, {
        if (state$latlon_method != "drawPoly" & state$draw_programmatically) {
            shinyjs::click("createTypedPoly")
            state$draw_programmatically <- FALSE
        }
    })
    
    observeEvent(input$custom_name, {
        if (state$applyname_programmatically) {
            shinyjs::click("applyname")
            state$applyname_programmatically <- FALSE
        }
    })
    
    observeEvent(input$applyname, {
        state$custom_name <- gsub("[^[:alnum:]_.]", "", input$custom_name)
    })
    
    observe({
        reg <- isolate(state$region)
        state$poly_name <- ifelse(state$box=='custom',
                                  ifelse(nchar(state$custom_name)==0, "Custom polygon", state$custom_name),
                                  paste0(full_names[[reg]][which(state$box==poly_ID[[reg]])]))
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
        time_variables <- get_time_vars(interval=state$interval, year=state$year,
                                        yearday=state$yearday, doys_per_week=doys_per_week)
        state$day_label <- time_variables$day_label
        state$time_ind <- time_variables$time_ind
    })
    
    
    # GET STATISTICS ####
    
    observeEvent(input$outlier, {
        state$outlier <- input$outlier
    })
    observeEvent(input$dailystat,{
        state$dailystat <- input$dailystat
    })
    observeEvent(input$percent, {
        state$percent <- input$percent
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
    observeEvent(input$ti_threshold_type,{
        state$ti_threshold_type <- input$ti_threshold_type
    })
    observeEvent(input$ti_threshold_constant,{
        ittt <- input$ti_threshold_constant
        # run some checks
        if (is.finite(ittt)) {
            if (ittt < 0.01) {
                ittt <- 0.01
                updateNumericInput(session, inputId = "ti_threshold_constant", value = 0.01)
            } else if (ittt > 1) {
                ittt <- 1
                updateNumericInput(session, inputId = "ti_threshold_constant", value = 1)
            }
            state$ti_threshold_constant <- ittt
        }
    })
    observeEvent(input$tm,{
        state$tm <- input$tm
    })
    observeEvent(input$beta,{
        state$beta <- input$beta
    })
    observeEvent(input$use_weights,{
        state$use_weights <- input$use_weights
    })
    observeEvent(input$rm_bkrnd,{
        state$rm_bkrnd <- input$rm_bkrnd
    })
    # popup explaining gaussian fit flags
    observeEvent(input$flagdescriptions, {
        showModal(modalDialog(
            title = "Gaussian fit flag descriptions",
            HTML(gauss_flag_popup),
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
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
    
    # ignoreNULL is necessary to for cases where all boxes are deselected and fullrunboxes
    # is therefore set to NULL - this forces it to trigger anyway so that the "run time series"
    # button can be greyed out in this case
    observeEvent(input$fullrunboxes, {
        state$fullrunboxes <- input$fullrunboxes
    }, ignoreNULL = FALSE)
    
    
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
        state$sat_alg <- input$sat_alg
        state$satellite <- strsplit(state$sat_alg, split="_")[[1]][1]
        state$algorithm <- strsplit(state$sat_alg, split="_")[[1]][2]
        state$concentration_type <- input$concentration_type
        state$cell_size_model1 <- input$cell_size_model1
        state$cell_size_model2 <- input$cell_size_model2
        state$year <- input$year
        state$interval <- input$interval
        state$log_chla <- input$log_chla
        state$leg_title <- paste0("<center>Chlorophyll-a</br>[ ",
                                  ifelse(state$log_chla, "log<sub>10</sub> ", ""),
                                  "mg m<sup>-3</sup> ]</center>")
        
        enable("savesettings")
        
        # Load full map data
        sslat <- reginfo[[state$region]]$lat
        sslon <- reginfo[[state$region]]$lon
        ssbin <- reginfo[[state$region]]$bin
        state$max_area <- reginfo[[state$region]]$max_area
        state$data_resolution <- reginfo[[state$region]]$data_resolution
        
        all_data <- get_data(state$region, state$satellite, state$algorithm, state$year,
                             state$yearday, state$interval, state$log_chla, length(sslat),
                             doys_per_week, doy_week_start, doy_week_end,
                             state$concentration_type, state$cell_size_model1, state$cell_size_model2)
        sschla <- all_data$sschla
        state$available_days <- all_data$available_days
        state$doy_vec <- all_data$doy_vec # days of the year, whether you're using daily or weekly data
        state$day_label <- all_data$day_label
        state$time_ind <- all_data$time_ind
        
        secondary_settings <- state$secondary_settings
        
        if (is.null(secondary_settings)) {
            
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
            
            # Update full_run slider input
            tmp_years <- as.numeric(years[[state$region]][[state$sat_alg]])
            updateSliderInput(session, inputId = 'fullrunyears', min = min(tmp_years), max = max(tmp_years), value = range(tmp_years))
            
        } else {
            
            extra_inds <- secondary_settings$setting_id %in% c("yearday_slide", "fullrunyears", "box", "custom_name", "polylon", "polylat")
            extra_df <- secondary_settings[extra_inds,]
            
            # get box details to update later
            predefined_polygon <- extra_df$value[extra_df$setting_id=="box"]
            custom_polygon_name <- extra_df$value[extra_df$setting_id=="custom_name"]
            predefined_custom_lons <- extra_df$value[extra_df$setting_id=="polylon"]
            predefined_custom_lats <- extra_df$value[extra_df$setting_id=="polylat"]
            
            # update yearday_slider with proper choices and selection
            yearday_value <- as.numeric(trimws(extra_df$value[extra_df$setting_id=="yearday_slide"]))
            if (state$interval=="daily") {
                updateSliderInput(session, inputId = "yearday_slide", value = yearday_value, step = 1)
            } else if (state$interval=="weekly") {
                updateSliderInput(session, inputId = "yearday_slide", value = yearday_value, step = 8)
            }
            
            # update full_run slider input with proper choices and selection
            tmp_years <- as.numeric(years[[state$region]][[state$sat_alg]])
            fullrunyears_value <- trimws(extra_df$value[extra_df$setting_id=="fullrunyears"])
            fullrunyears_value <- as.numeric(strsplit(fullrunyears_value, split=",")[[1]])
            updateSliderInput(session, inputId = 'fullrunyears', min = min(tmp_years), max = max(tmp_years), value = fullrunyears_value)
            
            # update remaining secondary settings
            secondary_settings <- secondary_settings[!extra_inds,]
            formatted_settings <- format_settings_to_load(secondary_settings)
            tmp_ids <- formatted_settings$ids
            tmp_values <- formatted_settings$values
            tmp_widgets <- formatted_settings$widgets
            lapply(1:sum(tmp_widgets==1), function(i) updateSelectInput(session, inputId = tmp_ids[tmp_widgets==1][i], selected = tmp_values[tmp_widgets==1][[i]]))
            lapply(1:sum(tmp_widgets==2), function(i) updateSliderInput(session, inputId = tmp_ids[tmp_widgets==2][i], value = tmp_values[tmp_widgets==2][[i]]))
            lapply(1:sum(tmp_widgets==3), function(i) updateNumericInput(session, inputId = tmp_ids[tmp_widgets==3][i], value = tmp_values[tmp_widgets==3][[i]]))
            lapply(1:sum(tmp_widgets==4), function(i) updateTextInput(session, inputId = tmp_ids[tmp_widgets==4][i], value = tmp_values[tmp_widgets==4][[i]]))
            lapply(1:sum(tmp_widgets==5), function(i) updateRadioButtons(session, inputId = tmp_ids[tmp_widgets==5][i], selected = tmp_values[tmp_widgets==5][[i]]))
            lapply(1:sum(tmp_widgets==6), function(i) updateCheckboxInput(session, inputId = tmp_ids[tmp_widgets==6][i], value = tmp_values[tmp_widgets==6][[i]]))
            lapply(1:sum(tmp_widgets==7), function(i) updateSwitchInput(session, inputId = tmp_ids[tmp_widgets==7][i], value = tmp_values[tmp_widgets==7][[i]]))
            lapply(1:sum(tmp_widgets==8), function(i) updatePickerInput(session, inputId = tmp_ids[tmp_widgets==8][i], selected = tmp_values[tmp_widgets==8][[i]]))
            
            # now update the box input
            updateSelectInput(session, inputId = "box", choices = poly_choices[[isolate(input$region)]], selected=predefined_polygon)
            # if it's a custom box, update lat/lon input and add it to the map and stats using the "typeCoords" method
            if (predefined_polygon=="custom" & !is.na(predefined_custom_lats) & !is.na(predefined_custom_lons)) {
                updateRadioButtons(session, inputId="latlon_method", selected="typeCoords")
                updateTextInput(session, inputId="manual_lats", value=predefined_custom_lats)
                updateTextInput(session, inputId="manual_lons", value=predefined_custom_lons)
                # set a variable to automatically click "createTypedPoly" after the lats/lons are updated
                state$draw_programmatically <- TRUE
                # if the custom polygon has a name, apply it
                if (nchar(custom_polygon_name) > 0) {
                    updateTextInput(session, inputId="custom_name", value=custom_polygon_name)
                    state$applyname_programmatically <- TRUE
                }
            }
            
            # update the box state variable as well
            # this is needed because the input choices/widget will be updated during this round of reactive updates,
            # then the stats will be calculated, but the actual box state won't be updated until next round, so it
            # won't be calculating the right stats (if you change region, this will make it crash)
            state$box <- predefined_polygon
            
            # now reset secondary_settings
            state$secondary_settings <- NULL
            
        }
        
        remove_modal_spinner()
        
        gc()
        
        return(list(sschla=sschla,
                    sslon=sslon,
                    sslat=sslat,
                    ssbin=ssbin))
        
    })
    
    
    #***************************************************************************
    # MAP BASE ####
    
    # Raster data will be overlaid after this
    map_reactive <- reactive({
        
        reg <- isolate(state$region)
        # Use leaflet() here, and only include aspects of the map that won't need
        # to change dynamically unless the entire map is torn down and recreated.
        lf <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles("Esri.WorldGrayCanvas",
                             options = providerTileOptions(
                                 minZoom = 4, maxZoom = 10,
                                 updateWhenZooming=FALSE,  # don't update tiles until zoom is done
                                 updateWhenIdle=TRUE)) %>% # don't load new tiles when panning
            setView(lng = reginfo[[reg]]$center_lon,
                    lat = reginfo[[reg]]$center_lat,
                    zoom = reginfo[[reg]]$zoom_level) %>%
            # Add mouse coordinates to top of map
            # Note: need to "remove" first, otherwise it gets stuck if you try
            # to reload the base map (for example, switching from Atlantic to Pacific)
            removeMouseCoordinates() %>%
            addMouseCoordinates() %>%
            # Add gridlines
            addSimpleGraticule(group = "Gridlines", interval = 5, showOriginLabel = FALSE) %>%
            # Add option to remove the gridlines or existing statistic boxes
            addLayersControl(overlayGroups = c("Gridlines", "Stats boxes"),
                             options = layersControlOptions(collapsed = FALSE))
        # Add predefined polygons to the map
        if (!is.null(state$original_polylist)) {
            lf <- lf %>%
                addPolygons(group = "Stats boxes", data = state$original_polylist,
                            stroke = TRUE, color = "darkgrey", weight = 2, opacity = 1, fill = FALSE,
                            label = abbrev[[reg]],
                            labelOptions = labelOptions(
                                noHide=TRUE, textOnly=TRUE, textsize='13px', direction="center",
                                style=list('color'='white', 'text-shadow' = '0px 0px 4px #000000')))
        }
        
        lf
        
    })
    
    # MAP UPDATE ####
    # based on user input
    observe({
        
        # Get the selected annual dataset
        ssfull <- full_data()
        sschla <- ssfull$sschla
        sslat <- ssfull$sslat
        sslon <- ssfull$sslon
        ssbin <- ssfull$ssbin
        
        # Get the selected day of year
        yearday <- state$yearday
        day_label <- state$day_label
        time_ind <- state$time_ind
        
        map_update_nodata <- function(session, tag.map.title, day_label, message) {
            disable("savemap")
            disable("savedensplot")
            leafletProxy("fullmap", session) %>%
                clearPopups() %>% clearControls() %>% clearGroup("georaster") %>%
                addControl(tags$div(tag.map.title, HTML(paste0(day_label, "<br>", message))),
                           position = "topleft", className = "map-title")
        }
        
        # check if data is available in the file for the selected day
        if (yearday > isolate(state$available_days)) {
            
            lfp <- map_update_nodata(session, tag.map.title, day_label, "NO DATA AVAILABLE YET")
            
        } else {
            
            # Subset chla, lat, lon by day, and remove NA cells
            chla_ind <- !is.na(sschla[,time_ind]) & is.finite(sschla[,time_ind])
            
            if (sum(chla_ind)==0) {
                
                lfp <- map_update_nodata(session, tag.map.title, day_label, "NO DATA")
                
            } else {
                
                # Make raster for leaflet map
                tr <- var_to_rast(df=data.frame(bin=ssbin[chla_ind],
                                                chl=sschla[chla_ind,time_ind],
                                                stringsAsFactors = FALSE),
                                  resolution=isolate(state$data_resolution),
                                  ext=c(reginfo[[state$region]]$lon_range,
                                        reginfo[[state$region]]$lat_range),
                                  max_bins=600000000)
                state$tr <- tr # used for input$fullmap_click, currently disabled
                
                # Get colour scale for leaflet map
                zlim <- c(state$zlim1, state$zlim2)
                
                # Update map based on choices of year day
                lfp <- leafletProxy("fullmap", session) %>%
                    clearPopups() %>% clearControls() %>% clearGroup("georaster") %>%
                    addGeoRaster(x = tr,
                                 group = "georaster",
                                 colorOptions = colorOptions(palette=map_cols,domain=zlim,na.color="#00000000"),
                                 autozoom = FALSE) %>%
                    addLegend(position = 'topright',
                              pal = colorNumeric(palette=map_cols, domain=zlim, na.color="#00000000"),
                              values = zlim, title = state$leg_title, bins = 10, opacity = 1) %>%
                    # Label map with current year and day of year
                    addControl(tags$div(tag.map.title, HTML(day_label)),
                               position = "topleft", className = "map-title")
                
                # now that data has been loaded, make the download button visible
                enable("savemap")
                
            }
            
        }
        
        if (state$draw_toolbar) {
            shape_options <- drawShapeOptions(color="yellow", fill=FALSE, weight=2.5)
            lfp <- lfp %>%
                addDrawToolbar(
                    # remove options to draw lines, circles, or single markers
                    polylineOptions=FALSE, circleOptions=FALSE, markerOptions=FALSE, circleMarkerOptions=FALSE,
                    # only one custom polygon at a time
                    singleFeature=TRUE,
                    # adjust custom polygon colors
                    polygonOptions=drawPolygonOptions(shapeOptions = shape_options),
                    rectangleOptions=drawRectangleOptions(shapeOptions = shape_options),
                    # custom polygons can be edited
                    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
        } else {
            lfp <- lfp %>% removeDrawToolbar(clearFeatures = TRUE)
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
        state$typedpoly <- NULL
    })
    observeEvent(input$fullmap_draw_edited_features, {
        state$newpoly <- NULL
        state$editedpoly <- input$fullmap_draw_edited_features
    })
    observeEvent(input$fullmap_draw_deleted_features, {
        state$newpoly <- NULL
        state$editedpoly <- NULL
        state$custom_name <- ""
        updateTextInput(session, inputId="custom_name", value="")
    })
    
    # If new polygons are created, edited, or deleted, this block of code
    # will trigger to get the coordinates of the polygon and send them to annual
    # stats to update the density plot and bloom fit scatterplot
    draw_polygon <- reactive({
        
        # Check for new or edited coordinates
        coords <- try(unlist(state$newpoly$geometry$coordinates))
        if (is.null(coords)) {
            coords <- try(unlist(state$editedpoly$features[[1]]$geometry$coordinates))
        }
        
        # Check if polygon area is too large
        state$latlon_toolarge <- FALSE
        if (!is.null(coords)) {
          polygon_area <- polyarea(x=coords[seq(1,length(coords), 2)], y=coords[seq(2,length(coords), 2)])
          ma <- isolate(state$max_area)
          if (polygon_area > ma) {
            coords <- NULL
            state$latlon_toolarge <- TRUE
            state$nbad_polys <- state$nbad_polys + 1
            state$help_latlon_txt <- paste0("Polygon is too large (must be <= ", ma, " degrees squared).")
          }
        }
        
        return(coords)
        
    })
    
    # Instead of getting coordinates of a polygon drawn directly on the map,
    # get coordinates entered in a box by the user.
    observeEvent(input$createTypedPoly, {
        good_latlons <- check_latlons(input$manual_lats, input$manual_lons, state$nbad_polys)
        state$typedpoly <- good_latlons$coords
        state$nbad_polys <- good_latlons$nbad_polys
        state$latlon_invalid <- good_latlons$latlon_invalid
        state$latlon_toolarge <- good_latlons$latlon_toolarge
        state$help_latlon_txt <- good_latlons$help_latlon_txt
    })
    
    type_polygon <- reactive({
        return(state$typedpoly)
    })
    
    # Or read a shapefile containing a polygon. This makes use of the "typed polygon"
    # code above, entering the shapefile vertex lat/lons in the text boxes and programmatically
    # clicking the "Create Polygon" button to trigger input$createTypedPoly
    observeEvent(input$shapefile, {
        ext <- tools::file_ext(input$shapefile$name)
        show_modal_spinner(spin = "atom",
                           color = "#112446",
                           text = paste0("Loading ", input$shapefile$name[ext=="shp"], "..."))
        mydir <- tempdir()
        on.exit(unlink(mydir))
        file <- file.path(mydir, input$shapefile$name)
        file.copy(input$shapefile$datapath, file)
        # check the extensions, try to load the file, and check file contents
        if ("shp" %in% ext) {
            # try to load the file
            file <- file[ext=="shp"]
            file_contents <- try(readOGR(dsn=file, verbose=FALSE), silent=TRUE)
            if (class(file_contents)[1]=="try-error") {
                help_latlon_txt <- "Invalid input files. Make sure all files with the same name (and different extensions, e.g. shp, dbf, prj, sbx...) are selected."
            } else {
                # check the file contents
                if (class(file_contents)[1] == "SpatialPolygonsDataFrame") {
                    file_coords <- try(file_contents@polygons[[1]]@Polygons[[1]]@coords, silent=TRUE)
                    if (class(file_coords)[1]=="try-error") {
                        help_latlon_txt <- "Invalid file contents."
                    } else {
                        help_latlon_txt <- ""
                        # format coordinates and update manual_lats and manual_lons
                        shapefile_custom_lons <- as.numeric(file_coords[,1])
                        shapefile_custom_lats <- as.numeric(file_coords[,2])
                        updateTextInput(session, inputId="manual_lats", value=paste0(shapefile_custom_lats, collapse=","))
                        updateTextInput(session, inputId="manual_lons", value=paste0(shapefile_custom_lons, collapse=","))
                        updateTextInput(session, inputId="custom_name", value=input$shapefile$name[ext=="shp"])
                        state$draw_programmatically <- TRUE
                        state$applyname_programmatically <- TRUE
                    }
                } else {
                    help_latlon_txt <- "Invalid file contents. Shapefile must contain a SpatialPolygonsDataFrame."
                }
            }
        } else {
            help_latlon_txt <- "Please select a shapefile (.shp) and all corresponding files with the same name but different extensions (e.g. dbf, prj, sbx...)"
        }
        remove_modal_spinner()
        state$help_latlon_txt <- help_latlon_txt
    })
    
    # If there's something wrong with the custom polygon coordinates, show an error message on the sidebar
    output$help_latlon <- renderUI({
        helpText(state$help_latlon_txt,
                 width = widget_width,
                 style = error_text_style)
    })
    
    # POLYGON TITLE PANEL
    output$poly_title <- renderUI({
        if (state$data_loaded) {
            str1 <- paste0(state$year, " ", names(regions)[state$region==regions], ", ", state$poly_name)
            if (is.null(state$polylat)) {
                str2 <- str3 <- ""
            } else {
                if (length(state$polylat) > 20) {
                    extra_coords <- paste0("... <i>[truncated, ", length(state$polylat)-20, " remaining]</i>")
                    str2 <- paste0(paste0(round(state$polylat,6)[1:20], collapse=", "), extra_coords)
                    str3 <- paste0(paste0(round(state$polylon,6)[1:20], collapse=", "), extra_coords)
                } else {
                    str2 <- paste0(round(state$polylat,6), collapse=", ")
                    str3 <- paste0(round(state$polylon,6), collapse=", ")
                }
            }
            HTML(paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">", str1, "</font><br/>",
                        "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Latitudes:</font> ", str2, "<br/>",
                        "<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">Longitudes:</font> ", str3))
        }
    })
    
    #***************************************************************************
    # DATE BAR AT TOP
    
    output$datebar <- renderPlot({
      pmonth
    })
    
    
    #***************************************************************************
    # ANNUAL STATS FOR BOX/POLYGON ####
    
    # If box or latlon_method have changed, reset/delete old highlighted boxes
    delete_boxes <- eventReactive({
        state$box
        state$latlon_method
        state$nbad_polys
        state$region
    }, {
        
        # Remove popups, previously selected existing AZMP boxes, or previously typed polygons
        leafletProxy("fullmap", session) %>%
            clearPopups() %>% removeShape("highlighted_box") %>% removeShape("typedpoly")
        
        # Remove drawn polygons
        snp <- isolate(state$newpoly)
        sep <- isolate(state$editedpoly)
        if (!is.null(snp)) {
            shapes <- snp$properties$`_leaflet_id`
            session$sendCustomMessage("removeleaflet", list(elid="fullmap", layerid=shapes))
        } else if (!is.null(sep)) {
            shapes <- sep$features[[1]]$properties$`_leaflet_id`
            session$sendCustomMessage("removeleaflet", list(elid="fullmap", layerid=shapes))
        }
        
        # Reset drawn/typed polygon variables to NULL
        state$newpoly <- NULL
        state$editedpoly <- NULL
        state$typedpoly <- NULL
        
    })
    
    annual_stats <- reactive({
        
        # Make sure you have the updated satellite/year data
        ssfull <- full_data()
        
        # Reset/erase boxes if state$box or state$latlon_method have changed
        delete_boxes()
        
        coords <- NULL
        add_poly <- FALSE # add new polygons using leafletProxy?
        
        region <- isolate(state$region)
        
        # Get coordinates from a custom polygon
        if (state$box=="custom" & state$latlon_method=="drawPoly") {
            coords <- draw_polygon()
        } else if (state$box=="custom" & (state$latlon_method=="typeCoords" | state$latlon_method=="loadShapefile")) {
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
                # Use point.in.polygon to extract data from existing boxes
                Longitude <- as.numeric(all_regions[[region]][[which(state$box==poly_ID[[region]])]]$lon)
                Latitude <- as.numeric(all_regions[[region]][[which(state$box==poly_ID[[region]])]]$lat)
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
        
        # If there is valid data in this region for any day, compute statistics and add to state list
        if (!is.null(rchla)) {
            all_stats <- get_stats(rchla=rchla, outlier=state$outlier)
            dummy <- lapply(names(all_stats), function(nm) {state[[nm]] <- all_stats[[nm]]})
        }
        
        gc()
        
        return(rchla)
        
    })
    
    
    #***************************************************************************
    # DENSITY PLOT, DAILY/WEEKLY STATS ####
    
    make_density_plot <- reactive({
        
        # get the most recent annual data
        rchla <- annual_stats()
        
        # get the latest day of year
        yearday <- state$yearday
        day_label <- state$day_label
        time_ind <- state$time_ind
        
        isolate({
            concentration_type <- state$concentration_type
            cell_size_model1 <- state$cell_size_model1
            cell_size_model2 <- state$cell_size_model2
        })
        
        plot_title <- paste0(ifelse(concentration_type=="model1", paste0(proper(cell_size_model1), " cell size: "),
                                    ifelse(concentration_type=="model2", paste0(proper(cell_size_model2), " cell size: "), "")),
                             'Density plot of chlorophyll concentration for ', day_label)
        
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
              
                em <- paste0("Polygon is too large (max allowed area = ", isolate(state$max_area), " degrees^2).")
            
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
                ok <- 100 * lenok[time_ind] / nrow(rchla) > state$percent
                if (!ok) {
                    em <- paste0("Insufficient data, coverage < ", state$percent, "%")
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
        
        isolate({
            concentration_type <- state$concentration_type
            cell_size_model1 <- state$cell_size_model1
            cell_size_model2 <- state$cell_size_model2
            interval <- state$interval
            dailystat <- state$dailystat
            year <- state$year
            log_chla <- state$log_chla
        })
        
        plot_title <- paste0(ifelse(concentration_type=="model1", paste0(proper(cell_size_model1), " cell size: "),
                                    ifelse(concentration_type=="model2", paste0(proper(cell_size_model2), " cell size: "), "")),
                             "Time series of ", interval, " ", dailystat, " chlorophyll concentration for ", year)
        
        # Get the vector of dataframe names
        pnames <- pnlist[[state$fitmethod]]
        if (state$fitmethod=="gauss") {
            pnames <- pnames[[state$bloomShape]]
            if (!state$beta) {pnames <- pnames[!grepl("beta", pnames)]}
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
            
              em <- paste0("Polygon is too large (max allowed area = ", isolate(state$max_area), " degrees^2).")
            
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
                
                daily_percov <- 100 * lenok / nrow(rchla)
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
                                 last_day, " with >= ", state$percent, "% coverage")
                    
                }
                
            }
            
        } else {
            em <- "Load data to begin"
        }
        
        
        #***********************************************************************
        # IF VALUES CAN BE FITTED, FIT THEM
        
        if (is.null(em)) {
            
            bf_data <- get_bloom_fit_data(interval=interval,
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
                                          t_range = c(first_day, last_day),
                                          log_chla = log_chla,
                                          threshcoef = state$threshcoef,
                                          doy_vec = doy_vec,
                                          plot_title = plot_title,
                                          flag1_lim1 = state$flag1_lim1,
                                          flag1_lim2 = state$flag1_lim2,
                                          flag2_lim1 = state$flag2_lim1,
                                          flag2_lim2 = state$flag2_lim2,
                                          ti_threshold = state$ti_threshold,
                                          tt_threshold = state$tt_threshold,
                                          rm_bkrnd = state$rm_bkrnd,
                                          ti_threshold_type = state$ti_threshold_type,
                                          ti_threshold_constant = state$ti_threshold_constant)
            loess_smooth <- rep(NA,length(daily_percov))
            if (state$smoothMethod == 'loess') {
                loess_smooth[ind_dayrange_percov] <- bf_data$y$y
            }
            state$loess_smooth <- loess_smooth
            
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
            npyday <- nearPoints(state$dfbloomparms, coordinfo = state$bloomfit_click, xvar = "x", yvar = "y")$x
            updateSliderInput(session, inputId = 'yearday_slide', value = npyday)
        }
    })
    
    
    # RUN FULL TIME SERIES ####
    
    # Take a vector of years selected by the user, and compute annual statistics
    # and bloom fits for each, using the current settings
    observeEvent(input$fullrun_process, {
        
        regs <- isolate(state$fullrunboxes)
        
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
            rm_bkrnd = state$rm_bkrnd
            ti_threshold_type = state$ti_threshold_type
            ti_threshold_constant = state$ti_threshold_constant
            newpoly = state$newpoly
            editedpoly = state$editedpoly
            typedpoly = state$typedpoly
            fullrunoutput_png <- input$fullrunoutput_png
            fullrunoutput_statcsv <- input$fullrunoutput_statcsv
            concentration_type = state$concentration_type
            cell_size_model1 = state$cell_size_model1
            cell_size_model2 = state$cell_size_model2
        })
        
        # create column names for parameter table
        pnames <- pnlist[[fitmethod]]
        if (fitmethod=="gauss") {
            pnames <- pnames[[bloomShape]]
            if (!beta) {pnames <- pnames[!grepl("beta", pnames)]}
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
                                           custom_end=NULL,
                                           concentration_type=concentration_type,
                                           cell_size_model1=cell_size_model1,
                                           cell_size_model2=cell_size_model2))
        dir.create(output_dir)
        
        steps <- 100/length(year_list)
        progress_updates <- round(seq(steps[1], 100, by=steps),1)
        
        polygon_list <- get_polygon_details(regs, custom_name, region, polylat, polylon, all_regions, full_names, poly_ID, newpoly, editedpoly, typedpoly)
        
        total_params_df <- data.frame(matrix(nrow=(length(year_list)*length(polygon_list$full_names)), ncol=(length(pnames)+2)), stringsAsFactors = FALSE)
        colnames(total_params_df) <- c("Region", "Year", pnames)
        
        sslat <- reginfo[[region]]$lat
        sslon <- reginfo[[region]]$lon
        
        for (x in 1:length(year_list)) {
            
            tmp_par <- full_run(
                year = year_list[x],
                satellite = satellite,
                region = region,
                algorithm = algorithm,
                interval = interval,
                sslat = sslat,
                sslon = sslon,
                polygon_list = polygon_list,
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
                tt_threshold = tt_threshold,
                rm_bkrnd = rm_bkrnd,
                ti_threshold_type = ti_threshold_type,
                ti_threshold_constant = ti_threshold_constant,
                fullrunoutput_png = fullrunoutput_png,
                fullrunoutput_statcsv = fullrunoutput_statcsv,
                concentration_type = concentration_type,
                cell_size_model1 = cell_size_model1,
                cell_size_model2 = cell_size_model2)
            
            # add to final output dataframe
            total_params_df[((x-1)*length(polygon_list$full_names)+1):(x*length(polygon_list$full_names)),] <- tmp_par
            
            # update progress bar
            if (x==length(year_list)) {update_text <- "Zipping output files..."
            } else {update_text <- paste0("Computing fits for ", year_list[x+1], "...")}
            update_modal_progress(value = (progress_updates[x]/100), text = update_text)
            
            gc()
            
        }
        
        write.csv(total_params_df %>% dplyr::arrange(., Region, Year),
                  file=file.path(output_dir, "bloom_fit_params.csv"),
                  quote=FALSE, na=" ", row.names=FALSE)
        
        # SAVE SETTINGS
        if (isolate(state$box)=="custom") {
            plons <- polylon
            plats <- polylat
        } else {
            plons <- plats <- NA
        }
        info <- format_settings_to_save(all_inputs=reactiveValuesToList(isolate(input)),
                                        custom_name=isolate(state$custom_name),
                                        polylon=plons,
                                        polylat=plats,
                                        regions=regions,
                                        sat_algs=sat_algs)
        write.table(info, file=file.path(output_dir, "settings.csv"), row.names=FALSE, na=" ", sep="\\")
        
        gc()
        
        fname <- paste0(output_str(satellite=satellite,
                            region=region,
                            algorithm=algorithm,
                            year=isolate(input$fullrunyears),
                            interval=interval,
                            log_chla=log_chla,
                            fitmethod=fitmethod,
                            custom_end=NULL,
                            concentration_type=concentration_type,
                            cell_size_model1=cell_size_model1,
                            cell_size_model2=cell_size_model2),".zip")
        
        # zip files up to be downloaded
        # j flag prevents files from being sorted into subdirectories inside the zip file (the other flags are defaults)
        zip(zipfile=file.path(output_dir, fname), files=list.files(output_dir, full.names=TRUE), flags="-r9Xj")
        
        # remove progress bar and return to normal screen
        remove_modal_progress()
        
        state$fullrun_outputdir <- output_dir
        state$fullrun_fname <- fname
        
        enable("fullrun_download")
        
    })
    
    output$fullrun_fname <- renderUI({
        if (is.null(state$fullrun_fname)) {
            helpText("", width = widget_width, style = help_text_style)
        } else {
            helpText(HTML(paste0("File ready for download:<br>", gsub("_", "_ ", state$fullrun_fname))),
                     width = widget_width,
                     style = help_text_style)
        }
    })
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    # DOWNLOAD OUTPUT ####
    # Map, density plot, bloom fit plot, annual stats, bloom parameters, and settings.
    # Downloads to browser's default downloads folder.
    
    # Download the results from "fullrun_process"
    output$fullrun_download <- downloadHandler(
        filename <- isolate(state$fullrun_fname),
        content <- function(file) {
          file.copy(file.path(isolate(state$fullrun_outputdir), isolate(state$fullrun_fname)), file)
        },
        contentType = "application/zip"
    )
    
    # SAVE MAP (.html)
    output$savemap <- downloadHandler(
        filename <- filename_map(isolate(state)),
        content <- function(file) {
          saveWidget(widget=content_map(d=isolate(state),map=map_reactive()), file=file)
        }
    )
    
    # SAVE DENSITY PLOT (.png)
    output$savedensplot <- downloadHandler(
        filename <- filename_dens(d=isolate(state)),
        content <- function(file) {
          ggsave(file=file, plot=isolate(make_density_plot()), width=12, height=5, units="in")
        }
    )
    
    # SAVE BLOOM FIT PLOT (.png)
    output$savebloomfit <- downloadHandler(
        filename <- filename_bfp(d=isolate(state)),
        content <- function(file) {
          ggsave(file=file, plot=isolate(make_bloom_fit()), width=12, height=5, units="in")
        }
    )
    
    # SAVE ANNUAL STATS (.csv)
    output$saveannualstats <- downloadHandler(
        filename <- filename_stats(d=isolate(state)),
        content <- function(file) {
          sdf <- content_stats(d=isolate(state))
          write.csv(sdf, file=file, quote=FALSE, na=" ", row.names=FALSE)
        }
    )
    
    # SAVE BLOOM FIT PARAMETERS (.csv)
    output$savebloomparams <- downloadHandler(
        filename <- filename_bfparam(d=isolate(state)),
        content <- function(file) {
          write.csv(isolate(state$fitparams), file=file, quote=FALSE, na=" ", row.names=FALSE)
        }
    )
    
    # SAVE CURRENT SETTINGS (.txt)
    output$savesettings <- downloadHandler(
        filename <- filename_settings(isolate(state)),
        content <- function(file) {
          info <- content_settings(d=isolate(state), inp=reactiveValuesToList(isolate(input)))
          write.table(info, file=file, row.names=FALSE, na="NA", sep="\\")
        }
    )
    
}

# RUN APPLICATION ####
shinyApp(ui = ui, server = server)
