# clear memory to free up space
gc()

library(shiny)
library(shinyWidgets)   # for updating buttons
library(shinyjs)        # for enabling/disabling download buttons
library(shinybusy)      # to show progress when processing a full time series
library(leaflet)        # for creating map
library(leafpm)         # for drawing/editing polygons on the map (replacing leaflet.extras)
library(fst)            # for speedier data file loading
library(minpack.lm)     # to use nlsLM for gaussian fit
library(sf)             # to make polygons into simple feature collections (sfc)
library(sp)             # convert sfc to spatial to use sp::over() to extract pixels in polygons
library(ggplot2)        # for the density and time series plots
library(ggpp)           # for overlaying tables and text on plots
library(dplyr)          # for formatting data tables
library(stringr)        # for str_pad
library(terra)          # for making rasters
# other required packages: raster, leafem, quantreg, fs (functions called using :: notation)

source("scripts/rateOfChange.R")        # rate of change (ROC) function for model fit
source("scripts/threshold.R")           # threshold function for model fit
source("scripts/gaussFit.R")            # gaussian function for model fit
source("scripts/full_run.R")            # contains function to run full time series with current settings
source("scripts/functions.R")           # extra functions
source("scripts/00_input_variables.R")  # load pre-defined variables


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
               input[type=checkbox] {transform: scale(1.5);}"),

    # old polygon removal code
    tags$div(remove_custom_poly),
    
    # color and style for sliderInput (note this does not affect sliderTextInput)
    chooseSliderSkin("Flat", color="#00cc00"),
    # needed for zlim sliderTextInput
    setSliderColor("#00cc00", sliderId=1),
    
    titlePanel("Satellite Data Visualization"),
    
    fluidRow(
        
        sidebarPanel(width=3,
            
            #*******************************************************************
            # UI MAIN SETTINGS ####
            
            helpText(HTML(bhelp$settings_file), width=widget_width, style=label_text_style_main_options),
            br(),
            fileInput(inputId="settings_file", label=NULL, multiple=FALSE, accept=".txt", width=widget_width),
            actionButton(inputId="applysettings", label="Apply settings", style=button_style),
            uiOutput("help_settings_file", width=widget_width, style="white-space: normal;"),
            hr(),
            helpText(HTML(bhelp$main_settings), width=widget_width, style=label_text_style_main_options),
            br(),br(),
            helpText("Region", width=widget_width, style=label_text_style_main_options),
            selectInput(inputId="region", label=NULL, choices=regions, selected=default_region, width=widget_width),
            helpText("Satellite and variable", width=widget_width, style=label_text_style_main_options),
            selectInput(inputId="sat_alg", label=NULL, choices=default_sat_algs, selected=default_sat_algs[1], width=widget_width),
            helpText(bhelp$concentration_type, width=widget_width, style=paste(label_text_style_main_options, "margin-bottom: 20px;")),
            radioButtons(inputId="concentration_type", label=NULL, choices=concentration_types, selected="full", width=widget_width),
            conditionalPanel(condition = "input.concentration_type == 'model1'",
                             radioGroupButtons(inputId = "cell_size_model1", label=NULL,
                                               choices=cell_sizes_model1, selected = "small", width=widget_width)),
            conditionalPanel(condition = "input.concentration_type == 'model2'",
                             radioGroupButtons(inputId = "cell_size_model2", label=NULL,
                                               choices=cell_sizes_model2, selected = "small", width=widget_width)),
            br(),
            helpText("Year", width=widget_width, style=label_text_style_main_options),
            selectInput(inputId = "year", label=NULL, choices=rev(default_years), width=widget_width),
            helpText("Data composite length", width=widget_width, style=label_text_style_main_options),
            selectInput(inputId = "composite", label=NULL, choices=composites, width=widget_width),
            # this will be enabled if data is available for the combination of variables selected above
            disabled(actionButton(inputId = "load", label = "Load data", style=button_style)),
            uiOutput("help_load", width=widget_width, style = "white-space: normal;"),
            br(),
            
            shinyjs::hidden(div(id="hiddenPanel",
                                
                                helpText("Log map color scale and axes on plots? (This does NOT log the actual data)", width=widget_width, style=label_text_style_main_options),
                                checkboxInput(inputId="log_display", label="Log10 display", value=default_variable$log),
                                br(),
                                
            
            #*******************************************************************
            # UI POLYGON ####
            
            actionButton(inputId="polygonButton", label="Polygon",
                         "data-toggle"='collapse', "data-target"='#polygonDiv', style=button_style),
            
            div(id = 'polygonDiv', class="collapse",
                
            br(),
            selectInput(inputId = 'box',
                        label=HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">Choose a polygon</font>"),
                        choices=poly_choices[[default_region]], selected = 'custom', width=widget_width),
            conditionalPanel(condition = "input.box =='custom'",
                             helpText(bhelp$box_custom, width=widget_width, style=help_text_style),
                             div(style="display: inline-block; vertical-align:top; align:center; width: 58%;",
                                 textInput(inputId = "custom_name", label=NULL, value=NULL)),
                             div(style="display: inline-block; width: 40%",
                                 actionButton(inputId="applyname", label="Apply", style=button_style)),
                             helpText(bhelp$box_method, width=widget_width, style=help_text_style),
                             radioButtons(inputId = "latlon_method", label=NULL,
                                          choices=latlon_methods, selected = "drawPoly", width=widget_width),
                            conditionalPanel(condition = "input.latlon_method =='drawPoly'",
                                             helpText(bhelp$box_draw, width=widget_width, style=help_text_style)),
                            conditionalPanel(condition = "input.latlon_method =='typeCoords'",
                                             helpText(HTML(bhelp$box_type), width=widget_width, style=help_text_style)),
                            conditionalPanel(condition = "input.latlon_method =='loadShapefile'",
                                             helpText(HTML(bhelp$box_shp), width=widget_width, style=help_text_style),
                                             fileInput(inputId = "shapefile", label=NULL,
                                                       multiple=TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qix'), width=widget_width)),
                            conditionalPanel(condition = "input.latlon_method =='typeCoords'",
                                             helpText(HTML("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">List of latitudes:</font>"),
                                                      width=widget_width, style=paste(help_text_style, "margin-bottom: -2px; margin-top: -5px;")),
                                             textInput(inputId = "manual_lats", label=NULL,
                                                       value = "", width=widget_width, placeholder = "42.6, 43, 42, 40.4, 40, 42.6"),
                                             helpText(HTML("<font style=\"font-size: 12px; color: #555555; font-weight: bold;\">List of longitudes:</font>"),
                                                      width=widget_width, style=paste(help_text_style, "margin-bottom: -2px; margin-top: -5px;")),
                                             textInput(inputId = "manual_lons", label=NULL,
                                                       value = "", width=widget_width, placeholder = "-61, -59, -55, -57, -60.4, -61"),
                                             actionButton(inputId='createTypedPoly', label='Create polygon',width=widget_width)),
                            uiOutput("shapefile_button", width=widget_width, style = "white-space: normal;"),
                            uiOutput("help_latlon", width=widget_width, style = "white-space: normal;"),
                            br()),
            br()
            ), # closes polygon panel
            
            
            #*******************************************************************
            # UI STATISTICS ####
            
            actionButton(inputId="statsButton", label="Statistics",
                         "data-toggle"='collapse', "data-target"='#statsDiv', style=button_style),
            
            div(id = 'statsDiv', class="collapse",
            br(),
            helpText(HTML(bhelp$percent), width=widget_width, style=help_text_style),
            numericInput(inputId = 'percent', label=NULL, value=10, min=0, max=100, width=widget_width),
            helpText(HTML(bhelp$outlier), width=widget_width, style=help_text_style),
            selectInput(inputId='outlier', label=NULL, choices=outliers, selected = 'none', width=widget_width),
            helpText(HTML(bhelp$dailystat), width=widget_width, style=help_text_style),
            selectInput(inputId = 'dailystat', label=NULL, choices=dailystats, selected = 'average', width=widget_width),
            helpText(HTML(bhelp$pixrange), width=widget_width, style=help_text_style),
            div(style="display: inline-block; vertical-align:top; width: 50px;",
                textInput(inputId = "pixrange1", label=NULL, value=NA)),
            div(style="display: inline-block; vertical-align:top; width: 10px;",
                helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
            div(style="display: inline-block; vertical-align:top; width: 50px;",
                textInput(inputId = "pixrange2", label=NULL, value=NA)),
            div(style="display: inline-block;vertical-align:top; width: 60px;",
                actionButton(inputId="applypixrange", label="Apply", style=button_style)),
            br(),br()
            ), # closes stats panel
            
            
            #*******************************************************************
            # UI MODEL FIT ####
            
            actionButton(inputId="bfButton", label="Model fit",
                         "data-toggle"='collapse', "data-target"='#bfDiv', style=button_style),
            
            div(id = 'bfDiv', class="collapse",
            helpText(bhelp$bf_desc, width=widget_width, style="white-space: normal; font-size: 14px; font-weight: bold;"),
            br(),
            helpText(bhelp$bf, width=widget_width, style=help_text_style),
            selectInput(inputId='fitmethod', label=NULL, choices=fitmethods, selected='gauss', width=widget_width),
            selectInput(inputId='bloomShape', label=NULL, choices=bloomShapes, selected='symmetric', width=widget_width),
            selectInput(inputId='smoothMethod', label=NULL, choices=smoothMethods, selected='nofit', width=widget_width),
            conditionalPanel(condition = "input.smoothMethod == 'loess'",
                             helpText(HTML(bhelp$loessSpan), width=widget_width, style=help_text_style),
                             numericInput(inputId = 'loessSpan', label=NULL, value=0.3, min=0.04, max=1, step=0.02, width=widget_width)),
            helpText(bhelp$log_chla, width=widget_width, style=help_text_style),
            checkboxInput(inputId = "log_chla", label=HTML("log<sub>10</sub>(input data)"), value=default_variable$log, width = widget_width),
            helpText(bhelp$fit_days, width=widget_width, style=help_text_style),
            sliderInput(inputId='t_range', min=1, max=365, value=c(31,274), ticks=FALSE,
                        label=HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">t<sub>range</sub></font>")),
            sliderInput(inputId='tm_limits', min=1, max=365, value=c(91,181), ticks=FALSE,
                        label=HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">t<sub>max</sub></font>")),
            sliderInput(inputId='ti_limits', min=1, max=365, value=c(60,151), ticks=FALSE,
                        label=HTML("<font style=\"font-size: 14px; color: #555555; font-weight: bold;\">t<sub>start</sub></font>")),
            helpText(bhelp$fit_weights, width=widget_width, style=help_text_style),
            switchInput(inputId='use_weights', label='weights', value=FALSE, onStatus="success"),
            helpText(HTML(bhelp$fit_bkrnd), width=widget_width, style=help_text_style),
            checkboxInput(inputId="rm_bkrnd", label="Remove background", value=TRUE, width=widget_width),
            helpText(HTML(bhelp$fit_flags), width=widget_width, style=help_text_style),
            conditionalPanel(condition = "input.fitmethod == 'gauss'",
                             helpText(HTML(bhelp$fit_tstart_method), width=widget_width, style=help_text_style),
                             radioButtons(inputId = "ti_threshold_type", label=NULL, choices=ti_threshold_types, selected = "percent_thresh", width=widget_width),
                             conditionalPanel(condition = "input.ti_threshold_type == 'percent_thresh'",
                                              helpText(HTML(bhelp$fit_ti_threshold_percent), width=widget_width, style=help_text_style),
                                              numericInputIcon(inputId="ti_threshold_percent", label=NULL, value=20, min=0.01, max=90, step=5, icon=list(NULL,icon("percent")), width="75%")),
                             conditionalPanel(condition = "input.ti_threshold_type == 'constant_thresh'",
                                              helpText(HTML(bhelp$fit_ti_threshold_constant), width=widget_width, style=help_text_style),
                                              numericInput(inputId="ti_threshold_constant", label=NULL, value=0.1, min=0.01, max=5, step=0.05, width="75%")),
                             helpText(HTML(bhelp$fit_tmax), width=widget_width, style=help_text_style),
                             switchInput(inputId='tm', label=HTML('t<sub>max</sub>'), value=FALSE, onStatus="success"),
                             helpText(HTML(bhelp$fit_beta), width=widget_width, style=help_text_style),
                             switchInput(inputId='beta', label='\u03B2t', value=FALSE, onStatus="success"),
                             actionButton(inputId="flagdescriptions", label="Flag descriptions", style=button_style),
                             br(),br(),
                             helpText(HTML(bhelp$fit_flag1), width=widget_width, style=help_text_style),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId="flag1_lim1", label=NULL, value=0.75)),
                             div(style="display: inline-block; vertical-align:top; width: 10px; margin-top: -10px;",
                                 helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId="flag1_lim2", label=NULL, value=1.25)),
                             div(style="display: inline-block;vertical-align:top; width: 60px; margin-top: -10px;",
                                 actionButton(inputId="apply_flag1_lim", label="Apply", style=button_style)),
                             helpText(HTML(bhelp$fit_flag2), width=widget_width,style=help_text_style),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId="flag2_lim1", label=NULL, value=0.85)),
                             div(style="display: inline-block; vertical-align:top; width: 10px; margin-top: -10px;",
                                 helpText(HTML(paste0("<font style=\"font-size: 14px; color: #555555;\">&ndash;</font>")))),
                             div(style="display: inline-block; vertical-align:top; width: 50px; margin-top: -10px;",
                                 textInput(inputId="flag2_lim2", label=NULL, value=1.15)),
                             div(style="display: inline-block;vertical-align:top; width: 60px; margin-top: -10px;",
                                 actionButton(inputId="apply_flag2_lim", label="Apply", style=button_style))),
            conditionalPanel(condition = "input.fitmethod == 'thresh'",
                             helpText(HTML(bhelp$fit_threshcoef), width=widget_width, style=help_text_style),
                             numericInput(inputId='threshcoef', label=NULL, value=1.05, min=1.00, width=widget_width),
                             helpText(HTML(bhelp$fit_threshcoefnote), width=widget_width, style=help_text_style)),
            ), # closes model fit panel
            
            br(),br(),hr(),
            
            #*******************************************************************
            # UI SAVE OPTIONS ####
            
            helpText(HTML(bhelp$fullrun), width=widget_width, style=help_text_style),
            checkboxInput(inputId = "fullrunoutput_png", value=TRUE, width=widget_width,
                          label=HTML("<font style=\"white-space: normal; font-size: 10px;\">Include .png files of time series plots?</font>")),
            sliderInput(inputId="fullrunyears", label=NULL, min=min(default_years), max=max(default_years), value=range(default_years), ticks=FALSE, step=1, sep = ""),
            pickerInput(inputId="fullrunboxes", label="Select your polygons", choices=multipoly_choices[[default_region]], selected="custom",
                        options=list(`actions-box`=TRUE,size=10,`selected-text-format`="count > 3"), multiple=TRUE, width=widget_width),
            br(),
            actionButton(inputId = "fullrun_process", label = "Run time series", style=button_style),
            uiOutput("fullrun_fname", width=widget_width, style = "white-space: normal;"),
            disabled(downloadButton(outputId = "fullrun_download", label = "Download results (.zip)", style=button_style)),
            br(),br(),
            disabled(downloadButton(outputId = "savesettings", label = "Save settings (.txt)", style=button_style))
            
            )) # close side panel that's hidden when data hasn't been loaded yet
            
        ),
        
        #***********************************************************************
        # UI DISPLAY ####
        
        mainPanel(width=9,
                  
               div(style="margin-bottom: -30px; margin-left: 4px; margin-right: 7px;",
                   plotOutput(outputId='datebar',height="40px",width='100%')),
               sliderInput(inputId="yearday_slide", label=NULL, min=1, max=365, value=1,
                           animate=animationOptions(interval=4000), ticks=TRUE, width='100%'),
               leafletOutput(outputId='fullmap', height='700px'),
               div(style="display: inline-block; vertical-align:top; width: 74%; margin-top: 5px;",
                   sliderTextInput(inputId="zlim", label=NULL, choices=default_colscale, selected=range(default_colscale), width="100%", grid=TRUE)),
               div(style="display: inline-block; vertical-align:top; width: 25%; margin-top: 5px;",
                   actionButton(inputId="applyzlim", label="Apply color scale", style=button_style)),
               br(),
               htmlOutput(outputId="poly_title"),
               br(),
               div(style="display: inline-block; vertical-align:top; width: 82%; margin-top: 5px;",
                   plotOutput(outputId='chla_hist',height='360px')),
               div(style="display: inline-block; vertical-align:top; width: 17%; margin-top: 27px;",
                   tableOutput(outputId="chla_hist_df")),
               disabled(downloadButton(outputId="savedensplot",label="Download density plot (.png)",style=button_style)),
               br(),br(),
               div(style="display: inline-block; vertical-align:top; width: 82%; margin-top: 5px;",
                   plotOutput(outputId='bloomfit',height='440px',click='bloomfit_click')),
               div(style="display: inline-block; vertical-align:top; width: 17%; margin-top: 27px; font-size:80%;",
                   tableOutput(outputId="bloomfit_df")),
               disabled(downloadButton(outputId="savebloomfit",label="Download time series plot (.png)",style=button_style)),
               disabled(downloadButton(outputId="saveannualstats",label="Download time series table of statistics (.csv)",style=button_style)),
               disabled(downloadButton(outputId="savebloomparams",label="Download fit parameters (.csv)",style=button_style)),
               br(),br()
               
        )
        
    ) # closes fluidRow
    
)




#*******************************************************************************

server <- function(input, output, session) {
    
    # 2023-05-01 When I close the app, sometimes R stops responding. Can't find any issues with the code that could be causing it, and it appears totally random - often it does, sometimes it doesn't. This line is required to make sure the app stops and doesn't crash R on the way out.
  # https://community.rstudio.com/t/r-crashes-when-closing-shiny-app-window-instead-of-clicking-red-stop-button-in-rstudio/131951/2
    session$onSessionEnded(function() stopApp())
  
    # Create a list of reactive values to collect widget input after running checks on it.
    state <- reactiveValues()
    
    # initialize some defaults so the code doesn't break
    state$data_loaded <- FALSE
    state$region <- default_region
    state$predefined_polys <- predefined_polys[[default_region]]
    state$sat_alg <- default_sat_algs[1]
    state$variable <- default_variable
    state$concentration_type <- "full"
    state$cell_size_model1 <- state$cell_size_model2 <- "small"
    state$year <- as.numeric(format(Sys.Date(),"%Y"))
    state$composite <- 1
    state$log_display <- default_variable$log
    state$log_chla <- default_variable$log
    state$zlim <- range(default_colscale)
    state$doy_vec <- 1:365
    state$sschla <- matrix(nrow=1,ncol=365)
    state$flag1_lim1 <- 0.75
    state$flag1_lim2 <- 1.25
    state$flag2_lim1 <- 0.85
    state$flag2_lim2 <- 1.15
    state$pixrange1 <- -Inf
    state$pixrange2 <- Inf
    state$latlon_method <- "drawPoly"
    state$draw_toolbar <- TRUE
    state$latlon_invalid <- state$latlon_toolarge <- FALSE
    state$newpoly <- state$editedpoly <- NULL
    state$ti_threshold <- state$tt_threshold <- 0.2
    state$fullrun_fname <- NULL
    state$box <- state$fullrunboxes <- "custom"
    state$custom_name <- ""
    state$help_load_txt <- state$help_latlon_txt <- state$help_settings_file_txt <- ""
    state$secondary_settings <- NULL
    state$draw_programmatically <- state$applyname_programmatically <- FALSE
    # These are used to check which specific inputs have been updated in the code block
    # below that hides the left panel if any of the main inputs have changed.
    state$current_sat_algs <- default_sat_algs
    state$current_years <- default_years
    # Used for deleting typed or shapefile polygons
    state$typed_polys_deleted <- 0
    state$shapefile_polys_deleted <- 0
    state$shapefile <- NULL
    
    # START SCREEN POPUP
    observe({
        showModal(modalDialog(
            title = "Satellite Data Visualization",
            HTML(startup_popup), easyClose=TRUE, footer=modalButton("OK")
        ))
    })
    
    
    #***************************************************************************
    # APPLY SETTINGS FILE ####
    
    # If user uploads an existing settings file, read the settings and apply them. Either:
    # 1. All the main settings are the same but some secondary settings (i.e. in the hidden panel)
    #    might be changed, in which case you just need to change those settings, or
    # 2. Some main settings are different - update them and programmatically click the "load"
    #    button, then apply the secondary settings.
    # If a valid custom polygon is present in the settings file, extract the coordinates and apply them
    # to the typeCoords option, programmatically clicking the "Create polygon" button.
    # Afterwards, reset state$secondary_settings to NULL, and "programmatic" button clicking 
    # variables to FALSE.
    observeEvent(input$applysettings, {
        help_settings_file_txt <- ""
        file <- input$settings_file
        ext <- tools::file_ext(file$datapath)
        # check the extension
        if (ext == "txt") {
            # try to load the file
            file_contents <- try(read.table(file$datapath, header = TRUE, sep="\\"), silent=TRUE)
            if (class(file_contents)!="try-error") {
                # check the file contents
                if (all(colnames(file_contents)==c("setting_id","value","setting_description","value_description", "variable_type"))) {
                    # get updated values of main input buttons
                    main_ids <- c("region","sat_alg","concentration_type","cell_size_model1","cell_size_model2","year","composite")
                    main_inds <- file_contents$setting_id %in% main_ids
                    primary_settings <- file_contents[main_inds,]
                    new_inputs <- format_settings_to_load(primary_settings)
                    # check if data actually exists for this combo
                    if (dexist(new_inputs$region,new_inputs$sat_alg,new_inputs$year)) {
                      state$secondary_settings <- file_contents[!main_inds,]
                      current_inputs <- reactiveValuesToList(input)
                      current_inputs <- current_inputs[match(names(new_inputs),names(current_inputs))]
                      ci <- as.character(unname(unlist(current_inputs)))
                      ni <- as.character(unname(unlist(new_inputs)))
                      if (identical(ci,ni)) { # main settings haven't changed, just check secondary settings
                        update_secondary_settings()
                        if (!state$data_loaded) {
                          state$help_load_txt <- ""
                          enable("load")
                          shinyjs::click("load")
                        }
                      } else { # main settings have changed, update them and then update secondary settings after data loads
                        nir <- new_inputs$region
                        nisa <- new_inputs$sat_alg
                        updateSelectInput(session, inputId="region", selected=nir)
                        updateSelectInput(session, inputId="sat_alg", selected=nisa, choices=sat_algs[[nir]])
                        updateRadioButtons(session, inputId="concentration_type", selected=new_inputs$concentration_type)
                        updateRadioGroupButtons(session, inputId="cell_size_model1", selected=new_inputs$cell_size_model1)
                        updateRadioGroupButtons(session, inputId="cell_size_model2", selected=new_inputs$cell_size_model2)
                        updateSelectInput(session, inputId="year", selected=new_inputs$year, choices=rev(years[[nir]][[nisa]]))
                        updateSelectInput(session, inputId="composite", selected=new_inputs$composite)
                      }
                    } else {
                        help_settings_file_txt <- "No data available for the region/satellite/algorithm/year in the settings file."
                    }
                } else {
                    help_settings_file_txt <- "Invalid file contents."
                }
            } else {
              help_settings_file_txt <- "Invalid input file."
            }
        } else {
            help_settings_file_txt <- "Please select a PhytoFit settings file with extension .txt."
        }
        state$help_settings_file_txt <- help_settings_file_txt
    })
    
    output$help_settings_file <- renderPrint({
        helpText(state$help_settings_file_txt, width=widget_width, style=error_text_style)
    })
    
    # function to apply secondary settings, either by themselves (if main settings haven't changed), or after main settings have been updated and data reloaded
    update_secondary_settings <- eventReactive(state$secondary_settings, {
      
      ssdf <- state$secondary_settings
      
      if (!is.null(ssdf)) {
        
        # update polygon input
        predefined_polygon <- ssdf$value[ssdf$setting_id=="box"]
        custom_polygon <- ssdf$value[ssdf$setting_id=="polystr"]
        updateSelectInput(session, inputId="box", choices=poly_choices[[state$region]], selected=predefined_polygon)
        # if it's a custom box, update lat/lon input and add it to the map and stats using the "typeCoords" method
        if (predefined_polygon=="custom" & !is.na(custom_polygon)) {
          custom_polygon_name <- trimws(ssdf$value[ssdf$setting_id=="custom_name"])
          spdf_name <- ifelse(nchar(custom_polygon_name) > 0,custom_polygon_name,"custom")
          updateRadioButtons(session, inputId="latlon_method", selected="typeCoords")
          spdf <- st_sf(tibble(poly_id="custom",group=NA,name=spdf_name,label=spdf_name,geometry=st_as_sfc(custom_polygon)))
          st_crs(spdf) <- "EPSG:4326"
          pcp_coords <- st_coordinates(spdf)
          updateTextInput(session, inputId="manual_lats", value=pcp_coords[,2])
          updateTextInput(session, inputId="manual_lons", value=pcp_coords[,1])
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
        
        # format remaining secondary settings for updating
        ssdf <- ssdf[!(ssdf$setting_id %in% c("box","custom_name","polystr")),]
        ssdf <- format_settings_to_load(ssdf)
        vals <- ssdf$values
        ids <- ssdf$ids
        
        # YEARDAY_SLIDER
        updateSliderInput(session, inputId = "yearday_slide", value = ssdf$yearday_slide, step = state$composite)
        
        # FULL_RUN
        updateCheckboxInput(session, inputId="fullrunoutput_png", value=ssdf$fullrunoutput_png)
        tmp_years <- as.numeric(years[[state$region]][[state$sat_alg]])
        updateSliderInput(session, inputId="fullrunyears", min=min(tmp_years), max=max(tmp_years), value=ssdf$fullrunyears)
        updatePickerInput(session, inputId="fullrunboxes", selected=ssdf$fullrunboxes)
        
        # STATS SETTINGS: percent, outlier, dailystat, pixrange1, pixrange2
        updateNumericInput(session, inputId="percent", value=ssdf$percent)
        updateSelectInput(session, inputId="outlier", selected=ssdf$outlier)
        updateSelectInput(session, inputId="dailystat", selected=ssdf$dailystat)
        updateTextInput(session, inputId="pixrange1", value=ssdf$pixrange1)
        updateTextInput(session, inputId="pixrange2", value=ssdf$pixrange2)
        
        # MODEL FIT SETTINGS: fitmethod, bloomShape, smoothMethod, log_chla, loessSpan, t_range, ti_limits, tm_limits, ti_threshold_type,
        # ti_threshold_constant, tm, beta, use_weights, rm_bkrnd, flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2, threshcoef
        updateSelectInput(session, inputId="fitmethod", selected=ssdf$fitmethod)
        updateSelectInput(session, inputId="bloomShape", selected=ssdf$bloomShape)
        updateSelectInput(session, inputId="smoothMethod", selected=ssdf$smoothMethod)
        updateCheckboxInput(session, inputId="log_chla", value=ssdf$log_chla)
        updateNumericInput(session, inputId="loessSpan", value=ssdf$loessSpan)
        updateSliderInput(session, inputId="t_range", value=ssdf$t_range)
        updateSliderInput(session, inputId="ti_limits", value=ssdf$ti_limits)
        updateSliderInput(session, inputId="tm_limits", value=ssdf$tm_limits)
        updateRadioButtons(session, inputId="ti_threshold_type", selected=ssdf$ti_threshold_type)
        updateNumericInput(session, inputId="ti_threshold_percent", value=ssdf$ti_threshold_percent)
        updateNumericInput(session, inputId="ti_threshold_constant", value=ssdf$ti_threshold_constant)
        updateSwitchInput(session, inputId="tm", value=ssdf$tm)
        updateSwitchInput(session, inputId="beta", value=ssdf$beta)
        updateSwitchInput(session, inputId="use_weights", value=ssdf$use_weights)
        updateCheckboxInput(session, inputId="rm_bkrnd", value=ssdf$rm_bkrnd)
        updateTextInput(session, inputId="flag1_lim1", value=ssdf$flag1_lim1)
        updateTextInput(session, inputId="flag1_lim2", value=ssdf$flag1_lim2)
        updateTextInput(session, inputId="flag2_lim1", value=ssdf$flag2_lim1)
        updateTextInput(session, inputId="flag2_lim2", value=ssdf$flag2_lim2)
        updateNumericInput(session, inputId="threscoef", value=ssdf$threshcoef)
        
        # now reset secondary_settings
        state$secondary_settings <- NULL
        
      }
      
    })
     
    
    #***************************************************************************
    # HIDE/DISABLE/TOGGLE BUTTONS AND PANELS ####
    
    # if "tm" is on (i.e. day of max. concentration is a parameter in the model fit),
    # then the start of the model can't be restricted, so turn off that slider button
    observe({
        toggleState("ti_limits", condition = !input$tm | input$ti_threshold_type=="constant_thresh")
    })
    
    # enable/disable "Run time series" button depending on which boxes are selected and if there are custom coordinates
    observe({
        frb <- state$fullrunboxes
        if (length(frb)==0) {
            disable("fullrun_process")
        } else if (length(frb)==1) {
            if (frb[1] == "custom" & is.null(get_polygon())) {
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
    
    # enable/disable cell size modelling options based on the selected variable
    observeEvent(input$sat_alg, {
      if (input$sat_alg!="empty_sat_alg") {
        v <- variables[[strsplit(input$sat_alg,split="_")[[1]][2]]]
        if (v$cell_model_option) {
          enable("concentration_type")
          enable("cell_size_model1")
          enable("cell_size_model2")
        } else {
          disable("concentration_type")
          disable("cell_size_model1")
          disable("cell_size_model2")
          updateRadioButtons(session, inputId="concentration_type", selected="full")
        }
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
        input$composite
    }, {
        
        hideElement(id = "hiddenPanel", anim = FALSE)
        disable("savedensplot")
        disable("savebloomfit")
        disable("savebloomparams")
        disable("saveannualstats")
        state$data_loaded <- FALSE
        
        # FOR APPLYING INDIVIDUAL SETTINGS MANUALLY
        if (is.null(state$secondary_settings)) {
          # if the region has changed, check if you need to update the sat_alg dropdown
          new_sat_algs <- sat_algs[[input$region]]
          if (!identical(new_sat_algs, state$current_sat_algs)) {
            if (state$sat_alg %in% new_sat_algs) {
              selected_sat_alg <- state$sat_alg
            } else {
              selected_sat_alg <- new_sat_algs[1]
            }
            updateSelectInput(session, inputId = "sat_alg", choices = new_sat_algs, selected = selected_sat_alg)
          } else {
            selected_sat_alg <- input$sat_alg
          }
          state$current_sat_algs <- new_sat_algs
          # if the region or sat_alg have changed, check if you need to update the year dropdown
          new_years <- years[[input$region]][[selected_sat_alg]]
          if (!identical(new_years, state$current_years)) {
            if (state$year %in% new_years) {
              selected_year <- state$year
            } else {
              selected_year <- max(new_years)
            }
            updateSelectInput(session, inputId = "year", choices = rev(new_years), selected = selected_year)
          } else {
            selected_year <- input$year
          }
          state$current_years <- new_years
          # enable/disable load button depending on whether or not data exists for these settings
          data_exists <- dexist(input$region, selected_sat_alg, selected_year)
          if (data_exists) {
            enable("load")
            state$help_load_txt <- ""
          } else {
            disable("load")
            state$help_load_txt <- "No data available for the selected options."
          }
          
        # FOR APPLYING A SETTINGS FILE
        # necessary update() functions have already been called in the applysettings code chunk,
        # so you don't need to do that here - you can just double-check that data is available
        } else {
          # reset "current" variables so it knows to change if you manually select
          # a region with different sat_algs or years
          new_sat_algs <- sat_algs[[input$region]]
          if (!identical(new_sat_algs, state$current_sat_algs)) {
            if (state$sat_alg %in% new_sat_algs) {
              selected_sat_alg <- state$sat_alg
            } else {
              selected_sat_alg <- new_sat_algs[1]
            }
            state$current_sat_algs <- new_sat_algs
          } else {
            selected_sat_alg <- state$sat_alg
          }
          new_years <- years[[input$region]][[selected_sat_alg]]
          if (!identical(new_years, state$current_years)) {state$current_years <- new_years}
          enable("load")
          state$help_load_txt <- ""
          shinyjs::click("load")
        }
        
    })
    
    output$help_load <- renderPrint({
        helpText(state$help_load_txt, width=widget_width, style=help_text_style)
    })
    
    
    #***************************************************************************
    # GET REGION ####
    
    # If region has changed, update predefined polygon choices and state variables
    observeEvent(input$region, {
      state$region <- input$region
      updateSelectInput(session, inputId = "box", choices = poly_choices[[state$region]], selected = "custom")
      updatePickerInput(session, inputId = "fullrunboxes", choices = multipoly_choices[[state$region]], selected = "custom")
      state$predefined_polys <- predefined_polys[[state$region]]
    })
    
    
    # GET POLYGON ####
    
    observeEvent(input$box,{
        if (state$latlon_method == "drawPoly") {
            # If switching from predefined to custom polygon, add the draw toolbar to the map.
            # If switching from custom to predefined polygon, remove the draw toolbar from the map.
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
        if (state$latlon_method == "typeCoords" & state$draw_programmatically) {
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
    
    
    # GET DISPLAY OPTIONS ####
    
    observeEvent({
      state$variable
      input$log_display
    }, {
      if (state$variable$log) { # possible to log the data
        if (!identical(state$log_display,input$log_display)) {
          state$log_display <- input$log_display
          if (input$log_display) {
            colscale <- state$variable$colscale_log
          } else {
            colscale <- state$variable$colscale
          }
          zlim <- range(colscale)
          updateSliderTextInput(session, inputId="zlim", choices=colscale, selected=zlim)
          state$zlim <- zlim
        }
      } else { # impossible to log the data, the option is disabled and you must make sure it's set to FALSE
        state$log_display <- FALSE
        if (input$log_display) {
          updateCheckboxInput(session, inputId="log_display", value=FALSE)
          updateSliderTextInput(session, inputId="zlim", choices=state$variable$colscale, selected=range(state$variable$colscale))
          state$zlim <- range(state$variable$colscale)
        }
      }
    })
    
    observeEvent(input$applyzlim, {
      state$zlim <- input$zlim
    })
    
    
    # GET YEAR DAY ####
    
    output$datebar <- renderPlot(pmonth) # date bar at top
    
    observeEvent(input$yearday_slide, {
        state$yearday <- input$yearday_slide
        # Get some time variables now that yearday is updated
        time_variables <- get_time_vars(composite=state$composite, year=state$year,
                                        yearday=state$yearday, dvecs=dvecs)
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
    
    
    # GET MODEL FIT VARIABLES ####
    
    observeEvent(input$fitmethod, {
        state$fitmethod <- input$fitmethod
        # Make sure tmax option starts out as "off" when fit method is changed.
        # Needs to be "off" for non-gauss methods so that ti_limits can be adjusted,
        # and need to reset it for gauss method anyway, otherwise the whole switch
        # disappears when you switch from another method to gauss method.
        updateSwitchInput(session, "tm", value = FALSE)
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
    observeEvent({
      state$variable
      input$log_chla
    }, {
      if (state$variable$log) {
        state$log_chla <- input$log_chla
      } else {
        if (input$log_chla) {
          updateCheckboxInput(session, inputId="log_chla", value=FALSE)
        } else {
          state$log_chla <- FALSE
        }
      }
    })
    
    # THRESHOLD METHOD SPECIFICALLY
    observeEvent(input$threshcoef, {
        state$threshcoef <- input$threshcoef
    })
    observeEvent(input$ti_threshold_type,{
        state$ti_threshold_type <- input$ti_threshold_type
    })
    observeEvent(input$ti_threshold_percent,{
      ittt <- input$ti_threshold_percent
      if (is.finite(ittt)) {
        if (ittt < 0.01) {
          ittt <- 0.01
          updateNumericInput(session, inputId = "ti_threshold_percent", value = 0.01)
        } else if (ittt > 90) {
          ittt <- 90
          updateNumericInput(session, inputId = "ti_threshold_percent", value = 90)
        } else {
          state$ti_threshold <- state$tt_threshold <- ittt/100
        }
      }
    })
    observeEvent(input$ti_threshold_constant,{
        ittt <- input$ti_threshold_constant
        if (is.finite(ittt)) {
            if (ittt < 0.01) {
                ittt <- 0.01
                updateNumericInput(session, inputId = "ti_threshold_constant", value = 0.01)
            } else if (ittt > 5) {
                ittt <- 5
                updateNumericInput(session, inputId = "ti_threshold_constant", value = 5)
            } else {
                state$ti_threshold_constant <- ittt
            }
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
        showModal(modalDialog(title="Gaussian fit flag descriptions",HTML(gauss_flag_popup),easyClose=TRUE,footer=modalButton("Close")))
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
        # Check if values are valid (not NA, NaN, Inf, -Inf, or flaglim1 > flaglim2),
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
    # the t_sliders are dependent on each other, so you might have to update them
    # to reflect this - only update their state$ variables once they're all valid
    observeEvent({
      input$t_range
      input$tm_limits
      input$ti_limits
    }, {
      tr1 <- input$t_range[1]
      tr2 <- input$t_range[2]
      tm1 <- input$tm_limits[1]
      tm2 <- input$tm_limits[2]
      ti1 <- input$ti_limits[1]
      ti2 <- input$ti_limits[2]
      update_t_range <- update_tm_limits <- update_ti_limits <- FALSE
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
      # check for errors in tm_limits - must be within trange
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
      # check for errors in ti_limits - must be within trange, and ti and tm limits depend on each other
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
      # if anything needs to be reset, do that here - otherwise, assign them to their state$ variables
      if (update_t_range) {
        updateSliderInput(session, inputId = 't_range', value = c(tr1, tr2))
      } else if (update_tm_limits) {
        updateSliderInput(session, inputId = 'tm_limits', value = c(tm1, tm2))
      } else if (update_ti_limits) {
        updateSliderInput(session, inputId = 'ti_limits', value = c(ti1, ti2))
      } else {
        state$t_range <- c(tr1, tr2)
        state$tm_limits <- c(tm1, tm2)
        state$ti_limits <- c(ti1, ti2)
      }
    })
    
    # Get list of column names for model fit parameters/metrics
    get_pnames <- reactive({
      pnames <- pnlist[[state$fitmethod]]
      if (state$fitmethod=="gauss") {
        pnames <- pnames[[state$bloomShape]]
        if (!state$beta) {pnames <- pnames[!grepl("beta", pnames)]}
      }
      return(pnames)
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
    
    full_data <- eventReactive(input$load, {
        
        show_modal_spinner(spin="atom", color="#112446", text=paste0("Loading data for ",input$year,"..."))
        
        showElement(id = "hiddenPanel", anim = FALSE)
        
        state$data_loaded <- TRUE
        
        # Assign main settings to reactive "state" variable
        state$sat_alg <- input$sat_alg
        state$concentration_type <- input$concentration_type
        state$cell_size_model1 <- input$cell_size_model1
        state$cell_size_model2 <- input$cell_size_model2
        state$year <- input$year
        state$composite <- as.numeric(input$composite)
        state$variable <- variables[[strsplit(input$sat_alg,split="_")[[1]][2]]]
        if (state$variable$log) {
          enable("log_display")
          enable("log_chla")
        } else {
          disable("log_display")
          disable("log_chla")
        }
        
        # Update the time-related variables now that composite and year are updated
        time_variables <- get_time_vars(composite=state$composite, year=state$year, yearday=state$yearday, dvecs=dvecs)
        state$day_label <- time_variables$day_label
        state$time_ind <- time_variables$time_ind
        
        enable("savesettings")
        
        # Load full map data
        state$sscoords <- reginfo[[state$region]]$coords
        state$binGrid <- reginfo[[state$region]]$binGrid
        state$ext <- reginfo[[state$region]]$extent
        state$ssbin <- reginfo[[state$region]]$bin
        all_data <- get_data(state$region, state$sat_alg, state$year, state$yearday, state$composite, length(state$sscoords), dvecs, state$concentration_type, state$cell_size_model1, state$cell_size_model2, state$variable$log)
        sschla <- all_data$sschla
        state$available_days <- all_data$available_days
        state$doy_vec <- all_data$doy_vec # days of the year, whether daily, 4-day, or 8-day composites
        
        if (is.null(state$secondary_settings)) {
            # Update yearday slider depending on composite length.
            # Don't update actual value in slider, or the map will update twice.
            updateSliderInput(session, inputId = "yearday_slide", step = state$composite)
            # Update full_run slider input
            tmp_years <- as.numeric(years[[state$region]][[state$sat_alg]])
            updateSliderInput(session, inputId='fullrunyears', min=min(tmp_years), max=max(tmp_years), value=range(tmp_years))
        } else {
            update_secondary_settings()
        }
        
        remove_modal_spinner()
        gc()
        return(sschla)
        
    })
    
    
    #***************************************************************************
    # MAP BASE ####
    
    # Raster data will be overlaid after this
    map_reactive <- reactive({
        ops <- state$predefined_polys
        reg <- isolate(state$region)
        ri <- reginfo[[reg]]
        # Use leaflet() here, and only include aspects of the map that won't need
        # to change dynamically unless the entire map is torn down and recreated.
        lf <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles("Esri.WorldGrayCanvas",
                             options = providerTileOptions(minZoom=4, maxZoom=12,
                                                           updateWhenZooming=FALSE,updateWhenIdle=TRUE)) %>%
            setView(lng = ri$center_lon, lat = ri$center_lat, zoom = ri$zoom_level) %>%
            # Note: need to "remove" mouse coordinates first, otherwise it gets stuck if you try
            # to reload the base map (for example, switching from Atlantic to Pacific)
            leafem::removeMouseCoordinates() %>%
            leafem::addMouseCoordinates() %>%
            addGraticule(group="Gridlines", interval=ri$gridline_interval,
                         style = list(color = "#333", weight = 0.1),
                         options = pathOptions(pointerEvents = "none", clickable = FALSE))
        # Add predefined polygons to the map
        if (length(ops)==0) {
          lf <- lf %>%
            addLayersControl(overlayGroups="Gridlines", position="topright",
                             options=layersControlOptions(collapsed=FALSE))
        } else {
          opnames <- names(ops)
          for (i in 1:length(ops)) {
            opname <- opnames[i]
            op <- ops[[i]]
            lf <- lf %>%
              addPolygons(group=opname, data = op, label = op$label,
                          stroke=TRUE, color="darkgrey", weight=2, opacity=1,
                          fill=TRUE, fillColor="white",fillOpacity=0.2,
                          labelOptions = labelOptions(
                            noHide=TRUE, textOnly=TRUE, textsize='13px', direction="center",
                            style=list('color'='white', 'text-shadow' = '0px 0px 4px #000000')))
          }
          lf <- lf %>%
            addLayersControl(overlayGroups=c("Gridlines",opnames), position="topright",
                             options=layersControlOptions(collapsed=FALSE)) %>%
            hideGroup(opnames) # on startup, hide polygons
        }
        # required to make addPmToolbar work
        lf$dependencies <- c(lf$dependencies, leafpm::pmDependencies())
        lf
    })
    
    # MAP UPDATE ####
    # based on user input
    observe({
        
        # Get the selected annual dataset
        sschla <- full_data()
        
        # Get the selected day of year
        yearday <- state$yearday
        day_label <- state$day_label
        time_ind <- state$time_ind
        
        # check if data is available in the file for the selected day
        if (yearday > isolate(state$available_days)) {
            
            disable("savedensplot")
            lfp <- leafletProxy("fullmap", session) %>%
              clearPopups() %>% clearControls() %>% clearGroup("georaster") %>%
              addControl(tags$div(tag.map.title, HTML(paste0(day_label, "<br>NO DATA AVAILABLE YET"))),
                         position = "topleft", className = "map-title")
            
        } else {
            
            # Subset chla, lat, lon, and time, and remove NA cells
            sschla_time_ind <- sschla[,time_ind]
            chla_ind <- !is.na(sschla_time_ind) & is.finite(sschla_time_ind)
            
            if (sum(chla_ind)==0) {
                
                disable("savedensplot")
                lfp <- leafletProxy("fullmap", session) %>%
                  clearPopups() %>% clearControls() %>% clearGroup("georaster") %>%
                  addControl(tags$div(tag.map.title, HTML(paste0(day_label, "<br>NO DATA"))),
                             position = "topleft", className = "map-title")
                
            } else {
                
                # Get colour scale and legend title for map
                zlim <- state$zlim
                
                # Update raster for leaflet map
                df_bins <- state$ssbin[chla_ind]
                ext <- state$ext
                binGrid <- state$binGrid
                bg_dim <- dim(binGrid)
                newmat <- rep(NA, length(binGrid))
                newmat[binGrid %in% df_bins] <- sschla_time_ind[chla_ind][match(binGrid, df_bins, nomatch=0)]
                tr <- terra::rast(ncols=bg_dim[2], nrows=bg_dim[1], xmin=ext[1], xmax=ext[2], ymin=ext[3], ymax=ext[4], vals=matrix(newmat, nrow=bg_dim[1]))
                if (state$log_display) {
                  tr <- log10(tr)
                  zlim <- log10(zlim)
                }
                tr <- terra::clamp(tr, lower=zlim[1]+1e-6, upper=zlim[2]-1e-6, values=TRUE)
                mypal <- colorNumeric(palette=map_cols, domain=zlim, na.color="#00000000")
                lt <- state$variable$map_legend_title
                
                # Update map based on choices of date
                lfp <- leafletProxy("fullmap", session) %>%
                    clearPopups() %>% clearControls() %>% clearGroup("georaster") %>%
                    addRasterImage(x=tr, group="georaster", project=TRUE, colors=mypal) %>%
                    # Label map with current year and day of year
                    addControl(tags$div(tag.map.title, HTML(day_label)), position="topleft", className="map-title")
                
                # Add color legend and adjust labels if data is logged
                if (state$log_display) {
                  lfp <- lfp %>% addLegend(position='topright', pal=mypal, values=zlim, title=lt, bins=10, opacity=1,
                                           labFormat=leaflet::labelFormat(digits=2, transform=function(x) 10^x))
                } else {
                  lfp <- lfp %>% addLegend(position='topright', pal=mypal, values=zlim, title=lt, bins=10, opacity=1)
                }
                
            }
            
        }
        
        if (state$draw_toolbar) {
            lfp <- lfp %>%
              addPmToolbar(toolbarOptions = pmToolbarOptions(drawPolygon=TRUE,drawRectangle=TRUE,
                                                             drawMarker=FALSE,drawPolyline=FALSE,drawCircle=FALSE,
                                                             editMode=TRUE,removalMode=TRUE,cutPolygon=FALSE,
                                                             position="topleft"),
                           drawOptions = pmDrawOptions(allowSelfIntersection=FALSE),
                           editOptions = pmEditOptions(allowSelfIntersection=FALSE))
        } else {
            lfp <- lfp %>% removePmToolbar(clearFeatures=TRUE)
        }
        
        gc()
        
    })
    
    # RENDER MAP (print to screen)
    output$fullmap <- renderLeaflet(map_reactive())
    
    
    #***************************************************************************
    # POLYGON FUNCTIONS ####
    
    # Get user input for new, edited, or deleted polygons
    observeEvent(input$fullmap_draw_new_feature, {
        snp <- isolate(state$newpoly)
        sep <- isolate(state$editedpoly)
        if (is.null(snp) & is.null(sep)) {
          state$newpoly <- input$fullmap_draw_new_feature
          state$help_latlon_txt <- ""
        } else {
          # a polygon already exists - delete the one you just drew and print a warning
          session$sendCustomMessage("removeleaflet", list(elid="fullmap", layerid=input$fullmap_draw_new_feature$properties$`_leaflet_id`))
          state$help_latlon_txt <- "Delete existing polygon before creating a new one."
        }
    })
    observeEvent(input$fullmap_draw_edited_features, {
        state$newpoly <- NULL
        state$editedpoly <- input$fullmap_draw_edited_features
    })
    observeEvent(input$fullmap_draw_deleted_features, {
        state$newpoly <- state$editedpoly <- NULL
        state$custom_name <- ""
        updateTextInput(session, inputId="custom_name", value="")
    })
    
    # If new polygons are created, edited, or deleted, this block of code
    # will trigger to get the coordinates of the polygon, check if it's too big,
    # and if not, send them to annual stats to update the density plot and model fit scatterplot
    draw_polygon <- reactive({
        coords <- try(unlist(state$newpoly$geometry$coordinates))
        if (is.null(coords)) {
            coords <- try(unlist(state$editedpoly$geometry$coordinates))
        }
        spdf <- NULL
        if (!is.null(coords)) {
          spdf <- make_custom_spdf(lats=coords[seq(2,length(coords), 2)],
                                   lons=coords[seq(1,length(coords), 2)],
                                   name=isolate(state$custom_name))
          bc <- check_custom_spdf_size(spdf,pixels=isolate(state$sscoords))
          state$latlon_toolarge <- bc$latlon_toolarge
          state$help_latlon_txt <- bc$help_latlon_txt
          if (state$latlon_toolarge) {spdf <- NULL}
        }
        return(spdf)
    })
    
    # Instead of getting coordinates of a polygon drawn directly on the map,
    # get coordinates entered in a box by the user.
    type_polygon <- eventReactive({
      input$createTypedPoly
      state$typed_polys_deleted
    }, {
      spdf <- NULL
      cll <- check_latlons(input$manual_lats, input$manual_lons)
      state$latlon_invalid <- cll$invalid_latlon
      state$help_latlon_txt <- cll$help_latlon_txt
      if (!state$latlon_invalid) {
        spdf <- make_custom_spdf(lats=cll$coords$lats,lons=cll$coords$lons,name=isolate(state$custom_name))
        bc <- check_custom_spdf_size(spdf,pixels=isolate(state$sscoords))
        state$latlon_toolarge <- bc$latlon_toolarge
        state$help_latlon_txt <- bc$help_latlon_txt
        if (state$latlon_toolarge) {spdf <- NULL}
      }
      return(spdf)
    })
    
    # Or read a shapefile containing a polygon.
    observeEvent(input$shapefile, {
      state$shapefile <- input$shapefile
    })
    read_shapefile_contents <- eventReactive({
      state$shapefile
      state$shapefile_polys_deleted
    }, {
      shp_file_contents <- shp_poly_list <- shp_poly_ind <- NULL
      shp_num_polys <- 0
      if (!is.null(state$shapefile)) {
        ext <- tools::file_ext(input$shapefile$name)
        msg <-  paste0("Loading ", input$shapefile$name[ext=="shp"], "...")
        show_modal_spinner(spin="atom", color="#112446", text=msg)
        mydir <- tempdir()
        on.exit(unlink(mydir))
        file <- file.path(mydir, input$shapefile$name)
        file.copy(input$shapefile$datapath, file)
        # check the extensions, try to load the file, and check file contents
        if (!("shp" %in% ext)) {
          help_latlon_txt <- "Please select a shapefile (.shp) and all corresponding files with the same name but different extensions (e.g. dbf, prj, sbx...)"
        } else {
          file_contents <- try(st_as_sf(st_transform(read_sf(file[ext=="shp"]),crs=st_crs(4326))), silent=TRUE)
          if (class(file_contents)[1]=="try-error") {
            help_latlon_txt <- "Invalid input files. Make sure all files with the same name (and different extensions, e.g. shp, dbf, prj, sbx...) are selected."
          } else {
            if (class(file_contents)[1] != "sf") {
              help_latlon_txt <- "Invalid file contents. Shapefile must contain a Simple features collection of polygon or multipolygon objects."
            } else {
              is_all_poly <- all(grepl("POLYGON",st_geometry_type(file_contents)))
              if (!is_all_poly) {
                help_latlon_txt <- "Invalid file contents. Shapefile must contain a Simple features collection of polygon or multipolygon objects ONLY."
              } else {
                file_coords <- try(st_coordinates(file_contents), silent=TRUE)
                if (class(file_coords)[1]=="try-error") {
                  help_latlon_txt <- "Invalid file contents."
                } else {
                  help_latlon_txt <- ""
                  shp_file_contents <- file_contents
                  shp_num_polys <- nrow(file_contents)
                  shp_poly_ind <- 1
                  fc <- suppressWarnings(sapply(file_contents$geometry, function(x) st_as_text(st_cast(x, to="POINT"))))
                  file_contents$geometry <- paste0(substr(fc,7,nchar(fc)),"...")
                  shp_poly_list <- apply(as.data.frame(file_contents),MARGIN=1,FUN=paste,collapse=" | ")
                }
              }
            }
          }
        }
        remove_modal_spinner()
        state$help_latlon_txt <- help_latlon_txt
      }
      return(list(sfc=shp_file_contents,spl=shp_poly_list,spi=shp_poly_ind,snp=shp_num_polys))
    })
    
    # radioButton popup that appears to select polygon from shapefile
    output$shp_radioButtons <- renderUI({
      spl <- read_shapefile_contents()$spl
      if (!is.null(spl)) {
        radioButtons(inputId="shp_poly",label="Select polygon from shapefile",choices=spl,selected=spl[1],width="100%")
      }
    })
    observeEvent(input$shp_popup, {
      showModal(modalDialog(uiOutput(outputId="shp_radioButtons"),footer=modalButton("Load polygon"),easyClose=TRUE))
    })
    output$shapefile_button <- renderUI({
      if (state$latlon_method=="loadShapefile") {
        spl <- read_shapefile_contents()$spl
        if (!is.null(spl)) {
          actionButton(inputId="shp_popup", label=HTML("Click to select polygon<br>from shapefile"))
        }
      }
    })
    
    # return the selected polygon from the shapefile
    get_shapefile_polygon <- reactive({
      lsf <- read_shapefile_contents()
      spdf <- NULL
      if (!is.null(lsf)) {
        spi <- which(lsf$spl==input$shp_poly)
        if (length(spi)>0) {
          ssfc <- lsf$sfc
          if (!is.null(ssfc)) {spdf <- ssfc[spi,]}
        }
      }
      return(spdf)
    })
    
    # help text for custom polygon if something goes wrong
    output$help_latlon <- renderUI({
      if (nchar(state$help_latlon_txt)>0) {
        helpText(state$help_latlon_txt, width=widget_width, style=error_text_style)
      }
    })
    
    # POLYGON TITLE PANEL
    output$poly_title <- renderPrint({
      if (state$data_loaded) {
          tmp_name <- ifelse(state$box=='custom', ifelse(nchar(state$custom_name)==0, "Custom polygon", state$custom_name), get_polygon()$name)
          HTML(paste0("<font style=\"font-size: 18px; color: #555555; font-weight: bold;\">",
                      state$year," ",names(regions)[state$region==regions],", ",tmp_name,"</font>"))
      }
    })
    
    # If box, latlon_method, or region have changed, reset/delete old highlighted boxes when annual_stats is triggered
    delete_polygon <- eventReactive({
        state$box
        state$latlon_method
        state$region
    }, {
        # Remove highlighted polygon on map
        leafletProxy("fullmap", session) %>% clearPopups() %>% removeShape("highlighted_box")
        # Remove drawn polygon
        snp <- isolate(state$newpoly)
        sep <- isolate(state$editedpoly)
        if (!is.null(snp)) {
            shapes <- snp$properties$`_leaflet_id`
            session$sendCustomMessage("removeleaflet", list(elid="fullmap", layerid=shapes))
        } else if (!is.null(sep)) {
            shapes <- sep$properties$`_leaflet_id`
            session$sendCustomMessage("removeleaflet", list(elid="fullmap", layerid=shapes))
        }
        state$newpoly <- state$editedpoly <- NULL
        # Remove typed lats/lons
        updateTextInput(session, inputId="manual_lats", value="")
        updateTextInput(session, inputId="manual_lons", value="")
        state$typed_polys_deleted <- state$typed_polys_deleted + 1
        # Remove shapefile polygons
        state$shapefile_polys_deleted <- state$shapefile_polys_deleted + 1
        state$shapefile <- NULL
        # Reset polygon help text
        state$help_latlon_txt <- ""
    })
    
    get_custom_polygon <- reactive({
      spdf <- NULL
      if (state$latlon_method=="drawPoly") {
        spdf <- draw_polygon()
      } else if (state$latlon_method=="typeCoords") {
        spdf <- type_polygon()
      } else if (state$latlon_method=="loadShapefile") {
        spdf <- get_shapefile_polygon()
      }
      return(spdf)
    })
    
    get_polygon <- reactive({
      # reset/erase polygons if state$box/latlon_method/region have changed
      delete_polygon()
      spdf <- NULL
      polystr <- NA
      # Retrieve Simple features collection for selected polygon
      if (state$box=="custom") {
        spdf <- get_custom_polygon()
        if (!is.null(spdf)) {polystr <- st_as_text(spdf$geometry)}
      } else {
        ppm <- ppolys_merged[[state$region]]
        spdf_ind <- ppm$poly_id==state$box
        if (sum(spdf_ind)==1) {spdf <- ppm[spdf_ind,]}
      }
      state$polystr <- polystr
      return(spdf)
    })
    
    # Add highlighted polygons to map
    observe({
      spdf <- get_polygon()
      if (!is.null(spdf) & (state$latlon_method!="drawPoly" | state$box!='custom')) {
        leafletProxy("fullmap", session) %>%
          addPolygons(layerId = "highlighted_box", data = spdf,
                      stroke = TRUE, color = "yellow", weight = 2.5, opacity = 1,
                      fill = TRUE, fillColor = "yellow", fillOpacity = 0.1)
      }
    })
    
    
    #***************************************************************************
    # ANNUAL STATS FOR BOX/POLYGON ####
    
    get_subset_data <- reactive({
      # Get most recent data and polygon, and reset other variables
      sschla <- full_data()
      spdf <- get_polygon()
      rchla <- NULL
      if (!is.null(spdf)) {
        # Extract pixels that are within the polygon
        rchla <- subset_data(spdf,state$sscoords,sschla,state$pixrange1,state$pixrange2)
      }
      gc()
      return(rchla)
    })
    
    annual_stats <- reactive({
      rchla <- get_subset_data()
      if (is.null(rchla)) {
        all_stats <- data.frame(matrix(nrow=0,ncol=18), stringsAsFactors=FALSE)
        colnames(all_stats) <- c("doy", "lower_limit", "upper_limit", "mean", "median", "stdev", "min", "max", "nobs", "percent_coverage", "lower_limit_log10", "upper_limit_log10", "mean_log10", "median_log10", "stdev_log10", "model","background","loess")
      } else {
        all_stats <- dplyr::bind_cols(data.frame(doy=state$doy_vec), get_stats(rchla=rchla, outlier=state$outlier, logvar=state$variable$log))
      }
      return(all_stats)
    })
    
    
    #***************************************************************************
    # DENSITY PLOT ####
    
    make_density_plot <- reactive({
        
        # get the most recent data
        all_stats <- annual_stats()
        day_label <- state$day_label
        time_ind <- state$time_ind
        concentration_type <- state$concentration_type
        outlier_names <- paste0(names(outliers)[outliers==state$outlier], c("","(log10)"))
        
        # CHECK IF THERE IS ENOUGH DATA
        # Reset error message to NULL, then check if it should be changed and printed instead of doing the density plot
        em <- NULL
        if (state$data_loaded) {
            if (state$yearday > state$available_days) {
                em <- paste0("No data available yet for ", day_label)
            } else if (state$box=="custom" & state$latlon_toolarge) {
                em <- paste0("Polygon is too large (max allowed pixels = ", max_pixels, ").")
            } else if (nrow(all_stats)==0) {
                if (state$box=="custom" & is.null(get_polygon())) {
                    em <- "Create your custom polygon in a region with sufficient data"
                } else {
                    em <- "No data in the selected region"
                }
            } else {
                # check if % coverage for this composite and region is high enough to create a density plot (default = 10%)
                if (all_stats$percent_coverage[time_ind] < state$percent) {
                    em <- paste0("Insufficient data, coverage < ", state$percent, "%")
                } else if (all_stats$nobs[time_ind]==1) {
                    em <- "Only one valid point selected"
                }
            }
        } else {
            em <- "Load data to begin"
        }
        
        # IF THERE IS ENOUGH DATA, PLOT IT
        p <- ggplot() + theme_bw() +
          ggtitle(paste0(ifelse(concentration_type=="model1", paste0(proper(state$cell_size_model1), " cell size: "),
                                ifelse(concentration_type=="model2", paste0(proper(state$cell_size_model2), " cell size: "), "")),
                         'Density plot of ',state$variable$name_plottitle,' for ', day_label))
        
        if (is.null(em)) {
          
            pcols <- c("#00497C","#7E0000","#00497C","#7E0000","#00497C","#7E0000")
            plins <- c("solid","solid","dashed","dashed","dotted","dotted")
            names(pcols) <- names(plins) <- c("Mean", "Mean(log10)", "Median","Median(log10)", outlier_names)
            sdf_plot <- data.frame(Stat=c(outlier_names[c(1,1)],"Mean","Median","StDev","Min","Max","N<sub>obs</sub>","% cov",outlier_names[c(2,2)],"Mean(log10)","Median(log10)","StDev(log10)"),
                                   Value=as.numeric(unlist((all_stats %>% dplyr::select(lower_limit:stdev_log10))[time_ind,])))
            if (state$log_display) {p <- p + scale_x_log10()}
            p <- p +
              geom_density(data=data.frame(x=get_subset_data()[,time_ind]), aes(x=x), fill="grey", alpha = 0.7) +
              geom_vline(data=sdf_plot %>% dplyr::filter(startsWith(Stat,"Mean") | startsWith(Stat,"Median") | startsWith(Stat,names(outliers)[outliers==state$outlier])) %>% tidyr::drop_na(),
                         aes(xintercept=Value, color=Stat, linetype=Stat), linewidth=0.8, key_glyph = "path") +
              scale_color_manual(values=pcols) +
              scale_linetype_manual(values=plins) +
              theme(legend.position="bottom",
                    legend.title=element_blank(),
                    legend.text=element_text(size=12),
                    legend.margin=margin(0,0,0,0),
                    legend.key.width = unit(15, 'mm'),
                    axis.title=element_blank(),
                    axis.text=element_text(size=12),
                    panel.border=element_rect(colour="black", fill=NA, linewidth=0.4))
            if (state$outlier=="none") {
              sdf_vals <- as.character(round(as.numeric(unlist((all_stats %>% dplyr::select(mean:percent_coverage,mean_log10:stdev_log10))[time_ind,])),2))
              sdf_print <- data.frame(Stat=c("Mean","Median","StDev","Min","Max","N<sub>obs</sub>","% cov","Mean(log10)","Median(log10)","StDev(log10)"), Value=sdf_vals)
            } else {
              sdf_vals <- as.character(round(as.numeric(unlist((all_stats %>% dplyr::select(lower_limit:stdev_log10))[time_ind,])),2))
              sdf_print <- data.frame(Stat=c(outlier_names[1],"Mean","Median","StDev","Min","Max","N<sub>obs</sub>","% cov",outlier_names[2],"Mean(log10)","Median(log10)","StDev(log10)"),
                                      Value=c(paste0(sdf_vals[1:2],collapse=","),sdf_vals[3:9],paste0(sdf_vals[10:11],collapse=","),sdf_vals[12:14]))
            }
            # make the download button visible since there are data available
            enable("savedensplot")
            
        } else {
            p <- p + geom_text_npc(aes(npcx=0.5,npcy=0.5,label=em))
            if (state$outlier=="none") {
              sdf_print <- data.frame(Stat=c("Mean","Median","StDev","Min","Max","N<sub>obs</sub>","% cov","Mean(log10)","Median(log10)","StDev(log10)"), Value=rep(NA,10))
            } else {
              sdf_print <- data.frame(Stat=c(outlier_names[1],"Mean","Median","StDev","Min","Max","N<sub>obs</sub>","% cov",outlier_names[2],"Mean(log10)","Median(log10)","StDev(log10)"), Value=rep(NA,12))
            }
            # make the download button invisible
            disable("savedensplot")
            
        }
        
        if (!state$variable$log) {sdf_print <- sdf_print %>% dplyr::filter(!grepl("log10",Stat))}
        return(list(p=p,df=sdf_print))
        
    })
    
    make_density_plot_for_export <- reactive({
      mdf <- make_density_plot()
      df <- mdf$df
      df$Stat[df$Stat=="N<sub>obs</sub>"] <- "N[obs]"
      p <- mdf$p +
        geom_table_npc(data=df, aes(npcx=0.95, npcy=0.95), label=list(df),
                       table.theme=tab_theme,table.colnames=FALSE,table.rownames=FALSE)
      return(p)
    })
    
    # RENDER DENSITY PLOT AND TABLE (print to screen)
    output$chla_hist <- renderPlot(make_density_plot()$p)
    output$chla_hist_df <- renderTable(make_density_plot()$df, rownames=FALSE, colnames=FALSE, na="", width="100%", sanitize.text.function = function(x) x)
    
    
    #***************************************************************************
    # TIME SERIES PLOT ####
    
    get_fit <- reactive({
      
      # get the most recent data
      all_stats <- annual_stats()
      composite <- state$composite
      log_chla <- state$log_chla
      dailystat <- state$dailystat
      
      # CHECK IF VALUES CAN BE FITTED
      # Reset error message to NULL, then check if it should be changed and printed instead of doing the bloom fit plot
      em <- NULL
      if (state$data_loaded) {
        if (state$box=="custom" & state$latlon_toolarge) {
          em <- paste0("Polygon is too large (max allowed pixels = ", max_pixels, ").")
        } else if (nrow(all_stats)==0) {
          if (state$box=="custom" & is.null(get_polygon())) {
            em <- "Create your custom polygon in a region with sufficient data"
          } else {
            em <- "No data in the selected region"
          }
        } else {
          first_day <- state$t_range[1]
          last_day <- state$t_range[2]
          doy_vec <- state$doy_vec
          daily_percov <- all_stats$percent_coverage
          ind_percov <- daily_percov > state$percent
          ind_dayrange <- doy_vec >= first_day & doy_vec <= min(last_day, state$available_days)
          ind_dayrange_percov <- ind_percov & ind_dayrange
          # If there is no data available for the fit after removing days outside
          # the day range and with insufficient data, print an error message instead.
          if (sum(ind_dayrange_percov)==0) {
            em <- paste0("No data available between day ", first_day, " and ",
                         last_day, " with >= ", state$percent, "% coverage")
          }
        }
      } else {
        em <- "Load data to begin"
      }
      
      # IF VALUES CAN BE FITTED, FIT THEM
      bf_data <- NULL
      if (is.null(em)) {
        if (log_chla) {
          if(dailystat == 'average'){
            chl_to_use <- log10(all_stats$mean_log10)
          } else if(dailystat == 'median'){
            chl_to_use <- log10(all_stats$median_log10)
          }
        } else {
          if(dailystat == 'average'){
            chl_to_use <- all_stats$mean
          } else if(dailystat == 'median'){
            chl_to_use <- all_stats$median
          }
        }
        sv = list(state$use_weights,state$smoothMethod,state$loessSpan,state$fitmethod,state$bloomShape,state$tm,
                  state$beta,state$tm_limits,state$ti_limits,state$threshcoef,state$flag1_lim1,state$flag1_lim2,
                  state$flag2_lim1,state$flag2_lim2,state$ti_threshold,state$tt_threshold,state$rm_bkrnd,
                  state$ti_threshold_type,state$ti_threshold_constant)
        names(sv) <- c("use_weights","smoothMethod","loessSpan","fitmethod","bloomShape","tm","beta","tm_limits",
                       "ti_limits","threshcoef","flag1_lim1","flag1_lim2","flag2_lim1","flag2_lim2","ti_threshold",
                       "tt_threshold","rm_bkrnd","ti_threshold_type","ti_threshold_constant")
        bf_data <- bf_data_calc(composite=composite,
                                chl_to_use = chl_to_use,
                                ind_dayrange_percov = ind_dayrange_percov,
                                ind_percov = ind_percov,
                                ind_dayrange = ind_dayrange,
                                daily_percov = daily_percov,
                                t_range = c(first_day, last_day),
                                log_chla = log_chla,
                                doy_vec = doy_vec,
                                variable=state$variable,
                                sv = sv)
        state$fitparams <- bf_data$fitparams # for saving to csv
      } else {
        state$fitparams <- NULL
      }
      
      return(list(em=em, bf_data=bf_data))
      
    })
    
    make_time_series_plot <- reactive({
        
        # get the most recent data
        get_fit_details <- get_fit()
        em <- get_fit_details$em
        bf_data <- get_fit_details$bf_data
        concentration_type <- state$concentration_type
        composite <- state$composite
        dailystat <- state$dailystat
        
        p <- ggplot() + theme_bw() +
          ggtitle(paste(ifelse(concentration_type=="model1", paste(proper(state$cell_size_model1),"cell size:"),
                               ifelse(concentration_type=="model2", paste(proper(state$cell_size_model2),"cell size:"), "")),
                        "Time series of", dailystat, tolower(names(composites)[composites==composite]), state$variable$name_plottitle, "for", state$year))
        if (is.null(em)) {
            df_final <- bf_data$df_final
            state$dfbloomparms <- df_final %>% dplyr::select(doy,bfy,percent_coverage) %>% tidyr::drop_na(bfy) # for nearPoints
            p <- bf_data_plot(p=p,
                              nofit_msg=bf_data$nofit_msg,
                              composite=composite,
                              dailystat=dailystat,
                              log_display=state$log_display,
                              percent=state$percent,
                              smoothMethod=state$smoothMethod,
                              fitmethod=state$fitmethod,
                              variable=state$variable,
                              fitparams=state$fitparams, # vertical model timing bars, threshold line, metrics table
                              df_final=df_final)
            # create the table of stats to render to the right of the plot (or overlaid in exported images)
            bdf <- data.frame(Stat=gsub("beta","\u03B2",colnames(state$fitparams)),Value=as.character(round(unlist(state$fitparams),3)))
            # make the download buttons visible since there are data available
            enable("saveannualstats")
            enable("savebloomfit")
            enable("savebloomparams")
        } else {
            state$dfbloomparms <- NULL
            p <- p + geom_text_npc(aes(npcx=0.5,npcy=0.5,label=em))
            pnames <- get_pnames()
            bdf <- data.frame(Stat=gsub("beta","\u03B2",pnames),Value=rep(NA,length(pnames)))
            # make the download model fit buttons invisible
            disable("savebloomfit")
            disable("savebloomparams")
            disable("saveannualstats")
        }
        
        # note: do not use "print(p)" or the bloomfit_click function will not work
        return(list(p=p,df=bdf))
        
    })
    
    make_time_series_for_export <- reactive({
      mdf <- make_time_series_plot()
      df <- mdf$df
      p <- mdf$p +
        geom_table_npc(data=df, aes(npcx=0.95, npcy=0.5), label=list(df),
                       table.theme=tab_theme,table.colnames=FALSE,table.rownames=FALSE)
      return(p)
    })
    
    
    # RENDER TIME SERIES PLOT AND TABLE (print to screen)
    output$bloomfit <- renderPlot({
      ptclick <- "** Double-click the center of a point and scroll up to see the map for that composite image"
      make_time_series_plot()$p + geom_text_npc(aes(npcx=0.01,npcy=0.98,label=ptclick),size=4,fontface="bold")
    })
    output$bloomfit_df <- renderTable({
      df <- make_time_series_plot()$df
      df$Stat <- gsub("\\[","<sub>",df$Stat)
      df$Stat <- gsub("\\]","</sub>",df$Stat)
      df
    }, rownames=FALSE, colnames=FALSE, na="", spacing="xs", width="100%", sanitize.text.function = function(x) x)
    
    
    # TIME SERIES POINT CLICK
    observeEvent(input$bloomfit_click, {
        if (!is.null(state$dfbloomparms)) {
            npyday <- nearPoints(state$dfbloomparms %>% dplyr::rename(x=doy,y=bfy), coordinfo = input$bloomfit_click, xvar = "x", yvar = "y")$x
            updateSliderInput(session, inputId = 'yearday_slide', value = npyday)
        }
    })
    
    
    #***************************************************************************
    # RUN FULL TIME SERIES ####
    
    # Take a vector of years selected by the user, and compute annual statistics
    # and model fits for each, using the current settings
    observeEvent(input$fullrun_process, {
        
        d <- reactiveValuesToList(isolate(state))
        regs <- isolate(state$fullrunboxes)
        variable <- isolate(state$variable)
        year_bounds <- isolate(input$fullrunyears)
        year_list <- (year_bounds[1]):(year_bounds[2])
        ppm <- ppolys_merged[[d$region]]
        polygon_list <- ppm[ppm$poly_id %in% regs,]
        custom_poly <- get_custom_polygon()
        if ("custom" %in% regs & !is.null(custom_poly)) {
          polygon_list <- dplyr::bind_rows(polygon_list,custom_poly)
        }
        # get column names for parameter table
        pnames <- get_pnames()
        
        # grey out the screen while processing, and show progress bar
        show_modal_progress_line(text = paste0("Computing fits for ", year_list[1], "..."))
        
        # create output subfolder and zip filename
        d$year <- year_bounds
        ostr_names <- c("sat_alg","region","year","composite","log_chla","fitmethod",
                        "concentration_type","cell_size_model1","cell_size_model2")
        ostr <- output_str(d=d[ostr_names],custom_end=NULL)
        output_dir <- file.path(tempdir(), ostr)
        dir.create(output_dir)
        fname <- paste0(ostr,".zip")
        
        # collect parameters and metrics for the selected polygons/years
        steps <- 100/length(year_list)
        progress_updates <- round(seq(steps[1], 100, by=steps),1)
        all_params <- list()
        for (x in 1:length(year_list)) {
            all_params[[x]] <- full_run(
                d = d,
                year = year_list[x],
                sscoords = state$sscoords,
                polygon_list = polygon_list,
                pnames = pnames,
                dvecs = dvecs,
                variable = variable,
                dir_name = output_dir,
                fullrunoutput_png = isolate(input$fullrunoutput_png),
                tab_theme = tab_theme)
            # update progress bar
            if (x==length(year_list)) {update_text <- "Zipping output files..."
            } else {update_text <- paste0("Computing fits for ", year_list[x+1], "...")}
            update_modal_progress(value = (progress_updates[x]/100), text = update_text)
            gc()
        }
        
        # save model metrics and settings
        write.csv(all_params %>% do.call(what=dplyr::bind_rows) %>% dplyr::arrange(., Region, Year),
                  file=file.path(output_dir, "model_fit_params.csv"),
                  quote=FALSE, na=" ", row.names=FALSE)
        write.table(format_settings_to_save(all_inputs=reactiveValuesToList(isolate(input)),
                                            custom_name=d$custom_name,
                                            polystr=d$polystr,
                                            regions=regions,
                                            sat_algs=sat_algs),
                    file=file.path(output_dir, "settings.txt"), row.names=FALSE, na="NA", sep="\\")
        
        gc()
        
        # zip files up to be downloaded
        # j flag prevents files from being sorted into subdirectories inside the zip file (the other flags are defaults)
        zip(zipfile=file.path(output_dir, fname), files=list.files(output_dir, full.names=TRUE), flags="-r9Xj")
        
        # remove progress bar and return to normal screen
        remove_modal_progress()
        
        state$fullrun_outputdir <- output_dir
        state$fullrun_fname <- fname
        
        enable("fullrun_download")
        
    })
    
    output$fullrun_fname <- renderPrint({
        if (!is.null(state$fullrun_fname)) {
            helpText(HTML(paste0("File ready for download:<br>", gsub("_", "_ ", state$fullrun_fname))),
                     width = widget_width, style = help_text_style)
        }
    })
    
    
    #***************************************************************************
    # DOWNLOAD OUTPUT ####
    # Density plot, model fit plot, annual stats, model parameters, and settings.
    # Downloads to browser's default downloads folder.
    # Note: Wrapping filename functions in another function() is necessary to force it to
    #       re-evaluate the expression at download time (to get a new box name, for example)
    
    # Download the results from "fullrun_process" (.zip)
    output$fullrun_download <- downloadHandler(
        filename <- function() isolate(state$fullrun_fname),
        content <- function(file) file.copy(file.path(isolate(state$fullrun_outputdir), isolate(state$fullrun_fname)), file),
        contentType = "application/zip"
    )
    
    # SAVE DENSITY PLOT (.png)
    output$savedensplot <- downloadHandler(
        filename <- function() output_str(d=isolate(state),custom_end="density_plot.png"),
        content <- function(file) ggsave(file=file, plot=isolate(make_density_plot_for_export()), width=13, height=5.5, units="in")
    )
    
    # SAVE MODEL FIT PLOT (.png)
    output$savebloomfit <- downloadHandler(
        filename <- function() output_str(d=isolate(state),custom_end="model_fit.png"),
        content <- function(file) ggsave(file=file, plot=isolate(make_time_series_for_export()), width=13, height=5.5, units="in")
    )
    
    # SAVE ANNUAL STATS (.csv)
    output$saveannualstats <- downloadHandler(
        filename <- function() output_str(d=isolate(state),custom_end="annual_stats.csv"),
        content <- function(file) write.csv(isolate(dplyr::full_join(annual_stats(),get_fit()$bf_data$df_final[,c("doy","model","background","loess")], by="doy")), file=file, quote=FALSE, na=" ", row.names=FALSE)
    )
    
    # SAVE MODEL FIT PARAMETERS (.csv)
    output$savebloomparams <- downloadHandler(
        filename <- function() output_str(d=isolate(state),custom_end="model_parameters.csv"),
        content <- function(file) write.csv(isolate(state$fitparams), file=file, quote=FALSE, na=" ", row.names=FALSE)
    )
    
    # SAVE CURRENT SETTINGS (.txt)
    output$savesettings <- downloadHandler(
        filename <- function() output_str(d=isolate(state),custom_end="settings.txt"),
        content <- function(file) write.table(format_settings_to_save(all_inputs=reactiveValuesToList(isolate(input)),
                                                                      custom_name=isolate(state$custom_name),
                                                                      polystr=isolate(state$polystr),
                                                                      regions=regions,
                                                                      sat_algs=sat_algs),
                                              file=file, row.names=FALSE, na="NA", sep="\\")
    )
    
}

# RUN APPLICATION ####
shinyApp(ui = ui, server = server)
