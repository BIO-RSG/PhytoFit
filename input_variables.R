# variable options in the sidebar

sensors <- c("MODIS 4km" = "modis",
             "VIIRS 4km" = "viirs",
             "SeaWiFS 4km" = "seawifs",
             "MODIS 1km" = "modis1km")

regions <- c("Atlantic"="atlantic",
             "Pacific"="pacific")

algorithms <- c("OCx (global, band ratio)"="ocx",
                "POLY4 (regional, band ratio)"="poly4",
                "GSM_GS (regional, semi-analytical)"="gsmgs",
                "EOF (regional, empirical)"="eof")

concentration_types <- list("Full chlorophyll-a concentration"="full",
                            "Small/Large cell concentrations"="model1",
                            "Small/Medium/Large cell concentrations"="model2")
cell_sizes_model1 <- list("Small"="small",
                          "Large"="large")
cell_sizes_model2 <- list("Small"="small",
                          "Medium"="medium",
                          "Large"="large")

# years with available data for each sensor
years <- list("modis"=2003:2021,
              "viirs"=2012:2021,
              "seawifs"=1997:2010,
              "modis1km"=2003:2021)
for (i in 1:length(years)) {names(years[[i]]) <- years[[i]]}
default_years <- years[["modis"]]

intervals <- c("Daily"="daily",
               "Weekly"="weekly")

polygonChoices <- list("atlantic"=c("custom", poly_ID[["atlantic"]]),
                       "pacific"=c("custom", poly_ID[["pacific"]]))
names(polygonChoices[["atlantic"]]) <- c("Custom polygon", full_names[["atlantic"]])
names(polygonChoices[["pacific"]]) <- c("Custom polygon", full_names[["pacific"]])

latlon_methods <- c("Draw polygon on map" = "drawPoly",
                    "Type coordinates" = "typeCoords",
                    "Load shapefile" = "loadShapefile")

outliers <- c('None' = 'none',
              '+/- 2 SD' = 'sd2',
              '+/- 3 SD' = 'sd3',
              '1.5 IQR' = 'iqr15')

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

multiPolygonChoices <- list("atlantic"=c("custom", poly_ID[["atlantic"]]),
                            "pacific"=c("custom", poly_ID[["pacific"]]))
names(multiPolygonChoices[["atlantic"]]) <- c("Custom", poly_ID[["atlantic"]])
names(multiPolygonChoices[["pacific"]]) <- c("Custom", poly_ID[["pacific"]])

# bloom fit table parameter names, depending on fitmethod, bloomShape, beta (code \u03B2 to get the symbol)
pnlist <- list("gauss"=list("symmetric"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                          "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags",
                                          "B0", "h", "sigma", "beta", "failure_code", "RMSE"),
                            "asymmetric"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                                           "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags",
                                           "B0[left]", "h[left]", "sigma[left]", "beta[left]",
                                           "B0[right]", "h[right]", "sigma[right]", "beta[right]", "failure_code", "RMSE")),
               "roc"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]", "Magnitude", "Amplitude"),
               "thresh"=c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]", "Magnitude", "Amplitude", "Threshold"))


#*******************************************************************************
# Variables in settings.csv file (each vector must be in the same order)

# inputId for each widget to save in the settings
# (note that polylon and polylat will be manually added at the end because they are actually part of the "state" reactive list, not "input")
input_ids_to_save <- c("satellite", "region", "algorithm", "concentration_type", "cell_size_model1", "cell_size_model2", "year", "interval", "log_chla",
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
input_ids_variable_type <- c(2,2,2,2,2,2,1,2,3,1,1,2,2,2,2,2,2,2,1,1,1,1,2,1,3,3,3,3,2,2,2,2,1,3,3,1,2,2,2,1,1)

# types of widgets used for each input (need this to update them properly)
# selectInput=1, sliderInput=2, numericInput=3, textInput=4, radioButtons=5,
# checkboxInput=6, switchInput=7, pickerInput=8, radioGroupButtons=9
input_ids_widget_type <- c(1,1,1,5,9,9,1,1,7,2,3,1,1,4,4,1,1,1,3,2,2,2,5,3,7,7,7,6,4,4,4,4,3,6,6,2,8,1,0,0,0)

# longer description of each inputId
input_ids_description <- c("Sensor", "Region (NW Atlantic or NE Pacific)", "Chlorophyll-a Algorithm",
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
