# Stephanie.Clay@dfo-mpo.gc.ca
# 2025-03-11

# If you have multiple settings files, use this script to run them and save the annual_stats csv, model_parameters csv, and settings.txt files for each fit to a new folder.

# This was created specifically to rerun verified_fits manual fits using the same settings files after discovering an issue with the code. The code below is from chunks of PhytoFit code in app.R

rm(list=ls())
library(shiny)
library(ggpp)
library(fst)            # for speedier data file loading
library(minpack.lm)     # to use nlsLM for gaussian fit
library(sf)             # to make polygons into simple feature collections (sfc)
library(sp)             # convert sfc to spatial to use sp::over() to extract pixels in polygons
library(dplyr)          # for formatting data tables
library(stringr)        # for str_pad
# other required packages: quantreg, fs (functions called using :: notation)
source("scripts/rateOfChange.R")        # rate of change (ROC) function for model fit
source("scripts/threshold.R")           # threshold function for model fit
source("scripts/gaussFit.R")            # gaussian function for model fit
source("scripts/full_run.R")            # contains function to run full time series with current settings
source("scripts/functions.R")           # extra functions
source("scripts/00_input_variables.R")  # load pre-defined variables


# LIST OF SETTINGS FILES
settings_files <- list.files("verified_fits/occci/labrador_sea_fall/manual_fits", recursive=TRUE, full.names=TRUE)
settings_files <- settings_files[endsWith(settings_files,".txt")]

# OUTPUT FOLDER
output_folder <- "verified_fits/occci/labrador_sea_fall/manual_fits/new"



#*******************************************************************************

dir.create(output_folder, showWarnings=FALSE)


for (setfile in settings_files) {
  
  cat(basename(setfile),"\n")
  
  # load and format settings
  file_contents <- try(read.table(setfile, header = TRUE, sep="\\"), silent=TRUE)
  settings <- format_settings_to_load(file_contents)
  ti_threshold <- settings$ti_threshold_percent/100
  tt_threshold <- settings$ti_threshold_percent/100
  settings$day_label <- get_time_vars(settings$composite, settings$year, 1, dvecs)$day_label
  settings$pixrange1 <- ifelse(length(settings$pixrange1)==0, -Inf, settings$pixrange1)
  settings$pixrange2 <- ifelse(length(settings$pixrange2)==0, Inf, settings$pixrange2)
  
  # get variables based on settings
  variable <- variables[[strsplit(settings$sat_alg,split="_")[[1]][2]]]
  spdf <- NULL
  ppm <- ppolys_merged[[settings$region]]
  spdf_ind <- ppm$poly_id==settings$box
  if (sum(spdf_ind)==1) {spdf <- ppm[spdf_ind,]}
  
  # load full map data
  sscoords <- reginfo[[settings$region]]$coords
  binGrid <- reginfo[[settings$region]]$binGrid
  ext <- reginfo[[settings$region]]$extent
  ssbin <- reginfo[[settings$region]]$bin
  all_data <- get_data(settings$region, settings$sat_alg, settings$year, settings$composite, length(sscoords), dvecs, settings$concentration_type, settings$cell_size_model1, settings$cell_size_model2, variable$log)
  sschla <- all_data$sschla
  available_days <- all_data$available_days
  doy_vec <- all_data$doy_vec # days of the year, whether daily, 4-day, or 8-day composites
  
  # extract data within current polygon
  rchla <- NULL
  if (!is.null(spdf)) {rchla <- subset_data(spdf,sscoords,sschla,settings$pixrange1,settings$pixrange2)}
  
  # get the annual stats
  if (is.null(rchla)) {
    all_stats <- data.frame(matrix(nrow=0,ncol=18), stringsAsFactors=FALSE)
    colnames(all_stats) <- c("doy", "lower_limit", "upper_limit", "mean", "median", "stdev", "min", "max", "nobs", "percent_coverage", "lower_limit_log10", "upper_limit_log10", "mean_log10", "median_log10", "stdev_log10", "model","background","loess")
  } else {
    all_stats <- dplyr::bind_cols(data.frame(doy=doy_vec), get_stats(rchla=rchla, outlier=settings$outlier, logvar=variable$log))
  }
  
  # CHECK IF VALUES CAN BE FITTED
  # Reset error message to NULL, then check if it should be changed and printed instead of doing the bloom fit plot
  em <- NULL
  if (nrow(all_stats)==0) {
    em <- "No data in the selected region"
  } else {
    first_day <- settings$t_range[1]
    last_day <- settings$t_range[2]
    daily_percov <- all_stats$percent_coverage
    ind_percov <- daily_percov > settings$percent
    ind_dayrange <- doy_vec >= first_day & doy_vec <= min(last_day, available_days)
    ind_dayrange_percov <- ind_percov & ind_dayrange
    # If there is no data available for the fit after removing days outside
    # the day range and with insufficient data, print an error message instead.
    if (sum(ind_dayrange_percov)==0) {
      em <- paste0("No data available between day ", first_day, " and ",
                   last_day, " with >= ", settings$percent, "% coverage")
    }
  }
  
  # IF VALUES CAN BE FITTED, FIT THEM
  bf_data <- NULL
  if (is.null(em)) {
    if (settings$log_chla) {
      if(settings$dailystat == 'average'){
        chl_to_use <- log10(all_stats$mean_log10)
      } else if(settings$dailystat == 'median'){
        chl_to_use <- log10(all_stats$median_log10)
      }
    } else {
      if(settings$dailystat == 'average'){
        chl_to_use <- all_stats$mean
      } else if(settings$dailystat == 'median'){
        chl_to_use <- all_stats$median
      }
    }
    sv = list(settings$use_weights,settings$smoothMethod,settings$loessSpan,settings$fitmethod,settings$bloomShape,settings$tm,
              settings$beta,settings$tm_limits,settings$ti_limits,settings$threshcoef,settings$flag1_lim1,settings$flag1_lim2,
              settings$flag2_lim1,settings$flag2_lim2,ti_threshold,tt_threshold,settings$rm_bkrnd,
              settings$ti_threshold_type,settings$ti_threshold_constant)
    names(sv) <- c("use_weights","smoothMethod","loessSpan","fitmethod","bloomShape","tm","beta","tm_limits",
                   "ti_limits","threshcoef","flag1_lim1","flag1_lim2","flag2_lim1","flag2_lim2","ti_threshold",
                   "tt_threshold","rm_bkrnd","ti_threshold_type","ti_threshold_constant")
    bf_data <- bf_data_calc(composite=settings$composite,
                            chl_to_use = chl_to_use,
                            ind_dayrange_percov = ind_dayrange_percov,
                            ind_percov = ind_percov,
                            ind_dayrange = ind_dayrange,
                            daily_percov = daily_percov,
                            t_range = c(first_day, last_day),
                            log_chla = settings$log_chla,
                            doy_vec = doy_vec,
                            variable=variable,
                            sv = sv)
    fitparams <- bf_data$fitparams # for saving to csv
  } else {
    fitparams <- NULL
  }
  
  # save settings files
  write.table(format_settings_to_save(all_inputs=settings,
                                      custom_name=settings$custom_name,
                                      polystr=settings$polystr,
                                      regions=regions,
                                      sat_algs=sat_algs),
              file=file.path(output_folder,output_str(d=settings,custom_end="settings.txt")),
              row.names=FALSE, na="NA", sep="\\")
  
  # save annual stats csv
  write.csv(dplyr::full_join(all_stats, bf_data$df_final[,c("doy","model","background","loess")], by="doy"),
            file=file.path(output_folder,output_str(d=settings,custom_end="annual_stats.csv")),
            quote=FALSE, na=" ", row.names=FALSE)
  
  # save model parameters csv
  write.csv(fitparams,
            file=file.path(output_folder,output_str(d=settings,custom_end="model_parameters.csv")),
            quote=FALSE, na=" ", row.names=FALSE)
  
}

