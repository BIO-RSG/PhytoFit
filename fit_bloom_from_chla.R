# Stephanie.Clay@dfo-mpo.gc.ca
# 6 Aug 2021

# Given a csv file with chlorophyll data, fit a bloom using the same settings from PhytoFit.

# Notes:
#     - if there are fewer than 3 days of data, this does not attempt a fit
#     - day 366 is removed from leap years
#     - the "fitted_chl" and "fitted_bkrnd_chl" columns in the output data csv (for fitmethod=="gauss") only span the range of selected days to use in the fit (rather than the entire year)

# To install oceancolouR package:
# remotes::install_github("BIO-RSG/oceancolouR", build_vignettes = TRUE)

# library(lubridate) # for converting month/day to day of year
library(dplyr)
library(oceancolouR) # see note about installation above
library(minpack.lm)

# for plotting (see "PLOT" section at end of script, comment it out if you don't want to see a plot of the fitted data/metrics)
library(ggplot2)
library(patchwork)

# required R scripts from the PhytoFit repository
source("functions.R")
source("gaussFit.R")
source("threshold.R")
source("rateOfChange.R")


#*******************************************************************************
# VARIABLES TO SET ####

# this must contain columns year, doy, chl
# (doy = day of year)
input_file <- "fit_bloom_from_chla_example.csv"

# this is the prefix for the output files
output_file <- "fit_bloom_from_chla_example"
# these are the files that will be created using this prefix:
#       output_file_bloom_metrics_YYYY-MM-DD-HHMMSS.csv
#       output_file_data_YYYY-MM-DD-HHMMSS.csv
#       output_file_settings_YYYY-MM-DD-HHMMSS.csv (list of the settings used for this fit)
# YYYY-MM-DD-HHMMSS = year month, day, hour, minute, second of file creation
# WARNING: the settings file created from this script can NOT be used in the PhytoFit app

# calculate bloom fits for these years (if there is enough data in the input file)
years <- 2003:2021

concentration_type <- "full" # options: full, model1 (small/large), or model2 (small/medium/large)
cell_size_model1 <- "small" # (only used if concentration_type="model1") options: small, large
cell_size_model2 <- "small" # (only used if concentration_type="model2") options: small, medium, large
interval <- "daily" # options: daily, weekly
log_chla <- TRUE
fitmethod <- "gauss" # options: gauss, roc, thresh
bloomShape <- "symmetric" # options: symmetric, asymmetric
smoothMethod <- "nofit" # options: nofit, loess ("nofit" means the points are not smoothed with loess before fitting a gaussian curve or calculating the metrics)
loessSpan <- 0.3
t_range <- c(31,274)
tm_limits <- c(91,181)
ti_limits <- c(60,151) # WARNING: this will not be used if tm=TRUE

# options for fitmethod="gauss" only
ti_threshold_type <- "percent_thresh" # options: percent_thresh, constant_thresh
ti_threshold_constant <- 0.1
tm <- FALSE
beta <- FALSE
use_weights <- FALSE
rm_bkrnd <- TRUE # this affects the way magnitude/amplitude are calculated
# these flag limits don't change the fit, only the result that will trigger a warning about a potential bad fit
flag1_lim1 <- 0.75
flag1_lim2 <- 1.25
flag2_lim1 <- 0.85
flag2_lim2 <- 1.15

# option for fitmethod="thresh" only
threshcoef <- 1.05


# read input, convert month/day to day of year, and select year, doy, and chl columns
df <- read.csv(input_file) %>%
  # dplyr::mutate(doy=yday(as_date(paste(year,month,day),format="%Y %m %d"))) %>%
  dplyr::select(year, doy, chl)


#*******************************************************************************
# BLOOM FIT CALCULATIONS ####

# these are not an option in PhytoFit yet, so they must be hard-coded
ti_threshold <- 0.2
tt_threshold <- 0.2

# make sure missing days have rows of NA
df <- dplyr::left_join(data.frame(year=rep(sort(unique(df$year)), each=365),
                                  doy=rep(1:365,length(unique(df$year))),
                                  stringsAsFactors = FALSE),
                       df %>% dplyr::filter(doy<366),
                       by=c("year","doy")) %>%
  dplyr::arrange(year, doy)

# collect output in these lists
bloom_fits <- list()
all_data <- list()

for (i in 1:length(years)) {
  
  year <- years[i]
  
  # subset data by year
  tmp_df <- df %>% dplyr::filter(year==!!year)
  
  # if there is not enough data for this year, skip it
  if (nrow(tmp_df)<3) {next}
  
  # get the number of "available days", i.e. the last day of the year with valid data
  available_days <- max(tmp_df[!is.na(tmp_df$chl),"doy"])
  tmp_df <- tmp_df %>% dplyr::filter(doy <= available_days)
  
  # get the chlorophyll data from the input dataframe
  chl <- tmp_df$chl
  
  # transform the data to small/medium/large cell size concentrations, if concentration_type is not "full"
  if (concentration_type=="model1") {
    chl <- as.numeric(phyto_cellsize_model1(matrix(chl,nrow=1), cell_size_model1))
  } else if (concentration_type=="model2") {
    chl <- as.numeric(phyto_cellsize_model2(matrix(chl,nrow=1), cell_size_model2))
  }
  
  # get a vector of days depending on whether "daily" or "weekly" is selected, convert chl to weekly values if "weekly" is selected, and adjust "available_days" if necessary
  if (interval=="daily") {
    doy_vec <- 1:available_days
  } else if (interval=="weekly") {
    doy_week_start <- as.integer(8*(0:45)+1) # note: this is the same for leap years, using NASA's system
    doy_week_end <- c(doy_week_start[2:length(doy_week_start)] - 1, 365)
    # make a list, where each element is a vector of days of the year for that week
    doys_per_week <- lapply(1:length(doy_week_start), function(i) {doy_week_start[i]:doy_week_end[i]})
    # get a vector of week start days, and a vector of week end days
    week_ind <- which(sapply(1:length(doys_per_week), function(i) {available_days %in% doys_per_week[[i]]}))
    sdoy_vec <- doy_week_start[1:week_ind]
    edoy_vec <- doy_week_end[1:week_ind]
    # subset to the weeks that are available in the input data
    doys_per_week_sub <- doys_per_week[1:week_ind]
    last_week <- tail(doys_per_week_sub,1)[[1]]
    doys_per_week_sub[length(doys_per_week_sub)][[1]] <- last_week[last_week <= available_days]
    # update available days to go to the end of the week that available_days falls into
    available_days <- edoy_vec[length(edoy_vec)]
    # convert daily data to weekly data
    chl <- sapply(1:length(doys_per_week_sub), function(i) mean(chl[doys_per_week_sub[[i]]],na.rm=TRUE))
    doy_vec <- sdoy_vec
  }
  
  # log chlorophyll, if selected
  if (log_chla) {chl <- log10(chl)}
  
  # check if there is still enough good data
  if (sum(!is.na(chl))<3) {next}
  
  # create vectors of valid indices based on selected range of days and percent coverage to use in bloom fit
  daily_percov <- 100 * as.numeric(!is.na(chl))
  ind_percov <- daily_percov > 0
  ind_dayrange <- doy_vec >= t_range[1] & doy_vec <= min(t_range[2], available_days)
  ind_dayrange_percov <- ind_percov & ind_dayrange
  
  # subset day of year vectors based on valid indices
  ydays_percov <- doy_vec[ind_percov] # all days with high enough percent coverage
  ydays_dayrange <- doy_vec[ind_dayrange]
  ydays_dayrange_percov <- doy_vec[ind_dayrange_percov] # subset of days used for fit
  
  # subset chlorophyll vectors based on valid indices
  #   "chl_percov" is plotted as the points in the scatterplot, sized by percent coverage
  #   "chl_dayrange_percov" is used for the fit (all chl within the day range, with sufficient percent coverage)
  chl_dayrange_percov <- chl[ind_dayrange_percov]
  chl_percov <- chl[ind_percov]
  
  # create final day/chlorophyll vectors for the fit, smoothed or not
  t <- ydays_dayrange_percov
  if (smoothMethod == 'loess'){
    mod <- try(loess(chl_dayrange_percov ~ ydays_dayrange_percov, span = loessSpan, degree = 2), silent=TRUE)
    bad_loess <- class(mod)=="try-error" | is.null(mod)
    if (bad_loess) {y <- chl_dayrange_percov
    } else {y <- fitted(mod)}
  } else if (smoothMethod == 'nofit'){
    y <- chl_dayrange_percov
  }
  
  
  if (fitmethod == 'gauss') {
    
    if (use_weights) {weights <- daily_percov[ind_dayrange_percov]
    } else {weights <- rep(1,length(chl_dayrange_percov))}
    
    if (tm) {tmp_ti_lim <- c(1,365)
    } else {tmp_ti_lim <- ti_limits}
    
    gauss_res <- gaussFit(t = t, y = y, w = weights,
                          bloomShape = bloomShape,
                          tm = tm, beta = beta,
                          tm_limits = tm_limits,
                          ti_limits = tmp_ti_lim,
                          t_range = t_range,
                          log_chla = log_chla,
                          interval = interval,
                          flag1_lim1 = flag1_lim1,
                          flag1_lim2 = flag1_lim2,
                          flag2_lim1 = flag2_lim1,
                          flag2_lim2 = flag2_lim2,
                          ti_threshold = ti_threshold,
                          tt_threshold = tt_threshold,
                          ydays_dayrange = ydays_dayrange,
                          rm_bkrnd = rm_bkrnd,
                          ti_threshold_type = ti_threshold_type,
                          ti_threshold_constant = ti_threshold_constant)
    
    # collect parameters from fit, and round the days
    bf_results <- gauss_res$values
    bf_results[,3:6] <- round(as.numeric(bf_results[,3:6]))
    
    # calculate RMSE and retrieve fitted vectors
    # for the fitted vector, you use everything in the selected day range, even 
    # though some of those points were not used in the fit because they were NA
    # or had insufficient coverage
    if (is.null(gauss_res$fit)) {
      bf_results$RMSE <- NA
      yfit <- rep(NA, sum(ind_dayrange))
      ybkrnd <- rep(NA, sum(ind_dayrange))
    } else {
      bf_results$RMSE <- ifelse(log_chla,
                                rmse(chl_dayrange_percov, predict(gauss_res$fit)),
                                rmse(log10(chl_dayrange_percov), log10(predict(gauss_res$fit))))
      yfit <- gauss_res$yfit
      ybkrnd <- gauss_res$ybkrnd
    }
    
    # transform fitted data and chl back to linear space if necessary
    if (log_chla) {
      yfit <- 10^yfit
      ybkrnd <- 10^ybkrnd
      chl <- 10^chl
    }
    
    # gather fitted data in an output dataframe
    new_df <- data.frame(year=year,
                         doy=ydays_dayrange,
                         fitted_chl=yfit,
                         fitted_bkrnd_chl=ybkrnd,
                         stringsAsFactors = FALSE)
    # add data and to output list
    all_data[[i]] <- dplyr::left_join(data.frame(year=year, doy=doy_vec, chl=chl),
                                      new_df,
                                      by=c("year","doy")) %>%
      dplyr::filter(is.finite(chl) & !is.na(chl) & !is.nan(chl))
    
  } else if (fitmethod=="roc" | fitmethod=="thresh") {
    
    if (fitmethod == 'roc') {
      bf_results <- rateOfChange(y = y, t = t,
                                 yall = chl_percov, tall = ydays_percov,
                                 bloomShape = bloomShape,
                                 tm_limits = tm_limits,
                                 ti_limits = ti_limits)
    } else if (fitmethod == "thresh") {
      bf_results <- threshold(t = t, y = y, 
                              tall = ydays_percov, yall = chl_percov,
                              threshcoef = threshcoef, 
                              bloomShape = bloomShape,
                              tm_limits = tm_limits,
                              ti_limits = ti_limits,
                              log_chla = log_chla)
    }
    
    # collect parameters from "rate of change" or "threshold" fit, and round the days
    bf_results <- bf_results$values
    bf_results[,3:6] <- round(as.numeric(bf_results[,3:6]))
    
    # transform chl back to linear space if necessary
    if (log_chla) {
      chl <- 10^chl
    }
    
    # add data to output list
    all_data[[i]] <- data.frame(year=year, doy=doy_vec, chl=chl) %>%
      dplyr::filter(is.finite(chl) & !is.na(chl) & !is.nan(chl))
    
  }
  
  # add bloom fit metrics to output list
  bloom_fits[[i]] <- dplyr::bind_cols(data.frame(year=year, stringsAsFactors=FALSE), bf_results)
  
}


#*******************************************************************************
# WRITE OUTPUT ####

file_creation_time <- format(Sys.time(), "%Y-%m-%d-%H%M%S")

# join output from each year into a final dataframe, then write to csv
all_data <- do.call(rbind, all_data)
write.csv(all_data,
          file=paste0(output_file, "_data_", file_creation_time, ".csv"),
          quote=FALSE,
          row.names=FALSE)
bloom_fits <- do.call(rbind, bloom_fits)
write.csv(bloom_fits,
          file=paste0(output_file, "_bloom_metrics_", file_creation_time, ".csv"),
          quote=FALSE,
          row.names=FALSE)

# create settings file for output
settings <- c(input_file, paste0(range(years),collapse="-"), concentration_type,
              cell_size_model1, cell_size_model2, interval, log_chla, fitmethod,
              bloomShape, smoothMethod, loessSpan, paste0(range(t_range),collapse="-"),
              paste0(range(tm_limits),collapse="-"), paste0(range(ti_limits),collapse="-"),
              ti_threshold_type, ti_threshold_constant, tm, beta, use_weights,
              rm_bkrnd, flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2, threshcoef)
settings <- data.frame(matrix(settings, ncol=1))
row.names(settings) <- c("input_file", "year_range", "concentration_type",
                         "cell_size_model1", "cell_size_model2", "interval", "log_chla",
                         "fitmethod", "bloomShape", "smoothMethod", "loessSpan",
                         "t_range", "tm_limits", "ti_limits", "ti_threshold_type",
                         "ti_threshold_constant", "tm", "beta", "use_weights",
                         "rm_bkrnd", "flag1_lim1", "flag1_lim2", "flag2_lim1",
                         "flag2_lim2", "threshcoef")
write.table(settings,
            file=paste0(output_file, "_settings_", file_creation_time, ".csv"),
            quote=FALSE,
            col.names=FALSE,
            sep=",")



#*******************************************************************************
# PLOT ####

suppressWarnings({ # ignore warnings from ggplot about missing data points

years_to_plot <- sort(unique(all_data$year))
plots <- list()

for (i in 1:length(years_to_plot)) {
  
  year_to_plot <- years_to_plot[i]
  tmp_data <- all_data %>% dplyr::filter(year==year_to_plot)
  
  p <- ggplot(tmp_data) +
    geom_point(aes(x=doy, y=chl), alpha=0.6) +
    geom_vline(xintercept=as.numeric(bloom_fits[i,c("t[start]","t[max]","t[end]")]),
               linetype="dashed") +
    theme_bw() +
    scale_y_log10() +
    scale_x_continuous(limits=c(1,365), expand=c(0,0)) +
    ggtitle(year_to_plot) +
    labs(x="Day of year", y="Chl-a (mg m^-3)")
  
  if (fitmethod=="gauss") {
    p <- p + 
      geom_line(aes(x=doy, y=fitted_chl), linetype="dashed", color="red") +
      geom_line(aes(x=doy, y=fitted_bkrnd_chl), linetype="dashed", color="red")
  } else if (fitmethod=="thresh") {
    tmp_threshold <- median(tmp_data$chl,na.rm=TRUE) * threshcoef
    p <- p +
      geom_hline(yintercept=tmp_threshold, color="red", alpha=0.4) +
      geom_label(aes(20, tmp_threshold, label="Threshold", vjust=0),
                 color="red", label.size=0, position=position_dodge(0.9), alpha=0.5)
  }
  
  if (i != ceiling(length(years_to_plot)/2)) {
    p <- p + theme(axis.title.y=element_blank())
  }
  
  if (i < length(years_to_plot)) {
    p <- p + theme(axis.title.x=element_blank(), axis.text.x=element_blank())
  }
  
  plots[[i]] <- p
  
}

print(wrap_plots(plots, ncol=1))

})

