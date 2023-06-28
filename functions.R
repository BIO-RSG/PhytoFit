# function to check if a data file exists and is listed in input_variables.R
dexist <- function(region,sat_alg,year) {
  region_listed <- region %in% regions
  sat_alg_listed <- sat_alg %in% unlist(sat_algs[[region]])
  year_listed <- year %in% years[[region]][[sat_alg]]
  file_exists <- file.exists(paste0("./data/", region, "/", region, "_", sat_alg, "_", year, ".fst"))
  return(region_listed & sat_alg_listed & year_listed & file_exists)
}

# Create day label for plots/maps and time index for subsetting data.
# This changes when year, composite, or day of year change.
get_time_vars <- function(composite, year, yearday, dvecs) {
  if (composite==1) {
    time_ind <- yearday
    day_label <- paste0(format(as.Date((yearday-1), origin = paste0(year, "-01-01")), "%d %b %Y"), " (day ", yearday, ")")
  } else {
    time_ind <- as.integer((yearday - 1)%/%composite + 1)
    d <- range(dvecs[[paste0("X",composite)]]$dlist[[time_ind]])
    date1 <- format(as.Date((d[1]-1), origin = paste0(year, "-01-01")), "%d %b")
    date2 <- format(as.Date((d[2]-1), origin = paste0(year, "-01-01")), "%d %b %Y")
    day_label <- paste0(date1, " - ", date2, " (days ", d[1], "-", d[2], ")")
  }
  return(list(day_label=day_label, time_ind=time_ind))
}

# Load and format a new dataset
get_data <- function(region, sat_alg, year, yearday, composite, log_chla, num_pix, dvecs,
                     concentration_type="full", cell_size_model1="small", cell_size_model2="small") {
  
  sschla_filename <- paste0("./data/", region, "/", region, "_", sat_alg, "_", year, ".fst")
  if (!file.exists(sschla_filename)) {
    return(list(sschla=matrix(nrow=num_pix, ncol=1), available_days=0, doy_vec=0))
  }
  sschla <- read_fst(sschla_filename)
  colnames(sschla) <- "var"
  sschla <- matrix(sschla$var, nrow=num_pix)
  available_days <- min(ncol(sschla),365)
  sschla <- sschla[,1:available_days]
  
  # If separating chla into phytoplankton cells of different sizes
  if (concentration_type=="model1") {
    sschla <- phyto_cellsize_model1(sschla, cell_size_model1)
  } else if (concentration_type=="model2") {
    sschla <- phyto_cellsize_model2(sschla, cell_size_model2)
  }
  
  # If necessary, average data over the select number of days depending on composite length (4 or 8)
  if (composite==1) {
    sdoy_vec <- 1:available_days
  } else {
    d2 <- dvecs[[paste0("X",composite)]]
    sdoy_vec <- d2$sdoy_vec
    dlist <- d2$dlist
    ind <- sdoy_vec <= available_days
    sdoy_vec <- sdoy_vec[ind]
    dlist <- dlist[ind]
    dlist_end <- length(dlist)
    dlist[[dlist_end]] <- dlist[[dlist_end]][dlist[[dlist_end]] <= available_days]
    available_days <- min(tail(sdoy_vec,1)+composite-1,365)
    sschla <- avg_columns(mat=sschla, dlist=dlist, year=year)
  }
  
  if (log_chla) {sschla <- log10(sschla)}
  
  return(list(sschla=sschla, available_days=available_days, doy_vec=sdoy_vec))
  
}

# Extract pixels that are within a polygon, and set pixels beyond a threshold to NA.
# spdf is a Simple feature collection, sscoords is a SpatialPointsDataframe
# (sp::over is faster than sf::st_intersects)
subset_data <- function(spdf,sscoords,sschla,pixrange1,pixrange2) {
  if (nrow(spdf)==0) return(NULL)
  mask <- over(x=as_Spatial(spdf),y=sscoords,returnList=TRUE)[[1]]
  if (length(mask)==0) {
    rchla <- NULL
  } else if (length(mask)==1) {
    rchla <- matrix(sschla[mask,], nrow=1) # make sure rchla is in matrix format
  } else {
    rchla <- sschla[mask,]
  }
  # Set pixels to NA if they're outside a specified range
  if (!is.na(pixrange1)) {rchla[rchla < pixrange1] <- NA}
  if (!is.na(pixrange2)) {rchla[rchla > pixrange2] <- NA}
  if (all(is.na(rchla))) {rchla <- NULL}
  return(rchla)
}

# Get statistics for the selected region and composite
get_stats <- function(rchla, outlier) {
  
  if (nrow(rchla)==1) {
    
    rchlav <- as.numeric(rchla)
    chl_mean <- chl_median <- chl_min <- chl_max <- rchlav
    limits <- cbind(rchlav,rchlav)
    chl_sd <- rep(0,length(rchlav))
    nobs <- as.numeric(is.finite(rchlav))
    percent_coverage <- nobs*100
    
  } else {
    
    chl_mean <- apply(rchla, 2, mean, na.rm = TRUE)
    chl_sd <- apply(rchla, 2, sd, na.rm = TRUE)  
    chl_median <- apply(rchla, 2, median, na.rm = TRUE)
    # Note: can't use "min" and "max" functions alone because unlike the mean and median functions,
    # which return NA if all their input is NA, min and max return Inf
    nobs <- colSums(!is.na(rchla))
    lennobs <- nrow(rchla)
    good <- nobs > 0
    chl_min <- chl_max <- rep(NaN,ncol(rchla))
    rchla_good <- matrix(rchla[,good],nrow=nrow(rchla))
    chl_min[good] <- apply(rchla_good, MARGIN=2, FUN=min, na.rm = TRUE)
    chl_max[good] <- apply(rchla_good, MARGIN=2, FUN=max, na.rm = TRUE)
    percent_coverage <- (nobs/lennobs)*100
    
    # Limits based on outlier method
    limits <- matrix(nrow = length(chl_sd), ncol = 2)
    if(outlier == 'sd2'){
      limits[,1] <- -1 * 2 * chl_sd + chl_mean
      limits[,2] <- 1 * 2 * chl_sd + chl_mean
    } else if (outlier == 'sd3'){
      limits[,1] <- -1 * 3 * chl_sd + chl_mean
      limits[,2] <- 1 * 3 * chl_sd + chl_mean
    } else if (outlier == 'iqr15'){
      chl_iqr <- apply(rchla, 2, IQR, na.rm = TRUE)
      limits[,1] <- -1 * 1.5 * chl_iqr + chl_median
      limits[,2] <- 1 * 1.5 * chl_iqr + chl_median
    } else if (startsWith(outlier,"q")) {
      qnum <- as.numeric(paste0(substr(outlier,2,3),".",substr(outlier,4,5)))/100
      limits <- t(apply(rchla, 2, quantile, probs=c(qnum,1-qnum), na.rm = TRUE))
    }
    
    if (outlier != "none") {
      for (i in which(good)) {
        d <- rchla[,i]
        ok <- which(d >= limits[i,1] & d <= limits[i,2]) # this filters out NA pixels
        # update stats for this composite after removing outliers
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
    }
  }
  
  df <- data.frame(lower_limit=limits[,1],
                   upper_limit=limits[,2],
                   mean=chl_mean,
                   median=chl_median,
                   stdev=chl_sd,
                   min=chl_min,
                   max=chl_max,
                   nobs=nobs,
                   percent_coverage=percent_coverage)
  
  return(df)
  
}


bf_data_calc <- function(composite, chl_to_use, ind_dayrange_percov, ind_percov, ind_dayrange,
                         daily_percov, t_range, log_chla, doy_vec, variable, sv) {
  
  smoothMethod = sv$smoothMethod
  fitmethod = sv$fitmethod
  tm = sv$tm
  ti_limits = sv$ti_limits
  
  ydays_percov <- doy_vec[ind_percov] # all days with high enough percent coverage
  ydays_dayrange <- doy_vec[ind_dayrange]
  ydays_dayrange_percov <- doy_vec[ind_dayrange_percov] # subset of days used for fit
  
  nofit_msg <- NULL
  yfit <- ybkrnd <- rep(NA, length(ydays_dayrange))
  loess_result <- rep(NA, length(ydays_dayrange_percov))
  
  # "chl_percov" is plotted as the points in the scatterplot, sized by percent coverage, also used in RoC and Threshold
  # "chl_dayrange_percov" is used for the fit (all chl within the day range, with sufficient percent coverage).
  chl_dayrange_percov <- chl_to_use[ind_dayrange_percov]
  chl_percov <- chl_to_use[ind_percov]
  
  if (sv$use_weights) {
    weights <- daily_percov[ind_dayrange_percov]
  } else {
    weights <- rep(1,length(chl_dayrange_percov))
  }
  
  # list containing the values to fit and the real chla values
  y <- list(y=chl_dayrange_percov, chla=chl_dayrange_percov)
  
  # use loess smooth on the real data points, if selected, and assign the results
  # to y so they will be used in the fit instead of the real chla values
  if (smoothMethod == 'loess'){
    mod <- try(loess(chl_dayrange_percov ~ ydays_dayrange_percov,
                     weights=weights, span=sv$loessSpan, degree=2), silent=TRUE)
    bad_loess <- class(mod)=="try-error" | is.null(mod)
    if (!bad_loess) {
      loess_result <- fitted(mod)
      y$y <- loess_result # use loess for fitting instead of real values
      weights <- rep(1,length(chl_dayrange_percov)) # reset weights so they aren't used again in a gaussian fit
    }
  }
  
  if (fitmethod == 'gauss') {
    
    if (tm) {tmp_ti_lim <- c(1,365)
    } else {tmp_ti_lim <- ti_limits}
    gauss_res <- gaussFit(t=ydays_dayrange_percov, y=y, w=weights,
                          bloomShape=sv$bloomShape,
                          tm=tm, beta=sv$beta,
                          tm_limits=sv$tm_limits,
                          ti_limits=tmp_ti_lim,
                          t_range=t_range,
                          log_chla=log_chla,
                          composite=composite,
                          flag1_lim1=sv$flag1_lim1,
                          flag1_lim2=sv$flag1_lim2,
                          flag2_lim1=sv$flag2_lim1,
                          flag2_lim2=sv$flag2_lim2,
                          ti_threshold=sv$ti_threshold,
                          tt_threshold=sv$tt_threshold,
                          ydays_dayrange=ydays_dayrange,
                          rm_bkrnd=sv$rm_bkrnd,
                          ti_threshold_type=sv$ti_threshold_type,
                          ti_threshold_constant=sv$ti_threshold_constant)
    fitparams <- gauss_res$values
    # calculate rmse if non-null fit exists
    if (is.null(gauss_res$fit)) {
      fitparams$RMSE <- NA
      nofit_msg <- gauss_res$nofit_msg
    } else {
      pgrf <- predict(gauss_res$fit)
      fitparams$RMSE <- ifelse(!log_chla & variable$log, # if you haven't logged the data, but technically it's "loggable"
                               rmse(log10(chl_dayrange_percov), log10(pgrf)),
                               rmse(chl_dayrange_percov, pgrf))
      yfit <- gauss_res$yfit
      ybkrnd <- gauss_res$ybkrnd
    }
    # round tmax_fit day
    fitparams[,"t[max_fit]"] <- round(as.numeric(fitparams[,"t[max_fit]"]))
  
  } else {
    
    if (fitmethod == 'roc') {
      fitparams <- rateOfChange(y=y, t=ydays_dayrange_percov,
                                 yall=chl_percov, tall=ydays_percov,
                                 bloomShape=sv$bloomShape,
                                 tm_limits=sv$tm_limits,
                                 ti_limits=ti_limits,
                                 log_chla=log_chla)
    } else if (fitmethod == "thresh") {
      fitparams <- threshold(t=ydays_dayrange_percov, y=y, 
                              tall=ydays_percov, yall=chl_percov,
                              threshcoef=sv$threshcoef, 
                              bloomShape=sv$bloomShape,
                              tm_limits=sv$tm_limits,
                              ti_limits=ti_limits,
                              log_chla=log_chla)
    }
    
    nofit_msg <- fitparams$nofit_msg
    modelfit <- fitparams$yfit
    if (!is.null(modelfit)) {
      ybkrnd <- predict(modelfit,newdata=data.frame(tall=ydays_dayrange))
    }
    fitparams <- fitparams$values
    
  }
  
  # round more days
  tvars <- c("t[start]", "t[max_real]", "t[end]", "t[duration]")
  fitparams[,tvars] <- round(as.numeric(fitparams[,tvars]))
  # add extra summary stats
  bf_extra <- data.frame(Mean = mean(chl_dayrange_percov, na.rm=TRUE),
                         Median = median(chl_dayrange_percov, na.rm=TRUE),
                         StDev = sd(chl_dayrange_percov, na.rm=TRUE))
  # transform extra summary stats back to linear space if necessary
  if (log_chla) {bf_extra[1:3] <- lapply(bf_extra[1:3], function(x) 10^x)}
  fitparams <- dplyr::bind_cols(bf_extra,fitparams)
  
  # merge stuff together in lists for output
  df_percov <- data.frame(x=ydays_percov,y=chl_percov,percov=daily_percov[ind_percov])
  df_dayrange <- data.frame(x=ydays_dayrange,yfit=yfit,ybkrnd=ybkrnd)
  df_dayrange_percov <- data.frame(x=ydays_dayrange_percov,loess=loess_result)
  
  return(list(fitparams=fitparams, nofit_msg=nofit_msg, df_percov=df_percov,
              df_dayrange=df_dayrange, df_dayrange_percov=df_dayrange_percov))
  
}

bf_data_plot <- function(p, nofit_msg, composite, dailystat, log_chla, smoothMethod, fitmethod, variable,
                         fitparams, df_percov, df_dayrange, df_dayrange_percov) {
  
  # add points sized by percent coverage, vertical model timing bars, and do other formatting
  p <- p +
    geom_point(data=df_percov, aes(x=x, y=y, size=percov), alpha=0.6) +
    geom_vline(xintercept=as.numeric(fitparams[,c("t[start]","t[max_real]","t[end]")]), col="black", alpha=0.6) +
    labs(x='Day of year',size="% coverage") +
    scale_x_continuous(limits=c(0,365), breaks=seq(0,365,by=50)) +
    scale_size_area(minor_breaks=c(25, 50, 75, 100), limits=c(0, 100), max_size=10) +
    theme(legend.position="bottom",
          legend.title=element_text(size=14),
          legend.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.text=element_text(size=12),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.4)) +
    labs(y=bquote(.(proper(dailystat)) * " " * .(tolower(names(composites)[composites==composite])) * " " * .(variable$timeseries_ytitle)))
  if (any(is.finite(df_dayrange$ybkrnd))) {
    p <- p +
      geom_line(data=df_dayrange, aes(x,ybkrnd), color="red", linetype="dashed") +
      geom_text_npc(aes(npcx=0.01,npcy=0.79,label=paste("- - Background",variable$abbrev)),size=4,fontface="bold",color="red")
  }
  
  if (log_chla) {p <- p + scale_y_log10()}
  
  # if you used LOESS smoothing, add the line to the data
  if (smoothMethod == 'loess' & any(is.finite(df_dayrange_percov$loess))) {
    p <- p +
      geom_line(data=df_dayrange_percov, aes(x=x, y=loess), color="green") +
      geom_text_npc(aes(npcx=0.01,npcy=0.89,label="** LOESS smoothing"),size=4,fontface="bold",color="green")
  }
  
  # add gaussian line or threshold line, based on user-selected fit method
  if (fitmethod=='gauss') {
    # add t[max_fit] line
    p <- p + geom_vline(xintercept=as.numeric(fitparams$`t[max_fit]`), col="black", alpha=0.6)
    # add gauss fit line if it exists
    if (all(is.na(df_dayrange$yfit))) {
      p <- p + geom_text_npc(aes(npcx=0.01,npcy=0.85,label="Unable to fit"),size=8,fontface="bold",color="red")
    } else {
      # add data to base plot
      fit_col <- ifelse(dailystat=="average","dodgerblue2","red2")
      tmp_lab <- paste0("** Gaussian fitted to ",ifelse(smoothMethod=='loess',"LOESS","points"))
      p <- p +
        geom_line(data=df_dayrange, aes(x,yfit), color=fit_col) +
        geom_text_npc(aes(npcx=0.01,npcy=0.84,label=tmp_lab),size=4,fontface="bold",color=fit_col)
      if (!is.na(fitparams$Flags)) {
        p <- p +
          geom_label_npc(aes(npcx=0.5,npcy=0.94,label="WARNING: Flagged fit"),size=8,fontface="bold",color="red",fill="white",alpha=0.5,label.size=0,label.padding=unit(0.05,"lines"))
      }
    }
  } else if (fitmethod=="thresh") {
    thresh_val <- as.numeric(fitparams[,"Threshold"])
    p <- p +
      geom_hline(yintercept=thresh_val, color="red", alpha=0.4) +
      geom_label(aes(5, thresh_val, label="Threshold", vjust=0), color="red",
                 label.size=0, position=position_dodge(0.9), alpha=0.5)
  }
  
  # if no fit, add error message to plot
  if (!is.null(nofit_msg)) {p <- p + geom_text_npc(aes(npcx=0.01,npcy=0.76,label=nofit_msg),size=4)}
  
  return(p)
  
}


# Create an output filename based on user-selected settings. d = "state" list of reactive values
output_str <- function(d, custom_end) {
  sat_alg <- d$sat_alg
  year <- d$year
  concentration_type <- d$concentration_type
  day_label <- polygon <- NULL
  if (!is.null(custom_end)) {
    if (custom_end %in% c("density_plot.png","settings.txt")) {
      day_label <- gsub(" ", "", strsplit(d$day_label, "[()]+")[[1]][2])
    }
    if (custom_end %in% c("density_plot.png","model_fit.png","annual_stats.csv","model_parameters.csv","settings.txt")) {
      polygon <- d$box
    }
  }
  if (length(year) > 1) {
    if (year[1]==year[2]) {year <- year[1]
    } else {year <- paste0(year, collapse="-")}
  }
  cellSize <- paste0("cellSize", ifelse(concentration_type=="full", "All",
                                        ifelse(concentration_type=="model1", paste0(proper(d$cell_size_model1), "-mod1"),
                                               paste0(proper(d$cell_size_model2), "-mod2"))))
  sat_alg <- strsplit(sat_alg,split="_")[[1]]
  output_name <- paste(c(sat_alg[1], d$region, polygon,
                         gsub("-","",tolower(names(composites)[composites==d$composite])),
                         year, day_label, cellSize,
                         ifelse(d$log_chla, paste0("log", proper(sat_alg[2])), proper(sat_alg[2])),
                         d$fitmethod, paste0("created", format(Sys.time(),"%Y-%m-%d-%H%M%S")), custom_end),
                       collapse="_")
  # If some variables are NULL, their space is blank but there are _ on either side. Remove the extra _ here
  output_name <- rm_dup(output_name, "_")
  return(output_name)
}

# Format current input settings to write to csv
format_settings_to_save <- function(all_inputs, custom_name, polystr, regions, sat_algs) {
  
  all_inputs$custom_name <- custom_name
  all_inputs$polystr <- polystr
  
  # create a vector of expanded descriptions/names of current values
  # (note this is not yet in the same order as all_inputs - this will be reordered and attached later)
  val_desc <- c(names(regions)[regions==all_inputs$region],
                names(sat_algs[[all_inputs$region]])[sat_algs[[all_inputs$region]]==all_inputs$sat_alg],
                names(concentration_types)[concentration_types==all_inputs$concentration_type],
                names(cell_sizes_model1)[cell_sizes_model1==all_inputs$cell_size_model1],
                names(cell_sizes_model2)[cell_sizes_model2==all_inputs$cell_size_model2],
                NA, # year
                names(composites)[composites==all_inputs$composite],
                NA, # logging
                NA, # day of year
                NA, # percent coverage
                names(outliers)[outliers==all_inputs$outlier],
                names(dailystats)[dailystats==all_inputs$dailystat],
                NA, NA, # min/max pixel values
                names(fitmethods)[fitmethods==all_inputs$fitmethod],
                names(bloomShapes)[bloomShapes==all_inputs$bloomShape],
                names(smoothMethods)[smoothMethods==all_inputs$smoothMethod],
                NA, # loess span
                NA, NA, NA, # range of days for fit, initiation, and peak
                names(ti_threshold_types)[ti_threshold_types==all_inputs$ti_threshold_type],
                NA, # percent threshold
                NA, # constant threshold
                NA, NA, NA, NA, # tmax, beta, weights, remove background
                NA, NA, NA, NA, # flag 1/2 limits
                NA, # threshold fit method coefficient
                NA, NA, NA, # full run options: png, csv, range of years
                "See user guide for full names and boundaries of polygons", # full run polygons
                "See user guide for full names and boundaries of polygons", # selected predefined polygon
                NA, # custom polygon name
                "Decimal degrees") # custom polygon string
  val_desc <- gsub(",","",val_desc) # remove commas so they don't break csv columns
  
  # subset them based on the ones you need to save
  all_inputs <- all_inputs[names(all_inputs) %in% input_ids_to_save]
  
  # if a list element is a vector, collapse it into one string value separated by comma(s)
  all_inputs <- lapply(all_inputs, paste0, collapse=",")
  
  # convert list to matrix
  inputs_to_save <- do.call(rbind,all_inputs)
  
  # fix the order of inputs and their descriptions, and subset them properly
  rnames <- rownames(inputs_to_save)
  proper_order <- match(input_ids_to_save[input_ids_to_save %in% rnames], rnames)
  inputs_to_save <- inputs_to_save[proper_order,]
  
  subset_ind <- input_ids_to_save %in% names(inputs_to_save)
  input_ids_description <- input_ids_description[subset_ind]
  val_desc <- val_desc[subset_ind]
  variable_type <- input_ids_variable_type[subset_ind]
  
  # combine ids, values, and description in a dataframe, and fix column names
  inputs_to_save <- as.data.frame(cbind(names(inputs_to_save),inputs_to_save,input_ids_description,val_desc,variable_type),
                                  row.names=FALSE, stringsAsFactors=FALSE)
  colnames(inputs_to_save) <- c("setting_id","value","setting_description","value_description","variable_type")
  
  return(inputs_to_save)
  
}


format_settings_to_load <- function(settings) {
  # get IDs and their values, and trim white space
  tmp_ids <- trimws(settings$setting_id)
  tmp_values <- trimws(settings$value)
  tmp_types <- as.numeric(trimws(settings$variable_type))
  # separate ones that were collapsed by commas
  tmp_values <- sapply(tmp_values, strsplit, split=",")
  # coerce to numeric, string, or logical
  tmp_values[tmp_types==1] <- lapply(tmp_values[tmp_types==1], as.numeric)
  tmp_values[tmp_types==2] <- lapply(tmp_values[tmp_types==2], as.character)
  tmp_values[tmp_types==3] <- lapply(tmp_values[tmp_types==3], as.logical)
  names(tmp_values) <- unlist(tmp_ids)
  return(tmp_values)
}


# Check user-entered latitudes and longitudes for the coordinate typing option
check_latlons <- function(lats, lons) {
  coords <- NULL
  help_latlon_txt <- ""
  invalid_latlon <- FALSE
  # split by commas, trim white space from each, and convert to numeric
  lats <- as.numeric(sapply(strsplit(lats, ",")[[1]], trimws))
  lons <- as.numeric(sapply(strsplit(lons, ",")[[1]], trimws))
  # check if any lat/lons are NA or NULL or INF
  if (any(!is.finite(lats)) | any(!is.finite(lons)) | any(is.null(lats)) | any(is.null(lons))) {
    invalid_latlon <- TRUE
    help_latlon_txt <- "Invalid latitude/longitude."
  # check if lat/lons are not numeric, or not the same length, or empty
  } else if (!(all(is.numeric(lats)) & all(is.numeric(lons))) | (length(lats) != length(lons)) | length(lats)==0) {
    invalid_latlon <- TRUE
    help_latlon_txt <- "Invalid latitude/longitude."
  } else {
    # if user forgets to enter first point to close the polygon, fix that here
    if (!(lats[1]==lats[length(lats)] & lons[1]==lons[length(lons)])) {
      lats <- c(lats, lats[1])
      lons <- c(lons, lons[1])
    }
    coords <- list(lons=lons,lats=lats)
  }
  return(list(coords=coords,
              invalid_latlon=invalid_latlon,
              help_latlon_txt=help_latlon_txt))
}

# Convert lat/lons (drawn or typed) to Simple feature collection (sfc)
make_custom_spdf <- function(lats,lons,name) {
  name <- trimws(name)
  name <- ifelse(nchar(name)==0, "Custom polygon", name)
  gm <- list(st_multipolygon(x=list(list(cbind(lons,lats)))))
  spdf <- st_sf(tibble(poly_id="custom", group=NA, name=name, label=name, geometry=gm))
  st_crs(spdf) <- "EPSG:4326"
  return(spdf)
}
# Check size of sfc (spdf is an sfc, pixels is a SpatialPointsDataframe)
check_custom_spdf_size <- function(spdf,pixels) {
  polygon_area <- length(over(x=as_Spatial(spdf),y=pixels,returnList=TRUE)[[1]])
  latlon_toolarge <- FALSE
  help_latlon_txt <- ""
  if (polygon_area > max_pixels) {
    latlon_toolarge <- TRUE
    help_latlon_txt <- paste0("Polygon is too large (max allowed pixels = ",max_pixels,").")
  }
  return(list(latlon_toolarge=latlon_toolarge,
              help_latlon_txt=help_latlon_txt))
}

# Two populations - small/medium and large
phyto_cellsize_model1 <- function(chla, which_size="small") {
  new_chla <- matrix(nrow=nrow(chla), ncol=ncol(chla))
  good_chla <- !is.na(chla)
  chla_sub <- chla[good_chla]
  Ssm = 1.613
  CsmMax = 0.62
  if (which_size=="small") {
    new_chla[good_chla] <- CsmMax * (1 - exp(-Ssm * chla_sub))
  } else if (which_size=="large") {
    new_chla[good_chla] <- chla_sub - CsmMax * (1 - exp(-Ssm * chla_sub))
  }
  return(new_chla)
}
# Three populations - small, medium, large
phyto_cellsize_model2 <- function(chla, which_size="small") {
  new_chla <- matrix(nrow=nrow(chla), ncol=ncol(chla))
  good_chla <- !is.na(chla)
  chla_sub <- chla[good_chla]
  CsMax = 0.166
  Ss = 6.01
  CsmMax = 0.999
  Ssm = 1.00
  if (which_size=="small") {
    new_chla[good_chla] <- CsMax * (1 -exp(-Ss * chla_sub))
  } else if (which_size=="medium") {
    new_chla[good_chla] <- CsmMax * (1 - exp(-Ssm * chla_sub)) - CsMax * (1 -exp(-Ss * chla_sub))
  } else if (which_size=="large") {
    new_chla[good_chla] <- chla_sub - CsmMax * (1 - exp(-Ssm * chla_sub))
  }
  return(new_chla)
}
