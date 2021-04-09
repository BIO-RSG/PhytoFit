# Create day label for plots/maps and time index for subsetting data.
# This changes when year, interval, or day of year change.
get_time_vars <- function(interval, year, yearday, doys_per_week=NULL) {
  
  if (interval=="daily") {
    day_label <- paste0(format(as.Date((yearday-1), origin = paste0(year, "-01-01")), "%d %b %Y"), " (day ", yearday, ")")
    time_ind <- yearday
  } else if (interval=="weekly") {
    # Get the range of days for the week containing the selected day of year
    week_ind <- sapply(1:length(doys_per_week), function(i) {yearday %in% doys_per_week[[i]]})
    d1 <- range(doys_per_week[week_ind][[1]])[1]
    d2 <- range(doys_per_week[week_ind][[1]])[2]
    date1 <- format(as.Date((d1-1), origin = paste0(year, "-01-01")), "%d %b")
    date2 <- format(as.Date((d2-1), origin = paste0(year, "-01-01")), "%d %b %Y")
    day_label <- paste0(date1, " - ", date2, " (days ", d1, "-", d2, ")")
    time_ind <- which(week_ind)
  }
  
  return(list(day_label=day_label,
              time_ind=time_ind))
  
}

# Load and format a new dataset
get_data <- function(region, satellite, algorithm, year, yearday, interval, log_chla,
                     num_pix, doys_per_week, doy_week_start, doy_week_end) {
  
  sschla <- read_fst(paste0("./data/", region, "/", region, "_", satellite, "_", algorithm, "_", year, ".fst"))
  
  available_days <- nrow(sschla)/num_pix
  
  # Reshape
  sschla <- matrix(sschla$chl, ncol=available_days)
  
  # Convert to weekly data, if selected.
  if (interval=="daily") {
    
    sdoy_vec <- 1:available_days
    time_variables <- get_time_vars(interval, year, yearday)
    
  } else if (interval=="weekly") {
    
    # Subset the day list variables
    week_ind <- which(sapply(1:length(doys_per_week), function(i) {available_days %in% doys_per_week[[i]]}))
    sdoy_vec <- doy_week_start[1:week_ind]
    edoy_vec <- doy_week_end[1:week_ind]
    doys_per_week_sub <- doys_per_week[1:week_ind]
    last_week <- tail(doys_per_week_sub,1)[[1]]
    doys_per_week_sub[length(doys_per_week_sub)][[1]] <- last_week[last_week <= available_days]
    
    # Update available days to go to the end of the week that available_days falls into
    available_days <- edoy_vec[length(edoy_vec)]
    
    # Convert daily data to weekly data.
    sschla <- sapply(1:length(doys_per_week_sub), function(i) rowMeans(sschla[,doys_per_week_sub[[i]]],na.rm=TRUE))
    
    time_variables <- get_time_vars(interval, year, yearday, doys_per_week)
    
  }
  
  # Log data, if selected.
  if (log_chla) {sschla <- log10(sschla)}
  
  return(list(# change when new data is loaded after selecting interval, year, etc.
              sschla=sschla,
              available_days=available_days,
              doy_vec=sdoy_vec,
              # change when new data is loaded after selecting interval, year, etc, OR after day of year changes
              day_label=time_variables$day_label,
              time_ind=time_variables$time_ind))
  
}

# Get statistics for the selected region and day/week
get_stats <- function(rchla, outlier) {
  
  chl_mean <- apply(rchla, 2, mean, na.rm = TRUE)
  chl_sd <- apply(rchla, 2, sd, na.rm = TRUE)  
  chl_median <- apply(rchla, 2, median, na.rm = TRUE)
  chl_iqr <- apply(rchla, 2, IQR, na.rm = TRUE)
  # Note: can't use "min" and "max" functions alone because unlike the mean and median functions,
  # which return NA if all their input is NA, min and max return Inf
  chl_min <- sapply(1:ncol(rchla), function(i) {ifelse(all(is.na(rchla[,i])), NaN, min(rchla[,i], na.rm = TRUE))})
  chl_max <- sapply(1:ncol(rchla), function(i) {ifelse(all(is.na(rchla[,i])), NaN, max(rchla[,i], na.rm = TRUE))})
  nobs <- as.numeric(sapply(1:ncol(rchla), function(i) {sum(!is.na(rchla[,i]))}))
  percent_coverage <- (nobs/nrow(rchla))*100
  
  # Limits based on outlier method
  limits <- matrix(nrow = length(chl_sd), ncol = 2)
  if(outlier == 'sd2'){
    limits[,1] <- -1 * 2 * chl_sd + chl_mean
    limits[,2] <- 1 * 2 * chl_sd + chl_mean
  } else if (outlier == 'sd3'){
    limits[,1] <- -1 * 3 * chl_sd + chl_mean
    limits[,2] <- 1 * 3 * chl_sd + chl_mean
  } else if (outlier == 'iqr15'){
    limits[,1] <- -1 * 1.5 * chl_iqr + chl_median
    limits[,2] <- 1 * 1.5 * chl_iqr + chl_median
  }
  
  if (outlier == "none") {
    lenok <- apply(!is.na(rchla), 2, sum, na.rm=TRUE)
    bad <- lenok==0
    chl_mean[bad] <- chl_median[bad] <- chl_sd[bad] <- chl_min[bad] <- chl_max[bad] <- NA
    nobs[bad] <- percent_coverage[bad] <- 0
  } else {
    lenok <- vector(mode = 'logical', length = ncol(rchla))
    for (i in 1:ncol(rchla)) {
      d <- rchla[,i]
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
      lenok[i] <- length(ok)
    }
  }
  
  return(list(limits=limits,
              lenok=lenok,
              chl_mean=chl_mean,
              chl_median=chl_median,
              chl_sd=chl_sd,
              chl_min=chl_min,
              chl_max=chl_max,
              nobs=nobs,
              percent_coverage=percent_coverage))
  
}

# Get bloom fitted parameters and resulting plot
get_bloom_fit_data <- function(interval, p, pnames, dailystat, chl_mean, chl_median, lenok, ind_dayrange_percov,
                               ind_percov, ydays_dayrange_percov, ydays_percov, ydays_dayrange, rchla_nrow,
                               use_weights, smoothMethod, loessSpan, fitmethod, bloomShape, daily_percov,
                               tm, beta, tm_limits, ti_limits, t_range, log_chla, threshcoef, doy_vec, plot_title,
                               flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2, ti_threshold=0.2, tt_threshold=0.2,
                               rm_bkrnd=FALSE, ti_threshold_type = "percent_thresh", ti_threshold_constant = 0.1) {
  
  # Get vector of chlorophyll based on daily/weekly statistic and valid indices.
  # "chlall" is plotted as the points in the scatterplot, sized by percent coverage.
  # "chlorophyll" is used for the fit (all chl within the day range, with sufficient percent coverage).
  if(dailystat == 'average'){
    chlorophyll <- chl_mean[ind_dayrange_percov]
    chlall <- chl_mean[ind_percov]
  } else if(dailystat == 'median'){
    chlorophyll <- chl_median[ind_dayrange_percov]
    chlall <- chl_median[ind_percov]
  }
  
  # Create final day/chlorophyll vectors for the fit, smoothed or not
  t <- ydays_dayrange_percov
  if (smoothMethod == 'loess'){
    mod <- try(loess(chlorophyll ~ ydays_dayrange_percov, span = loessSpan, degree = 2), silent=TRUE)
    bad_loess <- class(mod)=="try-error" | is.null(mod)
    if (bad_loess) {y <- chlorophyll
    } else {y <- fitted(mod)}
  } else if (smoothMethod == 'nofit'){
    y <- chlorophyll
  }
  
  if (fitmethod == 'gauss') {
    
    if (use_weights) {weights <- daily_percov[ind_dayrange_percov]
    } else {weights <- rep(1,length(chlorophyll))}
    
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
    
    # collect parameters from fit
    bf_results <- gauss_res$values
    bf_results[,3:6] <- round(as.numeric(bf_results[,3:6])) # round days
    
    # CALCULATE RMSE AND FITTED VECTOR IF NON-NULL FIT EXISTS
    # For the fitted vector, you use everything in the selected day range, even 
    # though some of those points were not used in the fit because they were NA
    # or had insufficient coverage.
    if (is.null(gauss_res$fit)) {
      bf_results$RMSE <- NA
      nofit_msg <- gauss_res$nofit_msg
    } else {
      bf_results$RMSE <- ifelse(log_chla,
                                rmse(chlorophyll, predict(gauss_res$fit)),
                                rmse(log10(chlorophyll), log10(predict(gauss_res$fit))))
      yfit <- gauss_res$yfit
      ybkrnd <- gauss_res$ybkrnd
    }
    
    
  } else if (fitmethod=="roc" | fitmethod=="thresh") {
    
    if (fitmethod == 'roc') {
      bf_results <- rateOfChange(y = y, t = t,
                                 yall = chlall, tall = ydays_percov,
                                 bloomShape = bloomShape,
                                 tm_limits = tm_limits,
                                 ti_limits = ti_limits)
    } else if (fitmethod == "thresh") {
      bf_results <- threshold(t = t, y = y, 
                              tall = ydays_percov, yall = chlall,
                              threshcoef = threshcoef, 
                              bloomShape = bloomShape,
                              tm_limits = tm_limits,
                              ti_limits = ti_limits,
                              log_chla = log_chla)
    }
    
    # collect parameters from "rate of change" or "threshold" fit
    bf_results <- bf_results$values
    bf_results[,3:6] <- round(as.numeric(bf_results[,3:6])) # round days
    
  }
  
  # format output table for plotting
  tab_to_plot <- data.frame(parameter=sapply(1:ncol(bf_results), function(i) sub("beta", "\u03B2", colnames(bf_results)[i])),
                            value=unlist(bf_results),
                            stringsAsFactors = FALSE)
  tab_to_plot[c(1,2,7:nrow(tab_to_plot)),"value"] <- round(tab_to_plot[c(1,2,7:nrow(tab_to_plot)),"value"], 3)
  # convert parameter table to tableGrob to overlay on ggplot
  values_df <- tableGrob(d = tab_to_plot, rows = NULL, cols = NULL,
                         # Define theme to parse plotmath expressions
                         theme = ttheme_minimal(core=list(fg_params=list(parse=TRUE, hjust=0, x=0.02),
                                                          bg_params = list(fill="white", alpha=0.6)),
                                                base_size=10,
                                                padding=unit(c(1,1), "mm")))
  
  
  #*******************************************************************
  # PLOT FIT
  
  # get actual data (all days or weeks with good percent coverage)
  allchl <- rep(NA,length(doy_vec))
  allchl[ind_percov] <- chlall
  allpercov <- rep(NA,length(doy_vec))
  allpercov[ind_percov] <- (100 * lenok / rchla_nrow)[ind_percov]
  
  # initialize and format base plot (points sized by percent coverage)
  p <- p +
    geom_point(data=data.frame(x=doy_vec, y=allchl, percov=allpercov, stringsAsFactors = FALSE),
               aes(x=x, y=y, size=percov), alpha=0.6) +
    geom_vline(xintercept=as.numeric(bf_results[,c("t[start]", "t[max]", "t[end]")]), col="black", alpha=0.6) +
    ggtitle(plot_title) +
    labs(x='Day number') +
    scale_x_continuous(limits=c(0,365), breaks=seq(0,365,by=50)) +
    scale_size_continuous(name = "Percent coverage",
                          breaks = c(25, 50, 75, 100),
                          limits = c(10, 100),
                          labels = c(25, 50, 75, 100),
                          range = c(1, 6)) +
    theme(legend.position="bottom",
          legend.title=element_text(size=12),
          axis.title.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          panel.border = element_rect(colour="black", fill=NA, size=0.4)) +
    annotation_custom(grobTree(textGrob(paste0("** Click a point and scroll up to see the map for that ",
                                               ifelse(interval=="daily", "day", "week")),
                                        x=0.01, y=0.94,hjust=0,
                                        gp=gpar(fontsize=10, col="black", fontface="bold"))))
  
  if (log_chla) {
    p <- p + labs(y=bquote(.(proper(interval)) * " " * .(dailystat) * " chlorophyll [" * ~log[10]~ mg/m^3 * "]"))
  } else {
    p <- p + labs(y=bquote(.(proper(interval)) * " " * .(dailystat) * " chlorophyll [" * mg/m^3 * "]"))
  }
  
  # If you use LOESS smoothing, add the line to the data
  if (smoothMethod == 'loess') {
    if (!bad_loess & all(is.finite(y))) {
      p <- p +
        geom_line(data=data.frame(x=t, yloess=y, stringsAsFactors = FALSE),
                  aes(x=x, y=yloess), color="green") +
        annotation_custom(grobTree(textGrob("** LOESS smoothing",
                                            x=0.01, y=0.89, hjust=0,
                                            gp=gpar(fontsize=10, col="green", fontface="bold"))))
    }
  }
  
  # color of fit line, based on choice of mean/median daily/weekly statistic,
  # matched with the mean/median vertical bar coloring in the density plot
  fit_col <- ifelse(dailystat=="average","dodgerblue2","red2")
  
  # add line and statistics based on user-selected fit method
  if (fitmethod == 'gauss') {
    if (is.null(gauss_res$fit)) {
      p <- p +
        annotation_custom(rectGrob(gp=gpar(fill="white", alpha=0.6))) +
        annotation_custom(textGrob("Unable to fit:", x=0.018, y=0.8, hjust=0,
                                   gp=gpar(fontsize=16, col="red", fontface="bold"))) +
        annotation_custom(textGrob(nofit_msg, x=0.018, y=0.76, hjust=0,
                                   gp=gpar(fontsize=12, col="black")))
    } else {
      # add data to base plot
      p <- p +
        geom_line(data=data.frame(x=ydays_dayrange, yfit=yfit, stringsAsFactors = FALSE),
                  aes(x=x, y=yfit), color=fit_col) +
        geom_line(data=data.frame(x=ydays_dayrange, ybkrnd=ybkrnd, stringsAsFactors = FALSE),
                  aes(x=x, y=ybkrnd), color="red", linetype="dashed") +
        annotation_custom(grobTree(textGrob(ifelse(smoothMethod == 'loess',
                                                   "** Gaussian fitted to LOESS",
                                                   "** Gaussian fitted to points"),
                                            x=0.01, y=0.84, hjust=0,
                                            gp=gpar(fontsize=10, col=fit_col, fontface="bold")))) +
        annotation_custom(grobTree(textGrob("- - Background [chla]",
                                            x=0.01, y=0.79, hjust=0,
                                            gp=gpar(fontsize=10, col="red", fontface="bold"))))
    }
  } else if (fitmethod=="thresh") {
    thresh_val <- as.numeric(bf_results[,"Threshold"])
    p <- p +
      geom_hline(yintercept=thresh_val, color="red", alpha=0.4) +
      geom_label(aes(5, thresh_val, label="Threshold", vjust=0), color="red", label.size=0, position=position_dodge(0.9), alpha=0.5)
  }
  
  # add the parameter table to the right side of the plot
  p <- p + annotation_custom(values_df, xmin=325, xmax=360, ymin=-Inf, ymax=Inf)
  
  return(list(p=p, fitparams=bf_results, chlall=chlall))
  
}

# Format current input settings to write to csv
format_settings_to_save <- function(all_inputs, custom_name, polylon, polylat) {
  
  all_inputs$custom_name <- custom_name
  
  if (is.null(polylon)) {
    all_inputs$polylon <- all_inputs$polylat <- NA
  } else {
    all_inputs$polylon <- polylon
    all_inputs$polylat <- polylat
  }
  
  # create a vector of expanded descriptions/names of current values
  # (note this is not yet in the same order as all_inputs - this will be reordered and attached later)
  val_desc <- c(names(sensors)[sensors==all_inputs$satellite],
                names(regions)[regions==all_inputs$region],
                names(algorithms)[algorithms==all_inputs$algorithm],
                NA, ifelse(all_inputs$interval=="weekly", "8 days", NA), NA, NA, NA,
                names(outliers)[outliers==all_inputs$outlier],
                names(dailystats)[dailystats==all_inputs$dailystat],
                NA, NA,
                names(fitmethods)[fitmethods==all_inputs$fitmethod],
                names(bloomShapes)[bloomShapes==all_inputs$bloomShape],
                names(smoothMethods)[smoothMethods==all_inputs$smoothMethod],
                NA, NA, NA, NA,
                names(ti_threshold_types)[ti_threshold_types==all_inputs$ti_threshold_type],
                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                "See user guide for full names and boundaries of polygons",
                "See user guide for full names and boundaries of polygons",
                NA, "Decimal degrees", "Decimal degrees")
  
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
  input_ids_variable_type <- input_ids_variable_type[subset_ind]
  input_ids_widget_type <- input_ids_widget_type[subset_ind]
  
  # combine ids, values, and description in a dataframe, and fix column names
  inputs_to_save <- as.data.frame(cbind(names(inputs_to_save), inputs_to_save, input_ids_description,
                                        val_desc, input_ids_variable_type, input_ids_widget_type),
                                  row.names=FALSE, stringsAsFactors=FALSE)
  colnames(inputs_to_save) <- c("setting_id", "value", "setting_description", "value_description",
                                "setting_id_variable_type", "setting_id_widget_type")
  
  return(inputs_to_save)
  
}

# Create an output filename based on user-selected settings
output_str <- function(satellite, region, algorithm, year, interval, log_chla, day_label=NULL, polygon=NULL, fitmethod, custom_end) {
  
  if (length(year) > 1) {
    if (year[1]==year[2]) {
      year <- year[1]
    } else {
      year <- paste0(year, collapse="-")
    }
  }
  
  output_name <- paste(c(satellite, region, algorithm, year, interval,
                         ifelse(log_chla, "loggedChla", "unloggedChla"), day_label, polygon,
                         ifelse(fitmethod=="gauss", "Gaussian", ifelse(fitmethod=="roc", "RateOfChange", "Threshold")),
                         "created", format(Sys.time(),"%Y-%m-%d-%H%M%S"), custom_end),
                       collapse="_")
  
  # If some variables set to NULL, their space is blank but there are _ on either side.
  # Remove the extra _ here
  output_name <- gsub("([_])\\1+","\\1", output_name)
  
  return(output_name)
  
}


# given the selected set of boxes, collect IDs, full names, and lons/lats
get_polygon_details <- function(regs, custom_name, region, polylat, polylon, newpoly=NULL, editedpoly=NULL, typedpoly=NULL) {
  
  polygon_list <- list()
  
  # if "custom" box is selected but no polygon is drawn, unselect it
  if (is.null(newpoly) & is.null(editedpoly) & is.null(typedpoly)) {
    regs <- regs[regs != "custom"]
  }
  
  # get the full names of the user-selected polygons, in the same order
  polygon_list$full_names <- sapply(1:length(regs), function(r) ifelse(regs[r]=='custom',
                                                                       ifelse(nchar(custom_name)==0, "Custom polygon", custom_name),
                                                                       paste0(full_names[[region]][which(regs[r]==poly_ID[[region]])])))
  
  # get the coordinates of the selected polygons
  boxes <- all_regions[[region]]
  names(boxes) <- poly_ID[[region]]
  if ("custom" %in% regs) {
    boxes[["custom"]] <- list()
    boxes[["custom"]]$lat <- polylat
    boxes[["custom"]]$lon <- polylon
  }
  
  # subset boxes corresponding to user-selected polygons, in the same order
  boxes <- boxes[regs]
  
  polygon_list$poly_ID <- regs
  polygon_list$longitudes <- lapply(boxes, "[[", 2)
  polygon_list$latitudes <- lapply(boxes, "[[", 1)
  
  return(polygon_list)
  
}


format_settings_to_load <- function(settings) {
  
  # get IDs and their values, and trim white space
  tmp_ids <- trimws(settings$setting_id)
  tmp_values <- trimws(settings$value)
  tmp_types <- as.numeric(trimws(settings$setting_id_variable_type))
  tmp_widgets <- as.numeric(trimws(settings$setting_id_widget_type))
  
  # separate ones that were collapsed by commas
  tmp_values <- sapply(tmp_values, strsplit, split=",")
  
  # coerce to numeric or logical
  tmp_values[tmp_types==1] <- lapply(tmp_values[tmp_types==1], as.numeric)
  tmp_values[tmp_types==2] <- lapply(tmp_values[tmp_types==2], as.character)
  tmp_values[tmp_types==3] <- lapply(tmp_values[tmp_types==3], as.logical)
  
  names(tmp_ids) <- names(tmp_values) <- names(tmp_types) <- names(tmp_widgets) <- NULL
  
  return(list(ids=tmp_ids, values=tmp_values, types=tmp_types, widgets=tmp_widgets))
  
}
