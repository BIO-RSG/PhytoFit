# Pad a number with leading zeroes to make it length "len".
pad_num <- function(num, len) {
  num_len <- nchar(as.character(floor(num)))
  if (num_len > len) {len <- num_len}
  paste0(paste(replicate(len - num_len, '0'), collapse=''), num)
}

# Capitalize the first letter of a word
proper <- function(x) {
  paste0(toupper(substr(x,1,1)), tolower(substring(x,2)))
}

# Check for existence of a folder, and create it if necessary.
get_dir <- function(path) {
  if (!dir.exists(path)) {dir.create(path, showWarnings=F)}
  path
}

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
  if (log_chla) {sschla <- log(sschla)}
  
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
  
  # Remove outliers based on selected method and obtain indices where
  # data coverage is greater than defined percentage
  lenok <- vector(mode = 'logical', length = ncol(rchla))
  for (i in 1:ncol(rchla)) {
    d <- rchla[,i]
    ok <- which(!is.na(d))
    if(outlier != 'none'){
      ok <- which(d >= limits[i,1] & d <= limits[i,2])
      # update stats for this day after removing outliers
      if (length(ok)==0) {
        chl_mean[i] <- chl_median[i] <- chl_sd[i] <- chl_min[i] <- chl_max[i] <- NA
        nobs[i] <- percent_coverage[i] <- 0
      } else {
        chl_mean[i] <- chl_mean(d[ok])
        chl_median[i] <- chl_median(d[ok])
        chl_sd[i] <- chl_sd(d[ok])
        chl_min[i] <- chl_min(d[ok])
        chl_max[i] <- chl_max(d[ok])
        nobs[i] <- length(ok)
        percent_coverage[i] <- (nobs[i]/length(d))*100
      }
    }
    lenok[i] <- length(ok)
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
                               tm, beta, tm_limits, ti_limits, log_chla, threshcoef, doy_vec, plot_title) {
  
  # Get vector of chlorophyll based on daily/weekly statistic and valid indices
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
  
  if(fitmethod == 'gauss'){
    
    if (use_weights) {
      weights <- daily_percov * 100
      weights <- weights[ind_dayrange_percov]
    } else {
      weights <- rep(1,length(chlorophyll))
    }
    
    if (tm) {
      tmp_ti_lim <- c(1,365)
    } else {
      tmp_ti_lim <- ti_limits
    }
    
    gauss_res <- gaussFit(t = t, y = y, w = weights,
                          bloomShape = bloomShape,
                          tm = tm,
                          beta = beta,
                          tm_limits = tm_limits,
                          ti_limits = tmp_ti_lim,
                          logchla = log_chla)
    
    # collect parameters from fit
    bf_results <- gauss_res$values
    
    
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
                              logchla = log_chla)
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
  
  
  #*******************************************************************
  # PLOT FIT
  
  # get actual data (all days or weeks with good percent coverage)
  allchl <- rep(NA,length(doy_vec))
  allchl[ind_percov] <- chlall
  allpercov <- rep(NA,length(doy_vec))
  allpercov[ind_percov] <- (lenok * 100 / rchla_nrow)[ind_percov]
  
  # initialize and format base plot (points sized by percent coverage)
  p <- p +
    geom_point(data=data.frame(x=doy_vec, y=allchl, percov=allpercov,
                               stringsAsFactors = FALSE),
               aes(x=x, y=y, size=percov), alpha=0.6) +
    ggtitle(plot_title) +
    labs(x='Day number',
         y=expression(paste0(interval, " ", dailystat, " chlorophyll" * "[" * mg/m^3 * "]"))) +
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
  
  # color of fit line, based on choice of mean/median daily/weekly statistic,
  # matched with the mean/median vertical bar coloring in the density plot
  fit_col <- ifelse(dailystat=="average","dodgerblue2","red2")
  
  # color and transparency of vertical bars marking indices of fit
  ind_col <- "black"
  ind_alpha <- 0.6
  
  # parameter table location variables
  miny <- min(allchl, na.rm=TRUE)
  maxy <- max(allchl, na.rm=TRUE)
  table_ydiff <- maxy - miny
  
  # add line and statistics based on user-selected fit method
  if (fitmethod == 'gauss') {
    
    if (is.null(gauss_res$fit)) {
      
      p <- p + annotation_custom(grobTree(textGrob("unable to fit",
                                                   x=0.018, y=0.93,hjust=0,
                                                   gp=gpar(fontsize=16, col="red", fontface="bold"))))
      
    } else {
      
      # fitted data (everything in the day range)
      if (bloomShape=="symmetric") {
        
        tmp_beta <- ifelse(beta, as.numeric(bf_results$beta_value), 0)
        yfit <- as.numeric(bf_results$B0) + tmp_beta * ydays_dayrange + as.numeric(bf_results$h) / (sqrt(2*pi) * as.numeric(bf_results$sigma)) * exp(- (ydays_dayrange - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigma)^2))
        
      } else if (bloomShape=="asymmetric") {
        
        tmp_betaL <- ifelse(beta, as.numeric(bf_results$beta_valueL), 0)
        tmp_betaR <- ifelse(beta, as.numeric(bf_results$beta_valueR), 0)
        Lidx <- ydays_dayrange <= as.numeric(bf_results$tm)
        Ridx <- ydays_dayrange > as.numeric(bf_results$tm)
        yfitL <- as.numeric(bf_results$B0L) + tmp_betaL * ydays_dayrange[Lidx] + as.numeric(bf_results$hL) / (sqrt(2*pi) * as.numeric(bf_results$sigmaL)) * exp(- (ydays_dayrange[Lidx] - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigmaL)^2))
        yfitR <- as.numeric(bf_results$B0R) + tmp_betaR * ydays_dayrange[Ridx] + as.numeric(bf_results$hR) / (sqrt(2*pi) * as.numeric(bf_results$sigmaR)) * exp(- (ydays_dayrange[Ridx] - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigmaR)^2))
        yfit <- c(yfitL, yfitR)
        
      }
      
      # add data to base plot
      p <- p +
        geom_line(data=data.frame(x=ydays_dayrange, yfit=yfit, stringsAsFactors = FALSE),
                  aes(x=x, y=yfit), color=fit_col) +
        geom_vline(xintercept=as.numeric(bf_results$ti), col=ind_col, alpha=ind_alpha) +
        geom_vline(xintercept=as.numeric(bf_results$tm), col=ind_col, alpha=ind_alpha) +
        geom_vline(xintercept=as.numeric(bf_results$tt), col=ind_col, alpha=ind_alpha)
      
    }
    
  } else if (fitmethod=="roc" | fitmethod=="thresh") {
    
    # If you use LOESS smoothing, add the line to the data
    # (otherwise just add vertical lines for indices below)
    if (smoothMethod == 'loess') {
      if (!bad_loess) {
        p <- p +
          geom_line(data=data.frame(x=t, yfit=y, stringsAsFactors = FALSE),
                    aes(x=x, y=yfit), color=fit_col)
      }
    }
    
    if (fitmethod=="thresh") {
      p <- p + geom_hline(yintercept=as.numeric(bf_results$thresh), col="red", alpha=0.4)
    }
    
    p <- p +
      geom_vline(xintercept=as.numeric(bf_results$ti), col=ind_col, alpha=ind_alpha) +
      geom_vline(xintercept=as.numeric(bf_results$tm), col=ind_col, alpha=ind_alpha) +
      geom_vline(xintercept=as.numeric(bf_results$tt), col=ind_col, alpha=ind_alpha)
    
  }
  
  # add the parameter table to the right side of the plot
  p <- p + annotation_custom(values_df, xmin=330, xmax=360, ymin=-Inf, ymax=Inf)
  
  return(list(p=p, fitparams=fitparams, chlall=chlall))
  
}

# Make a string to write to an output text file, containing the current user selections.
settings_str <- function(satellite, region, algorithm, year_list, date_var, interval, log_chla,
                         polygon_name_list, polygon_coord_list,
                         percent, outlier, dailystat, maxpixval,
                         fitmethod, bloomShape, smoothMethod, loessSpan=NULL,
                         t_range, ti_limits, tm_limits,
                         tm=NULL, beta=NULL, use_weights=NULL, threshcoef=NULL) {
  
  if (length(year_list) > 1) {
    if (year_list[1]==year_list[2]) {
      year_list <- year_list[1]
    } else {
      year_list <- paste0(year_list, collapse=" - ")
    }
  }
  
  info <- c("Satellite:", satellite, "",
            "Region:", region, "",
            "Algorithm:", algorithm, "",
            "Year(s):", year_list, "",
            "Date(s):", date_var, "",
            "Interval:", interval, "",
            "Chlorophyll-a logged:", log_chla, "",
            "Polygon(s):")
  
  for (i in 1:length(polygon_name_list)) {
    info <- c(info,
              paste0(polygon_name_list[[i]], ":"),
              paste0("\tLongitudes: ", paste(polygon_coord_list[[i]]$lon, collapse=", "),
                     "\n\tLatitudes: ", paste(polygon_coord_list[[i]]$lat, collapse=", ")), "")
  }
  
  info <- c(info,
            "Minimum ", interval, " percent coverage:", (percent * 100), "",
            "Outlier detection method:", ifelse(outlier=="none", "None",
                                                ifelse(outlier=="sd2", "+/- 2 sd",
                                                       ifelse(outlier=="sd3", "+/- 3 sd", "1.5 IQR"))), "",
            proper(interval), " statistic:", proper(dailystat), "",
            "Maximum pixel value used in statistics and fit:", maxpixval, "",
            "Fit method:", ifelse(fitmethod=="gauss", "Shifted Gaussian curve",
                                  ifelse(fitmethod=="roc", "Rate of change", "Threshold")), "",
            "Bloom fit shape:", bloomShape, "",
            "Smoothing method:", smoothMethod, "")
  
  if (smoothMethod=="LOESS smooth") {
    info <- c(info, "LOESS span:", loessSpan, "")
  }
  
  info <- c(info,
            "Allowed range of days for bloom fitting:", paste(t_range, collapse="-"), "",
            "Allowed range of days for bloom initiation:", paste(ti_limits, collapse="-"), "",
            "Allowed range of days for maximum concentration of bloom:", paste(tm_limits, collapse="-"), "")
  
  if (fitmethod == "Shifted Gaussian") {
    
    info <- c(info,
              "Use t[max] parameter:", tm, "",
              "Use beta parameter:", beta, "",
              "Weight fit points by ", interval, " percent coverage:", use_weights, "")
    
  } else if (fitmethod == "Threshold") {
    
    info <- c(info, "Threshold coefficient:", threshcoef, "")
    
  }
  
  return(info)
  
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
