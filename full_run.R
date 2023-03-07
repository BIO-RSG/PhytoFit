# return stats and bloom fit for a selected year

full_run <- function(d, year, sslat, sslon, polygon_list, pnames, doys_per_week, doy_week_start,
                     doy_week_end, dir_name, fullrunoutput_png=TRUE, fullrunoutput_statcsv=TRUE) {
    
    interval = d$interval
    dailystat = d$dailystat
    pixrange1 = d$pixrange1
    pixrange2 = d$pixrange2
    percent = d$percent
    log_chla = d$log_chla
    smoothMethod = d$smoothMethod
    t_range = d$t_range

    #***************************************************************************
    # load data and create base plot and plot title
    
    all_data <- get_data(d$region, d$sat_alg, year, d$yearday, interval,
                         log_chla, length(sslat), doys_per_week, doy_week_start, doy_week_end,
                         d$concentration_type, d$cell_size_model1, d$cell_size_model2)
    
    sschla <- all_data$sschla
    available_days <- all_data$available_days
    day_label <- all_data$day_label
    time_ind <- all_data$time_ind
    doy_vec <- all_data$doy_vec # days of the year, whether you're using daily or weekly data
    
    poly_names <- polygon_list$full_names
    poly_IDs <- polygon_list$poly_ID
    all_lons <- polygon_list$longitudes
    all_lats <- polygon_list$latitudes
    
    # create output dataframe for fitted coefficients
    full_fit_params <- data.frame(matrix(nrow=length(poly_names), ncol=(length(pnames)+2)), stringsAsFactors = FALSE)
    colnames(full_fit_params) <- c("Region", "Year", pnames)
    
    
    for (reg_ind in 1:length(poly_names)) {
        
        poly_name <- poly_names[reg_ind]
        
        Latitude <- all_lats[[reg_ind]]
        Longitude <- all_lons[[reg_ind]]
        
        plot_title <- paste0("Time series of ", interval, " ", dailystat, " Chlorophyll concentration for ", year, ", ", poly_name)
        
        # create base plot
        p <- ggplot() + theme_bw()
        
        
        #***********************************************************************
        # Subset by lats, lons of the current region
        
        # Create the mask for the subregion
        mask <- point.in.polygon(sslon, sslat, Longitude, Latitude)
        mask <- as.logical(mask)
        
        if (sum(mask)==0) {
            rchla <- NULL
        } else if (sum(mask)==1) {
            # make sure rchla is in matrix format
            rchla <- matrix(sschla[mask,], nrow=1)
        } else {
            rchla <- sschla[mask,]
        }
        
        # Should the range of pixel values used in the stats be restricted?
        # (i.e. set pixels beyond a threshold to NA?)
        if (!is.na(pixrange1)) {
            rchla[rchla < pixrange1] <- NA
        }
        if (!is.na(pixrange2)) {
            rchla[rchla > pixrange2] <- NA
        }
        if (all(is.na(rchla))) {rchla <- NULL}
        
        
        #***********************************************************************
        # If data exists, get its statistics
        
        # Reset error message to NULL, then check if it should be changed and
        # printed instead of doing the density plot
        em <- NULL
        
        if (is.null(rchla)) {
            em <- "No data in the selected region"
            stats_df <- data.frame(matrix(nrow=1,ncol=7), stringsAsFactors=FALSE)
            colnames(stats_df) <- c("mean_chl", "median_chl", "stdev_chl", "min_chl", "max_chl", "nobs", "percent_coverage")
        } else {
            all_stats <- get_stats(rchla, d$outlier)
            lenok <- all_stats$lenok
            chl_mean <- all_stats$chl_mean
            chl_median <- all_stats$chl_median
            stats_df <- data.frame(doy=doy_vec,
                                   mean_chl=chl_mean,
                                   median_chl=chl_median,
                                   stdev_chl=all_stats$chl_sd,
                                   min_chl=all_stats$chl_min,
                                   max_chl=all_stats$chl_max,
                                   nobs=all_stats$nobs,
                                   percent_coverage=all_stats$percent_coverage,
                                   stringsAsFactors=FALSE)
            first_day <- t_range[1]
            last_day <- t_range[2]
            daily_percov <- 100 * lenok / nrow(rchla)
            ind_percov <- daily_percov > percent
            ind_dayrange <- doy_vec >= first_day & doy_vec <= min(last_day, available_days)
            ind_dayrange_percov <- ind_percov & ind_dayrange
            ydays_percov <- doy_vec[ind_percov] # all days with high enough percent coverage
            ydays_dayrange <- doy_vec[ind_dayrange]
            ydays_dayrange_percov <- doy_vec[ind_dayrange_percov] # subset of days used for fit
            # If there is no data available for the fit after removing days outside
            # the day range and with insufficient data, print an error message instead.
            if (sum(ydays_dayrange_percov)==0) {
                em <- paste0("No data available between day ", first_day, " and ",
                             last_day, " with >= ", percent, "% coverage")
            }
        }
        
        
        #***********************************************************************
        # Get bloom fitted parameters and create plot, unless there's an error message
        
        if (is.null(em)) {
            bf_data <- get_bloom_fit_data(interval=interval,
                                          p=p,
                                          pnames = pnames,
                                          dailystat = dailystat,
                                          chl_mean = chl_mean,
                                          chl_median = chl_median,
                                          lenok = lenok,
                                          ind_dayrange_percov = ind_dayrange_percov,
                                          ind_percov = ind_percov,
                                          ydays_dayrange_percov = ydays_dayrange_percov,
                                          ydays_percov = ydays_percov,
                                          ydays_dayrange = ydays_dayrange,
                                          rchla_nrow = nrow(rchla),
                                          daily_percov = daily_percov,
                                          t_range = t_range,
                                          log_chla = log_chla,
                                          doy_vec = doy_vec,
                                          plot_title = plot_title,
                                          sv = d)
            loess_smooth <- rep(NA,length(daily_percov))
            if (smoothMethod == 'loess') {
                loess_smooth[ind_dayrange_percov] <- bf_data$y$y
            }
            stats_df$loess_smooth <- loess_smooth
            p <- bf_data$p
            fitparams <- bf_data$fitparams
        } else {
            p <- p + ggtitle(plot_title) + annotation_custom(grobTree(textGrob(em)))
            fitparams <- data.frame(matrix(rep(NA, length(pnames)), nrow=1), stringsAsFactors = FALSE)
            colnames(fitparams) <- pnames
        }
        
        
        #***********************************************************************
        # Write stats to csv and ggplot to png, and add fit parameters for the current region to the full dataframe
        
        basef <- file.path(dir_name, paste0(year, "_", poly_IDs[reg_ind]))
        if (fullrunoutput_statcsv) {
            write.csv(stats_df, file=paste0(basef,"_stats.csv"), quote=FALSE, na=" ", row.names=FALSE)
        }
        if (fullrunoutput_png) {
            ggsave(file=paste0(basef,"_bloomfit.png"), plot=p, width=12, height=5, units="in")
        }
        full_fit_params[reg_ind,] <- c(toupper(poly_IDs[reg_ind]), year, as.numeric(fitparams))
        
        gc()
        
    }
    
    return(full_fit_params)
    
}
