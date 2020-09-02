# return stats and bloom fit for a selected year

full_run <- function(year, satellite, region, algorithm, interval, sslat, sslon,
                     boxes, latlon_method, pnames, yearday, doys_per_week, doy_week_start, doy_week_end,
                     dailystat, pixrange1, pixrange2, outlier, percent, log_chla, poly_names,
                     fitmethod, bloomShape, smoothMethod, loessSpan=NA, use_weights,
                     threshcoef=NA, tm=FALSE, beta=FALSE, t_range = c(1,365),
                     tm_limits = c(1,365), ti_limits = c(1,365), dir_name) {
    
    
    #***************************************************************************
    # load data and create base plot and plot title
    
    all_data <- get_data(region, satellite, algorithm, year, yearday, interval,
                         log_chla, length(sslat), doys_per_week, doy_week_start, doy_week_end)
    
    sschla <- all_data$sschla
    available_days <- all_data$available_days
    day_label <- all_data$day_label
    time_ind <- all_data$time_ind
    doy_vec <- all_data$doy_vec # days of the year, whether you're using daily or weekly data
    
    # create output dataframe for fitted coefficients
    full_fit_params <- data.frame(matrix(nrow=length(boxes), ncol=(length(pnames)+2)), stringsAsFactors = FALSE)
    colnames(full_fit_params) <- c("Region", "Year", sub("\u03B2", "beta", pnames))
    
    for (reg_ind in 1:length(boxes)) {
        
        poly_name <- poly_names[reg_ind]
        box <- boxes[[reg_ind]]
        
        Latitude <- box$lat
        Longitude <- box$lon
        
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
            
            # process full annual stats
            all_stats <- get_stats(rchla, outlier)
            limits <- all_stats$limits
            lenok <- all_stats$lenok
            chl_mean <- all_stats$chl_mean
            chl_median <- all_stats$chl_median
            chl_sd <- all_stats$chl_sd
            chl_min <- all_stats$chl_min
            chl_max <- all_stats$chl_max
            nobs <- all_stats$nobs
            percent_coverage <- all_stats$percent_coverage
            
            first_day <- t_range[1]
            last_day <- t_range[2]
            
            daily_percov <- lenok / nrow(rchla)
            ind_percov <- daily_percov > percent
            ind_dayrange <- doy_vec > first_day & doy_vec <= min(last_day-1, available_days)
            ind_dayrange_percov <- ind_percov & ind_dayrange
            ydays_percov <- doy_vec[ind_percov] # all days with high enough percent coverage
            ydays_dayrange <- doy_vec[ind_dayrange]
            ydays_dayrange_percov <- doy_vec[ind_dayrange_percov] # subset of days used for fit
            
            # If there is no data available for the fit after removing days outside
            # the day range and with insufficient data, print an error message instead.
            if (sum(ydays_dayrange_percov)==0) {
                
                em <- paste0("No data available between day ", first_day, " and ",
                             last_day, " with >= ", (percent*100), "% coverage")
                
            }
            
            stats_df <- data.frame(doy=doy_vec,
                                   mean_chl=chl_mean,
                                   median_chl=chl_median,
                                   stdev_chl=chl_sd,
                                   min_chl=chl_min,
                                   max_chl=chl_max,
                                   nobs=nobs,
                                   percent_coverage=percent_coverage,
                                   stringsAsFactors=FALSE)
            
        }
        
        
        #***********************************************************************
        # Get bloom fitted parameters and create plot, unless there's an error message
        
        if (is.null(em)) {
        
            bf_data <- get_bloom_fit_data(interval=isolate(state$interval),
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
                                          use_weights = use_weights,
                                          smoothMethod = smoothMethod,
                                          loessSpan = loessSpan,
                                          fitmethod = fitmethod,
                                          bloomShape = bloomShape,
                                          daily_percov = daily_percov,
                                          tm = tm,
                                          beta = beta,
                                          tm_limits = tm_limits,
                                          ti_limits = ti_limits,
                                          log_chla = log_chla,
                                          threshcoef = threshcoef,
                                          doy_vec = doy_vec,
                                          plot_title = plot_title)
            
            p <- bf_data$p
            fitparams <- bf_data$fitparams
            
            
        } else {
            
            p <- p +
                ggtitle(plot_title) +
                annotation_custom(grobTree(textGrob(em)))
            
            fitparams <- data.frame(parameter=pnames,
                                    value=rep(NA, length(pnames)),
                                    stringsAsFactors = FALSE)
            
        }
        
        
        
        #***********************************************************************
        # Write stats to csv and ggplot to png, and add fit parameters for the current region to the full dataframe
        
        write.csv(stats_df,
                  file=file.path(dir_name, "stats_csv", paste0(year, "_", names(boxes)[reg_ind], "_stats.csv")),
                  quote=FALSE,
                  na=" ",
                  row.names=FALSE)

        ggsave(file=file.path(dir_name, "bloom_fit_pngs", paste0(year, "_", names(boxes)[reg_ind], "_bloomfit.png")),
               plot=p,
               width=12,
               height=5,
               units="in")
        
        full_fit_params[reg_ind,] <- c(toupper(names(boxes)[reg_ind]), year, fitparams$value)
        
        gc()
        
    }
    
    return(full_fit_params)
    
}
