# return stats and model fit for a selected year

full_run <- function(d, year, sscoords, polygon_list, pnames, dvecs, variable, dir_name, fullrunoutput_png=TRUE, fullrunoutput_statcsv=TRUE, tab_theme) {
    
    composite = d$composite
    dailystat = d$dailystat
    percent = d$percent
    log_chla = d$log_chla
    t_range = d$t_range
    concentration_type <- d$concentration_type
    cell_size_model1 <- d$cell_size_model1
    cell_size_model2 <- d$cell_size_model2
    poly_names <- polygon_list$name
    poly_IDs <- polygon_list$poly_id
    
    #***************************************************************************
    
    all_data <- get_data(d$region, d$sat_alg, year, d$yearday, composite, log_chla, length(sscoords), dvecs,
                         concentration_type, cell_size_model1, cell_size_model2)
    doy_vec <- all_data$doy_vec # days of the year, whether you're using daily, 4-day, or 8-day data
    
    full_fit_params <- list()
    
    for (reg_ind in 1:length(poly_names)) {
        
        poly_name <- poly_names[reg_ind]
        spdf <- polygon_list[reg_ind,]
        basef <- file.path(dir_name, paste0(year, "_", poly_IDs[reg_ind]))
        
        # Extract pixels that are within the polygon
        rchla <- subset_data(spdf,sscoords,all_data$sschla,d$pixrange1,d$pixrange2)
        
        #***********************************************************************
        # If data exists, get its statistics
        
        # Reset error message to NULL, then check if it should be changed and
        # printed instead of doing the model fit plot
        em <- NULL
        
        if (is.null(rchla)) {
            em <- "No data in the selected region"
            stats_df <- data.frame(matrix(nrow=1,ncol=13), stringsAsFactors=FALSE)
            colnames(stats_df) <- c("doy_vec", "lower_limit", "upper_limit", "mean_chl", "median_chl",
                                    "stdev_chl", "min_chl", "max_chl", "nobs", "percent_coverage", "model","background","loess")
        } else {
            stats_df <- dplyr::bind_cols(data.frame(doy=doy_vec), get_stats(rchla, d$outlier)) %>%
              dplyr::mutate(model=NA, background=NA, loess=NA)
            nobs <- stats_df$nobs
            first_day <- t_range[1]
            last_day <- t_range[2]
            daily_percov <- 100 * nobs / nrow(rchla)
            ind_percov <- daily_percov > percent
            ind_dayrange <- doy_vec >= first_day & doy_vec <= min(last_day, all_data$available_days)
            ind_dayrange_percov <- ind_percov & ind_dayrange
            # If there is no data available for the fit after removing days outside
            # the day range and with insufficient data, print an error message instead.
            if (sum(ind_dayrange_percov)==0) {
                em <- paste0("No data available between day ", first_day, " and ",
                             last_day, " with >= ", percent, "% coverage")
            }
        }
        
        
        #***********************************************************************
        # Get model fitted parameters and create plot, unless there's an error message
        
        if (is.null(em)) {
            if(dailystat == 'average'){
              chl_to_use <- stats_df$mean
            } else if(dailystat == 'median'){
              chl_to_use <- stats_df$median
            }
            bf_data <- bf_data_calc(composite=composite,
                                    chl_to_use = chl_to_use,
                                    ind_dayrange_percov = ind_dayrange_percov,
                                    ind_percov = ind_percov,
                                    ind_dayrange = ind_dayrange,
                                    daily_percov = daily_percov,
                                    t_range = c(first_day, last_day),
                                    log_chla = log_chla,
                                    doy_vec = doy_vec,
                                    variable = variable,
                                    sv = d)
            df_percov <- bf_data$df_percov
            df_dayrange <- bf_data$df_dayrange
            df_dayrange_percov <- bf_data$df_dayrange_percov
            # for output in annual stats csv, leave values in log space (if selected)
            stats_df$model[ind_dayrange] <- df_dayrange$yfit
            stats_df$background[ind_dayrange] <- df_dayrange$ybkrnd
            stats_df$loess[ind_dayrange_percov] <- df_dayrange_percov$loess
            fitparams <- bf_data$fitparams
        } else {
            fitparams <- rep(NA,length(pnames))
        }
        
        if (fullrunoutput_png) {
          p <- ggplot() + theme_bw() +
            ggtitle(paste0(ifelse(concentration_type=="model1", paste(proper(cell_size_model1),"cell size: "),
                                 ifelse(concentration_type=="model2", paste(proper(cell_size_model2),"cell size: "), "")),
                          "Time series of ", dailystat, " ", tolower(names(composites)[composites==composite]),
                          " chlorophyll concentration for ", year, ", ", poly_name))
          if (is.null(em)) {
            # for display on the plot, transform values back to linear space if necessary
            if (log_chla) {
              df_dayrange$yfit <- 10^df_dayrange$yfit
              df_dayrange$ybkrnd <- 10^df_dayrange$ybkrnd
              df_dayrange_percov$loess <- 10^df_dayrange_percov$loess
              df_percov$y <- 10^df_percov$y
            }
            bdf <- data.frame(Stat=gsub("beta","\u03B2",colnames(fitparams)),Value=as.character(round(unlist(fitparams),3)))
            p <- bf_data_plot(p=p,
                              nofit_msg=bf_data$nofit_msg,
                              composite=composite,
                              dailystat=dailystat,
                              log_chla=log_chla,
                              smoothMethod=d$smoothMethod,
                              fitmethod=d$fitmethod,
                              variable=variable,
                              fitparams=fitparams, # vertical model timing bars, threshold line, metrics table
                              df_percov=df_percov, # for individual points
                              df_dayrange=df_dayrange, # for loess line
                              df_dayrange_percov=df_dayrange_percov) + # for gaussian line
              geom_table_npc(data=bdf, aes(npcx=0.95, npcy=0.5), label=list(bdf),
                             table.theme=tab_theme,table.colnames=FALSE,table.rownames=FALSE)
          } else {
            p <- p + geom_text_npc(aes(npcx=0.5,npcy=0.5,label=em))
          }
          ggsave(file=paste0(basef,"_modelfit.png"), plot=p, width=13, height=5.5, units="in")
        }
        
        
        #***********************************************************************
        # Write stats to csv and add fit parameters for the current region to the full dataframe
        
        if (fullrunoutput_statcsv) write.csv(stats_df, file=paste0(basef,"_stats.csv"), quote=FALSE, na=" ", row.names=FALSE)
        full_fit_params[[reg_ind]] <- data.frame(matrix(c(toupper(poly_IDs[reg_ind]), year, as.numeric(fitparams)),nrow=1)) %>%
          setNames(c("Region", "Year", pnames))
        gc()
        
    }
    
    full_fit_params <- do.call(dplyr::bind_rows,full_fit_params)
    
    return(full_fit_params)
    
}
