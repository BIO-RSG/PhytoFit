# return stats and model fit for a selected year

full_run <- function(d, year, sscoords, polygon_list, pnames, dvecs, variable, dir_name, fullrunoutput_png=TRUE, tab_theme) {
    
    poly_names <- polygon_list$name
    poly_IDs <- polygon_list$poly_id
    
    #***************************************************************************
    
    all_data <- get_data(d$region, d$sat_alg, year, d$composite, length(sscoords), dvecs,
                         d$concentration_type, d$cell_size_model1, d$cell_size_model2, variable$log)
    doy_vec <- all_data$doy_vec # days of the year, whether you're using daily, 4-day, or 8-day data
    
    full_fit_params <- list()
    
    for (reg_ind in 1:length(poly_names)) {
        
        poly_name <- poly_names[reg_ind]
        spdf <- polygon_list[reg_ind,]
        basef <- file.path(dir_name, paste0(year, "_", poly_IDs[reg_ind]))
        
        
        #***********************************************************************
        # annual_stats() and get_fit()
        
        # Extract pixels that are within the polygon
        rchla <- subset_data(spdf,sscoords,all_data$sschla,d$pixrange1,d$pixrange2)
        
        # Reset error message to NULL, then check if it should be changed and printed instead of doing the model fit plot
        em <- NULL
        
        if (is.null(rchla)) {
            em <- "No data in the selected region"
            stats_df <- data.frame(matrix(nrow=0,ncol=18), stringsAsFactors=FALSE)
            colnames(stats_df) <- c("doy", "lower_limit", "upper_limit", "mean", "median", "stdev", "min", "max", "nobs", "percent_coverage", "lower_limit_log10", "upper_limit_log10", "mean_log10", "median_log10", "stdev_log10", "model","background","loess")
        } else {
            stats_df <- dplyr::bind_cols(data.frame(doy=doy_vec), get_stats(rchla=rchla, outlier=d$outlier, logvar=variable$log))
            first_day <- d$t_range[1]
            last_day <- d$t_range[2]
            daily_percov <- stats_df$percent_coverage
            ind_percov <- daily_percov > d$percent
            ind_dayrange <- doy_vec >= first_day & doy_vec <= min(last_day, all_data$available_days)
            ind_dayrange_percov <- ind_percov & ind_dayrange
            # If there is no data available for the fit after removing days outside
            # the day range and with insufficient data, print an error message instead.
            if (sum(ind_dayrange_percov)==0) {
                em <- paste0("No data available between day ", first_day, " and ",
                             last_day, " with >= ", d$percent, "% coverage")
            }
        }
        
        if (is.null(em)) {
            if (d$log_chla) {
              if(d$dailystat == 'average'){
                chl_to_use <- log10(stats_df$mean_log10)
              } else if(d$dailystat == 'median'){
                chl_to_use <- log10(stats_df$median_log10)
              }
            } else {
              if(d$dailystat == 'average'){
                chl_to_use <- stats_df$mean
              } else if(d$dailystat == 'median'){
                chl_to_use <- stats_df$median
              }
            }
            bf_data <- bf_data_calc(composite=d$composite,
                                    chl_to_use = chl_to_use,
                                    ind_dayrange_percov = ind_dayrange_percov,
                                    ind_percov = ind_percov,
                                    ind_dayrange = ind_dayrange,
                                    daily_percov = daily_percov,
                                    t_range = c(first_day, last_day),
                                    log_chla = d$log_chla,
                                    doy_vec = doy_vec,
                                    variable = variable,
                                    sv = d)
            df_final <- bf_data$df_final
            fitparams <- bf_data$fitparams
        } else {
            df_final <- data.frame(doy=doy_vec) %>% dplyr::mutate(model=NA, background=NA, loess=NA)
            fitparams <- rep(NA,length(pnames))
        }

        
        #***********************************************************************
        # make_time_series_plot()
        
        if (fullrunoutput_png) {
          p <- ggplot() + theme_bw() +
            ggtitle(paste0(ifelse(d$concentration_type=="model1", paste(proper(d$cell_size_model1),"cell size: "),
                                 ifelse(d$concentration_type=="model2", paste(proper(d$cell_size_model2),"cell size: "), "")),
                          "Time series of ", d$dailystat, " ", tolower(names(composites)[composites==d$composite]),
                          " chlorophyll concentration for ", year, ", ", poly_name))
          if (is.null(em)) {
            bdf <- data.frame(Stat=gsub("beta","\u03B2",colnames(fitparams)),Value=as.character(round(unlist(fitparams),3)))
            p <- bf_data_plot(p=p,
                              nofit_msg=bf_data$nofit_msg,
                              composite=d$composite,
                              dailystat=d$dailystat,
                              log_display=d$log_display,
                              percent=d$percent,
                              smoothMethod=d$smoothMethod,
                              fitmethod=d$fitmethod,
                              variable=variable,
                              fitparams=fitparams, # vertical model timing bars, threshold line, metrics table
                              df_final=df_final) +
              geom_table_npc(data=bdf, aes(npcx=0.95, npcy=0.5), label=list(bdf),
                             table.theme=tab_theme,table.colnames=FALSE,table.rownames=FALSE)
          } else {
            p <- p + geom_text_npc(aes(npcx=0.5,npcy=0.5,label=em))
          }
          ggsave(file=paste0(basef,"_modelfit.png"), plot=p, width=13, height=5.5, units="in")
        }
        
        
        #***********************************************************************
        # Write stats to csv and add fit parameters for the current region to the full dataframe
        
        write.csv(dplyr::full_join(stats_df,df_final[,c("doy","model","background","loess")], by="doy"),
                  file=paste0(basef,"_stats.csv"), quote=FALSE, na=" ", row.names=FALSE)
        full_fit_params[[reg_ind]] <- data.frame(matrix(c(toupper(poly_IDs[reg_ind]), year, as.numeric(fitparams)),nrow=1)) %>%
          setNames(c("Region", "Year", pnames))
        gc()
        
    }
    
    full_fit_params <- do.call(dplyr::bind_rows,full_fit_params)
    
    return(full_fit_params)
    
}
