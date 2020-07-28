# return stats and bloom fit for a selected year

full_run <- function(year, satellite, region, boxes, latlon_method, pnames,
                     dailystat, maxpixval, outlier, percent, log_chla, poly_names, ydays,
                     fitmethod, bloomShape, smoothMethod, loessSpan=NA, use_weights,
                     threshcoef=NA, tm=FALSE, beta=FALSE, t_range = c(1,365),
                     tm_limits = c(1,365), ti_limits = c(1,365), dir_name) {
    
    
    #***************************************************************************
    # load data and create base plot and plot title
    
    fileall <- paste0('./data/', region, '/', satellite, year, 'ss.rda')
    load(fileall)
    
    if (log_chla) {
        sschla <- log(sschla)
    }
    
    # create output dataframe for fitted coefficients
    full_fit_params <- data.frame(matrix(nrow=length(boxes), ncol=(length(pnames)+2)), stringsAsFactors = FALSE)
    colnames(full_fit_params) <- c("Region", "Year", pnames)
    
    
    for (reg_ind in 1:length(boxes)) {
        
        poly_name <- poly_names[reg_ind]
        box <- boxes[[reg_ind]]
        
        Latitude <- box$lat
        Longitude <- box$lon
        
        
        plot_title <- paste0('Time series of daily ', dailystat,
                             ' Chlorophyll concentration for ',
                             year, ', ', poly_name)
        
        # create base plot
        p <- ggplot() + theme_bw()
        
        
        #***************************************************************************
        # subset by lats, lons
        
        
        # Create the mask for the subregion
        mask <- point.in.polygon(sslon, sslat, Longitude, Latitude)
        mask <- as.logical(mask)
        
        if (sum(mask)==0) {
            rchla <- NULL
        } else if (sum(mask)==1) {
            # make sure rchla is in matrix format
            rchla <- matrix(sschla[,mask], ncol=1)
        } else {
            rchla <- sschla[,mask]
        }
        
        # Should the range of pixel values used in the stats be restricted?
        # (i.e. set pixels beyond a threshold to NA?)
        if (!is.na(maxpixval)) {
            rchla[rchla > maxpixval] <- NA
        }
        if (all(is.na(rchla))) {rchla <- NULL}
        
        
        
        # Reset error message to NULL, then check if it should be changed and
        # printed instead of doing the density plot
        em <- NULL
        
        if (is.null(rchla)) {
            
            em <- "No data in the selected region"
            
            stats_df <- data.frame(mean_chl=NA,
                                   median_chl=NA,
                                   stdev_chl=NA,
                                   min_chl=NA,
                                   max_chl=NA,
                                   nobs=NA,
                                   percent_coverage=NA,
                                   stringsAsFactors=FALSE)
            
        } else {
            
            
            #***********************************************************************
            # process full annual stats
            
            # Outlier method
            chl_mean <- apply(rchla, 1, mean, na.rm = TRUE)
            chl_sd <- apply(rchla, 1, sd, na.rm = TRUE)  
            chl_median <- apply(rchla, 1, median, na.rm = TRUE)
            iqr <- apply(rchla, 1, IQR, na.rm = TRUE) 
            limits <- matrix(nrow = length(sd), ncol = 2)
            if(outlier == 'sd2'){
                limits <- matrix(nrow = length(chl_sd), ncol = 2)
                limits[,1] <- -1 * 2 * chl_sd + chl_mean
                limits[,2] <- 1 * 2 * chl_sd + chl_mean
            } else if (outlier == 'sd3'){
                limits <- matrix(nrow = length(chl_sd), ncol = 2)
                limits[,1] <- -1 * 3 * chl_sd + chl_mean
                limits[,2] <- 1 * 3 * chl_sd + chl_mean
            } else if (outlier == 'iqr15'){
                limits <- matrix(nrow = length(iqr), ncol = 2)
                limits[,1] <- -1 * 1.5 * iqr + chl_median
                limits[,2] <- 1 * 1.5 * iqr + chl_median
            }
            
            # Extra stats
            # Note: can't use "min" and "max" functions alone because unlike the
            # mean and median functions, which return NA if all their input is
            # NA, min and max return Inf
            chl_min <- sapply(1:nrow(rchla), function(i) {ifelse(all(is.na(rchla[i,])), NaN, min(rchla[i,], na.rm = TRUE))})
            chl_max <- sapply(1:nrow(rchla), function(i) {ifelse(all(is.na(rchla[i,])), NaN, max(rchla[i,], na.rm = TRUE))})
            nobs <- as.numeric(sapply(1:nrow(rchla), function(i) {sum(!is.na(rchla[i,]))}))
            percent_coverage <- (nobs/ncol(rchla))*100
            
            # OUTLIERS
            # remove outliers based on selected method and obtain indices where
            # data coverage is greater than defined percentage
            lenok <- vector(mode = 'logical', length = nrow(rchla))
            for (i in 1:nrow(rchla)) {
                d <- rchla[i,]
                ok <- which(!is.na(d))
                if(outlier != 'none'){
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
                    
                }
                lenok[i] <- length(ok) 
            }
            
            first_day <- t_range[1]
            last_day <- t_range[2]
            
            daily_percov <- lenok / ncol(rchla)
            ind_percov <- daily_percov > percent
            ind_dayrange <- ydays > first_day & ydays < last_day
            ind_dayrange_percov <- ind_percov & ind_dayrange
            ydays_percov <- ydays[ind_percov] # all days with high enough percent coverage
            ydays_dayrange <- ydays[ind_dayrange]
            ydays_dayrange_percov <- ydays[ind_dayrange_percov] # subset of days used for fit
            
            # If there is no data available for the fit after removing days outside
            # the day range and with insufficient data, print an error message instead.
            if (sum(ydays_dayrange_percov)==0) {
                
                em <- paste0("No data available between day ", first_day, " and ",
                             last_day, " with >= ", (percent*100), "% coverage")
                
            }
            
            stats_df <- data.frame(mean_chl=chl_mean,
                                   median_chl=chl_median,
                                   stdev_chl=chl_sd,
                                   min_chl=chl_min,
                                   max_chl=chl_max,
                                   nobs=nobs,
                                   percent_coverage=percent_coverage,
                                   stringsAsFactors=FALSE)
            
        }
        
        
        
        
        # DO FIT AND MAKE PLOT, unless there's an error message
        if (is.null(em)) {
        
            #***********************************************************************
            # compute bloom fit
                
            # Get vector of chlorophyll based on daily statistic and valid indices
            if(dailystat == 'avg'){
                chlorophyll <- chl_mean[ind_dayrange_percov]
                chlall <- chl_mean[ind_percov]
            } else if(dailystat == 'med'){
                chlorophyll <- chl_median[ind_dayrange_percov]
                chlall <- chl_median[ind_percov]
            }
            
            # Create final day/chlorophyll vectors for the fit, smoothed or not
            t <- ydays_dayrange_percov
            if (smoothMethod == 'loess'){
                mod <- try(loess(chlorophyll ~ ydays_dayrange_percov, span = loessSpan, degree = 2),
                           silent=TRUE)
                
                bad_loess <- class(mod)=="try-error" | is.null(mod)
                
                if (bad_loess) {
                    y <- chlorophyll
                } else {
                    y <- fitted(mod)
                }
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
                
                gauss_res <- gaussFit(t = t, y = y, w = weights,
                                      bloomShape = bloomShape,
                                      tm = tm,
                                      beta = beta,
                                      tm_limits = tm_limits,
                                      ti_limits = ti_limits,
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
            values_df <- tableGrob(d = data.frame(parameter=sapply(1:length(pnames), function(ip) {sub("beta", "\u03B2", pnames[ip])}),
                                                  value=tmp_v,
                                                  stringsAsFactors = FALSE),
                                   rows = NULL, cols = NULL,
                                   # Define theme to parse plotmath expressions
                                   theme = ttheme_minimal(core=list(fg_params=list(parse=TRUE, hjust=0, x=0.02),
                                                                    bg_params = list(fill="white", alpha=0.6)),
                                                          base_size=10,
                                                          padding=unit(c(1,1), "mm")))
            
            
            #*******************************************************************
            # PLOT FIT
            
            alldays <- (1:nrow(rchla))
            
            # get actual data (all days with good percent coverage)
            allchl <- rep(NA,nrow(rchla))
            allchl[ind_percov] <- chlall
            allpercov <- rep(NA,nrow(rchla))
            allpercov[ind_percov] <- (lenok * 100 / ncol(rchla))[ind_percov]
            
            # dataframe with x, y, fitted y, and percent coverage for plotting
            # (points are sized by percent coverage)
            data_df <- data.frame(x=alldays,
                                  y=allchl,
                                  percov=allpercov,
                                  stringsAsFactors = FALSE)
            
            # initialize and format base plot
            p <- p +
                geom_point(data=data_df, aes(x=x, y=y, size=percov), alpha=0.6) +
                ggtitle(plot_title) +
                labs(x='Day number',
                     y=ifelse(dailystat == 'avg',
                              expression('Daily average Chlorophyll ' * '[' * mg/m^3 * ']'),
                              expression('Daily median Chlorophyll' * '[' * mg/m^3 * ']'))) +
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
            
            # color of fit line, based on choice of mean/median daily statistic,
            # matched with the mean/median vertical bar coloring in the density plot
            fit_col <- ifelse(dailystat=="avg","dodgerblue2","red2")
            
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
                    
                    # add data to base plot
                    p <- p +
                        annotation_custom(grobTree(textGrob("unable to fit",
                                                            x=0.018, y=0.93,hjust=0,
                                                            gp=gpar(fontsize=16, col="red", fontface="bold"))))
                    
                } else {
                    
                    # fitted data (everything in the day range)
                    if (bloomShape=="symmetric") {
                        
                        tmp_beta <- ifelse(beta, as.numeric(bf_results$beta_value), 0)
                        yfit <- as.numeric(bf_results$B0) + tmp_beta * ydays_dayrange + as.numeric(bf_results$h) / (sqrt(2*pi) * as.numeric(bf_results$sigma)) * exp(- (ydays_dayrange - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigma)^2))
                        
                        # adjust vertical location of parameter table
                        table_yminloc <- maxy - (3/5) * table_ydiff
                        table_ymaxloc <- maxy
                        
                    } else if (bloomShape=="asymmetric") {
                        
                        tmp_betaL <- ifelse(beta, as.numeric(bf_results$beta_valueL), 0)
                        tmp_betaR <- ifelse(beta, as.numeric(bf_results$beta_valueR), 0)
                        Lidx <- ydays_dayrange <= as.numeric(bf_results$tm)
                        Ridx <- ydays_dayrange > as.numeric(bf_results$tm)
                        yfitL <- as.numeric(bf_results$B0L) + tmp_betaL * ydays_dayrange[Lidx] + as.numeric(bf_results$hL) / (sqrt(2*pi) * as.numeric(bf_results$sigmaL)) * exp(- (ydays_dayrange[Lidx] - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigmaL)^2))
                        yfitR <- as.numeric(bf_results$B0R) + tmp_betaR * ydays_dayrange[Ridx] + as.numeric(bf_results$hR) / (sqrt(2*pi) * as.numeric(bf_results$sigmaR)) * exp(- (ydays_dayrange[Ridx] - as.numeric(bf_results$tm))^2 / (2 * as.numeric(bf_results$sigmaR)^2))
                        yfit <- c(yfitL, yfitR)
                        
                        table_yminloc <- -Inf
                        table_ymaxloc <- Inf
                        
                    }
                    
                    # add data to base plot
                    p <- p +
                        geom_line(data=data.frame(x=ydays_dayrange, yfit=yfit, stringsAsFactors = FALSE),
                                  aes(x=x, y=yfit), color=fit_col) +
                        geom_vline(xintercept=as.numeric(bf_results$ti), col=ind_col, alpha=ind_alpha) +
                        geom_vline(xintercept=as.numeric(bf_results$tm), col=ind_col, alpha=ind_alpha) +
                        geom_vline(xintercept=as.numeric(bf_results$tt), col=ind_col, alpha=ind_alpha) +
                        annotation_custom(values_df, xmin=330, xmax=360,
                                          ymin=table_yminloc, ymax=table_ymaxloc)
                    
                }
                
            } else if (fitmethod == 'roc' | fitmethod=="thresh") {
                
                table_ymaxloc <- maxy
                table_yminloc <- maxy - (2/5) * table_ydiff
                
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
                    geom_vline(xintercept=as.numeric(bf_results$tt), col=ind_col, alpha=ind_alpha) +
                    annotation_custom(values_df, xmin=330, xmax=360,
                                      ymin=table_yminloc, ymax=table_ymaxloc)
                
            }
            
            
        } else {
            
            p <- p +
                ggtitle(plot_title) +
                annotation_custom(grobTree(textGrob(em)))
            
            fitparams <- data.frame(parameter=pnames,
                                    value=rep(NA, length(pnames)),
                                    stringsAsFactors = FALSE)
            
        }
        
        
        
        #***************************************************************************
        # write stats to csv and ggplot to png, and add fit parameters for the current region to the full dataframe
        
        write.csv(stats_df,
                  file=paste0(dir_name, "/stats_csv/", year, "_", names(boxes)[reg_ind], "_stats.csv"),
                  quote=FALSE,
                  na="",
                  row.names=FALSE)

        ggsave(file=paste0(dir_name, "/bloom_fit_pngs/", year, "_", names(boxes)[reg_ind], "_bloomfit.png"),
               plot=p,
               width=12,
               height=5,
               units="in")
        
        
        full_fit_params[reg_ind,] <- c(toupper(names(boxes)[reg_ind]), year, fitparams$value)
        
        gc()
        
    }
    
    return(full_fit_params)
    
}