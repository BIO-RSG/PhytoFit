
# custom functions from oceancolouR package
proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
pad0 <- function (s, len) stringr::str_pad(s, width = len, side = "left", pad = "0")
rm_dup <- function (x, ch) gsub(paste0("([", ch, "])\\1+"), "\\1", x)
plus_minus <- function (x, n) return((x - n):(x + n))
find_line <- function (x1, y1, x2, y2) {
  m <- (y2 - y1)/(x2 - x1)
  b <- y1 - m * x1
  return(list(slope=m, intercept=b))
}
avg_columns <- function (mat, dlist=NULL, year=NULL) {
  new_mat <- lapply(1:length(dlist), function(x) rowMeans(matrix(mat[, dlist[[x]]], nrow=nrow(mat)), na.rm=TRUE))
  return(do.call(cbind, new_mat))
}

# deprecated shinyWidgets function
# https://github.com/dreamRs/shinyWidgets/blob/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/setSliderColor.R
setSliderColor <- function(color, sliderId) {
  # the css class for ionrangeslider starts from 0 so remove 1 from sliderId
  sliderId <- sliderId - 1
  # create custom css background for each slider selected by the user
  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i+1],
      "; border-top: 1px solid ", color[i+1],
      "; border-bottom: 1px solid ", color[i+1],
      ";}"
    )
  })
  # insert this custom css code in the head of the shiny app
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

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
get_data <- function(region, sat_alg, year, composite, num_pix, dvecs,
                     concentration_type="full", cell_size_model1="small", cell_size_model2="small", loggable) {
  
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
  
  if (loggable) {sschla[sschla<=0] <- NA}
  
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
get_stats <- function(rchla, outlier, logvar=TRUE) {
  
  df <- data.frame(matrix(nrow=ncol(rchla),ncol=14))
  colnames(df) <- c('lower_limit','upper_limit','mean','median','stdev','min','max','nobs','percent_coverage','lower_limit_log10','upper_limit_log10','mean_log10','median_log10','stdev_log10')
  df$nobs <- df$percent_coverage <- 0

  if (nrow(rchla)==1) { # if your polygon only has one pixel in it
    
    rchlav <- as.numeric(rchla)
    df$lower_limit <- df$upper_limit <- df$mean <- df$median <- df$min <- df$max <- rchlav
    df$stdev <- NA
    df$nobs <- as.numeric(is.finite(rchlav))
    df$percent_coverage <- 100*df$nobs
    if (logvar) {
      lr <- log10(rchlav)
      lr[!is.finite(lr)] <- NA
      df$lower_limit_log10 <- df$upper_limit_log10 <- df$mean_log10 <- df$median_log10 <- lr
      df$stdev_log10 <- NA
    }
    
  } else { # if your polygon has multiple pixels
    
    statfn <- function(rchla,nobs,log) {
      if (outlier=="none") {
        rchla_good <- lapply(1:ncol(rchla), function(i) {x <- rchla[,i]; x[is.finite(x)]})
        tmp <- dplyr::bind_cols(
          data.frame(lower_limit=NA, upper_limit=NA),
          lapply(1:ncol(rchla), function(i) {
            x <- rchla_good[[i]]
            data.frame(mean=mean(x),median=median(x),stdev=sd(x))
          }) %>% do.call(what=dplyr::bind_rows))
        if (!log) {
          tmp <- dplyr::bind_cols(
            tmp,
            lapply(1:ncol(rchla), function(i) {x <- rchla_good[[i]]; data.frame(min=min(x),max=max(x))}) %>% do.call(what=dplyr::bind_rows),
            data.frame(nobs=nobs, percent_coverage=100*nobs/nrow(rchla)))
        }
      } else {
        if (startsWith(outlier,"sd")) {
          sdfactor <- as.numeric(gsub("sd","",outlier))
          outliermean <- colMeans(rchla,na.rm=TRUE)
          outliersd <- rep(NA,length(outliermean))
          outliersd[nobs==1] <- 0
          outliersd[nobs>1] <- apply(rchla[,nobs>1],MARGIN=2,FUN=sd,na.rm=TRUE)
          limits <- data.frame(lower_limit=outliermean-sdfactor*outliersd,
                               upper_limit=outliermean+sdfactor*outliersd)
        } else {
          if (outlier=="iqr15") {
            limits <- lapply(1:ncol(rchla), function(i) {
              x <- rchla[,i]
              x <- x[is.finite(x)]
              iqr <- IQR(x)
              data.frame(lower_limit=quantile(x,probs=0.25)-1.5*iqr, upper_limit=quantile(x,probs=0.75)+1.5*iqr)
            }) %>% do.call(what=dplyr::bind_rows)
          } else if (startsWith(outlier,"q")) {
            qnum <- as.numeric(paste0(substr(outlier,2,3),".",substr(outlier,4,5)))/100
            limits <- lapply(1:ncol(rchla), function(i) {
              x <- rchla[,i]
              x <- x[is.finite(x)]
              iqr <- IQR(x)
              data.frame(matrix(quantile(x,probs=c(qnum,1-qnum)),nrow=1)) %>% setNames(c("lower_limit","upper_limit"))
            }) %>% do.call(what=dplyr::bind_rows)
          }
        }
        rchla_good <- lapply(1:ncol(rchla), function(i) {x <- rchla[,i]; tmpi <- limits[i,]; x[is.finite(x) & between(x,tmpi$lower_limit,tmpi$upper_limit)]})
        tmp <- dplyr::bind_cols(
          limits,
          lapply(1:ncol(rchla), function(i) {
            x <- rchla_good[[i]]
            if (length(x)==0) return(data.frame(mean=NA,median=NA,stdev=NA))
            data.frame(mean=mean(x),median=median(x),stdev=sd(x))
          }) %>% do.call(what=dplyr::bind_rows))
        if (!log) {
          nrowrchla <- nrow(rchla)
          tmp <- dplyr::bind_cols(
            tmp,
            lapply(1:ncol(rchla), function(i) {
              x <- rchla_good[[i]]
              if (length(x)==0) return(data.frame(min=NA,max=NA,nobs=0,percent_coverage=0))
              data.frame(min=min(x),max=max(x),nobs=length(x)) %>% dplyr::mutate(percent_coverage=100*nobs/nrowrchla)
            }) %>% do.call(what=dplyr::bind_rows))
        }
      }
      return(tmp)
    }
    nobs <- colSums(!is.na(rchla))
    good <- nobs > 0
    rchla <- matrix(rchla[,good],nrow=nrow(rchla))
    df[good,1:9] <- statfn(rchla,nobs[good],log=FALSE)
    if (logvar) {
      dftmp <- statfn(log10(rchla),nobs[good],log=TRUE) %>% lapply(FUN=function(x) 10^x) %>% do.call(what=dplyr::bind_cols)
      colnames(dftmp) <- paste0(colnames(dftmp),"_log10")
      df[good,10:14] <- dftmp
    }
    
  }
  
  return(df)
  
}


# chl_to_use is the mean or the median (logged or not), depending on user selection - it's not subset
bf_data_calc <- function(composite, chl_to_use, ind_dayrange_percov, ind_percov, ind_dayrange,
                         daily_percov, t_range, log_chla, doy_vec, variable, sv) {
  
  nofit_msg <- NULL
  yfit <- ybkrnd <- rep(NA, sum(ind_dayrange))
  
  # make a dataframe of input vectors
  # roc and thresh background lines, loess, and the scatterplot points (sized by percent coverage) use ind_percov
  # roc and thresh models, and gauss use ind_dayrange_percov (all chl within the day range, with sufficient percent coverage)
  # yfit and ybkrnd use ind_dayrange
  df_input <- data.frame(yday=doy_vec, var=chl_to_use, var_to_fit=chl_to_use) %>% dplyr::mutate(weight=1)
  if (sv$use_weights) {df_input$weight <- daily_percov}
  
  # make a dataframe for output
  df_output <- data.frame(doy=doy_vec, percent_coverage=daily_percov, bfy=df_input$var) %>% dplyr::mutate(model=NA,background=NA,loess=NA)
  
  # use loess smooth on the real data points
  if (sv$smoothMethod == 'loess'){
    mod <- try(loess(var_to_fit ~ yday, data=df_input[ind_percov,], weights=df_input$weight[ind_percov], span=sv$loessSpan, degree=2), silent=TRUE)
    if (!(class(mod)=="try-error" | is.null(mod))) {
      df_output$loess[ind_percov] <- df_input$var_to_fit[ind_percov] <- fitted(mod) # use loess for fitting instead of real values
      df_input$weight <- 1 # reset weights so they aren't used again in a gaussian fit
    }
  }
  
  if (sv$fitmethod == 'gauss') {
    
    if (sv$tm) {tmp_ti_lim <- c(1,365)
    } else {tmp_ti_lim <- sv$ti_limits}
    gauss_res <- gaussFit(dfin=df_input[ind_dayrange_percov,],
                          bloomShape=sv$bloomShape,
                          tm=sv$tm, beta=sv$beta,
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
                          ydays_dayrange=df_input$yday[ind_dayrange],
                          rm_bkrnd=sv$rm_bkrnd,
                          ti_threshold_type=sv$ti_threshold_type,
                          ti_threshold_constant=sv$ti_threshold_constant)
    fitparams <- gauss_res$values
    # calculate rmse if non-null fit exists
    if (is.null(gauss_res$fit)) {
      fitparams$RMSE <- NA
      fitparams$RMSLE <- NA
      nofit_msg <- gauss_res$nofit_msg
    } else {
      rmsedf <- data.frame(x=df_input$var[ind_dayrange_percov],
                           y=predict(gauss_res$fit, newdata=list(t=df_input$yday[ind_dayrange_percov])))
      if (log_chla) {rmsedf <- rmsedf %>% dplyr::mutate(x=10^x,  y=10^y)}
      fitparams$RMSE <- sqrt(mean((rmsedf$x - rmsedf$y)^2, na.rm = TRUE))
      rmsedf <- rmsedf %>% dplyr::mutate(x=log10(x),  y=log10(y)) %>% dplyr::filter(is.finite(x) & is.finite(y))
      fitparams$RMSLE <- sqrt(mean((rmsedf$x - rmsedf$y)^2, na.rm = TRUE))
      yfit <- gauss_res$yfit
      ybkrnd <- gauss_res$ybkrnd
    }
    # round tmax_fit day
    fitparams[,"t[max_fit]"] <- round(as.numeric(fitparams[,"t[max_fit]"]))
  
  } else {
    
    if (sv$fitmethod == 'roc') {
      fitparams <- rateOfChange(dfin=df_input[ind_dayrange_percov,],
                                dfin_all=df_input[ind_percov,],
                                 bloomShape=sv$bloomShape,
                                 tm_limits=sv$tm_limits,
                                 ti_limits=sv$ti_limits,
                                 log_chla=log_chla,
                                 rm_bkrnd=sv$rm_bkrnd,
                                use_weights=sv$use_weights)
    } else if (sv$fitmethod == "thresh") {
      fitparams <- threshold(dfin=df_input[ind_dayrange_percov,],
                             dfin_all=df_input[ind_percov,],
                              threshcoef=sv$threshcoef, 
                              bloomShape=sv$bloomShape,
                              tm_limits=sv$tm_limits,
                              ti_limits=sv$ti_limits,
                              log_chla=log_chla,
                              rm_bkrnd=sv$rm_bkrnd,
                             use_weights=sv$use_weights)
    }
    
    nofit_msg <- fitparams$nofit_msg
    modelfit <- fitparams$yfit
    if (!is.null(modelfit)) {
      ybkrnd <- predict(modelfit,newdata=data.frame(tall=df_input$yday[ind_dayrange]))
    }
    fitparams <- fitparams$values
    
  }
  
  # round more days
  tvars <- c("t[start]", "t[max_real]", "t[end]", "t[duration]")
  fitparams[,tvars] <- round(as.numeric(fitparams[,tvars]))
  # add extra summary stats 
  bf_extra <- data.frame(Annual_Mean = mean(chl_to_use, na.rm=TRUE),
                         Annual_Median = median(chl_to_use, na.rm=TRUE),
                         Annual_StDev = sd(chl_to_use, na.rm=TRUE))
  fitparams <- dplyr::bind_cols(bf_extra,fitparams)
  
  if (log_chla) {
    df_output$bfy <- 10^df_output$bfy
    df_output$loess <- 10^df_output$loess
    yfit <- 10^yfit
    ybkrnd <- 10^ybkrnd
    fitparams$Annual_Mean <- 10^fitparams$Annual_Mean
    fitparams$Annual_Median <- 10^fitparams$Annual_Median
    fitparams$Annual_StDev <- 10^fitparams$Annual_StDev
  }
  
  # after all the modelling and transformations, add to output dataframe
  df_output$model[ind_dayrange] <- yfit
  df_output$background[ind_dayrange] <- ybkrnd
  
  return(list(fitparams=fitparams, nofit_msg=nofit_msg, df_final=df_output))
  
}

bf_data_plot <- function(p, nofit_msg, composite, dailystat, log_display, percent, smoothMethod, fitmethod, variable, fitparams, df_final) {
  
  # add points sized by percent coverage, vertical model timing bars, and do other formatting
  x.days <- 1:365
  x.labs <- rep("",length(x.days))
  x.labs[c(16, 46, 77, 106, 137 ,167, 198, 229, 259, 290, 320, 350)] <- month.abb
  # the added .1 here is so the minor lines don't coincide with the major lines and get disabled by element_blank()
  x.minor <- as.numeric(paste0(c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),".1"))
  
  df_final <- df_final %>% dplyr::mutate(sufficient_percov=percent_coverage>=percent)
  
  p <- p +
    geom_point(data=df_final, aes(x=doy, y=bfy, size=percent_coverage, alpha=sufficient_percov)) +
    geom_vline(xintercept=as.numeric(fitparams[,c("t[start]","t[max_real]","t[end]")]), col="black", alpha=0.6) +
    labs(x='Month',size="% coverage") +
    # scale_x_continuous(limits=c(0,365), breaks=seq(0,365,by=50)) +
    scale_x_continuous(expand=c(0.01,0.01), limits=c(1,365), breaks=x.days, labels=x.labs, minor_breaks=x.minor)+
    scale_size_area(minor_breaks=c(25, 50, 75, 100), limits=c(0, 100), max_size=10) +
    scale_alpha_manual(values=c(0.2,0.7), breaks=c(FALSE,TRUE), guide="none") +
    theme(legend.position="bottom",
          legend.title=element_text(size=14),
          legend.text=element_text(size=12),
          axis.ticks.x.bottom=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_line(),
          axis.title=element_text(size=14),
          axis.text=element_text(size=12),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.4)) +
    labs(y=bquote(.(proper(dailystat)) * " " * .(tolower(names(composites)[composites==composite])) * " " * .(variable$timeseries_ytitle)))
  if (any(is.finite(df_final$background))) {
    p <- p +
      geom_line(data=df_final, aes(doy,background), color="red", linetype="dashed") +
      geom_text_npc(aes(npcx=0.01,npcy=0.79,label=paste("- - Background",variable$abbrev)),size=4,fontface="bold",color="red")
  }
  
  if (log_display) {p <- p + scale_y_log10()}
  
  # if you used LOESS smoothing, add the line to the data
  if (smoothMethod == 'loess' & any(is.finite(df_final$loess))) {
    p <- p +
      geom_line(data=df_final %>% dplyr::select(doy,loess) %>% tidyr::drop_na(), 
                aes(x=doy, y=loess), color="green") +
      geom_text_npc(aes(npcx=0.01,npcy=0.89,label="** LOESS smoothing"),size=4,fontface="bold",color="green")
  }
  
  # add gaussian line or threshold line, based on user-selected fit method
  if (fitmethod=='gauss') {
    # add t[max_fit] line
    p <- p + geom_vline(xintercept=as.numeric(fitparams$`t[max_fit]`), col="black", alpha=0.6)
    # add gauss fit line if it exists
    if (all(is.na(df_final$model))) {
      p <- p + geom_text_npc(aes(npcx=0.01,npcy=0.85,label="Unable to fit"),size=8,fontface="bold",color="red")
    } else {
      # add data to base plot
      fit_col <- ifelse(dailystat=="average","dodgerblue2","red2")
      tmp_lab <- paste0("** Gaussian fitted to ",ifelse(smoothMethod=='loess',"LOESS","points"))
      p <- p +
        geom_line(data=df_final, aes(doy,model), color=fit_col) +
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
                 label.size=0, position=position_dodge(1), alpha=0.5)
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
                NA, # day of year
                NA, # percent coverage
                names(outliers)[outliers==all_inputs$outlier],
                names(dailystats)[dailystats==all_inputs$dailystat],
                NA, NA, # min/max pixel values
                names(fitmethods)[fitmethods==all_inputs$fitmethod],
                names(bloomShapes)[bloomShapes==all_inputs$bloomShape],
                names(smoothMethods)[smoothMethods==all_inputs$smoothMethod],
                NA, # logging
                NA, # loess span
                NA, NA, NA, # range of days for fit, initiation, and peak
                names(ti_threshold_types)[ti_threshold_types==all_inputs$ti_threshold_type],
                NA, # percent threshold
                NA, # constant threshold
                NA, NA, NA, NA, # tmax, beta, weights, remove background
                NA, NA, NA, NA, # flag 1/2 limits
                NA, # threshold fit method coefficient
                NA, NA, # full run options: png, range of years
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
  latlon_toolarge <- FALSE
  help_latlon_txt <- ""
  if (nrow(spdf)>0) {
    polygon_area <- length(over(x=as_Spatial(spdf),y=pixels,returnList=TRUE)[[1]])
    if (polygon_area > max_pixels) {
      latlon_toolarge <- TRUE
      help_latlon_txt <- paste0("Polygon is too large (max allowed pixels = ",max_pixels,").")
    }
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
