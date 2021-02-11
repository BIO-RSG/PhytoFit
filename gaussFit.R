asymm_gaussian <- function(t, tm_value, beta_valueL, beta_valueR, B0L, B0R, sigmaL, sigmaR, hL, hR) {
  
    # from bloom_fitting.R
    # Created by A.R. Hanke , May 2nd, 2016
    # Edits by C. Fuentes-Yaco, May 10th, 2016
    # Edits by Stephanie Clay, March/April 2017 and August 2018, and March 2020
    #       for use in the bloom fitting app
    
    tL <- t[t <= tm_value]
    tR <- t[t > tm_value]
    
    c({B0L + beta_valueL * tL + hL / (sqrt(2*pi) * sigmaL) * exp(- (tL - tm_value)^2 / (2 * sigmaL^2))}, # vector of values from the left side of the curve
      {B0R + beta_valueR * tR + hR / (sqrt(2*pi) * sigmaR) * exp(- (tR - tm_value)^2 / (2 * sigmaR^2))}) # vector of values from the right side of the curve
    
}

# Compare the real and fitted time series, and flag it if it meets certain criteria
flag_check <- function(mag_real, mag_fit, amp_real, amp_fit, sigma, time_res=1, flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2) {
  
  flags <- c(ifelse(amp_fit/amp_real <= flag1_lim1 | amp_fit/amp_real >= flag1_lim2, TRUE, FALSE),
             ifelse(mag_fit/mag_real <= flag2_lim1 | mag_fit/mag_real >= flag2_lim2, TRUE, FALSE),
             ifelse(sigma <= time_res, TRUE, FALSE))
  
  return(as.numeric(paste0(which(flags), collapse="")))
  
}


# This is the main function that organizes the data and the limits and starting
# guesses for the nonlinear least squares regression to find the best fit.
# The function then computes the fit, using a simple gaussian model if the user
# chose a symmetric bloom shape, or the asymm_gaussian function above if the
# user chose the asymmetric shape.
gaussFit <- function(t, y, w, bloomShape = "symmetric", tm = FALSE, beta = FALSE,
                     tm_limits = c(1,365), ti_limits = c(1,365), log_chla = FALSE,
                     interval = "daily", flag1_lim1 = 0.75, flag1_lim2 = 1.25,
                     flag2_lim1 = 0.85, flag2_lim2 = 1.15, ti_threshold = 0.2,
                     tt_threshold = 0.2, ydays_dayrange){
  
    # t, y, w              = numeric vectors: day of year, chlorophyll concentration, and weights
    #                        (reduced by selected day range and percent coverage)
    # bloomShape           = string: "symmetric" or "asymmetric"
    # tm, beta             = logical values
    # tm_limits, ti_limits = numeric vectors: the range of days to search for max chla concentration and the start of the bloom
    # log_chla             = boolean: TRUE if chlorophyll values are logged (need this to know whether or not to log B0 parameter)
    # ydays_dayrange       = numeric vector: day of year, reduced by selected day range
  
    # FORMULAS:
    #   t_init = tmax - sqrt(-2 * log(ti_threshold)) * sigma
    #          = tmax - 1.79*sigma (for default 20% threshold)
    #   H = amplitude = h / (sqrt(2*pi)*sigma) = Bmax - B0
    #   tm FALSE and beta TRUE:
    #         t_dur = 2*(t_max - t_init) = 3.59*sigma
    #   tm TRUE and beta TRUE:
    #         t_dur = 3.59 * sigma
    
    # Calculate the scaling factor that will later be used to get the approximate
    # number of days between tmax and ti
    ti_width <- sqrt(-2 * log(ti_threshold))
    tt_width <- sqrt(-2 * log(tt_threshold))
    
    chlorophyll <- y
    yday <- t
    
    nls_data = list(B = chlorophyll, t = yday)
    
    # To collect output later
    if (bloomShape=="symmetric") {
      values <- data.frame(matrix(nrow=1,ncol=15), stringsAsFactors = FALSE)
      colnames(values) <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                            "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]",
                            "B0", "h", "sigma", "beta", "Flags")
    } else if (bloomShape=="asymmetric") {
      values <- data.frame(matrix(nrow=1,ncol=24), stringsAsFactors = FALSE)
      colnames(values) <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                            "Magnitude[real_left]", "Magnitude[fit_left]", "Amplitude[real_left]", "Amplitude[fit_left]",
                            "B0[left]", "h[left]", "sigma[left]", "beta[left]", "Flags[left]",
                            "Magnitude[real_right]", "Magnitude[fit_right]", "Amplitude[real_right]", "Amplitude[fit_right]",
                            "B0[right]", "h[right]", "sigma[right]", "beta[right]", "Flags[right]")
    }
    
    values[1,1:2] <- c(mean(chlorophyll, na.rm=TRUE), median(chlorophyll, na.rm=TRUE))
    
    
    # LIMITS & START GUESSES ####
    
    # Here, set the parameters' lower/upper bounds and starting guesses for nls().
    # For an assymetric curve, the same bounds will be used for either side.
    if (log_chla) {
      B0lower <- log10(1e-10) # note 0 can't be logged
      B0upper <- log10(5)
      B0start <- log10(0.5)
    } else {
      B0lower <- 0
      B0upper <- 5
      B0start <- 0.5
    }
    
    
    # lower/upper bounds
    lower <- list(B0 = B0lower, h = 0, sigma = 0)
    upper <- list(B0 = B0upper, h = 350, sigma = 100)
    
    # starting guesses
    params <- vector(mode = "list", length = 4)
    params[[1]]$B0 <- params[[2]]$B0 <- params[[3]]$B0 <- params[[4]]$B0 <- B0start
    params[[1]]$h <- params[[2]]$h <- 50
    params[[3]]$h <- params[[4]]$h <- 10
    params[[1]]$sigma <- 10
    params[[2]]$sigma <- params[[3]]$sigma <- 2
    params[[4]]$sigma <- 1
    
    
    # BETA AND TM ####
    
    # beta changes slope of straight line on either side of the bell curve
    # if beta = TRUE, set its lower/upper bounds and starting guess for nls
    if (beta) {
      
      lower$beta_value <- -0.02
      upper$beta_value <- 0.01
      params[[1]]$beta_value <- params[[2]]$beta_value <- -0.002
      params[[3]]$beta_value <- params[[4]]$beta_value <- -0.001
      
    } else {
      
      beta_value <- 0
      if (bloomShape=="symmetric") {
        #nls_data$beta_value <- 0
      } else if (bloomShape=="asymmetric") {
        beta_valueL <- beta_valueR <- 0
      }
      
      # remove beta column(s)
      values <- values[,!startsWith(colnames(values), "beta")]
      
    }
    
    # Get the range of possible days for the timing of max concentration (tm_value)
    limited_yday <- yday >= tm_limits[1] & yday <= tm_limits[2]
    # If there is no data available for the possible window of max concentration, return a NULL fit.
    # User can then manually adjust the restrictions on day of max concentration or initiation of bloom.
    if (sum(limited_yday)==0) {return(list(fit = NULL, values = values))}
    # Get the day of max concentration within this range
    tm_value <- yday[which(chlorophyll==max(chlorophyll[limited_yday],na.rm=TRUE) & limited_yday)]
    # Check again if no data available
    if (length(tm_value)==0) {return(list(fit = NULL, values = values))}
    
    # if tm = TRUE, make tm_value a parameter in the nonlinear least squares
    # regression, and set its lower/upper bounds and initial estimates
    if (tm) {
      
      lower$tm_value <- tm_limits[1]
      upper$tm_value <- tm_limits[2]
      params[[1]]$tm_value <- params[[2]]$tm_value <- params[[3]]$tm_value <- params[[4]]$tm_value <- tm_value
      
    }
    
    # if the bloom starting day should be restricted, do that here
    # bloom start is computed using the following formula:
    # ti = tm_value - 1.79*sigma
    # if tm = FALSE, tm_value is known so you can restrict sigma in order to restrict ti
    # if tm = TRUE, can't do this because both tmax and sigma vary in the nls regression
    # so there's no way to use either one to restrict ti
    if (!tm & !all(ti_limits==c(1,365))) {
      
      sigma_limit2 <- (tm_value - ti_limits[1])/ti_width
      sigma_limit1 <- (tm_value - ti_limits[2])/ti_width
      
      if (sigma_limit1 < 0) {sigma_limit1 <- 0}
      
      lower$sigma <- sigma_limit1
      upper$sigma <- sigma_limit2
      
    }
    
    
    # SYMMETRIC FIT ####
    
    if (bloomShape=="symmetric") {
      
      # GET THE FIT
      for (i in 1:length(params)){
          fit <- NULL
          try(fit <- nlsLM(B ~ B0 + beta_value * t + h / (sqrt(2*pi) * sigma) * exp(- (t - tm_value)^2 / (2 * sigma^2)),
                           data = nls_data,
                           weights = w,
                           lower = unlist(lower),
                           upper = unlist(upper),
                           start = params[[i]],
                           control = nls.lm.control(maxiter=60)),
              silent = TRUE)
          if(!is.null(fit)) break
      }
      
      # FILL IN VALUES FROM THE FIT
      if (!is.null(fit)) {
        
        # B0, h, and sigma are always the 1,2,3rd coef
        B0 <- unname(coef(fit)[1])
        h <- unname(coef(fit)[2])
        sigma <- unname(coef(fit)[3])
        
        if (beta) {
          beta_value <- unname(coef(fit)[4])
        }
        if (tm) {
          if (beta) {tm_ind <- 5
          } else {tm_ind <- 4}
          tm_value <- unname(coef(fit)[tm_ind])
        }
        
        ti <- tm_value - ti_width * sigma
        td <- 2 * ti_width * sigma
        tt <- ti + td
        
        # Experimental code below to allow user to adjust the threshold use to
        # calculate ti, down to a lower limit based on h/sigma.
        # NOTE: The asymmetric code hasn't been adjusted for this yet.
        
        # # Calculate approximate start day of the bloom using the log formula
        # ti <- tm_value - ti_width * sigma
        # 
        # # Calculate the minimum accepted start day of the bloom, to make sure
        # # the one calculated with the log formula is valid.
        # # With the user-selected range of days allowed in the fit, get the day
        # # where the fitted values first diverge from the background chla by at least 0.004 * h / sigma
        # fitted_values <- shifted_gaussian(tv = ydays_dayrange,
        #                                   B0 = B0,
        #                                   beta = beta_value,
        #                                   h = h,
        #                                   sigma = sigma,
        #                                   tmax = tm_value)
        # bg_line <- beta_value * ydays_dayrange + B0
        # ti_min <- ydays_dayrange[which(abs(fitted_values - bg_line) > 0.004*h/sigma)[1]]
        # 
        # # run a few checks to see if the log-calculated ti index is valid
        # invalid_ti <- FALSE
        # if (length(ti_min)==0) {
        #   invalid_ti <- TRUE
        # } else if (ti <= ti_min) {
        #   # ti calculated using threshold - is it too early? then reset ti
        #   ti <- ti_min
        #   # is the calculated ti too late?
        #   if (ti_min==ydays_dayrange[1]) {
        #     # get the curve and background values the day before ti_min
        #     day_before <- ydays_dayrange[1]-1
        #     chla_tminus1 <- predict(fit, data.frame(t=day_before))
        #     bg_tminus1 <- beta_value * day_before + B0
        #     # if the difference for the day before is also over the threshold,
        #     # then ti should have been earlier
        #     if (abs(chla_tminus1 - bg_tminus1) > 0.004*h/sigma) {
        #       invalid_ti <- TRUE
        #     }
        #   }
        # }
        # 
        # # based on ti, calculate the duration and the termination
        # td <- 2 * (tm_value-ti)
        # tt <- ti + td
        # 
        # # get the indices of valid ydays closest to ti, td, tm
        # tiidx <- which.min(abs(yday - ti))
        # tmidx <- which.min(abs(yday - tm_value))
        # ttidx <- which.min(abs(yday - tt))
        # 
        # if (!(tiidx < tmidx & ti < tm_value)) {invalid_ti <- TRUE}
        # 
        # if (invalid_ti) {
        
        if (ti < 1) {
          
          fit <- NULL
          
        } else {
          
          tiidx <- which.min(abs(yday - ti))
          tmidx <- which.min(abs(yday - tm_value))
          ttidx <- which.min(abs(yday - tt))
          
          if (beta) {
            bkrnd <- B0 + (beta_value * (yday[tiidx:ttidx]))
            bkrndBm <-  B0 + (beta_value * tm_value)
            values[1,"beta"] <- beta_value
          } else {
            bkrnd <- bkrndBm <- B0
          }
          
          mag_real <- sum(diff(yday[tiidx:ttidx]) * (head(chlorophyll[tiidx:ttidx] - bkrnd, -1) + tail(chlorophyll[tiidx:ttidx] - bkrnd, -1))/2)
          mag_fit <- sum(diff(yday[tiidx:ttidx]) * (head(fitted_values[tiidx:ttidx] - bkrnd, -1) + tail(fitted_values[tiidx:ttidx] - bkrnd, -1))/2)
          amp_real <- chlorophyll[tmidx] - bkrndBm
          amp_fit <- fitted_values[tmidx] - bkrndBm
          
          flags <- flag_check(mag_real=mag_real,
                              mag_fit=mag_fit,
                              amp_real=amp_real,
                              amp_fit=amp_fit,
                              sigma=sigma,
                              time_res=ifelse(interval=="daily", 1, 8),
                              flag1_lim1=flag1_lim1,
                              flag1_lim2=flag1_lim2,
                              flag2_lim1=flag2_lim1,
                              flag2_lim2=flag2_lim2)
          
          val_inds <- ifelse(beta, list(c(3:13, 15)), list(3:14))
          values[1,val_inds[[1]]] <- c(ti, tm_value, tt, td, mag_real, mag_fit, amp_real, amp_fit, B0, h, sigma, flags)
          
        }
        
      }
      
      
    # ASYMMETRIC FIT ####
    
    } else if (bloomShape=="asymmetric") {
      
      # Update lower/upper bounds and starting guesses, using the same for both sides of the curve
      
      param_names <- c("B0L", "hL", "sigmaL", "B0R", "hR", "sigmaR")
      tmp_lower <- rep(lower[1:3], 2)
      tmp_upper <- rep(upper[1:3], 2)
      tmp_params <- lapply(1:length(params), function(l) rep(params[[l]][1:3], 2))
      
      if (beta) {
        param_names <- c(param_names, "beta_valueL", "beta_valueR")
        tmp_lower <- c(tmp_lower, lower[4], lower[4])
        tmp_upper <- c(tmp_upper, upper[4], upper[4])
        tmp_params <- lapply(1:length(tmp_params), function(l) c(tmp_params[[l]], params[[l]][4], params[[l]][4]))
      }
      
      if (tm) {
        if (beta) {tm_ind <- 5
        } else {tm_ind <- 4}
        param_names <- c(param_names, "tm_value")
        tmp_lower <- c(tmp_lower, lower[tm_ind])
        tmp_upper <- c(tmp_upper, upper[tm_ind])
        tmp_params <- lapply(1:length(tmp_params), function(l) c(tmp_params[[l]], params[[l]][tm_ind]))
      }
      
      lower <- tmp_lower
      upper <- tmp_upper
      params <- tmp_params
      
      # Fix names
      names(lower) <- names(upper) <- param_names
      params <- lapply(1:length(params), function(l) {names(params[[l]]) <- param_names; params[[l]]})
      
      # GET THE FIT
      for (i in 1:length(params)){
        fit <- NULL
        try(fit <- nlsLM(B ~ asymm_gaussian(t, tm_value, beta_valueL, beta_valueR, B0L, B0R, sigmaL, sigmaR, hL, hR),
                         data = nls_data,
                         weights = w,
                         lower = unlist(lower),
                         upper = unlist(upper),
                         start = params[[i]],
                         control = nls.lm.control(maxiter=60)),
            silent = TRUE)
        if(!is.null(fit)) break
      }
      
      
      # FILL IN VALUES FROM THE FIT
      if (!is.null(fit)) {
        
        B0L <- unname(coef(fit)[1])
        hL <- unname(coef(fit)[2])
        sigmaL <- unname(coef(fit)[3])
        
        B0R <- unname(coef(fit)[4])
        hR <- unname(coef(fit)[5])
        sigmaR <- unname(coef(fit)[6])
        
        if (beta) {
          beta_valueL <- unname(coef(fit)[7])
          beta_valueR <- unname(coef(fit)[8])
        }
        
        if (tm) {
          if (beta) {tm_ind <- 9
          } else {tm_ind <- 7}
          tm_value <- unname(coef(fit)[tm_ind])
        }
        
        ti <- tm_value - ti_width * sigmaL
        td <- ti_width * sigmaL + tt_width * sigmaR
        tt <- ti + td
        
        
        if (ti < 1) {
          
          fit <- NULL
          
        } else {
          
          tiidx <- which.min(abs(yday - ti))
          tmidx <- which.min(abs(yday - tm_value))
          ttidx <- which.min(abs(yday - tt))
          
          if (beta) {
            
            bkrndL <- B0L + (beta_valueL * (yday[tiidx:tmidx]))
            bkrndR <- B0R + (beta_valueR * (yday[(tmidx+1):ttidx]))
            bkrndBmL <-  B0L + (beta_valueL * yday[tmidx])
            bkrndBmR <-  B0R + (beta_valueR * yday[tmidx + 1])
            values[1,"beta[left]"] <- beta_valueL
            values[1,"beta[right]"] <- beta_valueR
            
          } else {
            
            bkrndL <- bkrndBmL <- B0L
            bkrndR <- bkrndBmR <- B0R
            
          }
          
          fitted_values <- predict(fit)
          
          magL_real <- sum(diff(yday[tiidx:tmidx]) * (head(chlorophyll[tiidx:tmidx] - bkrndL, -1) + tail(chlorophyll[tiidx:tmidx] - bkrndL, -1))/2)
          magR_real <- sum(diff(yday[(tmidx+1):ttidx]) * (head(chlorophyll[(tmidx+1):ttidx] - bkrndR, -1) + tail(chlorophyll[(tmidx+1):ttidx] - bkrndR, -1))/2)
          magL_fit <- sum(diff(yday[tiidx:tmidx]) * (head(fitted_values[tiidx:tmidx] - bkrndL, -1) + tail(fitted_values[tiidx:tmidx] - bkrndL, -1))/2)
          magR_fit <- sum(diff(yday[(tmidx+1):ttidx]) * (head(fitted_values[(tmidx+1):ttidx] - bkrndR, -1) + tail(fitted_values[(tmidx+1):ttidx] - bkrndR, -1))/2)
          
          ampL_real <- chlorophyll[tmidx] - bkrndBmL
          ampR_real <- chlorophyll[tmidx+1] - bkrndBmR
          ampL_fit <- fitted_values[tmidx] - bkrndBmL
          ampR_fit <- fitted_values[tmidx+1] - bkrndBmR
          
          flagsL <- flag_check(mag_real=magL_real,
                               mag_fit=magL_fit,
                               amp_real=ampL_real,
                               amp_fit=ampL_fit,
                               sigma=sigmaL,
                               time_res=ifelse(interval=="daily", 1, 8),
                               flag1_lim1=flag1_lim1,
                               flag1_lim2=flag1_lim2,
                               flag2_lim1=flag2_lim1,
                               flag2_lim2=flag2_lim2)
          flagsR <- flag_check(mag_real=magR_real,
                               mag_fit=magR_fit,
                               amp_real=ampR_real,
                               amp_fit=ampR_fit,
                               sigma=sigmaR,
                               time_res=ifelse(interval=="daily", 1, 8),
                               flag1_lim1=flag1_lim1,
                               flag1_lim2=flag1_lim2,
                               flag2_lim1=flag2_lim1,
                               flag2_lim2=flag2_lim2)
          
          val_inds <- ifelse(beta, list(c(3:13, 15:22, 24)), list(3:22))
          values[1,val_inds[[1]]] <- c(ti, tm_value, tt, td,
                                  magL_real, magL_fit, ampL_real, ampL_fit, B0L, hL, sigmaL, flagsL,
                                  magR_real, magR_fit, ampR_real, ampR_fit, B0R, hR, sigmaR, flagsR)
          
        }
        
      }
      
    }
    
    return(list(fit = fit, values = values))
    
}