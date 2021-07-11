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

# Calculate the background line between ti and tt in the asymmetric case
get_asymm_bkrnd <- function(B0L, B0R, beta_valueL, beta_valueR, yday) {
  bkrndL <- B0L + beta_valueL * yday[1]
  bkrndR <- B0R + beta_valueR * yday[length(yday)]
  bloom_bkrnd_line <- find_line(yday[1], bkrndL, yday[length(yday)], bkrndR)
  return(bloom_bkrnd_line$intercept + bloom_bkrnd_line$slope * yday)
}

# Compare the real and fitted time series, and flag it if it meets certain criteria
flag_check <- function(mag_real, mag_fit, amp_real, amp_fit, sigma, time_res=1,
                       flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2,
                       ti, ti_limits, tm, tm_limits, tt, t_range) {
  
  flags <- c(amp_fit/amp_real <= flag1_lim1 | amp_fit/amp_real >= flag1_lim2,
             mag_fit/mag_real <= flag2_lim1 | mag_fit/mag_real >= flag2_lim2,
             sigma <= time_res,
             ti %in% ti_limits,
             tm %in% tm_limits,
             tt == t_range[2])
  
  return(as.numeric(paste0(which(flags), collapse="")))
  
}

get_failure_msg <- function(code) {
  if (code==0) {return(NULL)}
  messages <- c("not enough data in the selected limits",
                "nls failed",
                "t[start] threshold too high",
                "t[start] too early (before day 1)",
                "t[start] outside t_range",
                "t[end] outside t_range",
                "end of bloom [chla] > threshold")
  return(paste0("Code ", code, ": ", messages[code]))
}

# This is the main function that organizes the data and the limits and starting
# guesses for the nonlinear least squares regression to find the best fit.
# The function then computes the fit, using a simple gaussian model if the user
# chose a symmetric bloom shape, or the asymm_gaussian function above if the
# user chose the asymmetric shape.
gaussFit <- function(t, y, w, bloomShape = "symmetric", tm = FALSE, beta = FALSE,
                     tm_limits = c(2,364), ti_limits = c(1,363), t_range = c(1,365),
                     log_chla = FALSE, interval = "daily", flag1_lim1 = 0.75,
                     flag1_lim2 = 1.25, flag2_lim1 = 0.85, flag2_lim2 = 1.15,
                     ti_threshold = 0.2, tt_threshold = 0.2, ydays_dayrange, rm_bkrnd=FALSE,
                     ti_threshold_type="percent_thresh", ti_threshold_constant=0.1){
  
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
    
    # these are for the curve and background line, they will be filled in and returned to use in the plots
    yfit <- NULL
    ybkrnd <- NULL
    
    # error code to return if the points can't be fit with the current parameters and restrictions
    nofit_code <- 0
    
    # To collect output later
    if (bloomShape=="symmetric") {
      values <- data.frame(matrix(nrow=1,ncol=16), stringsAsFactors = FALSE)
      colnames(values) <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                            "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags",
                            "B0", "h", "sigma", "beta", "failure_code")
    } else if (bloomShape=="asymmetric") {
      values <- data.frame(matrix(nrow=1,ncol=20), stringsAsFactors = FALSE)
      colnames(values) <- c("Mean", "Median", "t[start]", "t[max]", "t[end]", "t[duration]",
                            "Magnitude[real]", "Magnitude[fit]", "Amplitude[real]", "Amplitude[fit]", "Flags",
                            "B0[left]", "h[left]", "sigma[left]", "beta[left]",
                            "B0[right]", "h[right]", "sigma[right]", "beta[right]", "failure_code")
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
    if (sum(limited_yday)==0) {
      nofit_code <- 1
      values[,"failure_code"] <- nofit_code
      return(list(fit = NULL, values = values,
                  yfit = yfit, ybkrnd = ybkrnd,
                  nofit_msg = get_failure_msg(nofit_code),
                  nofit_code = nofit_code))
    }
    # Get the day of max concentration within this range
    tm_value <- yday[which(chlorophyll==max(chlorophyll[limited_yday],na.rm=TRUE) & limited_yday)]
    # Check again if no data available
    if (length(tm_value)==0) {
      nofit_code <- 1
      values[,"failure_code"] <- nofit_code
      return(list(fit = NULL, values = values,
                  yfit = yfit, ybkrnd = ybkrnd,
                  nofit_msg = get_failure_msg(nofit_code),
                  nofit_code = nofit_code))
    }
    
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
    
    # need at least 3 points to attempt to fit a gaussian curve
    if (length(yday)<3) {
      nofit_code <- 1
      values[,"failure_code"] <- nofit_code
      return(list(fit = NULL, values = values,
                  yfit = yfit, ybkrnd = ybkrnd,
                  nofit_msg = get_failure_msg(nofit_code),
                  nofit_code = nofit_code))
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
      if (is.null(fit)) {
        
        nofit_code <- 2
        
      } else {
        
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
          tm_value <- round(unname(coef(fit)[tm_ind]))
        }
        
        # create the fit and background line vectors for every day within t_range
        # (this is used to calculate ti, tt, and td with the constant_thresh method,
        # but also returned to be used in the plots)
        yfit <- shifted_gaussian(ydays_dayrange, B0, beta_value, h, sigma, tm_value)
        ybkrnd <- B0 + beta_value * ydays_dayrange
        
        # calculate ti, tt, and td based on user-selected method
        # (20% of amplitude or the day where the curve departs from the background
        # by a selected threshold value)
        if (ti_threshold_type=="percent_thresh") {
          ti <- floor(tm_value - ti_width * sigma)
          td <- 2 * (tm_value - ti)
          tt <- ti + td
        } else {
          ti_ind <- ydays_dayrange < tm_value & ydays_dayrange >= ti_limits[1] & ydays_dayrange <= ti_limits[2]
          # note that yfit and ybkrnd are the fitted values, so the difference between them is constantly increasing before tmax
          if (log_chla) {
            ti <- ydays_dayrange[ti_ind][which(abs(10^yfit-10^ybkrnd)[ti_ind] >= ti_threshold_constant)[1]]
          } else {
            ti <- ydays_dayrange[ti_ind][which(abs(yfit-ybkrnd)[ti_ind] >= ti_threshold_constant)[1]]
          }
          td <- (tm_value-ti) * 2
          tt <- ti + td
        }
        
        # check for problems with the calculated ti, tt, and td, and if they're all
        # good, continue with calculating amplitude, magnitude, and flags
        if (!is.finite(ti)) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 3
          
        } else if (ti < 1) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 4
          
        } else if (ti < t_range[1]) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 5
          
        } else if (tt > t_range[2]) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 6
          
        } else {
          
          if (beta) {
            values[1,"beta"] <- beta_value
          }
          
          tiidx <- which.min(abs(yday - ti))
          ttidx <- which.min(abs(yday - tt))
          
          # if tiidx or ttidx do not land on existing indices in yday, add an interpolated value
          if (yday[tiidx] != ti) {
            chlorophyll <- c(chlorophyll, approx(x = yday, y = chlorophyll, xout = ti, rule = 2)$y)
            yday <- c(yday, ti)
          }
          if (yday[ttidx] != tt) {
            chlorophyll <- c(chlorophyll, approx(x = yday, y = chlorophyll, xout = tt, rule = 2)$y)
            yday <- c(yday, tt)
          }
          
          # now order yday and chlorophyll and recalculate tiidx and ttidx
          yday_order <- order(yday)
          yday <- yday[yday_order]
          chlorophyll <- chlorophyll[yday_order]
          tiidx <- which(yday==ti)
          ttidx <- which(yday==tt)
          
          # reduce real vectors to the bloom to calculate "real" amplitude and magnitude
          yday <- yday[tiidx:ttidx]
          chlorophyll <- chlorophyll[tiidx:ttidx]
          
          # calculate background chla corresponding to the real vector
          bkrnd <- B0 + beta_value * yday
          
          # get higher res curve and background to calculate "fit" amplitude and magnitude
          fitted_yday <- seq(ti, tt, by=(tt-ti)/200)
          fitted_chlorophyll <- shifted_gaussian(fitted_yday, B0, beta_value, h, sigma, tm_value)
          fitted_bkrnd <- B0 + beta_value * fitted_yday
          
          # transform back to linear space to do amplitude and magnitude calculations
          if (log_chla) {
            chlorophyll <- 10^chlorophyll
            fitted_chlorophyll <- 10^fitted_chlorophyll
            bkrnd <- 10^bkrnd
            fitted_bkrnd <- 10^fitted_bkrnd
          }
          
          if (rm_bkrnd) {
            chlorophyll <- chlorophyll - bkrnd
            fitted_chlorophyll <- fitted_chlorophyll - fitted_bkrnd
          }
          
          # calculate magnitude and amplitude using real values
          mag_real <- sum(diff(yday) * (head(chlorophyll, -1) + tail(chlorophyll, -1))/2)
          amp_real <- max(chlorophyll, na.rm=TRUE)
          # calculate magnitude and amplitude using fitted values
          mag_fit <- sum(diff(fitted_yday) * (head(fitted_chlorophyll, -1) + tail(fitted_chlorophyll, -1))/2)
          amp_fit <- fitted_chlorophyll[which.min(abs(fitted_yday - tm_value))[1]]
          
          flags <- flag_check(mag_real=mag_real,
                              mag_fit=mag_fit,
                              amp_real=amp_real,
                              amp_fit=amp_fit,
                              sigma=sigma,
                              time_res=ifelse(interval=="daily", 1, 8),
                              flag1_lim1=flag1_lim1,
                              flag1_lim2=flag1_lim2,
                              flag2_lim1=flag2_lim1,
                              flag2_lim2=flag2_lim2,
                              ti=ti,
                              ti_limits=ti_limits,
                              tm=tm_value,
                              tm_limits=tm_limits,
                              tt=tt,
                              t_range=t_range)
          
          values[1,3:14] <- c(ti, tm_value, tt, td, mag_real, mag_fit, amp_real, amp_fit, flags, B0, h, sigma)
          
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
      if (is.null(fit)) {
        
        nofit_code <- 2
        
      } else {
        
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
          tm_value <- round(unname(coef(fit)[tm_ind]))
        }
        
        # create the fit and background line vectors for every day within t_range
        # (this is used to calculate ti, tt, and td with the constant_thresh method,
        # but also returned to be used in the plots)
        yfit <- c(shifted_gaussian(ydays_dayrange[ydays_dayrange <= tm_value], B0L, beta_valueL, hL, sigmaL, tm_value),
                  shifted_gaussian(ydays_dayrange[ydays_dayrange > tm_value], B0R, beta_valueR, hR, sigmaR, tm_value))
        ybkrnd <- c(B0L + beta_valueL * ydays_dayrange[ydays_dayrange <= tm_value],
                    B0R + beta_valueR * ydays_dayrange[ydays_dayrange > tm_value])
        
        # calculate ti, tt, and td based on user-selected method
        # (20% of amplitude or the day where the curve departs from the background
        # by a selected threshold value)
        if (ti_threshold_type=="percent_thresh") {
          ti <- floor(tm_value - ti_width * sigmaL)
          tt <- ceiling(tm_value + tt_width * sigmaR)
          td <- tt - ti
        } else {
          ti_ind <- ydays_dayrange < tm_value & ydays_dayrange >= ti_limits[1] & ydays_dayrange <= ti_limits[2]
          tt_ind <- ydays_dayrange > tm_value
          if (log_chla) {
            ti <- ydays_dayrange[ti_ind][which(abs(10^yfit-10^ybkrnd)[ti_ind] >= ti_threshold_constant)[1]]
            tt <- ydays_dayrange[tt_ind][which(abs(10^yfit-10^ybkrnd)[tt_ind] < ti_threshold_constant)[1]]
          } else {
            ti <- ydays_dayrange[ti_ind][which(abs(yfit-ybkrnd)[ti_ind] >= ti_threshold_constant)[1]]
            tt <- ydays_dayrange[tt_ind][which(abs(yfit-ybkrnd)[tt_ind] < ti_threshold_constant)[1]]
          }
          td <- tt - ti
        }
        
        
        # check for problems with the calculated ti, tt, and td, and if they're all
        # good, continue with calculating amplitude, magnitude, and flags
        if (!is.finite(ti)) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 3
          
        } else if (ti < 1) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 4
          
        } else if (ti < t_range[1]) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 5
          
        } else if (!is.finite(tt)) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 7
          
        } else if (tt > t_range[2]) {
          
          fit <- yfit <- ybkrnd <- NULL
          nofit_code <- 6
        
        } else {
          
          # update the background using ti and tt
          bloom_ind <- ydays_dayrange >= ti & ydays_dayrange <= tt
          ybkrnd[bloom_ind] <- get_asymm_bkrnd(B0L, B0R, beta_valueL, beta_valueR, ydays_dayrange[bloom_ind])
          
          if (beta) {
            values[1,"beta[left]"] <- beta_valueL
            values[1,"beta[right]"] <- beta_valueR
          }
          
          tiidx <- which.min(abs(yday - ti))
          ttidx <- which.min(abs(yday - tt))
          
          # if tiidx or ttidx do not land on existing indices in yday, add an interpolated value
          if (yday[tiidx] != ti) {
            chlorophyll <- c(chlorophyll, approx(x = yday, y = chlorophyll, xout = ti, rule = 2)$y)
            yday <- c(yday, ti)
          }
          if (yday[ttidx] != tt) {
            chlorophyll <- c(chlorophyll, approx(x = yday, y = chlorophyll, xout = tt, rule = 2)$y)
            yday <- c(yday, tt)
          }
          
          # now order yday and chlorophyll and recalculate tiidx and ttidx
          yday_order <- order(yday)
          yday <- yday[yday_order]
          chlorophyll <- chlorophyll[yday_order]
          tiidx <- which(yday==ti)
          ttidx <- which(yday==tt)
          
          # reduce real vectors to the bloom to calculate "real" amplitude and magnitude
          yday <- yday[tiidx:ttidx]
          chlorophyll <- chlorophyll[tiidx:ttidx]
          
          # calculate background chla corresponding to the real vector, between ti and tt
          bkrnd <- get_asymm_bkrnd(B0L, B0R, beta_valueL, beta_valueR, yday)
          
          # get higher res curve and background to calculate "fit" amplitude and magnitude
          fitted_yday <- seq(ti, tt, by=(tt-ti)/200)
          fitted_chlorophyll <- c(shifted_gaussian(fitted_yday[fitted_yday <= tm_value], B0L, beta_valueL, hL, sigmaL, tm_value),
                                  shifted_gaussian(fitted_yday[fitted_yday > tm_value], B0R, beta_valueR, hR, sigmaR, tm_value))
          fitted_bkrnd <- get_asymm_bkrnd(B0L, B0R, beta_valueL, beta_valueR, fitted_yday)
          
          # transform back to linear space to do amplitude and magnitude calculations
          if (log_chla) {
            chlorophyll <- 10^chlorophyll
            fitted_chlorophyll <- 10^fitted_chlorophyll
            bkrnd <- 10^bkrnd
            fitted_bkrnd <- 10^fitted_bkrnd
          }
          
          if (rm_bkrnd) {
            chlorophyll <- chlorophyll - bkrnd
            fitted_chlorophyll <- fitted_chlorophyll - fitted_bkrnd
          }
          
          # calculate magnitude and amplitude using real values
          mag_real <- sum(diff(yday) * (head(chlorophyll, -1) + tail(chlorophyll, -1))/2)
          amp_real <- max(chlorophyll, na.rm=TRUE)
          # calculate magnitude and amplitude using fitted values
          mag_fit <- sum(diff(fitted_yday) * (head(fitted_chlorophyll, -1) + tail(fitted_chlorophyll, -1))/2)
          amp_fit <- max(fitted_chlorophyll[plus_minus(which.min(abs(fitted_yday - tm_value))[1], 1)], na.rm=TRUE)
          
          flags <- flag_check(mag_real=mag_real,
                              mag_fit=mag_fit,
                              amp_real=amp_real,
                              amp_fit=amp_fit,
                              sigma=min(sigmaL, sigmaR),
                              time_res=ifelse(interval=="daily", 1, 8),
                              flag1_lim1=flag1_lim1,
                              flag1_lim2=flag1_lim2,
                              flag2_lim1=flag2_lim1,
                              flag2_lim2=flag2_lim2,
                              ti=ti,
                              ti_limits=ti_limits,
                              tm=tm_value,
                              tm_limits=tm_limits,
                              tt=tt,
                              t_range=t_range)
          
          val_inds <- ifelse(beta, list(c(3:14,16:18)), list(3:17))
          values[1,val_inds[[1]]] <- c(ti, tm_value, tt, td, mag_real, mag_fit, amp_real, amp_fit,
                                       flags, B0L, hL, sigmaL, B0R, hR, sigmaR)
          
        }
        
      }
      
    }
    
    values[,"failure_code"] <- nofit_code
    return(list(fit = fit, values = values, yfit = yfit, ybkrnd = ybkrnd,
                nofit_msg = get_failure_msg(nofit_code), nofit_code))
    
}