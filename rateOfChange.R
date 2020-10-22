rateOfChange <- function(y, yall, t, tall, bloomShape = "symmetric", tm_limits = c(1, 365), ti_limits = c(1, 365)) {
  
  require(quantreg)
  
  # yall, tall - vector of points with high enough % coverage
  # y, t - vector of points within the selected range of days and with high enough % coverage
  
  # quantile regression will not work with only one point
  if (length(y)==1) {
    rqfit <- NULL
  } else {
    # use quantile regression to model the line of background chla
    rqfit <- rq(yall ~ tall, tau = 0.25)
  }
  
  # make sure all values of y and t are valid
  yok <- !is.na(y)
  y <- y[yok]
  t <- t[yok]
  
  # To collect output later
  values <- data.frame(matrix(nrow=1,ncol=8), stringsAsFactors = FALSE)
  colnames(values) <- c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                        "t[duration]", "Magnitude", "Amplitude")
  ti <- tm <- tt <- td <- mag <- amp <- NA
  
  values[1,1:2] <- c(mean(y, na.rm=TRUE), median(y, na.rm=TRUE))
  
  # get index of max concentration, subset to limited range of days
  yday_tm <- t >= tm_limits[1] & t <= tm_limits[2]
  if (sum(yday_tm)==0) {return(list(values = values))}
  maxidx <- which(y==max(y[yday_tm],na.rm=TRUE) & yday_tm)
  if (length(maxidx)==0) {return(list(values = values))}
  
  if (maxidx != 1) {
    
    # Get index of max concentration, and the day
    Bm <- y[maxidx]
    tm <- t[maxidx]
    
    # Get vector of chla and days from first day to day of max concentration
    ynpm <- y[1:maxidx]
    tpm <- t[1:maxidx]
    
    # Find the slope between each point from first day to day of max concentration
    dchladt <- diff(ynpm) / diff(tpm)
    
    # diff vector is one shorter than ynpm and tpm, so use short tpm for comparison
    tpm_short <- tpm[1:(maxidx-1)]
    
    # pick the largest slope (rate of change) - this is the bloom start
    yday_ti <- tpm_short >= ti_limits[1] & tpm_short <= ti_limits[2]
    if (sum(yday_ti)==0) {return(list(values = values))}
    
    maxidxdcdt <- which(dchladt==max(dchladt[yday_ti],na.rm=TRUE) & yday_ti)
    ti <- tpm_short[maxidxdcdt]
    
    # get bloom start index within the full time vector
    yday_ti_full <- t >= ti_limits[1] & t <= ti_limits[2]
    tiidx <- which(abs(t - ti)==min(abs(tpm_short[yday_ti] - ti),na.rm=TRUE) & yday_ti_full)
    
    if (bloomShape == 'symmetric') {
      
      # duration assuming symmetric
      td <- 2*(tm - ti)
      
      # termination day and index of tt
      tt <- ti + td
      ttidx <- which(abs(t - tt)==min(abs(t - tt),na.rm=TRUE))
      
      # if termination day tt has no valid data, and has days with valid data 
      # before and after that are equally spaced, choose the first one (the one
      # closest to the max day)
      
      if (length(ttidx) > 1) {
        
        ttidx <- ttidx[1]
        
      }
      
      if (!is.null(rqfit)) {
        # get background concentration based on full model, for the days of the bloom
        bkrnd <- predict(rqfit, newdata = data.frame(tall = t[tiidx:ttidx]))
        # calculate integral (magnitude under curve) and amplitude (max chla minus background chla)
        mag <- sum(diff(t[tiidx:ttidx]) * (head(y[tiidx:ttidx] - bkrnd, -1) + tail(y[tiidx:ttidx] - bkrnd, -1))/2)
        amp <- Bm - predict(rqfit, newdata = data.frame(tall = tm))
      }
      
    } else if (bloomShape == 'asymmetric') {
      
      # calculate integral (magnitude)
      revidx <- which(abs(t - 2*tm)==min(abs(t - 2*tm),na.rm=TRUE))
      
      if (revidx != maxidx) {
        
        # get the right side of the curve using the same method as above
        yntm <- rev(y[maxidx:revidx])
        ttm <- rev(t[maxidx:revidx])
        
        dcdtrev <- diff(yntm) / diff(ttm) * -1 # multiply by -1 since dt is negative
        
        maxttaidx <- which.max(dcdtrev)
        tt <- ttm[maxttaidx] # termination day asymmetric
        
        td <- tt - ti
        
        ttaidx <- which.min(abs(t - tt))
        
        if (!is.null(rqfit)) {
          bkrnd <- predict(rqfit, newdata = data.frame(tall = t[tiidx:ttaidx]))
          # duration assuming asymmetric  
          mag <- sum(diff(t[tiidx:ttaidx]) * (head(y[tiidx:ttaidx] - bkrnd, -1) + tail(y[tiidx:ttaidx] - bkrnd, -1))/2)
          amp <- Bm - predict(rqfit, newdata = data.frame(tall = tm))
        }
        
      }
    }
  }
  
  values[1,3:8] <- c(ti, tm, tt, td, mag, amp)
  return(list(values = values))
  
}
