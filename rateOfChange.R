rateOfChange <- function(y, yall, t, tall, bloomShape = "symmetric", tm_limits = c(1, 365), ti_limits = c(1, 365), log_chla=FALSE) {
  
  require(quantreg)
  
  y <- y$y
  
  rqfit <- NULL
  
  # yall, tall - vector of points with high enough % coverage
  # y, t - vector of points within the selected range of days and with high enough % coverage
  
  # use quantile regression to model the line of background chla
  if (length(y)>1) {
    rqfit <- quantreg::rq(yall ~ tall, tau = 0.25)
  }
  
  # make sure all values of y and t are valid
  yok <- !is.na(y)
  y <- y[yok]
  t <- t[yok]
  
  # To collect output later
  values <- data.frame(matrix(nrow=1,ncol=6), stringsAsFactors = FALSE)
  colnames(values) <- c("t[start]","t[max_real]","t[end]","t[duration]","Magnitude","Amplitude")
  ti <- tm <- tt <- td <- mag <- amp <- NA
  
  # need at least 3 points
  if (length(y)<3) {return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data"))}
  
  # get index of max concentration, subset to limited range of days
  yday_tm <- t >= tm_limits[1] & t <= tm_limits[2]
  if (sum(yday_tm)==0) {return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data between selected t[max] limits"))}
  maxidx <- which(y==max(y[yday_tm],na.rm=TRUE) & yday_tm)
  if (length(maxidx)==0) {return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data between selected t[max] limits"))}
  
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
    if (sum(yday_ti)==0) {return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data between selected t[start] limits"))}
    
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
        bkrndBm <- predict(rqfit, newdata = data.frame(tall = tm))
        # calculate integral (magnitude)
        if (log_chla) {
          y <- 10^y
          bkrnd <- 10^bkrnd
          Bm <- 10^Bm
          bkrndBm <- 10^bkrndBm
        }
        # calculate integral (magnitude under curve) and amplitude (max chla minus background chla)
        mag <- sum(diff(t[tiidx:ttidx]) * (head(y[tiidx:ttidx] - bkrnd, -1) + tail(y[tiidx:ttidx] - bkrnd, -1))/2)
        amp <- Bm - bkrndBm
      }
      
    } else if (bloomShape == 'asymmetric') {
      
      # get the index of 2*tm so that you can search for tt before this point
      revidx <- which(abs(t - 2*tm)==min(abs(t - 2*tm),na.rm=TRUE))[1]
      
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
          bkrndBm <- predict(rqfit, newdata = data.frame(tall = tm))
          # calculate integral (magnitude)
          if (log_chla) {
            y <- 10^y
            bkrnd <- 10^bkrnd
            Bm <- 10^Bm
            bkrndBm <- 10^bkrndBm
          }
          # duration assuming asymmetric  
          mag <- sum(diff(t[tiidx:ttaidx]) * (head(y[tiidx:ttaidx] - bkrnd, -1) + tail(y[tiidx:ttaidx] - bkrnd, -1))/2)
          amp <- Bm - bkrndBm
        }
        
      }
    }
  }
  
  values[1,] <- c(ti, tm, tt, td, mag, amp)
  return(list(values = values, yfit=rqfit, nofit_msg=NULL))
  
}
