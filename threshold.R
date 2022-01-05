threshold <- function(t, y, tall, yall, threshcoef, bloomShape = 'symmetric', tm_limits = c(1, 365), ti_limits = c(1, 365), log_chla=FALSE) {
  
  require(quantreg)
  
  y <- y$y
  
  # yall, tall - vector of points with high enough % coverage
  # y, t - vector of points within the selected range of days and with high enough % coverage
  
  if (length(y)==1) {
    rqfit <- NULL
  } else {
    # use quantile regression to model the line of background chla
    rqfit <- rq(yall ~ tall, tau = 0.25)
  }

  # To collect output later
  values <- data.frame(matrix(nrow=1,ncol=9), stringsAsFactors = FALSE)
  colnames(values) <- c("Mean", "Median", "t[start]", "t[max]", "t[end]",
                        "t[duration]", "Magnitude", "Amplitude", "Threshold")
  ti <- tm <- tt <- td <- mag <- amp <- thresh <- NA
  
  # make sure all values of y and t are valid
  yok <- !is.na(y)
  y <- y[yok]
  t <- t[yok]
  
  values[1,1:2] <- c(mean(y, na.rm=TRUE), median(y, na.rm=TRUE))
  
  # need at least 3 points
  if (length(y)<3) {return(list(values = values, nofit_msg="Unable to fit: Not enough data"))}
  
  # get index of max concentration, subset to limited range of days
  yday_tm <- t >= tm_limits[1] & t <= tm_limits[2]
  if (sum(yday_tm)==0) {return(list(values = values, nofit_msg="Unable to fit: Not enough data between selected t[max] limits"))}
  maxidx <- which(y==max(y[yday_tm],na.rm=TRUE) & yday_tm)
  if (length(maxidx)==0) {return(list(values = values, nofit_msg="Unable to fit: Not enough data between selected t[max] limits"))}
  
  tm <- t[maxidx]
  Bm <- y[maxidx]
  
  # find median of yearly data, and define threshold as threshcoef * median
  # note that if using logged chla, the median must be temporarily shifted before
  # multiplying it by the threshcoef
  med <- median(y, na.rm = TRUE)
  miny <- min(y, na.rm=TRUE)
  if (log_chla) {
    tmp_med <- 10^med
    thresh <- threshcoef * tmp_med
    thresh <- log10(thresh)
  } else {
    thresh <- threshcoef * med
  }
  
  # find where concentration drops below thresh for 14 days (before the bloom)
  yday_ti <- t >= ti_limits[1] & t <= tm
  if (sum(yday_ti)==0) {return(list(values = values, nofit_msg="Unable to fit: Not enough data between t[start] lower limit and t[max]"))}
  ytmp <- y[yday_ti]
  ttmp <- t[yday_ti]
  # temporarily reverse vector
  ytr <- rev(ytmp) - thresh
  # find runlength for above/below threshold (TRUE = above, FALSE = below)
  rle <- rle(ytr > 0)
  csrle <- cumsum(rle$lengths)
  #if(csrle[1] != 1) csrle <- c(1, csrle)
  dcsrle <- diff(rev(ttmp)[csrle]) * -1
  # find where values are below threshold for more than 14 days
  dvalues <- rle$values[2:length(rle$values)]
  # get a matching vector of backward days so you can use ti_limits
  tt_tmp <- rev(ttmp)[csrle][-1]
  # get the first instance of this, within the range of user-selected bloom start days
  okth <- which(dcsrle > 14 & !dvalues & tt_tmp <= ti_limits[2])[1]
  
  # NOTE: do not add 1 to index due to diff, want the time closest to maximum
  # esentially the last day of the 14 consecutive days where values were below thresh
  
  ti <- t[((maxidx - csrle[okth])+1)]
  
  if (bloomShape == 'symmetric') {
    
    td <- 2*(tm - ti)
    tt <- ti + td # termination time assuming symmetry  
    
    # calculate integral if ti and tt exist
    mag <- NA
    
    if (!is.na(ti) && !is.na(tt)) {
      
      tiidx <- which.min(abs(t - ti))
      ttidx <- which.min(abs(t - tt))
      
      # if termination day tt has no valid data, and has days with valid data 
      # before and after that are equally spaced, choose the first one (the one
      # closest to the max day)
      
      if (length(ttidx) > 1) {
        
        ttidx <- ttidx[1]
        
      }
      
      if (!is.null(rqfit)) {
        bkrnd <- predict(rqfit, newdata = data.frame(tall = t[tiidx:ttidx]))
        # calculate integral (magnitude)
        mag <- sum(diff(t[tiidx:ttidx]) * (head(y[tiidx:ttidx] - bkrnd, -1) + tail(y[tiidx:ttidx] - bkrnd, -1))/2)
        amp <- unname(Bm - predict(rqfit, newdata = data.frame(tall = tm)))
      }
      
    }
    
  } else if (bloomShape == 'asymmetric') {
    
    # find termination assuming assymetry
    ya <- y[maxidx:length(y)]
    ta <- t[maxidx:length(y)]
  
    ytar <- ya - thresh
    rler <- rle(ytar > 0)
  
    csrler <- cumsum(rler$lengths)
    #if(csrler[1] != 1) csrler <- c(1, csrler)
    dcsrler <- diff(ta[csrler])
    dvaluesa <- rler$values[2:length(rler$values)]
    oktha <- which(dcsrler > 14 & !dvaluesa)[1]
    tt <- ta[csrler[oktha]]
    td <- tt - ti
    
    mag <- NA
    
    if (!is.na(ti) && !is.na(tt)) {
      
      tiidx <- which.min(abs(t - ti))
      ttidx <- which.min(abs(t - tt))
      
      if (!is.null(rqfit)) {
        bkrnd <- predict(rqfit, newdata = data.frame(tall = t[tiidx:ttidx]))
        # calculate integral (magnitude)
        mag <- sum(diff(t[tiidx:ttidx]) * (head(y[tiidx:ttidx] - bkrnd, -1) + tail(y[tiidx:ttidx] - bkrnd, -1))/2)
        amp <- unname(Bm - predict(rqfit, newdata = data.frame(tall = tm)))
      }
      
    }
  }
  
  values[1,3:9] <- c(ti, tm, tt, td, mag, amp, thresh)
  
  return(list(values = values, nofit_msg=NULL))
  
}
