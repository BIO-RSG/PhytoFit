threshold <- function(dfin, dfin_all, threshcoef, bloomShape = 'symmetric', tm_limits = c(1, 365), ti_limits = c(1, 365), log_chla=FALSE, rm_bkrnd=FALSE, use_weights=FALSE) {
  
  require(quantreg)
  
  yv <- dfin$var
  yvf <- dfin$var_to_fit
  t <- dfin$yday
  yall <- dfin_all$var
  tall <- dfin_all$yday
  wall <- dfin_all$weight
  
  rqfit <- NULL
  
  # yall, tall - vector of points with high enough % coverage, used here to calculate background line
  # yvf, t - vector of points within the selected range of days and with high enough % coverage
  
  # use quantile regression to model the line of background chla
  if (length(yvf)>1) {
    if (use_weights) {
      rqfit <- quantreg::rq(yall ~ tall, tau = 0.25, weights=wall)
    } else {
      rqfit <- quantreg::rq(yall ~ tall, tau = 0.25)
    }
  }

  # To collect output later
  values <- data.frame(matrix(nrow=1,ncol=7), stringsAsFactors = FALSE)
  colnames(values) <- c("t[start]","t[max_real]","t[end]","t[duration]","Magnitude","Amplitude","Threshold")
  ti <- tm <- tt <- td <- mag <- amp <- thresh <- NA
  
  # make sure all values of yvf and t are valid
  yok <- !is.na(yvf)
  yv <- yv[yok]
  yvf <- yvf[yok]
  t <- t[yok]
  
  # need at least 3 points
  if (length(yvf)<3) return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data"))
  
  # get index of max concentration, subset to limited range of days
  yday_tm <- t >= tm_limits[1] & t <= tm_limits[2]
  if (sum(yday_tm)==0) return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data between selected t[max] limits"))
  maxidx <- which(yvf==max(yvf[yday_tm],na.rm=TRUE) & yday_tm)
  if (length(maxidx)==0) return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data between selected t[max] limits"))
  
  tm <- t[maxidx]
  Bm <- yvf[maxidx]
  
  # find median of yearly data, and define threshold as threshcoef * median
  # note that if using logged chla, the median must be temporarily shifted before
  # multiplying it by the threshcoef
  med <- median(yvf, na.rm = TRUE)
  miny <- min(yvf, na.rm=TRUE)
  if (log_chla) {
    tmp_med <- 10^med
    thresh <- threshcoef * tmp_med
    thresh <- log10(thresh)
  } else {
    thresh <- threshcoef * med
  }
  
  # find where concentration drops below thresh for 14 days (before the bloom)
  yday_ti <- t >= ti_limits[1] & t <= tm
  if (sum(yday_ti)==0) return(list(values = values, yfit=rqfit, nofit_msg="Unable to fit: Not enough data between t[start] lower limit and t[max]"))
  ytmp <- yvf[yday_ti]
  ttmp <- t[yday_ti]
  # temporarily reverse vector and subtract the threshold so you can test when it crosses the zero line
  ytr <- rev(ytmp) - thresh
  # find runlength for above/below threshold (TRUE = above, FALSE = below)
  # note that this only includes days with valid data
  rle <- rle(ytr > 0)
  csrle <- cumsum(rle$lengths)
  #if(csrle[1] != 1) csrle <- c(1, csrle)
  # you have a vector of backward yvf values, now get a matching vector of backward days, subset to the days when yvf crosses the threshold
  tt_tmp <- rev(ttmp)[csrle]
  # calculate the difference between days when yvf crosses the threshold, and multiply by -1 since your time vector is reversed and these results will be negative
  dcsrle <- diff(tt_tmp) * -1
  # get the runlength vector indicating whether each run is above or below the threshold
  dvalues <- rle$values[2:length(rle$values)]
  # get a matching vector of backward days so you can use ti_limits
  tt_tmp <- tt_tmp[-1] # remove first element of tt_tmp
  # get the first instance of 14 consecutive days over the threshold, using dvalues to ensure these are days ABOVE the threshold instead of below, and within the range of user-selected bloom start days
  okth <- which(dcsrle > 14 & !dvalues & tt_tmp <= ti_limits[2])[1]
  
  # NOTE: do not add 1 to index due to diff, want the time closest to maximum
  # esentially the last day of the 14 consecutive days where values were below thresh
  
  ti <- t[(maxidx - csrle[okth])+1]
  
  if (bloomShape == 'symmetric') {
    td <- 2*(tm - ti)
    tt <- ti + td # termination time assuming symmetry  
  } else if (bloomShape == 'asymmetric') {
    # find termination assuming asymmetry
    ya <- yvf[maxidx:length(yvf)]
    ta <- t[maxidx:length(yvf)]
    ytar <- ya - thresh
    rler <- rle(ytar > 0)
    csrler <- cumsum(rler$lengths)
    #if(csrler[1] != 1) csrler <- c(1, csrler)
    dcsrler <- diff(ta[csrler])
    dvaluesa <- rler$values[2:length(rler$values)]
    oktha <- which(dcsrler > 14 & !dvaluesa)[1]
    tt <- ta[csrler[oktha]]
    td <- tt - ti
  }
  
  # calculate integral (magnitude under curve) and amplitude (max chla minus background chla) if ti and tt exist
  if (!is.na(ti) && !is.na(tt)) {
    # if termination day tt has no valid data, and has days with valid data before and after that are equally spaced, choose the first one (the one closest to the max day)
    tiidx <- which.min(abs(t - ti))[1]
    ttidx <- which.min(abs(t - tt))[1]
    if (!is.null(rqfit)) {
      yvf <- yvf[tiidx:ttidx]
      t <- t[tiidx:ttidx]
      bkrnd <- predict(rqfit, newdata = data.frame(tall = t))
      bkrndBm <- predict(rqfit, newdata = data.frame(tall = tm))
      if (log_chla) {
        yvf <- 10^yvf
        bkrnd <- 10^bkrnd
        Bm <- 10^Bm
        bkrndBm <- 10^bkrndBm
      }
      if (rm_bkrnd) {
        yvf <- yvf - bkrnd
        Bm <- Bm - bkrndBm
      }
      mag <- sum(diff(t) * (head(yvf, -1) + tail(yvf, -1))/2)
      amp <- unname(Bm)
    }
  }
  
  if (log_chla) {thresh <- 10^thresh} # convert threshold back to linear space for presentation, now that modelling is done
  values[1,] <- c(ti, tm, tt, td, mag, amp, thresh)
  
  return(list(values = values, yfit=rqfit, nofit_msg=NULL))
  
}
