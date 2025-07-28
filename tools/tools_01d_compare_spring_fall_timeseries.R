# Stephanie.Clay@dfo-mpo.gc.ca
# 2025-03-11

# Run a quick check of the annual stats tables in verified_fits of spring and fall - THEY SHOULD BE THE SAME (only the bloom metrics themselves are different)

rm(list=ls())
library(dplyr)
library(lubridate)

# spring_file <- "verified_fits/occci/scotian_shelf_spring/verified_fits_scotian_shelf_spring.Rdata"
# fall_file <- "verified_fits/occci/scotian_shelf_fall/verified_fits_scotian_shelf_fall.Rdata"

spring_file <- "verified_fits/occci/labrador_sea_spring/verified_fits_labrador_sea_spring.Rdata"
fall_file <- "verified_fits/occci/labrador_sea_fall/verified_fits_labrador_sea_fall.Rdata"


#*******************************************************************************

df_spring <- get(load(spring_file)) %>%
  dplyr::select(Region,Year,stats) %>%
  tidyr::unnest(col=stats) %>%
  dplyr::arrange(Region,Year,doy) %>%
  dplyr::select(-model,-background,-loess)
df_fall <- get(load(fall_file)) %>%
  dplyr::select(Region,Year,stats) %>%
  tidyr::unnest(col=stats) %>%
  dplyr::arrange(Region,Year,doy) %>%
  dplyr::select(-model,-background,-loess)

# are they identical?
cat("Identical spring and fall stats dataframes? (Minus model/background/loess)\n")
print(identical(df_spring,df_fall))

# if not, are some regions/years/doys present in one but not the other?
cat("Identical Region, Year, doy columns in spring and fall dataframes?\n")
print(all(df_spring$Region==df_fall$Region))
print(all(df_spring$Year==df_fall$Year))
print(all(df_spring$doy==df_fall$doy))

#*****************************************
# DIAGNOSE DIFFERENCES BELOW

# if region, year, and doy are identical, which other columns have different values?
coldiffs <- sapply(4:ncol(df_spring), function(i) !identical(unlist(df_spring[,i]),unlist(df_fall[,i])))

# within those columns, which values are different?
diffcols <- lapply(which(coldiffs)+3, function(i) {
  tmps <- unlist(df_spring[,i])
  tmpf <- unlist(df_fall[,i])
  inds <- which(sapply(1:length(tmps), function(j) !identical(tmps[j],tmpf[j])))
  dplyr::full_join(
    df_spring[,c(1:3,i)],
    df_fall[,c(1:3,i)],
    by=c("Region","Year","doy"))[inds,]
})

# days with only one valid observation
sum(df_spring$nobs==1)
df_spring %>% dplyr::group_by(Region,Year) %>% dplyr::summarize(nobs1=sum(nobs==1,na.rm=TRUE)) %>% dplyr::ungroup() %>% print() # how many per polygon/year?
df_spring %>% dplyr::group_by(Region) %>% dplyr::summarize(percov1=first(percent_coverage[nobs==1])) %>% dplyr::ungroup() %>% print() # what %cov is this for each polygon?


