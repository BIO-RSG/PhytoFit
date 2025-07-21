# Stephanie.Clay@dfo-mpo.gc.ca
# 2025-06-25

# Download gridded OC-CCI daily 1km resolution files and format for use in PhytoFit.
# For a given year, the daily netcdfs are merged into an annual multilayer netcdf.
# Missing days or days with no valid data in your region of interest are included as blank layers, except for days in the most recent year where data does not exist yet.
# Maximum 365 days used, day 366 of leap years are cut out.

# https://climate.esa.int/en/projects/ocean-colour/#_data-tab
# https://www.oceancolour.org/thredds/ncss/grid/CCI_ALL-v6.0-1km-DAILY/dataset.html

rm(list=ls())
library(RCurl)
library(httr)
library(httr2)
library(dplyr)
library(lubridate)
library(oceancolouR)
library(terra)
library(stringr)
library(pbapply)

# overwrite existing daily files with new ones?
# note: if there are new files for a given year, the annual netcdf will be remade for that year
overwrite <- FALSE

# # download full time series
# start <- as_date("19970904")
# end <- Sys.Date()

# or update
start <- as_date("20250623")
end <- Sys.Date()

# NOTE:
#   Output filenames will be in the format [region]_occciv6.0_chloccci_[year].nc.
#   **If you change "spatial_res" but not "region", files with different spatial resolutions will all be written to the same folder and will not merge correctly.
#   To avoid this, simply change the region name if you change spatial_res (e.g. region="tanzania4km")
region <- "tanzania" # this should be identical to one of the subfolder names within the "data" folder
spatial_res <- "1km" # 1km or 4km (CURRENTLY 4KM IS NOT WORKING)
lonlim <- c(38.5,41.5)
latlim <- c(-10,-3.5)


#*******************************************************************************

output_path <- paste0("data/",region)
output_path_temp <- paste0("data/temp/",region)

dir.create(output_path, showWarnings=FALSE)
dir.create(output_path_temp, showWarnings=FALSE)

dates <- as_date(start:end)

# which oc-cci files exist for the selected dates?
years <- sort(unique(year(dates)))
existing_dates <- pblapply(years, function(y) {
  link <- paste0("https://www.oceancolour.org/thredds/catalog/cci/v6.0-1km-release/geographic/",y,"/catalog.html")
  test <- httr2::request(link) %>% req_perform() %>% resp_body_string()
  tmp_date <- dates[year(dates)==y]
  tmp_date[str_detect(test,format(tmp_date,"%Y%m%d"))]
}) %>% do.call(what=c)
dates <- existing_dates


#*******************************************************************************
# download individual days

if (!overwrite) {
  # subset to the files that don't already exist on your local machine
  local_files <- list.files(output_path_temp)
  local_file_dates <- strsplit(local_files,split="_") %>% sapply(FUN=tail,n=1) %>% substr(start=1,stop=8) %>% as_date()
  dates <- dates[!(dates %in% local_file_dates)]
}

if (length(dates)>0) {
  cat("Downloading new daily files...\n")
  occci_base <- paste0("https://www.oceancolour.org/thredds/ncss/CCI_ALL-v6.0-",ifelse(spatial_res=="1km","1km-",""),"DAILY")
  # download all existing files for the selected date range first, subset to the region of interest
  for (i in 1:length(dates)) {
    tmp_date <- dates[i]
    print(tmp_date)
    ofile <- paste0(output_path_temp,"/",region,"_occciv6.0_chloccci_",format(tmp_date,"%Y%m%d"),".nc4")
    link <- paste0(occci_base,"?var=chlor_a&north=",latlim[2],"&west=",lonlim[1],"&east=",lonlim[2],"&south=",latlim[1],"&horizStride=1&time_start=",tmp_date,"T00%3A00%3A00Z&time_end=",tmp_date,"T23%3A59%3A59Z&timeStride=1&accept=netcdf4")
    cmd <- paste0("wget -O '",ofile,"' --quiet '",link,"'")
    system(cmd)
  }
}


#*******************************************************************************
# merge daily data into annual tifs

if (length(dates)>0) {
  
  cat("Merging new files into annual netcdfs...\n")
  
  # get the years that need to be remade
  years <- sort(unique(year(dates)))
  
  # get the full list of available files
  files_available <- list.files(output_path_temp, full.names=TRUE)
  dates_available <- strsplit(basename(files_available),split="_") %>% sapply(FUN=tail,n=1) %>% substr(start=1,stop=8) %>% as_date()
  
  for (y in years) {
    print(y)
    tmp_files <- files_available[y==year(dates_available)]
    output_filename <- paste0(output_path,"/",region,"_occciv6.0_chloccci_",y,".nc")
    r <- terra::rast(tmp_files)
    # remove empty layers that might have formatting issues
    rempty <- r
    rempty[!is.finite(rempty)] <- NA
    rempty_ind <- is.finite(as.numeric(minmax(rempty)[1,]))
    r <- r[[rempty_ind]]
    # fill in missing dates
    r <- fillTime(r)
    minday <- yday(min(time(r)))
    if (minday!=1) {
      empty_layer <- rep(r[[1]],minday-1)
      values(empty_layer) <- NA
      time(empty_layer) <- as_date(as_date(paste0(y,"0101")):(min(time(r))-1))
      r <- c(empty_layer,r)
    }
    # for consistency with binned data handling, only use 365 days per year
    if (yday(max(time(r)))>365) { 
      r <- r[[which(yday(time(r))<=365)]]
    }
    # write to netcdf file
    terra::writeCDF(r, output_filename, compress=9, overwrite=TRUE)
    # test <- terra::rast(output_filename)
    # make_raster_map(test[[1]],xlim=lonlim,ylim=latlim,trans="log10")
  }
  
}
