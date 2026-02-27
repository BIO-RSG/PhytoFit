# Stephanie.Clay@dfo-mpo.gc.ca
# 2022-09-20

# Take the stats, metrics, and input settings from multiple fits run in PhytoFit, and pull
# them together into a single table. This can consolidate the results of both standard fits
# (i.e. multiple fits done using the "full run" feature with a single group of settings),
# and manual fits (i.e. fits done using unique settings).

rm(list=ls())
library(lubridate)
library(dplyr)
library(stringr)

#*******************************************************************************
# VARIABLES TO CHANGE

# Path to fits done using a standard group of settings (set standard_path=NULL to ignore these).
# This folder must contain subfolders named using the long filename describing the fit settings in PhytoFit, e.g: occciv6.0_atlantic_daily_1997-2024_cellSizeAll_logChlpoly4_thresh_created2025-03-11-092820.
# The subfolders must contain the stats, model metrics/parameters, and settings files. If you did a "full run" and downloaded the zip file, these are provided automatically:
#   - [year]_[box]_stats.csv files for EACH box/year that was fit,
#   - model_fit_params.csv, and
#   - settings.txt
# If you downloaded the stats, model metrics, and settings files individually, you must rename them to the naming convention above so they can be read into this script.

standard_path <- "verified_fits/occci/labrador_sea_fall/standard_fits/"
# TO ADD STANDARD FITS FOR SUBSEQUENT YEARS:
# - Use the settings.txt file from the standard_path (i.e. load it into PhytoFit and run it with the next year)
# - Add the rows from the new model_fit_params.csv file to the original model_fit_params.csv file

# Path to fits done with manual adjustments per fit (set manual_path=NULL to ignore these).
# Folder must contain the following files from EACH box/year that was fit:
#   - [fit details]_annual_stats.csv
#   - [fit details]_model_parameters.csv
#   - [fit details]_settings.txt
# WARNING!!!! Each box/year must have only ONE fit in this folder. Manual fits will
#   be grouped by box/year and sorted by datetime_fitted, and if any box/year has
#   more than one fit, only the most recent fit will be kept.
manual_path <- "verified_fits/occci/labrador_sea_fall/manual_fits/"

# If a box/year was fit with both standard and manual settings, should the manual fit replace the standard fit?
replace_standard <- TRUE

# Output csv filename that will contain the table with model metrics, parameters, and settings for each polygon/year that was fit.
# Full daily statistic tables for each fit will be written to an Rdata file with the same name.
output_file <- "verified_fits/occci/labrador_sea_fall/verified_fits_labrador_sea_fall.csv"

# include fits for these polygons
polys <- c("CLS","GS","LAS") # labrador sea
# polys <- c("P5") # bay of fundy
# polys <- c("CSS_V02","ESS_V02","WSS_V02","LS_V02","GB_V02","HL2","P5") # scotian shelf
# polys <- c("CS_V02","MS_V02","NEGSL_V02","NWGSL_V02") # gulf of saint lawrence
# polys <- c("AC","FP","HB","HIB","NENS","SAB","SES","SPB") # newfoundland
# polys <- c("SABMPA","SABUP","SABDOWN")
# polys <- c("custom")

# include fits for these years
years <- 1998:2025


#*******************************************************************************
# MAIN CODE

df_standard <- NULL
df_manual <- NULL


if (!is.null(standard_path)) {
  
  standard_groups <- list.files(standard_path, recursive=FALSE, full.names=TRUE)
  group_dates <- strsplit(basename(standard_groups),split="_") %>% sapply(FUN=tail,n=1) %>% gsub(pattern="created",replacement="") %>% as_datetime(format="%Y-%m-%d-%H%M%S")
  
  df_standard <- lapply(1:length(standard_groups), function(i) {
    # get the model fit parameters and metrics, and the settings used to create them
    df_standard_tmp <- dplyr::bind_cols(
      read.csv(file.path(standard_groups[i],"model_fit_params.csv")) %>%
        dplyr::mutate(datetime_fitted=format(group_dates[[i]],"%Y%m%d_%H%M%S"), manual_fit=FALSE),
      read.table(file.path(standard_groups[i],"settings.txt"), header = TRUE, sep="\\") %>%
        dplyr::select(setting_id,value) %>%
        tidyr::pivot_wider(names_from=setting_id,values_from=value) %>%
        dplyr::rename_with(function(x) paste0("settings_", x))) %>%
      dplyr::relocate(datetime_fitted,manual_fit) %>%
      tibble()
    st_files <- paste0(standard_groups[i],"/",df_standard_tmp$Year,"_",df_standard_tmp$Region,"_stats.csv")
    stats <- lapply(1:nrow(df_standard_tmp),FUN=function(i) read.csv(st_files[i]) %>% lapply(FUN=as.numeric) %>% do.call(what=dplyr::bind_cols))
    df_standard_tmp <- dplyr::bind_cols(df_standard_tmp, stats %>% tibble() %>% dplyr::rename(stats="."))
    return(df_standard_tmp)
  }) %>% do.call(what=dplyr::bind_rows)

}


if (!is.null(manual_path)) {
  
  manual_files <- list.files(manual_path,full.names=TRUE)
  manual_files <- manual_files[!endsWith(manual_files,".png")]
  
  if (length(manual_files)==0) {
    
    cat("No manual fits found.\n")
    
  } else {
    
    # FIGURE OUT THE FILENAMING SCHEME
    # i.e. did you run a series of fits after manually tweaking input parameters ("full run"), so that the files are in the format YYYY_[polygon]_stats.csv? or did you download each individual file manually
    fname_short <- !is.na(as.numeric(strsplit(basename(manual_files[1]),split="_")[[1]][1]))
    if (fname_short) {
      manual_fits <- gsub("_model_fit_params.csv|_settings.txt|_stats.csv","",basename(manual_files)) %>% strsplit(split="_")
      years_manual <- as.numeric(sapply(manual_fits,FUN=head,1))
      boxes_manual <- manual_fits %>% lapply(FUN=tail,-1) %>% sapply(FUN=paste0,collapse="_")
      manual_fits <- data.frame(Region=boxes_manual,Year=years_manual,File=manual_files) %>%
        dplyr::mutate(File_type=ifelse(endsWith(File,"_model_fit_params.csv"), "bf_file",
                                       ifelse(endsWith(File,"_settings.txt"), "set_file",
                                              ifelse(endsWith(File,"_stats.csv"), "st_file", NA)))) %>%
        tidyr::pivot_wider(names_from=File_type, values_from=File) %>%
        dplyr::distinct()
      dt_created <- file.info(manual_fits$bf_file)
      manual_fits$datetime_created <- ifelse(dt_created$mtime < dt_created$ctime, dt_created$mtime, dt_created$ctime) %>% as_datetime() %>% format("%Y%m%d_%H%M%S")
    } else {
        mf_try <- try({
          # get the list of years and polygons/boxes
          manual_fits <- strsplit(basename(manual_files),split="_") %>%
            lapply(FUN="[",3:6) %>%
            lapply(FUN=function(x) {
              year_loc <- str_locate(x,"\\d{4}")
              year_loc <- which((year_loc[,2]-year_loc[,1])==3)
              x <- x[1:year_loc]
              x <- x[!(x %in% c("daily","4day","8day"))]
              return(x)})
          years_manual <- as.numeric(sapply(manual_fits,FUN=tail,1))
          boxes_manual <- manual_fits %>% lapply(FUN=head,-1) %>% sapply(FUN=paste0,collapse="_")}, silent=TRUE)
        if (class(mf_try)=="try-error") {
          stop("manual_path must contain ONLY the annual_stats.csv, model_parameters.csv, and settings.txt files for the manually fitted box(es)/year(s). Make sure no files are missing and that there are no extra files or subfolders within that folder.\n")
        }
        manual_fits <- data.frame(Region=boxes_manual,Year=years_manual,File=manual_files) %>%
          dplyr::mutate(File_type=ifelse(endsWith(File,"_model_parameters.csv"), "bf_file",
                                         ifelse(endsWith(File,"_settings.txt"), "set_file",
                                                ifelse(endsWith(File,"_stats.csv"), "st_file", NA)))) %>%
          tidyr::pivot_wider(names_from=File_type, values_from=File) %>%
          dplyr::distinct() %>%
          dplyr::mutate(datetime_created=strsplit(bf_file,split="_") %>% lapply(FUN=function(x) x[which(grepl("created",x))]) %>% sapply(FUN=substr,start=8,stop=24) %>% as_datetime(format="%Y-%m-%d-%H%M%S") %>% format("%Y%m%d_%H%M%S"))
        
    }
    
    # READ THE FILES
    if (is.null(manual_fits$bf_file) | is.null(manual_fits$set_file) | is.null(manual_fits$st_file)) {
      stop("manual_path is missing annual_stats.csv, model_parameters.csv, or settings.txt files.\n")
    }
    if (any(is.na(unlist(manual_fits[,c("bf_file","set_file","st_file")])))) {
      stop("manual_path is missing annual_stats.csv, model_parameters.csv, or settings.txt files for some box(es)/year(s).\n")
    }
    
    df_manual <- lapply(1:nrow(manual_fits), FUN=function(i) {
      mf <- manual_fits[i,]
      dt_created <- mf$datetime_created
      df <- dplyr::bind_cols(
        read.csv(mf$bf_file) %>%
          dplyr::mutate(manual_fit=TRUE, datetime_fitted=dt_created, Region=mf$Region, Year=mf$Year),
        read.table(mf$set_file, header = TRUE, sep="\\") %>%
          dplyr::select(setting_id,value) %>%
          tidyr::pivot_wider(names_from=setting_id,values_from=value) %>%
          dplyr::rename_with(function(x) paste0("settings_", x))) %>%
        dplyr::relocate(datetime_fitted,manual_fit,Region,Year) %>%
        tibble()
      stats <- list(read.csv(mf$st_file) %>% lapply(FUN=as.numeric) %>% do.call(what=dplyr::bind_cols))
      df <- dplyr::bind_cols(df, stats %>% tibble() %>% dplyr::rename(stats="."))
      return(df)
    }) %>%
      do.call(what=dplyr::bind_rows) %>%
      # if there is more than one manual fit for a box/year, keep only the most recent fit
      dplyr::arrange(desc(datetime_fitted)) %>%
      dplyr::group_by(Region,Year) %>%
      dplyr::slice_head(n=1) %>%
      dplyr::ungroup()
    
  }
  
}

df <- dplyr::bind_rows(df_standard,df_manual)

if (replace_standard) {
  df <- df %>%
    dplyr::arrange(desc(manual_fit)) %>%
    dplyr::group_by(Region,Year) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup()
}

# reduce to the polygons and years you want
df <- df %>% dplyr::filter(Region %in% polys & Year %in% years)

# separate the full table of daily stats from each fit/row so it can be written to an Rdata file
df <- df %>% dplyr::arrange(Region,Year)

write.csv(df %>% dplyr::select(-stats), file=output_file, row.names=FALSE)
save(df, file=gsub(".csv",".Rdata",output_file), compress=TRUE)
