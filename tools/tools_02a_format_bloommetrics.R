# Stephanie.Clay@dfo-mpo.gc.ca
# 2022-09-20

# Format bloom metrics and settings file so they can be used by other tool scripts.
# 
# This script can take the output of a set of standard fits (i.e. multiple fits
# done using the "full run" feature with a single group of settings), and a set
# of manual fits (i.e. a single fit done using unique settings).
#
# WARNING!!! Each box/year must have only ONE fit in the manual_path.
# After reading manual fits, they will be grouped by box/year and sorted by datetime_fitted,
# and if any box/year has more than one manual fit, only the most recent fit will be kept.
# If replace_standard=TRUE and a box/year has both a standard and manual fit, the
# manual fit will replace the standard fit.

rm(list=ls())

library(lubridate)
library(dplyr)
library(stringr)

#*******************************************************************************
# VARIABLES TO CHANGE

# Path to standard fits (set standard_path=NULL to exclude them).
# Folder must contain the .zip file that was downloaded from PhytoFit after running
# multiple fits, along with the following files extracted from the .zip file:
#   - [year]_[box]_stats.csv files for EACH box/year that was fit
#   - bloom_fit_params.csv
#   - settings.txt
standard_path <- "verified_fits/bloomfits_azmp/standard_fits/"

# Path to manual fits (set manual_path=NULL to exclude them).
# Folder must contain the following files from EACH box/year that was fit:
#   - [fit details]_annual_stats.csv
#   - [fit details]_bloom_parameters.csv
#   - [fit details]_settings.txt
# WARNING!!!! Each box/year must have only ONE set of fits in this folder
manual_path <- "verified_fits/bloomfits_azmp/manual_fits/"

# If a box/year was fit with both standard and manual settings, should the manual fit replace the standard fit?
replace_standard <- TRUE

# Output csv filename that will contain the bloom metrics, parameters, and settings
output_file <- "verified_fits/bloomfits_azmp/verified_fits_azmp.csv"

# Output Rdata filename that will contain the annual stats in a list in an Rdata file
output_stats_file <- "verified_fits/bloomfits_azmp/verified_fits_azmp_annual_stats.Rdata"


#*******************************************************************************
# MAIN CODE

df_standard <- NULL
df_manual <- NULL

if (!is.null(standard_path)) {
  zp_file <- list.files(path=standard_path,pattern=".zip")
  if (!file.exists(paste0(standard_path,zp_file))) stop(".zip file missing from standard_path")
  dt_created <- strsplit(zp_file,split="_")[[1]]
  dt_created <- substr(dt_created[which(grepl("created",dt_created))],start=8,stop=24)
  dt_created <- format(as_datetime(dt_created,format="%Y-%m-%d-%H%M%S"),"%Y%m%d_%H%M%S")
  df_standard <- dplyr::bind_cols(
    read.csv(paste0(standard_path,"bloom_fit_params.csv")) %>%
      dplyr::mutate(manual_fit=FALSE, datetime_fitted=dt_created),
    read.table(paste0(standard_path,"settings.txt"), header = TRUE, sep="\\") %>%
      dplyr::select(setting_id,value) %>%
      tidyr::pivot_wider(names_from=setting_id,values_from=value) %>%
      dplyr::rename_with(function(x) paste0("settings_", x))) %>%
    dplyr::relocate(datetime_fitted,manual_fit) %>%
    tibble()
  st_files <- paste0(standard_path,df_standard$Year,"_",df_standard$Region,"_stats.csv")
  stats <- lapply(1:nrow(df_standard),FUN=function(i) read.csv(st_files[i]))
  df_standard <- dplyr::bind_cols(df_standard, stats %>% tibble() %>% dplyr::rename(stats="."))
}


if (!is.null(manual_path)) {
  manual_files <- list.files(manual_path)
  if (length(manual_files)==0) {
    cat("No manual fits found.\n")
  } else {
    mf_try <- try({
      manual_fits <- strsplit(manual_files,split="_") %>%
        lapply(FUN="[",3:6) %>%
        lapply(FUN=function(x) {
          year_loc <- str_locate(x,"\\d{4}")
          year_loc <- which((year_loc[,2]-year_loc[,1])==3)
          x <- x[1:year_loc]
          x <- x[!(x %in% c("daily","weekly"))]
          return(x)})
      years <- as.numeric(sapply(manual_fits,FUN=tail,1))
      boxes <- manual_fits %>% lapply(FUN=head,-1) %>% sapply(FUN=paste0,collapse="_")}, silent=TRUE)
    if (class(mf_try)=="try-error") {
      stop("manual_path must contain ONLY the annual_stats.csv, bloom_parameters.csv, and settings.txt files for the manually fitted box(es)/year(s). Make sure no files are missing and that there are no extra files or subfolders within that folder.\n")
    }
    manual_fits <- data.frame(Region=boxes,Year=years,File=manual_files) %>%
      dplyr::mutate(File_type=ifelse(endsWith(File,"_bloom_parameters.csv"), "bf_file",
                                     ifelse(endsWith(File,"_settings.txt"), "set_file",
                                            ifelse(endsWith(File,"_stats.csv"), "st_file", NA)))) %>%
      tidyr::pivot_wider(names_from=File_type, values_from=File) %>%
      dplyr::distinct()
    if (is.null(manual_fits$bf_file) | is.null(manual_fits$set_file) | is.null(manual_fits$st_file)) {
      stop("manual_path is missing annual_stats.csv, bloom_parameters.csv, or settings.txt files.\n")
    }
    if (any(is.na(unlist(manual_fits[,c("bf_file","set_file","st_file")])))) {
      stop("manual_path is missing annual_stats.csv, bloom_parameters.csv, or settings.txt files for some box(es)/year(s).\n")
    }
    df_manual <- lapply(1:nrow(manual_fits), FUN=function(i) {
      mf <- manual_fits[i,]
      dt_created <- strsplit(mf$bf_file,split="_")[[1]]
      dt_created <- dt_created[which(grepl("created",dt_created))] %>% substr(start=8,stop=24)
      dt_created <- format(as_datetime(dt_created,format="%Y-%m-%d-%H%M%S"),"%Y%m%d_%H%M%S")
      df <- dplyr::bind_cols(
        read.csv(paste0(manual_path,mf$bf_file)) %>%
          dplyr::mutate(manual_fit=TRUE, datetime_fitted=dt_created, Region=mf$Region, Year=mf$Year),
        read.table(paste0(manual_path,mf$set_file), header = TRUE, sep="\\") %>%
          dplyr::select(setting_id,value) %>%
          tidyr::pivot_wider(names_from=setting_id,values_from=value) %>%
          dplyr::rename_with(function(x) paste0("settings_", x))) %>%
        dplyr::relocate(datetime_fitted,manual_fit,Region,Year) %>%
        tibble()
      stats <- list(read.csv(paste0(manual_path,mf$st_file)))
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

df <- df %>% dplyr::arrange(Region,Year)
stats <- df$stats
names(stats) <- paste0(df$Region,"_",df$Year)

write.csv(df %>% dplyr::select(-stats), file=output_file, row.names=FALSE)
save(stats, file=output_stats_file, compress=TRUE)
