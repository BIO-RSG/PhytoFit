# Stephanie.Clay@dfo-mpo.gc.ca
# 28 Jul 2020

# Run this script to download new datasets.
# To run from the command line and ask before downloading each data subset:
#   Rscript [script directory]/00_download_new_datasets.R 'true'
# To run from the command line and download everything automatically:
#   Rscript [script directory]/00_download_new_datasets.R 'false'

library(dplyr)
library(lubridate)
library(RCurl)
source("scripts/readYN.R")

# should the script ask before downloading each data subset?
ask_user <- TRUE

# for bytes to kilobytes, megabytes, or gigabytes
conv_factor_dir <- 1/1024/1024/1024 # gigabytes, for directory sizes
conv_factor_file <- 1/1024/1024 # megabytes, for file sizes

# subdirectory in the phytofit repository, where the data is stored
base_local <- paste0(getwd(),"/data/")

# check if R script is run from command line with arguments
all_args <- commandArgs(trailingOnly=TRUE)
if (length(all_args) > 0) {
  ask_user <- as.logical(all_args[1])
}
print(ask_user)
#*******************************************************************************
# GET LIST OF LOCAL DATASETS ####

cat("Retrieving list of files in local directory...\n")

local_df <- data.frame(location=NA, region=NA, sensor=NA, variable=NA, local_exists=FALSE,
                       stringsAsFactors = FALSE)

local_regions <- list.files(base_local)

if (length(local_regions) > 0) {
  
  local_file_list <- lapply(local_regions, function(x) Sys.glob(paste0(base_local,x,"/*.fst"))) %>% do.call(what=c)
  local_file_list <- local_file_list[endsWith(local_file_list,".fst")]
  
  if (length(local_file_list) > 0) {
    local_df <- data.frame(do.call(rbind, strsplit(basename(local_file_list), split="_")), stringsAsFactors = FALSE)
    colnames(local_df) <- c("region","sensor","variable","year")
    local_df <- local_df %>% dplyr::select(-year) %>% dplyr::distinct() %>% dplyr::mutate(local_exists=TRUE)
  }
  
}


#*******************************************************************************
# GET LIST OF FTP SERVER DATASETS ####

base_ftp <- "ftp://ftp.dfo-mpo.gc.ca/bometrics/PhytoFit_datasets/"

cat("Retrieving list of files from",base_ftp,"...\n")

# get list of regions (subfolders)
ftp_reg_list <- sapply(strsplit(strsplit(getURL(base_ftp), split="\\r?\\n")[[1]], split="\\s+"), "[[", 4)
# get list of files per region/subfolder
ftp_res <- sapply(paste0(base_ftp, ftp_reg_list, "/"), FUN=getURL) %>% strsplit(split="\\r?\\n") %>% unlist() %>% unname()
# remove any non-.fst files
ftp_res <- ftp_res[endsWith(ftp_res,".fst")]
# convert to a dataframe
ftp_df <- do.call(rbind, strsplit(ftp_res, split="\\s+")) %>% data.frame(stringsAsFactors = FALSE)
colnames(ftp_df) <- c("date_modified","time_modified","size_mb","filename")
metadata_df <- do.call(rbind, strsplit(ftp_df$filename, split="_")) %>% data.frame(stringsAsFactors = FALSE)
colnames(metadata_df) <- c("region","sensor","variable","year")
ftp_df <- dplyr::bind_cols(ftp_df, metadata_df)
ftp_df <- ftp_df %>%
  dplyr::mutate(filename = paste0(region,"/",filename),
                size_mb = round(as.numeric(size_mb) * conv_factor_file, 2)) %>%
  dplyr::select(filename, size_mb, region, sensor, variable)


#*******************************************************************************
# COMPARE FTP AND LOCAL FILENAMES AND DOWNLOAD ####

# get the list of datasets missing from the local copy
missing_data <- dplyr::left_join(ftp_df, local_df, by=c("region","sensor","variable")) %>%
  tidyr::drop_na(region) %>%
  dplyr::group_by(region,sensor,variable) %>%
  dplyr::mutate(local_exists=any(!is.na(local_exists))) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!local_exists) %>%
  dplyr::select(-local_exists)

if (nrow(missing_data) > 0) {
  
  # prevent timeouts if download takes too long by increasing the timeout value (in seconds)
  # (timeout value will be reset after downloads are complete)
  current_timeout <- getOption("timeout")
  options(timeout=600)
  
  tmpmd <- missing_data %>%
    dplyr::group_by(region,sensor,variable) %>%
    dplyr::summarize(total_size_mb = sum(size_mb,na.rm=TRUE),
                     num_files_available = n()) %>%
    dplyr::ungroup()
  
  cat("Datasets to download:\n")
  print(tmpmd)
  cat("\n\n")
  
  for (i in 1:nrow(tmpmd)) {
    current_set <- tmpmd[i,]
    tmp_df <- dplyr::inner_join(missing_data,current_set,by=c("region","sensor","variable"))
    if (ask_user) {
      msg <- paste0("Download ",paste0(unlist(current_set[,1:3]),collapse="_"),
                    " (",nrow(tmp_df)," files, ",
                    sum(tmp_df$size_mb,na.rm=TRUE),"mb total)? Y/N ")
      ans <- readYN(msg)
      while (is.null(ans)) {
        ans <- readYN(msg)
      }
    } else {
      ans <- "Y"
    }
    if (ans=="Y") {
      # create output directory for the region, if necessary
      dir.create(paste0(base_local,current_set$region), showWarnings=FALSE, recursive=TRUE)
      # download each file for the region and dataset
      for (j in 1:nrow(tmp_df)) {
        filename <- tmp_df$filename[j]
        cat(paste0("Downloading ", filename, "...\n"))
        download.file(url = paste0(base_ftp, filename),
                      destfile = paste0(base_local, filename),
                      method = "libcurl",
                      quiet = TRUE,
                      mode = "wb")
      }
      cat("\n")
    }
    
  }
  
  cat("DONE!\n")
  
  # reset download timeout value
  options(timeout=current_timeout)
  
} else {
  cat("No new datasets to download.")
}
