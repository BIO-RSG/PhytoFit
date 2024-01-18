# Stephanie.Clay@dfo-mpo.gc.ca
# 11 Feb 2021

# Run this script to update your existing datasets.
# This will download the fst files for any years that are missing or out-of-date
# for the region/sensor/variable datasets existing in your PhytoFit/data/ subfolder.
# To run from the command line and ask before downloading each data subset:
#   Rscript [script directory]/00_update_datasets.R 'true'
# To run from the command line and download everything automatically:
#   Rscript [script directory]/00_update_datasets.R 'false'

library(dplyr)
library(lubridate)
library(RCurl)
source("readYN.R")

# should the script ask before downloading updates for each data subset?
ask_user <- FALSE

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


#*******************************************************************************
# GET LOCAL DATA ####

cat("Retrieving list of files in local directory...\n")

local_regions <- list.files(base_local)

if (length(local_regions)) {
  
  local_file_list <- list.files(base_local,recursive=TRUE)
  local_file_list <- sort(local_file_list[endsWith(local_file_list,".fst")])
  local_res <- file.info(file.path(base_local,local_file_list))
  
  if (nrow(local_res) > 0) {
    
    # convert to a dataframe
    local_df <- data.frame(date_modified_local=local_res$mtime,
                           size_mb_local=local_res$size,
                           filename=local_file_list,
                           stringsAsFactors = FALSE)
    tz(local_df$date_modified_local) <- Sys.timezone()
    metadata_df <- data.frame(do.call(rbind, strsplit(basename(local_df$filename), split="_")), stringsAsFactors = FALSE)
    colnames(metadata_df) <- c("region","sensor","variable","year")
    local_df <- dplyr::bind_cols(local_df, metadata_df)
    local_df <- local_df %>%
      dplyr::mutate(year = as.numeric(gsub(".fst","",year)),
                    size_mb_local = round(as.numeric(size_mb_local) * conv_factor_file, 2)) %>%
      dplyr::select(date_modified_local, size_mb_local, filename, year, region, sensor, variable)
    
  } else {
    
    stop("YOU HAVE NO DATASETS IN STORAGE!\nPlease run download_new_datasets.R")
    
  }
  
} else {
  
  stop("YOU HAVE NO DATASETS IN STORAGE!\nPlease run download_new_datasets.R")
  
}


#*******************************************************************************
# GET FTP DATA ####

base_ftp <- "ftp://ftp.dfo-mpo.gc.ca/bometrics/PhytoFit_datasets/"

cat("Retrieving list of files from",base_ftp,"...\n")

# get list of regions (subfolders)
ftp_reg_list <- sapply(strsplit(strsplit(getURL(base_ftp), split="\\r\\n")[[1]], split="\\s+"), "[[", 4)
# get list of files per region/subfolder
ftp_res <- sapply(paste0(base_ftp, ftp_reg_list, "/"), FUN=getURL) %>% strsplit(split="\\r\\n") %>% unlist() %>% unname()
# remove any non-.fst files
ftp_res <- ftp_res[endsWith(ftp_res,".fst")]
# convert to a dataframe
ftp_df <- do.call(rbind, strsplit(ftp_res, split="\\s+")) %>% data.frame(stringsAsFactors = FALSE)
colnames(ftp_df) <- c("date_modified_ftp","time_modified","size_mb_ftp","filename")
metadata_df <- do.call(rbind, strsplit(ftp_df$filename, split="_")) %>% data.frame(stringsAsFactors = FALSE)
colnames(metadata_df) <- c("region","sensor","variable","year")
ftp_df <- dplyr::bind_cols(ftp_df, metadata_df)
ftp_df <- ftp_df %>%
  dplyr::mutate(filename = paste0(region,"/",filename),
                year = as.numeric(gsub(".fst","",year)),
                size_mb_ftp = round(as.numeric(size_mb_ftp) * conv_factor_file, 2),
                date_modified_ftp = as_datetime(paste(date_modified_ftp,time_modified), format="%m-%d-%y %I:%M%p", tz="Canada/Eastern")) %>%
  dplyr::select(date_modified_ftp, size_mb_ftp, filename, year, region, sensor, variable)


#*******************************************************************************
# COMPARE FTP AND LOCAL TIMESTAMPS AND FILENAMES ####

# get the list of datasets that are in both the local and ftp copies
total_df <- dplyr::full_join(local_df, ftp_df, by=c("filename","year","region","sensor","variable")) %>%
  dplyr::group_by(region,sensor,variable) %>%
  dplyr::mutate(local_exists=any(is.finite(size_mb_local)),
                ftp_exists=any(is.finite(size_mb_ftp))) %>%
  dplyr::ungroup() %>%
  dplyr::filter(local_exists & ftp_exists)

# get the list of local files that are out of date or missing for each dataset
outdated <- total_df %>%
  dplyr::filter(date_modified_local < date_modified_ftp | !is.finite(size_mb_local)) %>%
  tidyr::unite(col="dataset",region,sensor,variable,sep="_")


#*******************************************************************************
# DOWNLOAD FILES ####

if (nrow(outdated) > 0) {
  ftp_sets <- outdated %>% dplyr::distinct(dataset) %>% unlist()
  for (i in 1:length(ftp_sets)) {
    current_set <- ftp_sets[i]
    tmp_df <- outdated %>% dplyr::filter(dataset==current_set)
    if (ask_user) {
      msg <- paste0("Update ",current_set," (",nrow(tmp_df)," files, ",
                    sum(tmp_df$size_mb_ftp,na.rm=TRUE),"mb total)? Y/N ")
      ans <- readYN(msg)
      while (is.null(ans)) {
        ans <- readYN(msg)
      }
    } else {
      ans <- "Y"
    }
    if (ans=="Y") {
      # download each file for the region and dataset
      tmpfiles <- tmp_df$filename
      for (j in 1:nrow(tmp_df)) {
        filename <- tmpfiles[j]
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
} else {
  cat("Everything up-to-date.")
}
