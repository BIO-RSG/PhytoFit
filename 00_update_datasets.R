# Stephanie.Clay@dfo-mpo.gc.ca
# 11 Feb 2021

# Run this script to update your existing datasets.
# This will download the fst files for any years that are missing or out-of-date
# for the region/sensor/variable datasets existing in your PhytoFit/data/ subfolder.

library(dplyr)
library(lubridate)
library(curl)
source("readYN.R")

# should the script ask before downloading updates for each data subset?
ask_user <- FALSE

# for bytes to kilobytes, megabytes, or gigabytes
conv_factor_dir <- 1/1024/1024/1024 # gigabytes, for directory sizes
conv_factor_file <- 1/1024/1024 # megabytes, for file sizes

# subdirectory in the phytofit repository, where the data is stored
base_local <- paste0(getwd(),"/data/")


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
    local_df <- data.frame(date_modified=local_res$mtime,
                           size_mb=local_res$size,
                           filename=local_file_list,
                           stringsAsFactors = FALSE)
    tz(local_df$date_modified) <- Sys.timezone()
    metadata_df <- data.frame(do.call(rbind, strsplit(basename(local_df$filename), split="_")), stringsAsFactors = FALSE)
    colnames(metadata_df) <- c("region","sensor","variable","year")
    local_df <- dplyr::bind_cols(local_df, metadata_df)
    local_df <- local_df %>%
      dplyr::mutate(location = "local",
                    year = as.numeric(gsub(".fst","",year)),
                    size_mb = round(as.numeric(size_mb) * conv_factor_file, 2)) %>%
      dplyr::select(location, filename, date_modified, size_mb, region, sensor, variable, year) %>%
      tidyr::unite(col="reg_sens_var", region, sensor, variable, remove=FALSE)
    
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

# get list of regions
con <- curl(base_ftp)
ftp_reg_list <- readLines(con)
close(con)
ftp_reg_list <- sapply(strsplit(ftp_reg_list, split="\\s+"), "[[", 4)

# get list of datasets/files from each region
ftp_res <- lapply(ftp_reg_list, function(x) {
  con <- curl(paste0(base_ftp, x, "/"))
  ftp_file_list <- readLines(con)
  close(con)
  return(ftp_file_list)
}) %>% do.call(what=c)
# remove any non-.fst files
ftp_res <- ftp_res[endsWith(ftp_res,".fst")]
# convert to a dataframe
ftp_df <- do.call(rbind, strsplit(ftp_res, split="\\s+")) %>% data.frame(stringsAsFactors = FALSE)
colnames(ftp_df) <- c("date_modified","time_modified","size_mb","filename")
metadata_df <- do.call(rbind, strsplit(ftp_df$filename, split="_")) %>% data.frame(stringsAsFactors = FALSE)
colnames(metadata_df) <- c("region","sensor","variable","year")
ftp_df <- dplyr::bind_cols(ftp_df, metadata_df)
ftp_df <- ftp_df %>%
  dplyr::mutate(location = "ftp",
                filename = paste0(region,"/",filename),
                year = as.numeric(gsub(".fst","",year)),
                size_mb = round(as.numeric(size_mb) * conv_factor_file, 2),
                date_modified = as_datetime(paste(date_modified,time_modified),
                                            format="%m-%d-%y %I:%M%p",
                                            tz="Canada/Eastern")) %>%
  dplyr::select(location, filename, date_modified, size_mb, region, sensor, variable, year) %>%
  tidyr::unite(col="reg_sens_var", region, sensor, variable, remove=FALSE)


#*******************************************************************************
# COMPARE FTP AND LOCAL TIMESTAMPS AND FILENAMES ####

# subset ftp_df to the files from the existing local datasets
ftp_df <- ftp_df %>% dplyr::filter(reg_sens_var %in% unique(local_df$reg_sens_var))

# find existing local files that have more recent modified times on the ftp site
diff_times <- dplyr::inner_join(ftp_df, local_df, by="filename") %>% dplyr::filter(date_modified.x > date_modified.y)

# find ftp files missing from the existing local datasets
missing_files <- ftp_df %>% dplyr::filter(!(filename %in% diff_times$filename) & !(filename %in% local_df$filename))

# combine list of files to download, with their sizes (in mb)
files_to_download <- dplyr::bind_rows(
  diff_times %>%
    dplyr::select(reg_sens_var.x,filename,size_mb.x) %>%
    dplyr::rename(reg_sens_var=reg_sens_var.x,size_mb=size_mb.x),
  missing_files %>%
    dplyr::select(reg_sens_var,filename,size_mb)
)


#*******************************************************************************
# DOWNLOAD FILES ####

if (nrow(files_to_download) > 0) {
  ftp_sets <- unique(files_to_download$reg_sens_var)
  for (i in 1:length(ftp_sets)) {
    current_set <- ftp_sets[i]
    tmp_df <- files_to_download %>% dplyr::filter(reg_sens_var==current_set)
    if (ask_user) {
      msg <- paste0("Update ",current_set," (",nrow(tmp_df)," files, ",
                    sum(tmp_df$size_mb,na.rm=TRUE),"mb total)? Y/N ")
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
        curl_download(url = paste0(base_ftp, filename),
                      destfile = paste0(base_local, filename))
      }
      cat("\n")
    }
  }
} else {
  cat("Everything up-to-date.")
}
