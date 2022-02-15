# Stephanie.Clay@dfo-mpo.gc.ca
# 28 Jul 2020

# Run this script to download new datasets.

library(dplyr)
library(lubridate)
library(curl)

readYN <- function(pr) { 
  n <- toupper(readline(prompt=pr))
  if(!(n=="Y" | n=="N")) {return()}
  return(n)
}

# for bytes to kilobytes, megabytes, or gigabytes
conv_factor_dir <- 1/1024/1024/1024 # gigabytes, for directory sizes
conv_factor_file <- 1/1024/1024 # megabytes, for file sizes

# subdirectory in the phytofit repository, where the data is stored
base_local <- "data/"


#*******************************************************************************
# GET LIST OF LOCAL DATASETS ####

cat("Retrieving list of files in local directory...\n")

local_file_list <- c(Sys.glob(paste0(base_local,"atlantic/*.fst")),
                     Sys.glob(paste0(base_local,"pacific/*.fst")))
# remove any non-.fst files
local_file_list <- local_file_list[endsWith(local_file_list,".fst")]

if (length(local_file_list) > 0) {
  
  # convert to a dataframe
  local_df <- data.frame(do.call(rbind, strsplit(gsub("data/","",local_file_list), split="_")),
                         stringsAsFactors = FALSE)
  colnames(local_df) <- c("region","sensor","algorithm","year")
  local_df <- local_df %>%
    dplyr::mutate(location = "local",
                  region = sapply(strsplit(region, split="/"), "[[", 2)) %>%
    dplyr::select(location, region, sensor, algorithm) %>%
    tidyr::unite(col="reg_sens_alg", region, sensor, algorithm, remove=FALSE) %>%
    dplyr::distinct()
  
} else {
  
  local_df <- data.frame(location=character(),
                         reg_sens_alg=character(),
                         region=character(),
                         sensor=character(),
                         algorithm=character(),
                         stringsAsFactors = FALSE)
  
}


#*******************************************************************************
# GET FTP DATA ####

base_ftp <- "ftp://ftp.dfo-mpo.gc.ca/bometrics/PhytoFit_datasets/"

cat("Retrieving list of files from",base_ftp,"...\n")

con <- curl(paste0(base_ftp, "atlantic/"))
atlantic_ftp <- readLines(con)
close(con)
con <- curl(paste0(base_ftp, "pacific/"))
pacific_ftp <- readLines(con)
close(con)
ftp_res <- c(atlantic_ftp, pacific_ftp)
# remove any non-.fst files
ftp_res <- ftp_res[endsWith(ftp_res,".fst")]
# convert to a dataframe
ftp_df <- do.call(rbind, strsplit(ftp_res, split="\\s+")) %>% data.frame(stringsAsFactors = FALSE)
colnames(ftp_df) <- c("date_modified","time_modified","size_mb","filename")
metadata_df <- do.call(rbind, strsplit(ftp_df$filename, split="_")) %>% data.frame(stringsAsFactors = FALSE)
colnames(metadata_df) <- c("region","sensor","algorithm","year")
ftp_df <- dplyr::bind_cols(ftp_df, metadata_df)
ftp_df <- ftp_df %>%
  dplyr::mutate(location = "ftp",
                filename = paste0(region,"/",filename),
                size_mb = round(as.numeric(size_mb) * conv_factor_file, 2)) %>%
  dplyr::select(location, filename, size_mb, region, sensor, algorithm) %>%
  tidyr::unite(col="reg_sens_alg", region, sensor, algorithm, remove=FALSE)


#*******************************************************************************
# COMPARE FTP AND LOCAL FILENAMES ####

# subset ftp_df to the files from the existing local datasets
ftp_df <- ftp_df %>% dplyr::filter(!(reg_sens_alg %in% local_df$reg_sens_alg))

ftp_sets <- sort(unique(ftp_df$reg_sens_alg))


#*******************************************************************************
# DOWNLOAD FILES ####

dir.create(paste0(base_local,"atlantic"), showWarnings=FALSE, recursive=TRUE)
dir.create(paste0(base_local,"pacific"), showWarnings=FALSE, recursive=TRUE)

if (length(ftp_sets) > 0) {
  
  tmp_df <- ftp_df %>%
    dplyr::group_by(reg_sens_alg) %>%
    dplyr::summarize(total_size_mb = sum(size_mb,na.rm=TRUE),
                     num_files_available = n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(region_sensor_algorithm=reg_sens_alg) %>%
    as.data.frame()
  
  cat("Datasets to download:\n")
  print(tmp_df)
  cat("\n")
  
  for (i in 1:length(ftp_sets)) {
    current_set <- ftp_sets[i]
    tmp_df <- ftp_df %>% dplyr::filter(reg_sens_alg==current_set)
    msg <- paste0("Download ",current_set," (",nrow(tmp_df)," files, ",
                  sum(tmp_df$size_mb,na.rm=TRUE),"mb total)? Y/N ")
    ans <- readYN(msg)
    while (is.null(ans)) {
      ans <- readYN(msg)
    }
    if (ans=="Y") {
      for (j in 1:nrow(tmp_df)) {
        filename <- tmp_df$filename[j]
        cat(paste0("Downloading ", filename, "...\n"))
        curl_download(url = paste0(base_ftp, filename),
                      destfile = paste0(base_local, filename))
      }
      cat("\n")
    }
    
  }
  
  cat("DONE!")
  
} else {
  cat("No new datasets to download.")
}
