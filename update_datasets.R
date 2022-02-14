# Stephanie.Clay@dfo-mpo.gc.ca
# 11 Feb 2021

# Run this script to update your existing datasets.
# This will download the fst files for any years that are missing or out-of-date
# for the region/sensor/algorithm datasets existing in your PhytoFit/data/ subfolder.

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
# GET LOCAL DATA ####

cat("Retrieving list of files in local directory...\n")

local_file_list <- list.files(base_local, pattern=".fst", recursive=TRUE)
local_res <- file.info(paste0(base_local, local_file_list))

if (nrow(local_res) > 0) {
  
  # remove any non-.fst files
  local_res <- local_res[endsWith(rownames(local_res),".fst"),]
  # convert to a dataframe
  local_df <- data.frame(date_modified=local_res$mtime,
                         size_mb=local_res$size,
                         filename=gsub("data/","",local_file_list))
  tz(local_df$date_modified) <- Sys.timezone()
  metadata_df <- do.call(rbind, strsplit(local_df$filename, split="_"))
  colnames(metadata_df) <- c("region","sensor","algorithm","year")
  local_df <- dplyr::bind_cols(local_df, metadata_df)
  local_df <- local_df %>%
    dplyr::mutate(location = "local",
                  region = sapply(strsplit(region, split="/"), "[[", 2),
                  year = as.numeric(gsub(".fst","",year)),
                  size_mb = round(as.numeric(size_mb) * conv_factor_file, 2)) %>%
    dplyr::select(location, filename, date_modified, size_mb, region, sensor, algorithm, year) %>%
    tidyr::unite(col="reg_sens_alg", region, sensor, algorithm, remove=FALSE)
  
} else {
  
  stop("YOU HAVE NO DATASETS IN STORAGE!\nPlease run download_new_datasets.R")
  
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
ftp_df <- strsplit(ftp_res, split="\\s+") %>% do.call(what=rbind) %>% as.data.frame()
metadata_df <- do.call(rbind, strsplit(ftp_df$V4, split="_"))
colnames(metadata_df) <- c("region","sensor","algorithm","year")
colnames(ftp_df) <- c("date_modified","time_modified","size_mb","filename")
ftp_df <- dplyr::bind_cols(ftp_df, metadata_df)
ftp_df <- ftp_df %>%
  dplyr::mutate(location = "ftp",
                filename = paste0(region,"/",filename),
                year = as.numeric(gsub(".fst","",year)),
                size_mb = round(as.numeric(size_mb) * conv_factor_file, 2),
                date_modified = as_datetime(paste(date_modified,time_modified),
                                            format="%m-%d-%y %I:%M%p",
                                            tz="Canada/Eastern")) %>%
  dplyr::select(location, filename, date_modified, size_mb, region, sensor, algorithm, year) %>%
  tidyr::unite(col="reg_sens_alg", region, sensor, algorithm, remove=FALSE)


#*******************************************************************************
# COMPARE FTP AND LOCAL TIMESTAMPS AND FILENAMES ####

# subset ftp_df to the files from the existing local datasets
ftp_df <- ftp_df %>% dplyr::filter(reg_sens_alg %in% unique(local_df$reg_sens_alg))

# find existing local files that have different times on the ftp site
diff_times <- dplyr::inner_join(ftp_df, local_df, by="filename") %>% dplyr::filter(date_modified.x > date_modified.y)

# find ftp files missing from the existing local datasets
missing_files <- ftp_df %>% dplyr::filter(!(filename %in% diff_times$filename) & !(filename %in% local_df$filename))

# combine list of files to download, with their sizes (in mb)
files_to_download <- c(diff_times$filename, missing_files$filename)
sizes <- c(diff_times$size_mb.x, missing_files$size_mb)


#*******************************************************************************
# DOWNLOAD FILES ####

dir.create(paste0(base_local,"atlantic"), showWarnings=FALSE, recursive=TRUE)
dir.create(paste0(base_local,"pacific"), showWarnings=FALSE, recursive=TRUE)

if (length(files_to_download) > 0) {
  cat("Total download size =", sum(sizes), "mb, in the following files:\n")
  cat(paste0(files_to_download, collapse="\n"))
  ans <- readYN("Download all? Y/N ")
  while (is.null(ans)) {
    ans <- readYN("Download all? Y/N ")
  }
  if (ans=="Y") {
    for (i in 1:length(files_to_download)) {
      cat(paste0("Downloading ", files_to_download[i], "...\n"))
      curl_download(url = paste0(base_ftp, files_to_download[i]),
                    destfile = paste0(base_local, files_to_download[i]))
    }
    cat("DONE!")
  }
}
