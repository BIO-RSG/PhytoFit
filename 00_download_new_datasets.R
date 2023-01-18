# Stephanie.Clay@dfo-mpo.gc.ca
# 28 Jul 2020

# Run this script to download new datasets.

library(dplyr)
library(lubridate)
library(curl)

readYN <- function(pr) {
  if (interactive()) {
    n <- toupper(readline(prompt=pr))
  } else {
    cat(pr)
    con <- file("stdin")
    n <- toupper(readLines(con,1))
    close(con)
  }
  if(!(n=="Y" | n=="N")) {return()}
  return(n)
}

# for bytes to kilobytes, megabytes, or gigabytes
conv_factor_dir <- 1/1024/1024/1024 # gigabytes, for directory sizes
conv_factor_file <- 1/1024/1024 # megabytes, for file sizes

# subdirectory in the phytofit repository, where the data is stored
base_local <- paste0(getwd(),"/data/")


#*******************************************************************************
# GET LIST OF LOCAL DATASETS ####

cat("Retrieving list of files in local directory...\n")

local_df <- data.frame(location=character(),
                       reg_sens_var=character(),
                       region=character(),
                       sensor=character(),
                       variable=character(),
                       stringsAsFactors = FALSE)

local_regions <- list.files(base_local)

if (length(local_regions) > 0) {
  
  local_file_list <- lapply(local_regions, function(x) Sys.glob(paste0(base_local,x,"/*.fst"))) %>% do.call(what=c)
  local_file_list <- local_file_list[endsWith(local_file_list,".fst")]
  
  if (length(local_file_list) > 0) {
    
    # convert to a dataframe
    local_df <- data.frame(do.call(rbind, strsplit(basename(local_file_list), split="_")), stringsAsFactors = FALSE)
    colnames(local_df) <- c("region","sensor","variable","year")
    local_df <- local_df %>%
      dplyr::mutate(location = "local") %>%
      dplyr::select(-year) %>%
      tidyr::unite(col="reg_sens_var", region, sensor, variable, remove=FALSE) %>%
      dplyr::distinct()
    
  }
  
}


#*******************************************************************************
# GET LIST OF FTP SERVER DATASETS ####

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
                size_mb = round(as.numeric(size_mb) * conv_factor_file, 2)) %>%
  dplyr::select(location, filename, size_mb, region, sensor, variable) %>%
  tidyr::unite(col="reg_sens_var", region, sensor, variable, remove=FALSE)


#*******************************************************************************
# COMPARE FTP AND LOCAL FILENAMES ####

# subset ftp_df to the files from the existing local datasets
ftp_df <- ftp_df %>% dplyr::filter(!(reg_sens_var %in% local_df$reg_sens_var))

ftp_sets <- sort(unique(ftp_df$reg_sens_var))


#*******************************************************************************
# DOWNLOAD FILES ####

if (length(ftp_sets) > 0) {
  
  tmp_df <- ftp_df %>%
    dplyr::group_by(reg_sens_var) %>%
    dplyr::summarize(total_size_mb = sum(size_mb,na.rm=TRUE),
                     num_files_available = n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(region_sensor_variable=reg_sens_var) %>%
    as.data.frame()
  
  cat("Datasets to download:\n")
  print(tmp_df)
  cat("\n\n")
  
  for (i in 1:length(ftp_sets)) {
    current_set <- ftp_sets[i]
    tmp_df <- ftp_df %>% dplyr::filter(reg_sens_var==current_set)
    msg <- paste0("Download ",current_set," (",nrow(tmp_df)," files, ",
                  sum(tmp_df$size_mb,na.rm=TRUE),"mb total)? Y/N ")
    ans <- readYN(msg)
    while (is.null(ans)) {
      ans <- readYN(msg)
    }
    if (ans=="Y") {
      # create output directory for the region, if necessary
      current_region <- strsplit(current_set,"_")[[1]][1]
      dir.create(paste0(base_local,current_region), showWarnings=FALSE, recursive=TRUE)
      # download each file for the region and dataset
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
