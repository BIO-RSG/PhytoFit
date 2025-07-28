# Stephanie.Clay@dfo-mpo.gc.ca
# 2025-03-11

# Calculate the seasonal averages weighted by daily percent coverage, where season boundaries were defined in tools_03_define_polygon_season_boundaries.R

rm(list=ls())
library(tidyr)
library(dplyr)
library(stringr)

# # scotian shelf
# file_avgs <- "verified_fits/occci/scotian_shelf_fall/verified_fits_scotian_shelf_fall.Rdata" # fall and spring files should have the same time annual time series csv
# file_timing_spring <- "verified_fits/occci/scotian_shelf_spring/verified_fits_scotian_shelf_spring.csv"
# file_timing_fall <- "verified_fits/occci/scotian_shelf_fall/verified_fits_scotian_shelf_fall.csv"
# seasonbounds_file <- "verified_fits/occci/scotian_shelf_polygon_season_bounds.csv"
# output_file <- "verified_fits/occci/scotian_shelf_weighted_seasonal_averages.csv"
# polys <- c("CSS_V02","ESS_V02","GB_V02","HL2","LS_V02","P5","WSS_V02")

# labrador sea
file_avgs <- "verified_fits/occci/labrador_sea_fall/verified_fits_labrador_sea_fall.Rdata" # fall and spring files should have the same time annual time series csv
file_timing_spring <- "verified_fits/occci/labrador_sea_spring/verified_fits_labrador_sea_spring.csv"
file_timing_fall <- "verified_fits/occci/labrador_sea_fall/verified_fits_labrador_sea_fall.csv"
seasonbounds_file <- "verified_fits/occci/labrador_sea_polygon_season_bounds.csv"
output_file <- "verified_fits/occci/labrador_sea_weighted_seasonal_averages.csv"
polys <- c("LAS","CLS","GS")

# # labrador sea modis
# file_avgs <- "verified_fits/modisaqua/labrador_sea_spring_linear/verified_fits_labrador_sea_spring_linear.Rdata" # fall and spring files should have the same time annual time series csv
# file_timing_spring <- "verified_fits/modisaqua/labrador_sea_spring_log/verified_fits_labrador_sea_spring_modisaqua.csv"
# file_timing_fall <- "verified_fits/modisaqua/labrador_sea_fall_log/verified_fits_labrador_sea_fall_modisaqua.csv"
# seasonbounds_file <- "verified_fits/modisaqua/labrador_sea_modisaqua_polygon_season_bounds.csv"
# output_file <- "verified_fits/modisaqua/labrador_sea_modisaqua_weighted_seasonal_averages.csv"
# polys <- c("LAS","CLS","GS")

years <- 1998:2024
ref_years <- 1999:2020


#*******************************************************************************
# READ DATA, FORMAT/REARRANGE

if (ref_years[1] < min(years) | ref_years[length(ref_years)] > max(years)) stop("Reference years beyond range of selected years")

# read the csv file with potential season bounds and select the quantiles to use for spring/summer/fall start
dfsb <- read.csv(seasonbounds_file)

# get data for spring/fall averages
df <- get(load(file_avgs)) %>%
  dplyr::select(Region, Year, stats) %>%
  tidyr::unnest(cols=stats) %>%
  dplyr::filter(Year %in% years & Region %in% polys) %>%
  dplyr::arrange(Region,Year) %>%
  dplyr::left_join(y=dfsb, by="Region") %>%
  dplyr::mutate(season=ifelse(doy<spring, "Winter",
                              ifelse(doy>=spring & doy<summer, "Spring",
                                     ifelse(doy>=summer & doy<fall, "Summer", "Fall")))) %>%
  dplyr::filter(percent_coverage>0)
df_avgs <- dplyr::bind_rows(df, df %>% dplyr::mutate(season="Annual")) %>%
  dplyr::group_by(Region, Year, season) %>%
  dplyr::summarize(weighted_average=weighted.mean(x=mean,w=percent_coverage,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Region, Year, season, weighted_average)

write.csv(df_avgs, file=output_file, row.names=FALSE, na="")
