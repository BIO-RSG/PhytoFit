# Stephanie.Clay@dfo-mpo.gc.ca
# 2025-03-11

# Define "season" boundaries for each polygon based on spring and fall bloom start/end over a reference period.

rm(list=ls())
library(dplyr)

# spring and fall verified_fits for labrador sea
file_spring <- "verified_fits/occci/labrador_sea_spring/verified_fits_labrador_sea_spring.csv"
file_fall <- "verified_fits/occci/labrador_sea_fall/verified_fits_labrador_sea_fall.csv"
output_file <- "verified_fits/occci/labrador_sea_polygon_season_bounds.csv"
polys <- c("LAS","CLS","GS")

# # spring and fall verified_fits for scotian shelf
# file_spring <- "verified_fits/occci/scotian_shelf_spring/verified_fits_scotian_shelf_spring.csv"
# file_fall <- "verified_fits/occci/scotian_shelf_fall/verified_fits_scotian_shelf_fall.csv"
# output_file <- "verified_fits/occci/scotian_shelf_polygon_season_bounds.csv"
# polys <- c("CSS_V02","ESS_V02","GB_V02","HL2","LS_V02","P5","WSS_V02")

# define the climatological period used in calculations
ref_years <- 1999:2020


#*******************************************************************************

all_stats <- function(x,probs) {
  dplyr::bind_cols(
    data.frame(mean=mean(x,na.rm=TRUE),
               median=median(x,na.rm=TRUE),
               sd=sd(x,na.rm=TRUE)),
    t(as.data.frame(quantile(x,probs=probs,na.rm=TRUE))))
}

df <- dplyr::left_join(
  read.csv(file_spring) %>%
    dplyr::filter(Region %in% polys & Year %in% ref_years) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarize(tstart_spring=all_stats(t.start.,probs=c(0.05,0.1,0.2)),
                     tstart_summer=all_stats(t.end.,probs=c(0.8,0.9,0.95))) %>%
    tidyr::unnest(cols=c(tstart_spring,tstart_summer), names_sep="_") %>%
    dplyr::ungroup(),
  read.csv(file_fall) %>%
    dplyr::filter(Region %in% polys & Year %in% ref_years) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarize(tstart_fall=all_stats(t.start.,probs=c(0.05,0.1,0.2,0.5))) %>%
    tidyr::unnest(cols=c(tstart_fall), names_sep="_") %>%
    dplyr::ungroup(),
  by=c("Region"))

# FINAL CHOICE
df <- df %>%
  dplyr::rename(spring=`tstart_spring_20%`,
                summer=`tstart_summer_80%`,
                fall=`tstart_fall_20%`) %>%
  dplyr::select(Region,spring,summer,fall)

write.csv(df, file=output_file, row.names=FALSE)

