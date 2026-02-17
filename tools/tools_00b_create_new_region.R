# Stephanie.Clay@dfo-mpo.gc.ca
# 2022-06-10

# Use this to create reginfo.rds, containing the info on the regions you want to use in PhytoFit.
# See 00_create_new_region_tutorial.html for details and examples.
# To ignore a region defined below (e.g. atlantic), just comment out that block of code.

# remotes::install_github("BIO-RSG/oceancolouR", build_vignettes = TRUE)
library(oceancolouR)  # to get 4km-resolution lat/lon vectors of the atlantic/pacific/gosl4km/baffin regions
library(ncdf4)        # to read 1km-resolution gosl lat/lon file (../panCan_processing/data/gsl_1km.nc)
library(dplyr)
library(sp)           # to convert lat/lons to SpatialPointsDataframes
library(raster)
library(sf)

# create the base variable - regions will be added to this
reginfo <- list()


#*******************************************************************************
# SECTION 1A: BINNED DATA ####

# For each region, add:
#     full region name (this is used in the "region" dropdown menu),
#     vector of bin numbers,
#     global grid of bin numbers,
#     SpatialPointsDataframe with the coordinates of the region,
#     extent of the region,
#     map settings:
#         default center longitude,
#         default center latitude,
#         default zoom level,
#         gridline interval (in degrees)
# "bin" and "binGrid" are used to create the map used for display only.
# "coords" is used to extract data from a user-selected polygon.


# atlantic
ltbs = lat_bounds[["atlantic"]]
lnbs = lon_bounds[["atlantic"]]
df = get_bins(region="nwa", variables=c("bin","latitude","longitude")) %>%
  dplyr::filter(between(latitude,ltbs[1],ltbs[2]) & between(longitude,lnbs[1],lnbs[2]))
df_geo <- df %>% dplyr::select(longitude,latitude)
coordinates(df_geo) = ~longitude+latitude
slot(df_geo, "proj4string") <- CRS(SRS_string = "EPSG:4326")
extent <- c(range(df$longitude),range(df$latitude))
reginfo$atlantic = list(name = "Atlantic",
                        bin = df$bin,
                        binGrid = gen_bin_grid(resolution="4",ext=extent,rast=FALSE,max_bins=6e8),
                        coords = df_geo,
                        extent = extent,
                        center_lon = -55,
                        center_lat = 53,
                        zoom_level = 5,
                        gridline_interval = 5)


# pacific
ltbs = lat_bounds[["NEP"]]
lnbs = lon_bounds[["NEP"]]
df = get_bins(region="nep", variables=c("bin","latitude","longitude")) %>%
  dplyr::filter(between(latitude,ltbs[1],ltbs[2]) & between(longitude,lnbs[1],lnbs[2]))
df_geo <- df %>% dplyr::select(longitude,latitude)
coordinates(df_geo) = ~longitude+latitude
slot(df_geo, "proj4string") <- CRS(SRS_string = "EPSG:4326")
extent <- c(range(df$longitude),range(df$latitude))
reginfo$pacific = list(name = "Pacific",
                       bin = df$bin,
                       binGrid = gen_bin_grid(resolution="4",ext=extent,rast=FALSE,max_bins=6e8),
                       coords = df_geo,
                       extent = extent,
                       center_lon = -132.5,
                       center_lat = 51.5,
                       zoom_level = 6,
                       gridline_interval = 5)


# gosl1km
# 1km lat/lon vectors (note that endpoints in "ind" are not inclusive here)
ltbs = c(45.6, 51.8)
lnbs = c(-71.4, -55)
nc <- nc_open("../panCan_processing/data/gsl_1km.nc")
df <- data.frame(bin = ncvar_get(nc, "bin_num"),
                 latitude = ncvar_get(nc, "lat"),
                 longitude = ncvar_get(nc, "lon"),
                 stringsAsFactors = FALSE) %>%
  # 1km gosl files were subsetted by lat/lon vectors and the endpoints were non-inclusive.
  # Typically I use inclusive endpoints for regions, but we'll do it differently here:
  dplyr::filter(latitude > ltbs[1] & latitude < ltbs[2] & longitude > lnbs[1] & longitude < lnbs[2])
nc_close(nc)
df_geo <- df %>% dplyr::select(longitude,latitude)
coordinates(df_geo) = ~longitude+latitude
slot(df_geo, "proj4string") <- CRS(SRS_string = "EPSG:4326")
extent <- c(range(df$longitude),range(df$latitude))
reginfo$gosl1km = list(name = "Gulf of Saint Lawrence, 1km",
                       bin = df$bin,
                       binGrid = gen_bin_grid(resolution="1",ext=extent,rast=FALSE,max_bins=6e8),
                       coords = df_geo,
                       extent = extent,
                       center_lon = -62,
                       center_lat = 48,
                       zoom_level = 6,
                       gridline_interval = 5)


# gosl4km
ltbs = lat_bounds[["GoSL"]]
lnbs = lon_bounds[["GoSL"]]
df = get_bins(region="gosl", variables=c("bin","latitude","longitude")) %>%
  dplyr::filter(between(latitude,ltbs[1],ltbs[2]) & between(longitude,lnbs[1],lnbs[2]))
# subset further
v_coords <- cbind(c(-64.2,-72,-68.5,-55.6,-55.6,-56.1,-57.5,-57.5,-59.9,-62.5,-64.2),
                  c(46,46,52.2,52.2,51.4,51.4,49.5,48,46,45.2,46))
op <- tibble(geometry=list(st_multipolygon(x=list(list(v_coords))))) %>% st_sf()
st_crs(op) <- "EPSG:4326"
op <- as_Spatial(op)
tmpdf <- df %>% dplyr::select(longitude,latitude)
coordinates(tmpdf) = ~longitude+latitude
slot(tmpdf, "proj4string") <- CRS(SRS_string = "EPSG:4326")
mask <- over(x=op,y=tmpdf,returnList=TRUE)[[1]]
df <- df[mask,]
df_geo <- df %>% dplyr::select(longitude,latitude)
coordinates(df_geo) = ~longitude+latitude
slot(df_geo, "proj4string") <- CRS(SRS_string = "EPSG:4326")
extent <- c(range(df$longitude),range(df$latitude))
reginfo$gosl4km = list(name = "Gulf of Saint Lawrence, 4km",
                       bin = df$bin,
                       binGrid = gen_bin_grid(resolution="4",ext=extent,rast=FALSE,max_bins=6e8),
                       coords = df_geo,
                       extent = extent,
                       center_lon = -62,
                       center_lat = 48,
                       zoom_level = 6,
                       gridline_interval = 5)


# baffin
ltbs = c(60,lat_bounds$NWA[2])
lnbs = lon_bounds$NWA
df = get_bins(region="nwa", variables=c("bin","latitude","longitude")) %>%
  dplyr::filter(between(latitude,ltbs[1],ltbs[2]) & between(longitude,lnbs[1],lnbs[2]))
df_geo <- df %>% dplyr::select(longitude,latitude)
coordinates(df_geo) = ~longitude+latitude
slot(df_geo, "proj4string") <- CRS(SRS_string = "EPSG:4326")
extent <- c(range(df$longitude),range(df$latitude))
reginfo$baffin = list(name = "Baffin Bay",
                      bin = df$bin,
                      binGrid = gen_bin_grid(resolution="4",ext=extent,rast=FALSE,max_bins=6e8),
                      coords = df_geo,
                      extent = extent,
                      center_lon = -68,
                      center_lat = 72,
                      zoom_level = 5,
                      gridline_interval = 5)


# arctic
ltbs = lat_bounds$Arctic
lnbs = lon_bounds$Arctic
df = get_bins(region="pancan", variables=c("bin","latitude","longitude")) %>%
  dplyr::filter(between(latitude,ltbs[1],ltbs[2]) & between(longitude,lnbs[1],lnbs[2]))
df_geo <- df %>% dplyr::select(longitude,latitude)
coordinates(df_geo) = ~longitude+latitude
slot(df_geo, "proj4string") <- CRS(SRS_string = "EPSG:4326")
extent <- c(range(df$longitude),range(df$latitude))
reginfo$arctic = list(name = "Arctic",
                      bin = df$bin,
                      binGrid = gen_bin_grid(resolution="4",ext=extent,rast=FALSE,max_bins=6e8),
                      coords = df_geo,
                      extent = extent,
                      center_lon = -128,
                      center_lat = 72,
                      zoom_level = 6,
                      gridline_interval = 5)


#*******************************************************************************
# SECTION 1B: GRIDDED DATA ####

# For each region, add:
#     full region name (this is used in the "region" dropdown menu),
#     map settings:
#         default center longitude,
#         default center latitude,
#         default zoom level,
#         gridline interval (in degrees)


# tanzania (3.5-10S, 38.5-41.5E)
reginfo$tanzania = list(name = "Tanzania",
                        center_lon = 40.5,
                        center_lat = -7,
                        zoom_level = 7,
                        gridline_interval = 2)

reginfo$bayoffundy = list(name = "Bay of Fundy",
                          center_lon = -66,
                          center_lat = 44.5,
                          zoom_level = 7,
                          gridline_interval = 2)



#*******************************************************************************
# CHECK FOR ISSUES AND THEN SAVE ####

# Warn user if region is very large
coordlens <- sapply(lapply(reginfo, "[[", "coords"), length)
if (any(coordlens > 300000)) {
  warning("A region with > 300,000 pixels requires a lot of memory to load and might crash the app.")
}

# Save to rds file
saveRDS(reginfo, file = "reginfo.rds", compress=TRUE)

