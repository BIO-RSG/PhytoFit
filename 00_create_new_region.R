# Stephanie.Clay@dfo-mpo.gc.ca
# 2022-06-10

# Use this to create reginfo.rds, containing the info on the regions you want to use in PhytoFit. See below for details,
# and instructions how to create the corresponding data files for the region.

# remotes::install_github("BIO-RSG/oceancolouR", build_vignettes = TRUE)
library(oceancolouR)  # to get 4km-resolution lat/lon vectors of the atlantic/pacific regions
library(ncdf4)        # to read 1km-resolution gosl lat/lon file (../panCan_processing/data/gsl_1km.nc)


#*******************************************************************************
# INSTRUCTIONS ####


###########################
# STEP 1: Use this script to add the necessary info for the region so it can be read into PhytoFit.
###########################
#
# NOTE: To exclude a region (if you're missing the latitude/longitude vectors or simply want to leave it out),
#       for example "atlantic", just comment out the code used to create the lat/lon/poly elements for that region
#       in the "LAT/LON/POLYGON INFO" section, and the list item for that region in the "ADD TO LIST" section.
#
# WARNING: It's recommended your region be < 300,000 pixels so it doesn't overload your computer memory when you
#          load the data in the app, but this can be adjusted depending on your situation.
#
#
# In the "LAT/LON/POLYGON" section:
#
#     a. Create numeric latitude and longitude vectors for your region.
#        They must be the same length, in the same order (so each pixel gets a lat/lon pair).
#        Values are in decimal degrees, and west/south longitudes/latitudes should be negative.
#     b. Define polygons to draw on the map.
#         To skip this step, just create an empty list, like this: poly_regionname = list()
#         Otherwise, fill in the list in this format:
#         poly_regionname = list(poly_ID1 = list(name = poly_name1,
#                                                label = poly_label1,
#                                                lat = numeric_vector_of_latitudes_of_polygon_vertices1,
#                                                lon = numeric_vector_of_longitudes_of_polygon_vertices1),
#                                poly_ID2 = list(name = poly_name2,
#                                                label = poly_label2,
#                                                lat = numeric_vector_of_latitudes_of_polygon_vertices2,
#                                                lon = numeric_vector_of_longitudes_of_polygon_vertices2))
#         "poly_ID" must be unique to each polygon. If there are multiple versions of a polygon, use something
#         like poly_v01, poly_v02.
#         "name" is the full name of the polygon, which appears in the polygon drop-down menu.
#         "label" is the label that appears next to the polygon on the map. Set to NA for no label. If you have
#         multiple overlapping versions of a polygon, you could set the label on the first one and set the others to NA.
#         "lat" and "lon" vectors must be the same length, and the first and last values must be the same (to close the polygon).
#         WARNING: NA values are not allowed in the lat/lon vectors (i.e. polygons must be continuous, not split into multiple polys).
#
# EXAMPLE:
#   poly_atlantic = list("AC"=list(name = "Avalon Channel (AC)",
#                                  label = "AC",
#                                  lat = c(46, 48, 48, 46, 46),
#                                  lon = c(-53, -53, -51.5, -51.5, -53)),
#                        "CS_V01"=list(name = "Cabot Strait (CS) V01",
#                                      label = "CS",
#                                      lat = c(46.9, 48, 48, 46.9, 46.9),
#                                      lon = c(-60.4, -60.4, -59, -59, -60.4)),
#                        "CS_V02"=list(name = "Cabot Strait (CS) V02",
#                                      label = NA,
#                                      lat = c(47, 47.758, 48, 48, 47.436, 47),
#                                      lon = c(-59.903, -60.73, -60.322, -59.783, -59.168, -59.903)))
#
# In the "ADD TO LIST" section:
#     Follow the format in examples below, define elements for your region:
#         name: The full name of the region (this appears in the drop-down menu of the app)
#         lat: The numeric vector of latitudes for the region
#         lon: The numeric vector of longitudes for the region
#         data_resolution: Satellite data resolution, numeric value in km^2
#         map_resolution: This is the resolution (lon,lat) for the raster image on the map.
#                         You will need to experiment with this to get the resolution you want that does not
#                         look too pixelated/gappy, but doesn't sacrifice too much accuracy or closeness to the original
#                         points when they're rasterized and projected onto the map. The map_resolution will depend on your
#                         satellite data resolution and the range of latitudes used in your region.
#                         Note: Leaflet (the package used for the map) uses the Web Mercator Projection:
#                               https://spatialreference.org/ref/sr-org/45/
#         center_lon: The longitude at the center of the map when it first loads
#         center_lat: The latitude at the center of the map when it first loads
#         zoom_level: The zoom level of the map when it first loads
#         poly: The polygons you defined in LAT/LON/POLYGON section (add this to the list even if it's empty)



###########################
# STEP 2: Create data files for the region.
###########################
#
# 1. Download daily satellite data for your region. The latitude/longitude vectors that
#    you created for your region must match (same length and order) the vector
#    of daily satellite data.
#
# 2. Combine all daily data for a given year into a single numeric matrix, where row = pixel
#    and column = day of year.
#    IMPORTANT: Missing days should be included as a column of NA values at the appropriate column index
#               (e.g. if you don't have data in your region on day 36, then column 36 should be all NA).
#    Note that day 366 of a leap year is ignored/removed in the app.
#
# 3. Flatten the matrix into a dataframe:
#       df = data.frame(var = as.numeric(mat), stringsAsFactors = FALSE)
#    Note that pixels with missing data are retained here so that it can be quickly reshaped into the
#    pixel / day of year matrix for easier and faster subsetting. The data is saved in this flattened
#    format instead of as a matrix because the file size is smaller and faster to read.
#
# 4. Write the dataframe to an fst file and store it in the data/region subfolders following this filenaming scheme:
#       filename = "data/region/region_sensor_variable_year.fst"
#       write_fst(df, path=filename, compress=100)
#    In the filename, replace "region", "sensor", "variable", and "year" with the appropriate values
#    (e.g. "atlantic" for region, "modis" for sensor, "ocx" for variable, "2016" for year...).
#    Note that this step (and the app) requires the "fst" library in R.
#
# # EXAMPLE:
# # This file contains the pixel / day of year matrix, and the flattened dataframe,
# # from the Pacific MODIS-Aqua OCx chl-a file for 2021:
# load("example_sat_formatting.rda")
# str(mat)
# # num [1:48854, 1:365] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
# str(df)
# # 'data.frame':	17831710 obs. of  1 variable:
# #   $ var: num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
# # In this case, the phytofit data file would be created like this:
# #     write_fst(df, path="data/pacific/pacific_modis_ocx_2021.fst", compress=100)



###########################
# STEP 3: Adjust 00_input_variables.R "VARIABLES THAT CAN BE CHANGED" section, if necessary
###########################

# If you've added data files for a new sensor or variable, add their abbreviation and name 
# to the sensor_names and variable_names at the top of the "00_input_variables.R" script
# (see comments next to those variables for more details).



#*******************************************************************************
# LAT/LON/POLYGON INFO ####

# ATLANTIC ####

df = get_bins(region="nwa", variables=c("latitude","longitude"))
ltbs = lat_bounds[["atlantic"]]
lnbs = lon_bounds[["atlantic"]]
ind <- df$latitude >= ltbs[1] & df$latitude <= ltbs[2] & df$longitude >= lnbs[1] & df$longitude <= lnbs[2]
lat_atlantic = as.numeric(df$latitude)[ind]
lon_atlantic = as.numeric(df$longitude)[ind]
poly_atlantic = list("AC"=list(name = "Avalon Channel (AC)",
                               label = "AC",
                               lat = c(46, 48, 48, 46, 46),
                               lon = c(-53, -53, -51.5, -51.5, -53)),
                    "BRA"=list(name = "Bravo Station (BRA)",
                               label = "BRA",
                               lat = c(56.627, 58.127, 58.127, 56.627, 56.627),
                               lon = c(-53.168, -53.168, -50.415, -50.415, -53.168)),
                    "CS_V01"=list(name = "Cabot Strait (CS) V01",
                                  label = "CS",
                                  lat = c(46.9, 48, 48, 46.9, 46.9),
                                  lon = c(-60.4, -60.4, -59, -59, -60.4)),
                    "CS_V02"=list(name = "Cabot Strait (CS) V02",
                                  label = NA,
                                  lat = c(47, 47.758, 48, 48, 47.436, 47),
                                  lon = c(-59.903, -60.73, -60.322, -59.783, -59.168, -59.903)),
                    "CL"=list(name = "Central Labrador (CL)",
                              label = "CL",
                              lat=c(55.9, 55.9, 56.9, 56.9, 55.9),
                              lon=c(-60.25, -58.5, -58.5, -60.25, -60.25)),
                    "CLS"=list(name = "Central Labrador Sea (CLS)",
                               label = "CLS",
                               lat = c(55.5, 60.1, 60.1, 55.5, 55.5),
                               lon = c(-53.7, -53.7, -48.8, -48.8, -53.7)),
                    "CSS"=list(name = "Central Scotian Shelf (CSS)",
                               label = "CSS", 
                               lat = c(43.33, 44.33, 44.33, 43.33, 43.33),
                               lon = c(-64, -64, -62, -62, -64)),
                    "ELS"=list(name = "Eastern Labrador Shelf (ELS)",
                               label = "ELS",
                               lat = c(59, 60.5, 60.5, 59, 59),
                               lon = c(-49, -49, -48.3, -48.3, -49)),
                    "ESS"=list(name = "Eastern Scotian Shelf (ESS)",
                               label = "ESS",
                               lat = c(44.2, 45.67, 45.67, 44.2, 44.2),
                               lon = c(-60, -60, -58, -58, -60)),
                    "FC"=list(name = "Flemish Cap (FC)",
                              label = "FC",
                              lat=c(46.35, 46.35, 48.5, 48.5, 46.35),
                              lon=c(-46.5, -43.8, -43.8, -46.5, -46.5)),
                    "FP"=list(name = "Flemish Pass (FP)",
                              label = "FP",
                              lat = c(46, 48, 48, 46, 46),
                              lon = c(-47.5, -47.5, -46, -46, -47.5)),
                    "GB"=list(name = "Georges Bank (GB)",
                              label = "GB",
                              lat = c(41,42,42,41, 41),
                              lon = c(-68,-68, -66.5, -66.5, -68)),
                    "GS"=list(name = "Greenland Shelf (GS)",
                              label = "GS",
                              lat = c(60.1, 60.7, 60.7, 60.1, 60.1),
                              lon = c(-48.8, -48.8, -48.1, -48.1, -48.8)),
                    "HB"=list(name = "Hamilton Bank (HB)",
                              label = "HB",
                              lat = c(53.5, 54.5, 54.5, 53.5, 53.5),
                              lon = c(-56, -56, -54, -54, -56)),
                    "HIB"=list(name = "Hibernia (HIB)",
                               label = "HIB",
                               lat = c(46, 47, 47, 46, 46),
                               lon = c(-51, -51, -48.75, -48.75, -51)),
                    "HS"=list(name = "Hudson Strait (HS)",
                              label = "HS",
                              lat = c(60.5058 , 61.403, 61.4033, 60.5058, 60.5058),
                              lon = c(-64.5484, -64.5484, -62.7235, -62.7235 , -64.5484)),
                    "LAS"=list(name = "Labrador Shelf (LAS)",
                               label = "LAS",
                               lat = c(53.6, 55.5, 55.5, 53.6, 53.6),
                               lon = c(-55.7, -55.7, -53.7, -53.7, -55.7)),
                    "LS"=list(name = "Lurcher Shoal (LS)",
                              label = "LS",
                              lat = c(43, 44, 44, 43, 43),
                              lon = c(-66.7, -66.7, -66, -66, -66.7)),
                    "MS_V01"=list(name = "Magdalen Shallows (MS) V01",
                                  label = "MS",
                                  lat = c(46.5, 48, 48, 46.5, 46.5),
                                  lon = c(-64, -64, -61.5, -61.5, -64)),
                    "MS_V02"=list(name = "Magdalen Shallows (MS) V02",
                                  label = NA,
                                  lat = c(46.579, 46.579, 46.82, 47.82, 47.82, 47.26, 47.1, 46.579),
                                  lon = c(-61.9, -63.162, -63.766, -63.766, -61.606, -62.23, -61.9, -61.9)),
                    "NCLS"=list(name = "North Central Labrador Shelf (NCLS)",
                                label = "NCLS",
                                lat = c(60, 62.5, 62.5, 60, 60),
                                lon = c(-60, -60, -55, -55, -60)),
                    "NEGSL_V01"=list(name = "Northeast Gulf of St. Lawrence (NEGSL) V01",
                                     label = "NEGSL",
                                     lat = c(49, 50, 50, 49, 49),
                                     lon = c(-61, -61, -58, -58, -61)),
                    "NEGSL_V02"=list(name = "Northeast Gulf of St. Lawrence (NEGSL) V02",
                                     label = NA,
                                     lat = c(49, 50, 50, 49, 49),
                                     lon = c(-61, -61, -58, -58.73, -61)),
                    "NENS"=list(name = "Northeast Newfoundland Shelf (NENS)",
                                label = "NENS",
                                lat = c(48.5, 50, 50, 48.5, 48.5),
                                lon = c(-53, -53, -51, -51, -53)),
                    "NGB"=list(name = "Northern Grand Bank (NGB)",
                               label = "NGB",
                               lat=c(46.5, 46.5, 48.5, 48.5, 46.5),
                               lon=c(-52.4, -49, -49, -52.4, -52.4)),
                    "NL"=list(name = "Northern Labrador (NL)",
                              label = "NL",
                              lat=c(58.5, 58.5, 59.5, 59.5, 58.5),
                              lon=c(-62.5, -60.5, -60.5, -62.5, -62.5)),
                    "NWGSL_V01"=list(name = "Northwest Gulf of St. Lawrence (NWGSL) V01",
                                     label = "NWGSL",
                                     lat = c(49.7, 50.3, 50.3, 49.7, 49.7),
                                     lon = c(-67.0, -67.0, -64.5, -64.5, -67.0)),
                    "NWGSL_V02"=list(name = "Northwest Gulf of St. Lawrence (NWGSL) V02",
                                     label = NA,
                                     lat = c(49.7, 50.16, 50.16, 49.7, 49.7),
                                     lon = c(-66.86, -65.93, -64.76, -64.76, -66.86)),
                    "NLS"=list(name = "Northern Labrador Shelf (NLS)",
                               label = "NLS",
                               lat = c(56.9145, 57.8125, 57.8125 , 56.9145, 56.9145),
                               lon = c(-61.1957, -61.1957, -59.54983, -59.54983, -61.1957)),
                    "SES"=list(name = "Southeast Shoal (SES)",
                               label = "SES",
                               lat = c(44, 46, 46, 44, 44),
                               lon = c(-52, -52, -50, -50, -52)),
                    "SAB"=list(name = "St. Anthony Bank (SAB)",
                               label = "SAB",
                               lat = c(50, 52, 52, 50, 50),
                               lon = c(-55, -55, -53, -53, -55)),
                    "SPB"=list(name = "St. Pierre Bank (SPB)",
                               label = "SPB",
                               lat = c(45.33, 46.33, 46.33, 45.33, 45.33),
                               lon = c(-56, -56, -54, -54, -56)),
                    "WSS"=list(name = "Western Scotian Shelf (WSS)",
                               label = "WSS",
                               lat = c(42.5, 43.33, 43.33, 42.5, 42.5),
                               lon = c(-65.5, -65.5, -64.5, -64.5, -65.5)))


# PACIFIC ####

df = get_bins(region="nep", variables=c("latitude","longitude"))
ltbs = lat_bounds[["NEP"]]
lnbs = lon_bounds[["NEP"]]
ind <- df$latitude >= ltbs[1] & df$latitude <= ltbs[2] & df$longitude >= lnbs[1] & df$longitude <= lnbs[2]
lat_pacific = as.numeric(df$latitude)[ind]
lon_pacific = as.numeric(df$longitude)[ind]
poly_pacific = list("AOI"=list(name = "Offshore Pacific Area of Interest (AOI)",
                               label = "AOI",
                               lat = c(49.682, 51.064, 50.770, 50.405, 50.231, 49.529,
                                       49.265, 49.000,49.069, 47.793, 46.527, 49.682), 
                               lon = c(-134.901, -130.506, -130.078, -130.010, -129.534, -128.692,
                                       -129.027, -128.245, -128.158, -126.749, -129.129, -134.901)),
                    "SK_B"=list(name = "SGaan Kinghlas-Bowie Seamount MPA (SK_B)",
                                label = "SK_B",
                                lat = c(53.05211, 53.27247, 53.66367, 53.65500, 53.87131,
                                        53.82211, 53.66736, 53.23311, 53.05211),
                                lon = c(-135.8405, -134.9987, -135.2847, -135.8963, -136.5064,
                                        -136.7925, -136.9510, -136.1667, -135.8405)),
                    "EHV"=list(name = "Endeavour Hydrothermal Vents MPA (EHV)",
                               label = "EHV",
                               lat = c(47.90000, 47.90000, 48.01667, 48.01667, 47.90000),
                               lon = c(-129.0333, -129.1333, -129.1333, -129.0333, -129.0333)),
                    "GH"=list(name = "Gwaii Haanas (GH)",
                              label = "GH",
                              lat = c(52.20806, 52.44694, 52.83472, 52.80972, 52.77694,
                                      52.67667, 52.00000, 51.80167, 52.20806),
                              lon = c(-130.8167, -131.0886, -131.3361, -131.6556, -131.8192,
                                      -132.2208, -131.3000, -130.8919, -130.8167)),
                    "GHE"=list(name = "Gwaii Haanas east (GHE)",
                               label = "GHE",
                               lat = c(52.14583, 52.20806, 52.44694, 52.83472, 52.80972, 52.77694, 52.14583),
                               lon = c(-131.2292, -130.8167, -131.0886, -131.3361, -131.6556, -131.8192, -131.2292)),
                    "GHW"=list(name = "Gwaii Haanas west (GHW)",
                               label = "GHW",
                               lat = c(52.77694, 52.67667, 52.00000, 52.14583, 52.77694),
                               lon = c(-131.8192, -132.2208, -131.3000, -131.2292, -131.8192)),
                    "GHS"=list(name = "Gwaii Haanas south (GHS)",
                               label = "GHS",
                               lat = c(52.20806, 51.80167, 52.00000, 52.14583, 52.20806),
                               lon = c(-130.8167, -130.8919, -131.3000, -131.2292, -130.8167)),
                    "GHO"=list(name = "Gwaii Haanas Offshore (GHO)",
                               label = "GHO",
                               lat = c(52.18750, 51.60417, 51.60417, 52.18750, 52.18750),
                               lon = c(-132.1042, -131.3958, -132.2708, -132.9792, -132.1042)),
                    "SI"=list(name = "Scott Islands marine NWA (SI)",
                              label = "SI",
                              lat = c(50.69236, 50.34589, 50.16361, 50.40536, 51.10119, 51.24275,
                                      51.24933, 51.25139, 50.84367, 50.73981, 50.72765, 50.69236),
                              lon = c(-128.3775, -128.7590, -129.3522, -130.0104, -130.1406, -128.9293,
                                      -128.7837, -128.6126, -128.1775, -128.4026, -128.3896, -128.3775)),
                    "SRN"=list(name = "North Glass Sponge Reefs MPA (SRN)",
                               label = "SRN",
                               lat = c(53.19803, 53.15611, 53.04847, 53.05192, 53.12161, 53.12903, 53.22464,
                                       53.32222, 53.40150, 53.39464, 53.31181, 53.25572, 53.19803),
                               lon = c(-130.3298, -130.3147, -130.4212, -130.5099, -130.7009, -130.7740, -130.7913,
                                       -130.9067, -130.8105, -130.7145, -130.6359, -130.5504, -130.3298)),
                    "SRC"=list(name = "Central Glass Sponge Reefs MPA (SRC)",
                               label = "SRC",
                               lat = c(52.00678, 51.93069, 51.85903, 51.88353, 52.08725, 52.14611, 52.26183,
                                       52.49317, 52.53483, 52.56822, 52.42853, 52.33411, 52.16453, 52.00678),
                               lon = c(-129.2368, -129.3038, -129.6104, -129.7343, -129.6039, -129.5593, -129.7368,
                                       -129.8758, -129.8851, -129.7976, -129.5867, -129.4977, -129.4249, -129.2368)),
                    "SRS"=list(name = "South Glass Sponge Reefs MPA (SRS)",
                               label = "SRS",
                               lat = c(51.41228, 51.30903, 51.24933, 51.24275, 51.29508, 51.32347, 51.41228),
                               lon = c(-128.7995, -128.6766, -128.7837, -128.9293, -129.0081, -129.0149, -128.7995)))


# GoSL (1km and 4km) ####

# 1km lat/lon vectors (note that endpoints in "ind" are not inclusive here)
nc <- nc_open("../panCan_processing/data/gsl_1km.nc")
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
nc_close(nc)
ltbs = c(45.6, 51.8)
lnbs = c(-71.4, -55)
ind <- lat > ltbs[1] & lat < ltbs[2] & lon > lnbs[1] & lon < lnbs[2]
lat_gosl_1km = as.numeric(lat)[ind]
lon_gosl_1km = as.numeric(lon)[ind]

# 4km lat/lon vectors
df = get_bins(region="gosl", variables=c("latitude","longitude"))
ltbs = lat_bounds[["GoSL"]]
lnbs = lon_bounds[["GoSL"]]
ind <- df$latitude >= ltbs[1] & df$latitude <= ltbs[2] & df$longitude >= lnbs[1] & df$longitude <= lnbs[2]
lat_gosl_4km = as.numeric(df$latitude)[ind]
lon_gosl_4km = as.numeric(df$longitude)[ind]

poly_gosl <- poly_atlantic[names(poly_atlantic) %in% c("AC","CS_V01","CS_V02","CSS","ESS","GB","HIB","LS","MS_V01","MS_V02","NEGSL_V01","NEGSL_V02","NENS","NGB","NWGSL_V01","NWGSL_V02","SES","SAB","SPB","WSS")]


# BAFFIN BAY ####

df = get_bins(region="nwa", variables=c("latitude","longitude"))
ltbs = c(lat_bounds$atlantic[2],lat_bounds$NWA[2])
lnbs = lon_bounds$NWA
ind <- df$latitude >= ltbs[1] & df$latitude <= ltbs[2] & df$longitude >= lnbs[1] & df$longitude <= lnbs[2]
lat_baffin = as.numeric(df$latitude)[ind]
lon_baffin = as.numeric(df$longitude)[ind]
poly_baffin = list()


#*******************************************************************************
# ADD TO LIST ####

reginfo <- list(
  
  "atlantic" = list(name = "Atlantic",
                    lat = lat_atlantic,
                    lon = lon_atlantic,
                    data_resolution = 4.64^2,
                    map_resolution = c(0.09,0.04167),
                    center_lon = -55,
                    center_lat = 53,
                    zoom_level = 5,
                    poly = poly_atlantic),
  
  "pacific" = list(name = "Pacific",
                   lat = lat_pacific,
                   lon = lon_pacific,
                   data_resolution = 4.64^2,
                   map_resolution = c(0.08,0.04167),
                   center_lon = -132.5,
                   center_lat = 51.5,
                   zoom_level = 6,
                   poly = poly_pacific),
  
  "gosl1km" = list(name = "Gulf of Saint Lawrence, 1km",
                   lat = lat_gosl_1km,
                   lon = lon_gosl_1km,
                   data_resolution = 1,
                   map_resolution = c(0.03,0.01),
                   center_lon = -62,
                   center_lat = 48,
                   zoom_level = 6,
                   poly = poly_gosl),
  
  "gosl4km" = list(name = "Gulf of Saint Lawrence, 4km",
                   lat = lat_gosl_4km,
                   lon = lon_gosl_4km,
                   data_resolution = 4.64^2,
                   map_resolution = c(0.07,0.04167),
                   center_lon = -62,
                   center_lat = 47,
                   zoom_level = 6,
                   poly = poly_gosl),
  
  "baffin" = list(name = "Baffin Bay",
                lat = lat_baffin,
                lon = lon_baffin,
                data_resolution = 4.64^2,
                map_resolution = c(0.2,0.04167),
                center_lon = -68,
                center_lat = 72,
                zoom_level = 5,
                poly = poly_baffin)
  
)


#*******************************************************************************
# ERROR CHECKS AND EXTRA VARIABLES ####

latlens <- sapply(lapply(reginfo, "[[", "lat"), length)
lonlens <- sapply(lapply(reginfo, "[[", "lon"), length)
if (any(latlens != lonlens)) {
  file.remove("reginfo.rds")
  stop("Latitude and longitude vectors must be the same length.")
}
if (any(latlens > 300000)) {
  warning("A region with > 300,000 pixels requires a lot of memory to load and might crash the app.")
}

# Add max_area to list elements (maximum area allowed for a custom polygon, in degrees^2, to prevent app from crashing, based on user-supplied data resolution and center latitude of region)
reginfo <- lapply(reginfo, function(x) {x$max_area <- floor(x$data_resolution/0.02 * (abs(x$center_lat)/180+1)); x})


#*******************************************************************************
# SAVE RDS ####

saveRDS(reginfo, file = "reginfo.rds", compress=TRUE)

