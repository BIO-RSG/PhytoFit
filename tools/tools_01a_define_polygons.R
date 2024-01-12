# Stephanie.Clay@dfo-mpo.gc.ca
# 2023-03-28

# For each region (e.g. atlantic, pacific, baffin, gosl4km...), add the list of predefined polygons here.
# Within a region, polygons must be in subgroups (see "atlantic" region below for example, where subgroups are "AZMP" and "OtherPolygons").
# Each polygon within a region must have a unique ID (e.g. CS_V01, HIB, LAS,...), EVEN IF THEY'RE IN DIFFERENT SUBGROUPS!!!!!
# The set of polygons within a subgroup must be merged into a Simple feature collection (SFC) of geometry type MULTIPOLYGON with geographic CRS epsg:4326.
#
# If your region has no predefined polygons, set poly$your_region = list()
#
# The final product should be in a format like this:
# poly = list(your_region1 = list(your_subgroup1 = SFC1,
#                                 your_subgroup2 = SFC2,
#                                 ...),
#             your_region2 = list(your_subgroup1 = SFC3,
#                                 ...),
#             ...)

library(dplyr)
library(sf)
library(stringr)
# library(ggplot2)
library(tibble)

# polygons will be added to this list
poly <- list()



#*******************************************************************************
# atlantic ####

# Define ID, full name, map label, and coordinates for each atlantic polygon
poly$atlantic = list(
  "AZMP"=list("AC"=list(name = "Avalon Channel (AC)",
                        label = "AC",
                        lat = c(46, 48, 48, 46, 46),
                        lon = c(-53, -53, -51.5, -51.5, -53)),
              "CLS"=list(name = "Central Labrador Sea (CLS)",
                         label = "CLS",
                         lat = c(55.5, 60.1, 60.1, 55.5, 55.5),
                         lon = c(-53.7, -53.7, -48.8, -48.8, -53.7)),
              "CS_V01"=list(name = "Cabot Strait (CS) V01",
                            label = "CS",
                            lat = c(46.9, 48, 48, 46.9, 46.9),
                            lon = c(-60.4, -60.4, -59, -59, -60.4)),
              "CS_V02"=list(name = "Cabot Strait (CS) V02",
                            label = NA,
                            lat = c(47, 47.758, 48, 48, 47.436, 47),
                            lon = c(-59.903, -60.73, -60.322, -59.783, -59.168, -59.903)),
              "CSS_V01"=list(name = "Central Scotian Shelf (CSS) V01",
                             label = "CSS", 
                             lat = c(43.33, 44.33, 44.33, 43.33, 43.33),
                             lon = c(-64, -64, -62, -62, -64)),
              "CSS_V02"=list(name = "Central Scotian Shelf (CSS) V02",
                             label = NA, 
                             lat = c(43.558137,43.984863,44.182952,43.472424,42.984401,42.984401,43.558137),
                             lon = c(-63.4708,-62.881918,-61.390084,-61.115273,-62.293037,-63.353024,-63.4708)),
              "ESS_V01"=list(name = "Eastern Scotian Shelf (ESS) V01",
                             label = "ESS",
                             lat = c(44.2, 45.67, 45.67, 44.2, 44.2),
                             lon = c(-60, -60, -58, -58, -60)),
              "ESS_V02"=list(name = "Eastern Scotian Shelf (ESS) V02",
                             label = NA,
                             lat = c(44.267643,44.098138,44.52098,44.940753,45.107804,44.267643),
                             lon = c(-58.013828,-60.722685,-60.722685,-59.741215,-58.053087,-58.013828)),
              "FP"=list(name = "Flemish Pass (FP)",
                        label = "FP",
                        lat = c(46, 48, 48, 46, 46),
                        lon = c(-47.5, -47.5, -46, -46, -47.5)),
              "GB_V01"=list(name = "Georges Bank (GB) V01",
                            label = "GB",
                            lat = c(41,42,42,41, 41),
                            lon = c(-68,-68, -66.5, -66.5, -68)),
              "GB_V02"=list(name = "Georges Bank (GB) V02",
                            label = NA,
                            lat = c(41.879311,41.585239,41.082205,40.754628,41.052493,41.28981,41.526261,41.879311),
                            lon = c(-67.278903,-66.925574,-67.514456,-68.652961,-68.692219,-68.378149,-68.378149,-67.278903)),
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
              "HL2"=list(name="Halifax Line 2",
                         label="HL2",
                         lat=c(44.17,44.37,44.37,44.17,44.17),
                         lon=c(-63.42,-63.42,-63.22,-63.22,-63.42)),
              "LAS"=list(name = "Labrador Shelf (LAS)",
                         label = "LAS",
                         lat = c(53.6, 55.5, 55.5, 53.6, 53.6),
                         lon = c(-55.7, -55.7, -53.7, -53.7, -55.7)),
              "LS_V01"=list(name = "Lurcher Shoal (LS) V01",
                            label = "LS",
                            lat = c(43, 44, 44, 43, 43),
                            lon = c(-66.7, -66.7, -66, -66, -66.7)),
              "LS_V02"=list(name = "Lurcher Shoal (LS) V02",
                            label = NA,
                            lat = c(43.128346,42.753382,42.984401,43.871369,44.013202,43.27195,43.128346),
                            lon = c(-65.865586,-66.218916,-66.847056,-66.886315,-66.454468,-66.297433,-65.865586)),
              "MS_V01"=list(name = "Magdalen Shallows (MS) V01",
                            label = "MS",
                            lat = c(46.5, 48, 48, 46.5, 46.5),
                            lon = c(-64, -64, -61.5, -61.5, -64)),
              "MS_V02"=list(name = "Magdalen Shallows (MS) V02",
                            label = NA,
                            lat = c(46.579, 46.579, 46.82, 47.82, 47.82, 47.26, 47.1, 46.579),
                            lon = c(-61.9, -63.162, -63.766, -63.766, -61.606, -62.23, -61.9, -61.9)),
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
              "NWGSL_V01"=list(name = "Northwest Gulf of St. Lawrence (NWGSL) V01",
                               label = "NWGSL",
                               lat = c(49.7, 50.3, 50.3, 49.7, 49.7),
                               lon = c(-67.0, -67.0, -64.5, -64.5, -67.0)),
              "NWGSL_V02"=list(name = "Northwest Gulf of St. Lawrence (NWGSL) V02",
                               label = NA,
                               lat = c(49.7, 50.16, 50.16, 49.7, 49.7),
                               lon = c(-66.86, -65.93, -64.76, -64.76, -66.86)),
              "P5"=list(name="Prince5",
                        label="P5",
                        lat=c(44.83,45.03,45.03,44.83,44.83),
                        lon=c(-66.95,-66.95,-66.75,-66.75,-66.95)),
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
              "WSS_V01"=list(name = "Western Scotian Shelf (WSS) V01",
                             label = "WSS",
                             lat = c(42.5, 43.33, 43.33, 42.5, 42.5),
                             lon = c(-65.5, -65.5, -64.5, -64.5, -65.5)),
              "WSS_V02"=list(name = "Western Scotian Shelf (WSS) V02",
                             label = NA,
                             lat = c(43.013218,43.443826,42.869001,42.666526,42.259577,42.608553,43.013218),
                             lon = c(-65.551516,-64.727082,-64.1382,-64.923376,-65.551516,-65.708551,-65.551516))),
  "OtherPolygons"=list("BRA"=list(name = "Bravo Station (BRA)",
                                  label = "BRA",
                                  lat = c(56.627, 58.127, 58.127, 56.627, 56.627),
                                  lon = c(-53.168, -53.168, -50.415, -50.415, -53.168)),
                       "CL"=list(name = "Central Labrador (CL)",
                                 label = "CL",
                                 lat=c(55.9, 55.9, 56.9, 56.9, 55.9),
                                 lon=c(-60.25, -58.5, -58.5, -60.25, -60.25)),
                       "ELS"=list(name = "Eastern Labrador Shelf (ELS)",
                                  label = "ELS",
                                  lat = c(59, 60.5, 60.5, 59, 59),
                                  lon = c(-49, -49, -48.3, -48.3, -49)),
                       "FC"=list(name = "Flemish Cap (FC)",
                                 label = "FC",
                                 lat=c(46.35, 46.35, 48.5, 48.5, 46.35),
                                 lon=c(-46.5, -43.8, -43.8, -46.5, -46.5)),
                       "HS"=list(name = "Hudson Strait (HS)",
                                 label = "HS",
                                 lat = c(60.5058 , 61.403, 61.4033, 60.5058, 60.5058),
                                 lon = c(-64.5484, -64.5484, -62.7235, -62.7235 , -64.5484)),
                       "NCLS"=list(name = "North Central Labrador Shelf (NCLS)",
                                   label = "NCLS",
                                   lat = c(60, 62.5, 62.5, 60, 60),
                                   lon = c(-60, -60, -55, -55, -60)),
                       "NGB"=list(name = "Northern Grand Bank (NGB)",
                                  label = "NGB",
                                  lat=c(46.5, 46.5, 48.5, 48.5, 46.5),
                                  lon=c(-52.4, -49, -49, -52.4, -52.4)),
                       "NL"=list(name = "Northern Labrador (NL)",
                                 label = "NL",
                                 lat=c(58.5, 58.5, 59.5, 59.5, 58.5),
                                 lon=c(-62.5, -60.5, -60.5, -62.5, -62.5)),
                       "NLS"=list(name = "Northern Labrador Shelf (NLS)",
                                  label = "NLS",
                                  lat = c(56.9145, 57.8125, 57.8125 , 56.9145, 56.9145),
                                  lon = c(-61.1957, -61.1957, -59.54983, -59.54983, -61.1957))))

# Turn each group of polygons (AZMP and OtherPolygons) into Simple feature collection of multipolygon objects
pnames <- names(poly$atlantic)
poly$atlantic <- lapply(1:length(poly$atlantic), function(i) {
  y <- poly$atlantic[[i]]
  if (length(y)==0) return(NULL)
  poly_id <- names(y)
  op <- lapply(1:length(y), function(j) {
    yi <- y[[j]]
    coords <- matrix(unlist(yi[c("lon","lat")]),ncol=2)
    tmp <- tibble(poly_id=poly_id[j], group=pnames[i], name=yi$name, label=yi$label,
                  geometry=list(st_multipolygon(x=list(list(coords)))))
  }) %>% do.call(what=dplyr::bind_rows) %>% st_sf()
  st_crs(op) <- "EPSG:4326"
  return(op)
}) %>% setNames(pnames)

# Load MPA polygons shapefile, transform to same geographic CRS as other polygons
network <- read_sf("../MPA_work/MPANetwork/maritimes_draft_network_2023.shp") %>% st_transform(crs=st_crs(4326))
nw <- c("MPA","OECM","AOI")
network <- network[which(network$mar_type %in% nw),] # remove draft regions
# make abbreviations, remove dashes, fix duplicated abbreviations, and add to coords
abbrevs <- strsplit(network$mar_name,split=" ") %>%
  sapply(FUN=substr,start=1,stop=1) %>%
  sapply(FUN=paste0,collapse="") %>%
  stringr::str_replace_all(c("-"="","'"=""))
abbrevs[network$mar_name=="Bird Islands"] <- "BdI"
abbrevs[network$mar_name=="Brier Island"] <- "BrI"
network <- network %>%
  dplyr::rename(name=mar_name,group=mar_type) %>%
  dplyr::mutate(poly_id=abbrevs,label=abbrevs) %>%
  dplyr::select(poly_id,group,name,label,geometry)

# Combine first list of polygons with MPA shapefile polygons
poly$atlantic <- c(poly$atlantic,lapply(nw, function(x) network[which(toupper(network$group)==x),]) %>% setNames(nw))



#*******************************************************************************
# pacific ####

poly$pacific = list(
  "OtherPolygons"=list("AOI"=list(name = "Offshore Pacific Area of Interest (AOI)",
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
                             lon = c(-128.7995, -128.6766, -128.7837, -128.9293, -129.0081, -129.0149, -128.7995))))


# Convert to Simple feature collection of multipolygon objects
pnames <- names(poly$pacific)
poly$pacific <- lapply(1:length(poly$pacific), function(i) {
  y <- poly$pacific[[i]]
  if (length(y)==0) return(NULL)
  poly_id <- names(y)
  op <- lapply(1:length(y), function(j) {
    yi <- y[[j]]
    coords <- matrix(unlist(yi[c("lon","lat")]),ncol=2)
    tmp <- tibble(poly_id=poly_id[j], group=pnames[i], name=yi$name, label=yi$label,
                  geometry=list(st_multipolygon(x=list(list(coords)))))
  }) %>% do.call(what=dplyr::bind_rows) %>% st_sf()
  st_crs(op) <- "EPSG:4326"
  return(op)
}) %>% setNames(pnames)



#*******************************************************************************
# gosl1km, gosl4km ####

# reuse polygons from the atlantic region in the gosl region
poly$gosl1km$AZMP <- poly$gosl4km$AZMP <- poly$atlantic$AZMP[which(poly$atlantic$AZMP$poly_id %in% c("AC","CS_V01","CS_V02","CSS","ESS","GB","HIB","LS","MS_V01","MS_V02","NEGSL_V01","NEGSL_V02","NENS","NGB","NWGSL_V01","NWGSL_V02","SES","SAB","SPB","WSS")),]



#*******************************************************************************
# baffin ####

# no polygons for baffin bay
poly$baffin = list()



#*******************************************************************************
# error checks, plotting, and saving ####

# Double-check that all ids are unique
dup_ids <- lapply(poly, function(x) any(duplicated(unlist(lapply(x, function(y) y$poly_id))))) %>% unlist()
if (any(dup_ids)) {
  stop("Duplicate IDs in region(s): ",paste0(names(dup_ids)[dup_ids],collapse=", "))
}

# # PLOT YOUR POLYGONS AND PRINT TO PNG FOR QUICK REFERENCE
# worldmap <- ggplot2::map_data("world")
# lat_buffer <- 0.5
# lon_buffer <- 1
# p <- lapply(1:length(poly), function(i) {
#   reg <- names(poly)[i]
#   ptmp <- poly[[i]]
#   if (length(ptmp)==0) return(NULL)
#   ptmp <- do.call(dplyr::bind_rows,ptmp)
#   lims <- st_bbox(ptmp)
#   xlim <- c(lims[1]-lon_buffer, lims[3]+lon_buffer)
#   ylim <- c(lims[2]-lat_buffer, lims[4]+lat_buffer)
#   map <- ggplot() +
#     geom_map(data=worldmap, map=worldmap, aes(long, lat, group=group, map_id=region),
#              fill="grey", colour="darkgrey", linewidth=0.5, alpha=0.8) +
#     geom_sf(data=ptmp, aes(fill=group), color="black", alpha=0.5) +
#     coord_sf(xlim=xlim, ylim=ylim, expand = FALSE) +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           axis.text = element_text(size=30),
#           legend.title = element_blank(),
#           legend.margin = margin(0, 0, 0, 0),
#           legend.box.margin = margin(-10, 0, -10, -10),
#           legend.text=element_text(size=40)) +
#     guides(fill = guide_legend(override.aes = list(size=8)))
#   img_width <- ceiling(diff(xlim))*200
#   img_height <- img_width
#   ggsave(filename=paste0("polygons_",reg,".png"),
#          plot=map,
#          dpi=150,
#          units="px",
#          height=img_height,
#          width=img_width)
# })



# collapse list of polygons into an sfc object to write to a shp file
poly <- lapply(1:length(poly), function(i) {
  if (length(poly[[i]])==0) {
    emptysf <- data.frame(poly_id=NA,group=NA,name=NA,label=NA,geometry=st_sfc(st_geometrycollection()),region=names(poly[i])) %>% st_as_sf(crs=st_crs(poly$atlantic$AZMP))
    return(emptysf)
  }
  ptmp <- do.call(dplyr::bind_rows,poly[[i]])
  ptmp$region <- names(poly)[i]
  return(ptmp)
}) %>% do.call(what=dplyr::bind_rows)
sf::st_write(poly,dsn="polygons.shp",append=FALSE)

