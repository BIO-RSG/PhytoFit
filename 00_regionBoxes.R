# To add a new polygon to the map:
#
#     - Create a new variable in the "ATLANTIC BOXES" or "PACIFIC BOXES" sections below,
#       following the existing format with the latitudes and longitudes,
#       and make sure the variable name doesn't already exist.
#       NOTE: As of 24 Sep 2020, "NA" values can't be included in the lats/lons,
#             and variable names must be capitalized
#
#     - Scroll down to the "FULL LISTS" section to add these elements of the polygon
#       to the existing set:
#
#          - variable containing the lats/lons (add to "all_regions")
#          - full name (add to "full_names")
#          - unique ID (add to "poly_ID")
#          - abbreviation that appears on the map (add to "abbrev")



#*******************************************************************************
# ATLANTIC BOXES ####


# MARITIMES

# georges bank
GB <- list(lat = c(41,42,42,41, 41),
           lon = c(-68,-68, -66.5, -66.5, -68))
# central scotian shelf
CSS <- list(lat = c(43.33, 44.33, 44.33, 43.33, 43.33),
            lon = c(-64, -64, -62, -62, -64))
# eastern scotian shelf
ESS <- list(lat = c(44.2, 45.67, 45.67, 44.2, 44.2),
            lon = c(-60, -60, -58, -58, -60))
# western scotian shelf
WSS <- list(lat = c(42.5, 43.33, 43.33, 42.5, 42.5),
            lon = c(-65.5, -65.5, -64.5, -64.5, -65.5))
#lurcher shoal
LS <- list(lat = c(43, 44, 44, 43, 43),
           lon = c(-66.7, -66.7, -66, -66, -66.7))


# QUEBEC

# northwest gulf of st lawrence
NWGSL_V01 <- list(lat = c(49.7, 50.3, 50.3, 49.7, 49.7),
              lon = c(-67.0, -67.0, -64.5, -64.5, -67.0))
# northeast gulf of st lawrence
NEGSL_V01 <- list(lat = c(49, 50, 50, 49, 49),
              lon = c(-61, -61, -58, -58, -61))
# magdalen shallows
MS_V01 <- list(lat = c(46.5, 48, 48, 46.5, 46.5),
           lon = c(-64, -64, -61.5, -61.5, -64))
# cabot strait
CS_V01 <- list(lat = c(46.9, 48, 48, 46.9, 46.9),
           lon = c(-60.4, -60.4, -59, -59, -60.4))

# northwest gulf of st lawrence
NWGSL_V02 <- list(lat = c(49.7, 50.16, 50.16, 49.7, 49.7),
              lon = c(-66.86, -65.93, -64.76, -64.76, -66.86))
# northeast gulf of st lawrence
NEGSL_V02 <- list(lat = c(49, 50, 50, 49, 49),
              lon = c(-61, -61, -58, -58.73, -61))
# magdalen shallows
MS_V02 <- list(lat = c(46.579, 46.579, 46.82, 47.82, 47.82, 47.26, 47.1, 46.579),
           lon = c(-61.9, -63.162, -63.766, -63.766, -61.606, -62.23, -61.9, -61.9))
# cabot strait
CS_V02 <- list(lat = c(47, 47.758, 48, 48, 47.436, 47),
           lon = c(-59.903, -60.73, -60.322, -59.783, -59.168, -59.903))


# NEWFOUNDLAND

# st pierre bank
SPB <- list(lat = c(45.33, 46.33, 46.33, 45.33, 45.33),
            lon = c(-56, -56, -54, -54, -56))
# southeast shoal
SES <- list(lat = c(44, 46, 46, 44, 44),
            lon = c(-52, -52, -50, -50, -52))
# avalon channel
AC <- list(lat = c(46, 48, 48, 46, 46),
           lon = c(-53, -53, -51.5, -51.5, -53))
# hibernia
HIB <- list(lat = c(46, 47, 47, 46, 46),
            lon = c(-51, -51, -48.75, -48.75, -51))
# flemish pass
FP <- list(lat = c(46, 48, 48, 46, 46),
           lon = c(-47.5, -47.5, -46, -46, -47.5))
# northeast newfoundland shelf
NENS <- list(lat = c(48.5, 50, 50, 48.5, 48.5),
             lon = c(-53, -53, -51, -51, -53))
# st anthony bank
SAB <- list(lat = c(50, 52, 52, 50, 50),
            lon = c(-55, -55, -53, -53, -55))
# hamilton bank
HB <- list(lat = c(53.5, 54.5, 54.5, 53.5, 53.5),
           lon = c(-56, -56, -54, -54, -56))
# northern labrador shelf
NLS <- list(lat = c(56.9145, 57.8125, 57.8125 , 56.9145, 56.9145),
            lon = c(-61.1957, -61.1957, -59.54983, -59.54983, -61.1957))
# hudson strait
HS <- list(lat = c(60.5058 , 61.403, 61.4033, 60.5058, 60.5058),
           lon = c(-64.5484, -64.5484, -62.7235, -62.7235 , -64.5484))


# LABRADOR SEA

# north central labrador sea
NCLS <- list(lat = c(60, 62.5, 62.5, 60, 60),
             lon = c(-60, -60, -55, -55, -60))
# greenland sea
GS <- list(lat = c(60.1, 60.7, 60.7, 60.1, 60.1),
           lon = c(-48.8, -48.8, -48.1, -48.1, -48.8))
# central labrador sea
CLS <- list(lat = c(55.5, 60.1, 60.1, 55.5, 55.5),
            lon = c(-53.7, -53.7, -48.8, -48.8, -53.7))
# eastern labrador shelf
ELS <- list(lat = c(59, 60.5, 60.5, 59, 59),
            lon = c(-49, -49, -48.3, -48.3, -49))
# bravo station
BRA <- list(lat = c(56.627, 58.127, 58.127, 56.627, 56.627),
            lon = c(-53.168, -53.168, -50.415, -50.415, -53.168))
# labrador shelf
LAS <- list(lat = c(53.6, 55.5, 55.5, 53.6, 53.6),
            lon = c(-55.7, -55.7, -53.7, -53.7, -55.7))


# Boxes from David Belanger, Jan 2020

# northern labrador
NL <- list(lat=c(58.5, 58.5, 59.5, 59.5, 58.5),
           lon=c(-62.5, -60.5, -60.5, -62.5, -62.5))

# central labrador
CL <- list(lat=c(55.9, 55.9, 56.9, 56.9, 55.9),
           lon=c(-60.25, -58.5, -58.5, -60.25, -60.25))

# northern grand bank
NGB <- list(lat=c(46.5, 46.5, 48.5, 48.5, 46.5),
            lon=c(-52.4, -49, -49, -52.4, -52.4))

# flemish cap
FC <- list(lat=c(46.35, 46.35, 48.5, 48.5, 46.35),
           lon=c(-46.5, -43.8, -43.8, -46.5, -46.5))


# DEPRECIATED BOXES

#estuary - old quebec box
EST <- list(lat = c(48, 49.3, 49.3, 48, 48),
            lon = c(-69.5, -69.5, -67.4, -67.4, -69.5))
#bay of fundy - old maritimes box
BOF <- list(lat = c(44.5, 45.5, 45.5, 44.5, 44.5),
            lon = c(-66.33, -66.33, -65, -65, -66.33))
#bras d'Or - old maritimes  box
BDO <- list(lat = c(45.81666667, 45.9, 45.9, 45.81666667, 45.81666667),
            lon = c(-60.9, -60.9, -60.75, -60.75, -60.9))
# western bank - old maritimes box
WB <- list(lat = c(43.33, 44, 44, 43.33, 43.33),
           lon = c(-62, -62, -61, -61, -62))
# solas - old maritimes box
SOL <- list(lat = c(41, 43, 43, 41, 41),
            lon = c(-60, -60, -58, -58, -60))



#*******************************************************************************
# PACIFIC BOXES ####


# Offshore Pacific Area of Interest 
AOI <- list(lat = c(49.682, 51.064, 50.770, 50.405, 50.231, 49.529, 49.265, 49.000, 49.069, 47.793, 46.527, 49.682), 
            lon = c(-134.901, -130.506, -130.078, -130.010, -129.534, -128.692, -129.027, -128.245, -128.158, -126.749, -129.129, -134.901))

# SGaan Kinghlas-Bowie Seamount MPA
SK_B <- list(lat = c(53.05211, 53.27247, 53.66367, 53.65500, 53.87131, 53.82211, 53.66736, 53.23311, 53.05211),
           lon = c(-135.8405, -134.9987, -135.2847, -135.8963, -136.5064, -136.7925, -136.9510, -136.1667, -135.8405))

# Endeavour Hydrothermal Vents MPA
EHV <- list(lat = c(47.90000, 47.90000, 48.01667, 48.01667, 47.90000),
            lon = c(-129.0333, -129.1333, -129.1333, -129.0333, -129.0333))

# Gwaii Haanas
GH <- list(lat = c(52.20806, 52.44694, 52.83472, 52.80972, 52.77694, 52.67667, 52.00000, 51.80167, 52.20806),
           lon = c(-130.8167, -131.0886, -131.3361, -131.6556, -131.8192, -132.2208, -131.3000, -130.8919, -130.8167))

# Gwaii Haanas east region
GHE <- list(lat = c(52.14583, 52.20806, 52.44694, 52.83472, 52.80972, 52.77694, 52.14583),
            lon = c(-131.2292, -130.8167, -131.0886, -131.3361, -131.6556, -131.8192, -131.2292))

# Gwaii Haanas west region
GHW <- list(lat = c(52.77694, 52.67667, 52.00000, 52.14583, 52.77694),
            lon = c(-131.8192, -132.2208, -131.3000, -131.2292, -131.8192))

# Gwaii Haanas south region
GHS <- list(lat = c(52.20806, 51.80167, 52.00000, 52.14583, 52.20806),
            lon = c(-130.8167, -130.8919, -131.3000, -131.2292, -130.8167))

# Gwaii Haanas Offshore box
GHO <- list(lat = c(52.18750, 51.60417, 51.60417, 52.18750, 52.18750),
            lon = c(-132.1042, -131.3958, -132.2708, -132.9792, -132.1042))

# Scotts Island marine National Wildlife Area
SI <- list(lat = c(50.69236, 50.34589, 50.16361, 50.40536, 51.10119, 51.24275, 51.24933, 51.25139, 50.84367, 50.73981, 50.72765, 50.69236),
           lon = c(-128.3775, -128.7590, -129.3522, -130.0104, -130.1406, -128.9293, -128.7837, -128.6126, -128.1775, -128.4026, -128.3896, -128.3775))

# Hecate Strait/Queen Charlotte Sound Glass Sponge Reefs MPA
SRN <- list(lat = c(53.19803, 53.15611, 53.04847, 53.05192, 53.12161, 53.12903, 53.22464, 53.32222, 53.40150, 53.39464, 53.31181, 53.25572, 53.19803),
            lon = c(-130.3298, -130.3147, -130.4212, -130.5099, -130.7009, -130.7740, -130.7913, -130.9067, -130.8105, -130.7145, -130.6359, -130.5504, -130.3298))

SRC <- list(lat = c(52.00678, 51.93069, 51.85903, 51.88353, 52.08725, 52.14611, 52.26183, 52.49317, 52.53483, 52.56822, 52.42853, 52.33411, 52.16453, 52.00678),
            lon = c(-129.2368, -129.3038, -129.6104, -129.7343, -129.6039, -129.5593, -129.7368, -129.8758, -129.8851, -129.7976, -129.5867, -129.4977, -129.4249, -129.2368))

SRS <- list(lat = c(51.41228, 51.30903, 51.24933, 51.24275, 51.29508, 51.32347, 51.41228),
            lon = c(-128.7995, -128.6766, -128.7837, -128.9293, -129.0081, -129.0149, -128.7995))


#*******************************************************************************
# FULL LISTS ####

# These variables have various uses in the app.
# THEY MUST ALL BE IN THE SAME ORDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# List of polygon objects that were created above.
all_regions <- list(atlantic = list(AC, BRA, CS_V01, CS_V02, CL, CLS, CSS, ELS, ESS,
                                    FC, FP, GB, GS, HB, HIB, HS, LAS, LS, MS_V01, MS_V02,
                                    NCLS, NEGSL_V01, NEGSL_V02, NENS, NGB, NL, NWGSL_V01, NWGSL_V02,
                                    NLS, SES, SAB, SPB, WSS),
                    pacific = list(AOI, SK_B, EHV, GH, GHE, GHW, GHS, GHO, SI, SRN, SRC, SRS))

# Full names of the polygons, followed by the capitalized abbreviation in brackets,
# and then (optionally) "V" and the version number.
full_names <- list(atlantic = c('Avalon Channel (AC)',
                                'Bravo Station (BRA)',
                                'Cabot Strait (CS) V01',
                                'Cabot Strait (CS) V02',
                                'Central Labrador (CL)',
                                'Central Labrador Sea (CLS)',
                                'Central Scotian Shelf (CSS)',
                                'Eastern Labrador Shelf (ELS)',
                                'Eastern Scotian Shelf (ESS)',
                                'Flemish Cap (FC)',
                                'Flemish Pass (FP)',
                                'Georges Bank (GB)',
                                'Greenland Shelf (GS)',
                                'Hamilton Bank (HB)',
                                'Hibernia (HIB)',
                                'Hudson Strait (HS)',
                                'Labrador Shelf (LAS)',
                                'Lurcher Shoal (LS)',
                                'Magdalen Shallows (MS) V01',
                                'Magdalen Shallows (MS) V02',
                                'North Central Labrador Shelf (NCLS)',
                                'Northeast Gulf of St. Lawrence (NEGSL) V01',
                                'Northeast Gulf of St. Lawrence (NEGSL) V02',
                                'Northeast Newfoundland Shelf (NENS)',
                                'Northern Grand Bank (NGB)',
                                'Northern Labrador (NL)',
                                'Northwest Gulf of St. Lawrence (NWGSL) V01',
                                'Northwest Gulf of St. Lawrence (NWGSL) V02',
                                'Northern Labrador Shelf (NLS)',
                                'Southeast Shoal (SES)',
                                'St. Anthony Bank (SAB)',
                                'St. Pierre Bank (SPB)',
                                'Western Scotian Shelf (WSS)'),
                  pacific = c('Offshore Pacific Area of Interest (AOI)',
                              'SGaan Kinghlas-Bowie Seamount MPA (SK_B)',
                              'Endeavour Hydrothermal Vents MPA (EHV)',
                              'Gwaii Haanas (GH)',
                              'Gwaii Haanas east (GHE)',
                              'Gwaii Haanas west (GHW)',
                              'Gwaii Haanas south (GHS)',
                              'Gwaii Haanas Offshore (GHO)',
                              'Scott Islands marine NWA (SI)',
                              'North Glass Sponge Reefs MPA (SRN)',
                              'Central Glass Sponge Reefs MPA (SRC)',
                              'South Glass Sponge Reefs MPA (SRS)'))

# This is an ID for the polygons on the map (it only appears in the output filenames
# and settings, and needs to be unique -- so if a polygon has multiple versions,
# its IDs should be something like ABBREV_V01, ABBREV_V02, ...)
poly_ID <- list(atlantic=c("AC", "BRA", "CS_V01", "CS_V02", "CL", "CLS", "CSS", "ELS",
                           "ESS", "FC", "FP", "GB", "GS", "HB", "HIB", "HS", "LAS", 
                           "LS", "MS_V01", "MS_V02", "NCLS", "NEGSL_V01", "NEGSL_V02",
                           "NENS", "NGB", "NL", "NWGSL_V01", "NWGSL_V02", "NLS", "SES", "SAB", "SPB", "WSS"),
                pacific=c("AOI", "SK_B", "EHV", "GH", "GHE", "GHW", "GHS", "GHO", "SI", "SRN", "SRC", "SRS"))

# This is the abbreviation that appears next to the polygon on the map.
# If there are multiple versions of a box, you have a couple options:
#     If they overlap, try only using the abbreviation once, and the rest of the values should be NA (not in quotes).
#     If they don't overlap, give them unique names (for example, the abbreviation with a number on the end).
abbrev <- list(atlantic=c("AC", "BRA", "CS", NA, "CL", "CLS", "CSS", "ELS", "ESS", "FC", "FP",
                          "GB", "GS", "HB", "HIB", "HS", "LAS", "LS", "MS", NA,
                          "NCLS", "NEGSL", NA, "NENS", "NGB", "NL", "NWGSL", NA, "NLS", "SES",
                          "SAB", "SPB", "WSS"),
               pacific=c("AOI", "SK_B", "EHV", "GH", "GHE", "GHW", "GHS", "GHO", "SI", "SRN", "SRC", "SRS"))

