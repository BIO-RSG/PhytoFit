# Using the raw chlorophyll data

You can download the raw data used in PhytoFit in *fst* format (stored in the `data/` subdirectory [here](https://github.com/BIO-RSG/PhytoFit/tree/master/data)), which can be read into R.  

### Files you will need:  

- data/*region*/*region_sensor_algorithm_year*.fst  
- [coords.rds](https://github.com/BIO-RSG/PhytoFit/raw/master/coords.rds)  

### Example:

Using MODIS-Aqua OCx chlorophyll-a for 2003 in the Atlantic, and subsetting it to the Gulf of Saint Lawrence...  

First, load the necessary packages:  

```{r}
library(fst)    # to load the data
library(sp)     # to subset the region by lat/lon
```

To read a file:

```{r}
filename <- "data/atlantic/atlantic_modis_ocx_2003.fst"
dat <- read_fst(filename)
```

This loads daily data from 2003 for the full Atlantic region (39 to 63 degrees north, 42 to 71 degrees west), which has 183824 pixels for the 4km resolution binned files. You can preview the data like this:  

```{r}
str(dat)
```

The data has been flattened and formatted as a long dataframe to write to fst, so it needs to be reshaped to a matrix where rows = pixels, columns = days, like so:  

```{r}
# if you're using ocx, poly4, or gsm_gs
dat_mat <- matrix(dat$chl, nrow=183824)
# # if you're using eof
# dat_mat <- matrix(dat$chl, nrow=68067)
str(dat_mat)
```

(Note the different options depending on the algorithm you use - ocx, poly4, and gsmgs are on a 4km-resolution "atlantic" grid, which has 183824 pixels, but eof is on a 4km-resolution Gulf of Saint Lawrence grid, which only has 68067)  

Now load the coordinates file and grab the vector of coordinates associated with the pixels for the Atlantic (or GoSL) region:  

```{r}
coordinates <- readRDS("coords.rds")$atlantic
# coordinates <- readRDS("coords.rds")$gosl_4km
str(coordinates)
```

To subset the data to your region of choice, you can use the `point.in.polygon()` function from the `sp` package. Here we subset the data to the Gulf of Saint Lawrence, defined as 41 to 53 degrees latitude, -75 to -49 degrees longitude (decimal degrees). You need a vector of coordinates for each vertex of the polygon / box:  

```{r}
gosl_lats <- c(41, 53, 53, 41, 41)
gosl_lons <- c(-75, -75, -49, -49, -75)

pixel_index <- as.logical(point.in.polygon(point.x = coordinates$lon,
                                           point.y = coordinates$lat,
                                           pol.x = gosl_lons,
                                           pol.y = gosl_lats))
```

Note that `point.in.polygon()` returns an integer vector of values the same length as point.x and point.y, where the value is 0 if the point is outside the bounds, 1 if it's inside, 2 if on the edge, and 3 if on a vertex. Converting this to a logical vector makes all 0 points *FALSE* and everything else *TRUE*.  

Also note that if you're using eof data, which is already on a smaller grid defined by gosl_lats and gosl_lons, you can skip this step (but it won't hurt to do it anyway).  


Now use this index to subset the data and vectors of coordinates:  

```{r}
dat_mat_subset <- dat_mat[pixel_index,]
lat_subset <- coordinates$lat[pixel_index]
lon_subset <- coordinates$lon[pixel_index]
```

To turn this daily data for 2003 into a dataframe with columns latitude, longitude, day, and chlorophyll, you can do this:  

```{r}
df <- data.frame(latitude = rep(lat_subset, 365),
                 longitude = rep(lon_subset, 365),
                 day = rep(1:365, each = nrow(dat_mat_subset)),
                 chlorophyll = as.numeric(dat_mat_subset),
                 stringsAsFactors = FALSE)
str(df)
```

View the data for day of year 126:  

```{r}
# packages required to view the data on a map, with land boundaries
library(dplyr)          # dataframe manipulation tools
library(raster)         # to rasterize the binned data
library(latticeExtra)   # to plot the land boundaries
data("wrld_simpl", package = "maptools")

df <- df %>%
  # use only the data for day 126
  dplyr::filter(day == 126) %>%
  # remove the "day" column
  dplyr::select(-day)

coordinates(df) <- ~longitude + latitude

# create an empty raster object to the extent of the points
rast <- raster(ext=extent(df), resolution = c(0.065,0.04333333))

# rasterize the irregular points
rast <- rasterize(df, rast, df$chlorophyll, fun = mean, na.rm = TRUE)

# view it on a map (log10 transform first so it's easier to see the gradient)
spplot(log10(rast)) + latticeExtra::layer(sp.polygons(wrld_simpl))
```
