# Using the raw chlorophyll data

You can download the raw data used in PhytoFit in *fst* format (stored in the `data/` subdirectory [here](https://github.com/BIO-RSG/PhytoFit/tree/master/data)), which can be read into R. This data is binned, meaning that instead of being projected, it's been placed in "bins" of approximately equal area on the Earth's surface. See [NASA OBPG's explanation](https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/) of the binning scheme for details.  

### Files you will need:  

- data/*region*/*region_sensor_variable_year*.fst  
- [reginfo.rds](https://github.com/BIO-RSG/PhytoFit/raw/master/reginfo.rds)  

### Example:

Using the 2022 reprocessing of MODIS-Aqua OCI chlorophyll-a for 2003 in the Atlantic, and subsetting it to the Gulf of Saint Lawrence...  

First, load the necessary packages:  

```{r}
library(fst)    # to load the data
library(sp)     # to subset the region by lat/lon
```

To read a file:

```{r}
filename <- "data/atlantic/atlantic_modisaquar2022.0_chloci_2003.fst"
dat <- read_fst(filename)
```

This loads daily data from 2003 for the full Atlantic region (39 to 63 degrees north, 42 to 71 degrees west), which has 183824 pixels for the 4km resolution binned files. You can preview the data like this:  

```{r}
str(dat)
```

The data has been flattened and formatted as a long dataframe to write to fst, so it needs to be reshaped to a matrix where rows = pixels, columns = days, like so:  

```{r}
# if you're using ocx, poly4, or gsm_gs in the atlantic
dat_mat <- matrix(dat[[1]], nrow=183824)
# # if you're using oci, poly4, or gsm_gs in the pacific
# dat_mat <- matrix(dat[[1]], nrow=48854)
# # if you're using eof
# dat_mat <- matrix(dat[[1]], nrow=68067)
str(dat_mat)
```

(Note the different options depending on the algorithm you use - oci, poly4, and gsmgs are on a 4km-resolution "atlantic" or "pacific" grid, which has 183824 (or 48854) pixels, but eof is on a 4km-resolution Gulf of Saint Lawrence grid, which only has 68067)  

Now load the coordinates file and grab the vector of coordinates associated with the pixels for the Atlantic (or Pacific or GoSL) region:  

```{r}
coordinates <- readRDS("reginfo.rds")$atlantic
# coordinates <- readRDS("reginfo.rds")$pacific
# coordinates <- readRDS("reginfo.rds")$gosl4km
str(coordinates)
```

To subset the data to your region of choice, you can use the `point.in.polygon()` function from the `sp` package. Here we subset the data to the Gulf of Saint Lawrence, defined as 41 to 53 degrees latitude, -75 to -49 degrees longitude (decimal degrees). You need a vector of coordinates for each vertex of the polygon / box:  

```{r}
# example box vertices to subset the grid
lat_example <- c(41, 53, 53, 41, 41)
lon_example <- c(-75, -75, -49, -49, -75)

pixel_index <- as.logical(point.in.polygon(point.x = coordinates$lon,
                                           point.y = coordinates$lat,
                                           pol.x = lon_example,
                                           pol.y = lat_example))
```

Note that `point.in.polygon()` returns an integer vector of values the same length as point.x and point.y, where the value is 0 if the point is outside the bounds, 1 if it's inside, 2 if on the edge, and 3 if on a vertex. Converting this to a logical vector makes all 0 points *FALSE* and everything else *TRUE*.  

Now use this index to subset the data, coordinates, and bin number:  

```{r}
dat_mat_subset <- dat_mat[pixel_index,]
lat_subset <- coordinates$lat[pixel_index]
lon_subset <- coordinates$lon[pixel_index]
bin_subset <- coordinates$bin[pixel_index]
```

To turn this daily data for 2003 into a dataframe with columns latitude, longitude, day, and chlorophyll, you can do this:  

```{r}
df <- data.frame(latitude = rep(lat_subset, ncol(dat_mat)),
                 longitude = rep(lon_subset, ncol(dat_mat)),
                 bin = rep(bin_subset, ncol(dat_mat)),
                 day = rep(1:ncol(dat_mat), each = nrow(dat_mat_subset)),
                 chlorophyll = as.numeric(dat_mat_subset),
                 stringsAsFactors = FALSE)
str(df)
```

(Note that this dataframe might be empty if your example box above this is outside the bounds of the region you chose - i.e. *atlantic*, *pacific*, or *gosl*)  

View the data for day of year 126:  

```{r}
library(dplyr) # dataframe manipulation tools
df <- df %>%
  # use only the data for day 126
  dplyr::filter(day == 126) %>%
  # remove the "day" column
  dplyr::select(-day)

# MAP CODE OPTION 1:
library(oceancolouR) # install with remotes::install_github("BIO-RSG/oceancolouR")
# make a raster out of the points
rast <- var_to_rast(df=df %>% dplyr::select(bin, chlorophyll), ext=c(range(lon_example),range(lat_example)))
# view it on a map (log10 transform first so it's easier to see the gradient)
make_raster_map(log10(rast), xlim=range(lon_example), ylim=range(lat_example))

# # MAP CODE OPTION 2:
# library(raster)
# library(latticeExtra)
# data("wrld_simpl", package = "maptools")
# coordinates(df) <- ~longitude + latitude
# # create an empty raster object to the extent of the points
# rast <- raster(ext=extent(df), resolution = c(0.065,0.04333333))
# # rasterize the irregular points
# rast <- rasterize(df, rast, df$chlorophyll, fun = mean, na.rm = TRUE)
# # view it on a map
# spplot(log10(rast)) + latticeExtra::layer(sp.polygons(wrld_simpl))
```
