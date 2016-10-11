# ------------------------------------------------------------------------------
# Title: Creating dataframe of proportion of each zipcode in each WRF-Grid cell
# Author: Ryan Gan
# Date Created: 7/27/16
# ------------------------------------------------------------------------------

# Libraries 
library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(tidyverse)


# Import Shapefiles  -----------------------------------------------------------
# WRF Grid
grid_dir <- paste0('./washington/smoke/wash_grid_shapefile')

smoke_grid <- readOGR(dsn = grid_dir, layer = "wash_grid")
summary(smoke_grid) # has 1107 here
plot(smoke_grid) # check out the grid

# Zipcode shapefile
us_dir <- paste0('./washington/smoke/us_census_shapes/tl_2012_us_zcta510')

us_zip_2012 <- readOGR(dsn = us_dir, layer = 'tl_2012_us_zcta510')
summary(us_zip_2012)

# read in CHARS zip code file so I can subset the larger US shapefile to just
# Washington state

zip_file <- paste0("./washington/smoke/wash_zip_2012.csv")
chars_zip_2012 <- read_csv(zip_file)
chars_zip_2012 <- chars_zip_2012[,2:3]
chars_zip_2012$ZIPCODE <- as.character(chars_zip_2012$ZIPCODE)

# check on zip code 99998, appears to be in germany? is it a army base?
# removing anyways
chars_zip_2012 <- filter(chars_zip_2012, ZIPCODE != '99998')

# limit to just washington state zipcodes
wash_zip_map <- us_zip_2012[us_zip_2012$ZCTA5CE10 %in% chars_zip_2012$ZIPCODE,]

# output zipcodes from washington zipcode map to bind values to
 

# Set coordinate reference system for smoke gird
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83

# Plot overlay
plot(smoke_grid)
plot(wash_zip_map, add=T)
# looks like they overlay pretty well, same projections

# Test code to figure out proportion calculations in each WRF-Grid -------------
# Trying 'over' function in sp package
# limit to a specific zip code
test_zip <- c(98858)
test_zip_map <- wash_zip_map[wash_zip_map$ZCTA5CE10 %in% test_zip,]

plot(test_zip_map)
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))

# smoke grid over test zip map
test_grid <- over(smoke_grid, test_zip_map)
summary(test_grid) # 14 grids over the zip code
test_grid2 <- test_grid %>% filter(!is.na(ZCTA5CE10))
test_grid2


# test zip map over zip grid
plot(smoke_grid)
plot(test_zip_map, add = T)
zip_over_grid <- over(test_zip_map, smoke_grid)
summary(zip_over_grid) # this way retains the values


# try gIntersection function from rgeos
# http://stackoverflow.com/questions/35039614/r-calculate-overlapping-section-
# polygon-intersection-the-fast-way

shape_zip <- SpatialPolygons(test_zip_map@polygons)
shape_grid <- SpatialPolygons(smoke_grid@polygons)
plot(shape_zip)
plot(shape_grid, add = T)
# try and plot the values for this zipcode for 9/21
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))

# output the WRF Grid 719 and 718 for test
wrf_grid_718 <- smoke_grid[smoke_grid@data$WRFGRID_ID == 718, ]
plot(wrf_grid_718)

wrf_grid_719 <- smoke_grid[smoke_grid@data$WRFGRID_ID == 719, ]
plot(wrf_grid_719)

# area of grid 718
gArea(SpatialPolygons(wrf_grid_718@polygons))

# Subset to one zip code and two different WRF grids ---------------------------
# plot zip and 2 grids
plot(test_zip_map)
#invisible(text(getSpPPolygonsLabptSlots(test_zip_map), 
#               labels=as.character(test_zip_map$ZCTA5CE10)))
plot(wrf_grid_718, add = T)
invisible(text(getSpPPolygonsLabptSlots(wrf_grid_718), 
               labels=as.character(wrf_grid_718$WRFGRID_ID)))
plot(wrf_grid_719, add=T)
invisible(text(getSpPPolygonsLabptSlots(wrf_grid_719), 
               labels=as.character(wrf_grid_719$WRFGRID_ID)))

# Start with the intersection with wrf grid 718 and zipcode
# first I need to convert the spatial polygon to just polygon
poly_718 <- SpatialPolygons(wrf_grid_718@polygons)
shape_zip <- SpatialPolygons(test_zip_map@polygons)

zip_718_int <- gIntersection(poly_718, shape_zip)

plot(zip_718_int)
prop_int_718 <- gArea(zip_718_int)/gArea(poly_718)
prop_int_718 # 99.9% of grid is covered by zip

# now what about grid 719; should be much less
poly_719 <- SpatialPolygons(wrf_grid_719@polygons)
shape_zip <- SpatialPolygons(test_zip_map@polygons)

zip_719_int <- gIntersection(poly_719, shape_zip)

plot(zip_719_int)
prop_int_719 <- gArea(zip_719_int)/gArea(poly_719)
prop_int_719 # 55.2% of grid is covered by zip


# check the overlap  with zipcodes 98801  and 98807

# Loop to estimate proportion of area covered by each grid for each zip --------
# I'm expecting a matrix of 595 zipcodes * 1107 wrf_grids 


wa_zip_name <-as.character(wa_zip[,1])
length(wa_zip_name)
wrf_grid_name <- as.character(smoke_grid@data$WRFGRID_ID)
length(wrf_grid_name)

# empty matrix
zip_wrf_proportion <- matrix(nrow = 595, ncol = 1107, byrow = T,
                             dimnames = list(wa_zip_name, wrf_grid_name))

summary(zip_wrf_proportion)

# matrix should be faster and less memory than a df
# start time
start <- proc.time()

# first I want to subset out each zipcode shapefile
for(i in 1:length(wa_zip_name)){
  # output value of zipcode
  zipcode <- as.character(wa_zip[i,]) 
  # limit shapefile to particular zipcode
  zip_shape <- wash_zip_map[wash_zip_map$ZCTA5CE10 %in% zipcode, ]
  # convert to polygon
  zip_poly <-SpatialPolygons(zip_shape@polygons)
  
  # now I can create the second loop that finds the proportion of the area of
  # the zipcode polygon that overlaps with each WRF-Grid
  for(j in 1:length(wrf_grid_name)){
    # output each grid and create a polygon
    wrf_grid <- smoke_grid[smoke_grid@data$WRFGRID_ID == j, ]
    # now what about grid 719; should be much less
    wrf_poly <- SpatialPolygons(wrf_grid@polygons)

    zip_wrf_intersect <- gIntersection(wrf_poly, zip_poly)
    # if empty, then set to 0, else find the proportion
    grid_prop <- ifelse(is.null(zip_wrf_intersect),
                        0, gArea(zip_wrf_intersect)/gArea(wrf_poly))
    # populate the matrix based on i position and j position
    zip_wrf_proportion[[i,j]] <- grid_prop
  }
}

# stop time
stop <- proc.time() - start
stop

summary(zip_wrf_proportion[, 1:10])

# rename variables to denote this is the estimated smoke from geo-backgrnd model
x <- colnames(zip_wrf_proportion[, 1:1107])
x2 <- paste('wrf_grid_', x, sep = '')
colnames(zip_wrf_proportion) <- c(x2)

# bind in zipcode column
zip_wrf_df <- cbind(wa_zip, zip_wrf_proportion)

str(zip_wrf_df[,1:10]) # bind worked

# set matrix as dataframe
zip_wrf_proportion_df <- as_data_frame(zip_wrf_df)

str(zip_wrf_proportion_df[,1:10]) # bind worked

# check zip 98858 and gris 718 and 719 to see if proportions match what I got 
# above
# subset zipcode and grids
zip_wrf_check <- select(zip_wrf_proportion_df, ZIPCODE, wrf_grid_718, 
                        wrf_grid_719) %>% 
                 filter(ZIPCODE == 98858)

zip_wrf_check # these are the same numbers I get as above, good. Working.

# write permanent csv file
getwd()
write_path <- paste0('./washington/smoke/zip_wrfgrid_proportion.csv')

write_csv(zip_wrf_proportion_df, write_path)

# Checking Spokane zipcode 99205 -----------------------------------------------
spokane_zip <- c(99205)
spokane_zip_map <- wash_zip_map[wash_zip_map$ZCTA5CE10 %in% spokane_zip,]

# subset WRF-GRID to a smaller dataframe

plot(smoke_grid)
plot(spokane_zip_map, add = T)
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))



