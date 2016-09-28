# ------------------------------------------------------------------------------
# Title: Creating dataframe of proportion of each WRF-Grid cell in each 
#        Washingon county
# Author: Ryan Gan
# Date Created: 8/29/16
# ------------------------------------------------------------------------------

# Libraries 
library(readr)
library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(dplyr)

# Import Shapefiles  -----------------------------------------------------------
# WRF Grid
grid_dir <- paste0('./washington/smoke/wash_grid_shapefile')

smoke_grid <- readOGR(dsn = grid_dir, layer = "wash_grid")
summary(smoke_grid) # has 1107 here
plot(smoke_grid) # check out the grid

# US county shapefile
us_dir <- paste0('./washington/smoke/us_census_shapes/tl_2012_us_county')

us_county_2012 <- readOGR(dsn = us_dir, layer = 'tl_2012_us_county')
summary(us_county_2012)

# limit to washington counties (STATEFP = 53)
wa_county_2012 <- us_county_2012[us_county_2012$STATEFP == '53', ]

# Set coordinate reference system for smoke grid to align with county map
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83
summary(smoke_grid)

# Plot overlay
plot(smoke_grid)
plot(wa_county_2012, add=T)

# Loop to estimate proportion of overlap between grid and county ---------------

# output washington county names to a dataframe
wa_county <- data.frame(wa_county_2012@data$NAME) 
wa_county_name <- as.character(wa_county[, 1])
length(wa_county_name) # check length, should be 39 counties

wrf_grid_name <- as.character(smoke_grid@data$WRFGRID_ID)
length(wrf_grid_name) # 1107 grid ids

# empty matrix to fill
county_wrf <- matrix(nrow = 39, ncol = 1107, byrow = T,
              dimnames = list(wa_county_name, wrf_grid_name))

summary(county_wrf)

# matrix should be faster and less memory than a df
# start time
start <- proc.time()

# first I want to subset out each zipcode shapefile
for(i in 1:length(wa_county_name)){

  # output value of zipcode
  county <- as.character(wa_county_name[i]) 
  # limit shapefile to particular zipcode
  county_shape <- wa_county_2012[wa_county_2012$NAME %in% county, ]
  # convert to polygon
  county_poly <-SpatialPolygons(county_shape@polygons)
  
  # now I can create the second loop that finds the proportion of the area of
  # the zipcode polygon that overlaps with each WRF-Grid
  for(j in 1:length(wrf_grid_name)){

    # output each grid and create a polygon
    wrf_grid <- smoke_grid[smoke_grid@data$WRFGRID_ID == j, ]
    # now what about grid 719; should be much less
    wrf_poly <- SpatialPolygons(wrf_grid@polygons)

    county_wrf_intersect <- gIntersection(wrf_poly, county_poly)
    # if empty, then set to 0, else find the proportion
    grid_prop <- ifelse(is.null(county_wrf_intersect),
                        0, gArea(county_wrf_intersect)/gArea(wrf_poly))
    # populate the matrix based on i position and j position
    county_wrf[[i,j]] <- grid_prop
  }
}

# stop time
stop <- proc.time() - start
stop

summary(county_wrf)

# rename variables to denote this is the estimated smoke from geo-backgrnd model
x <- colnames(county_wrf[, 1:1107])
x2 <- paste('wrf_grid_', x, sep = '')
colnames(county_wrf) <- c(x2)

# bind in zipcode column
county_wrf_df <- cbind(wa_county_name, county_wrf)

str(county_wrf_df[,1:10]) # bind worked

# set matrix as dataframe
county_wrf_proportion_df <- as_data_frame(county_wrf_df)

str(county_wrf_proportion_df[,1:10]) # bind worked


# Write permanent csv file -----------------------------------------------------
getwd()
write_path <- paste0('./washington/smoke/county_wrfgrid_proportion.csv')

write_csv(county_wrf_proportion_df, write_path)

# Checking Spokane zipcode 99205 -----------------------------------------------
spokane_map <- wa_county_2012[wa_county_2012$NAME == 'Spokane',]

# subset WRF-GRID to a smaller dataframe and take a look with spokane county
plot(spokane_map)
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))

# check spokane county and grids  
# subset zipcode and grids
spokane <- county_wrf_proportion_df %>% filter(wa_county_name == "Spokane") %>% 
  select(wrf_grid_989, wrf_grid_988, wrf_grid_1016, wrf_grid_1043)

# numbers created look good based on overlap image.
