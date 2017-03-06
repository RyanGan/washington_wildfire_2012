#-------------------------------------------------------------------------------
#     Title: Washington population-weighted smoke PM2.5 by day and zip
#     Author: Ryan Gan                                                                   
#     Date Created: 6/16/16   Date Modified: 6/28/16                                                   
#     R version: 3.3.3                                                       
#-------------------------------------------------------------------------------

# Note: This is general code that could be submitted in batches to handle 
#       multiple imputs (dataframes)

# load libraries ---------------------------------------------------------------
library(dplyr) # data manipulation package 
library(readr)
library(rgeos)
library(maptools) # reading and handling spatial objects
library(rgdal) # package for shape files
library(sp)
library(raster) # need for crs (but masks select in dplyr)

# Setting Working Directory ----------------------------------------------------
#dir <- paste0("C:/Users/RGan/Google Drive/CSU/wild_fire/washington/",
#               "smoke_data/created_pm_estimates")

# relative path 
dir <- paste0("./washington/smoke")
setwd(dir)
getwd()
list.files()

# Import and prepare smoke PM 2.5 data for loop --------------------------------
# Import input (dataframe) and set to a general dataframe
daily_pm_grid_path <- paste("GeoWeightedRidgeRegression_PM2.5_wash2012.csv")
daily_pm_grid  <- read_csv(daily_pm_grid_path)

# Import population weight csv 
pop_path <-paste0('wash_popdens2010.csv')
pop_weight <- read_csv(pop_path)

# rename missing column
colnames(pop_weight) <- c('WRFGRID_ID', 'Longitude', 'Latitude', 'pop_density')
summary(pop_weight)

# remove missing pop values (most are in ocean or in canada)
population_wt <- pop_weight %>% #filter(!is.na(pop_density)) %>% 
                 mutate(pop_n = pop_density *(15^2)) 

summary(population_wt)

# create estimates of geo_smk 
geo_smk <- geo_wt_pm[, 4:126] - background_pm[, 4:126]
# set all values <0 to 0
geo_smk[geo_smk < 0] <- 0
# grid id, lat and long
grid_id <- geo_wt_pm[,1:3]

# final estimates of grid pm due to smoke
geo_smk_pm <- cbind(grid_id, geo_smk)

# rename variables to denote this is the estimated smoke from geo-backgrnd model
x <- colnames(geo_smk_pm[, 4:126])
x2 <- colnames(geo_smk_pm[, 1:3])
x3 <- paste('geo_smk', x, sep = '')
x4 <- c(x2, x3)
colnames(geo_smk_pm) <- c(x4)

summary(geo_smk_pm)


# join in smoke estimates to population dataframe
pop_smoke_df <- population_wt %>% 
  right_join(geo_smk_pm, by = c('WRFGRID_ID', 'Longitude', 'Latitude')) %>%
  mutate_each(funs(. = pop_n * (.)), geo_smk20120701:geo_smk20121031) 

summary(pop_smoke_df[129:251])

# pull out those variables as their own vector rename
pop_x_smoke <- pop_smoke_df[129:251]
x <- colnames(pop_x_smoke)
x2 <- gsub("_.", "", x) 
x3 <- gsub('geomk', 'geo_smk_pop_', x2)
colnames(pop_x_smoke) <- x3

geo_x_population <- cbind(pop_smoke_df, pop_x_smoke)
geo_smk_pop <- geo_x_population[, c(1:128, 252:374)]

summary(geo_smk_pop)

# looks like it worked.

# Shapefiles  ------------------------------------------------------------------
# WRF Grid
grid_dir <- paste0('./wash_grid_shapefile')

smoke_grid <- readOGR(dsn = grid_dir, layer = "wash_grid")
summary(smoke_grid) # has 1107 here
plot(smoke_grid) # check out the grid

# Join population data to grid
smoke_grid@data <- merge(smoke_grid@data, geo_smk_pop, by = 'WRFGRID_ID')
summary(smoke_grid)


# Zipcode shapefile
# pc directory (I need to really figure out relaitve paths)
us_dir <- paste0('./us_census_shapes/tl_2012_us_zcta510')

us_zip_2012 <- readOGR(dsn = us_dir, layer = 'tl_2012_us_zcta510')
summary(us_zip_2012)

# read in  CHARS zip code file
zip_file <- paste0("wash_zip_2012.csv")

chars_zip_2012 <- read_csv(zip_file)
chars_zip_2012 <- chars_zip_2012[,2:3]
chars_zip_2012$ZIPCODE <- as.character(chars_zip_2012$ZIPCODE)

# check on zip code 99998, appears to be in germany? is it a army base?
# removing anyways
chars_zip_2012 <- filter(chars_zip_2012, ZIPCODE != '99998')

# limit to just washington state zipcodes
wash_zip_map <- us_zip_2012[us_zip_2012$ZCTA5CE10 %in% chars_zip_2012$ZIPCODE,]

# output zipcodes from washington zipcode map to bind values to
wa_zip <- data.frame(wash_zip_map@data$ZCTA5CE10) 
wa_zip_list <- list(wa_zip)
wa_zip <- rename(wa_zip, ZIPCODE = wash_zip_map.data.ZCTA5CE10)

# Set coordinate reference system for smoke gird
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
crs(smoke_grid) <- nad83

summary(wash_zip_map)
summary(smoke_grid)
plot(smoke_grid)
plot(wash_zip_map, add=T)
# looks like they overlay pretty well, same projections

# create a raster of the wrf grid polygons
ras_grid <- raster(ncol= 41, nrow =27) # create raster with dimensions of the wrf_grid
extent(ras_grid) <- extent(smoke_grid) # make sure the extents of the raster cover polygon

# polygon needs to come before raster
smoke_raster <- rasterize(smoke_grid, ras_grid, 'pop_n') 
plot(smoke_raster)
plot(wash_zip_map, add=T)
# extract zip estimates of PM from the raster
pop_sum_zip <- extract(smoke_raster, wash_zip_map, fun = sum, na.rm=T)

# weight option in extract only works with fun = mean

smoke_raster_pm <- rasterize(smoke_grid, ras_grid, 'geo_smk_pop_20120921') 
smk_sum_zip <- extract(smoke_raster_pm, wash_zip_map, fun = sum, na.rm=T)
pop_wt_pm_avg <- smk_sum_zip / pop_sum_zip

# unweighted average
geo_smk_raster <- rasterize(smoke_grid, ras_grid, 'geo_smk20120921')
geo_09_21 <- extract(geo_smk_raster, wash_zip_map, fun = mean, na.rm=T)

# test weight option
geo_09_21_wt <- extract(geo_smk_raster, wash_zip_map, fun = mean, na.rm=T, weights=T)

plot(geo_smk_raster)
plot(wash_zip_map, add=T)

pop_wt_pm_df <- cbind(wa_zip, pop_sum_zip, smk_sum_zip, pop_wt_pm_avg, geo_09_21,
                      geo_09_21_wt)
head(pop_wt_pm_df)
summary(pop_wt_pm_df)
# looks like this population-weighted approach works.

# check correlations and scatter
plot(pop_wt_pm_df$pop_wt_pm_avg, pop_wt_pm_df$geo_09_21)
plot(pop_wt_pm_df$pop_wt_pm_avg, pop_wt_pm_df$geo_09_21_wt)
plot(pop_wt_pm_df$geo_09_21_wt, pop_wt_pm_df$geo_09_21)

summary(lm(pop_wt_pm_avg ~ geo_09_21_wt, data = pop_wt_pm_df))

# Now I just need to work it in to the loop and add other measures as well
# I should ask Jeff the different estimates they'd like to look at

