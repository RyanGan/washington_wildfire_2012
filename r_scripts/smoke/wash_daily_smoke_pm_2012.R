#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#     Title: Estimating PM and weather estimates from Washington for each Zip
#     Purpose: Creation of dataset with grid PM, temp, and planetary bound layer
#     Author: Ryan Gan                                                                   
#     Date Created: 6/16/16   Date Modified: 6/28/16                                                   
#     R version: 3.3.3                                                       
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Note: This script joins the values created by Bonne and Will to zip code
# spatial polygon in Washington for September of 2012

# 6/16/16 modifications: Bonne and Will have provided July through October 
#                        estimates. I will update the loops with new data as
#                        it comes in. 
 
# Consider a multi step loop to reduce repeated code

# load libraries ---------------------------------------------------------------
library(dplyr) # data manipulation package (might use plyr instead)
library(tidyr) # data manipulation package (might not use)
library(readr)
library(rgeos)
library(maptools) # reading and handling spatial objects
library(rgdal) # package for shape files
library(raster) # need for crs (but masks select in dplyr and extract in tidyr)
# library(twitteR) # to notify me when loop is done

# # twitter code to use for Tweets
# apikey <- "s1HNbPZeaIyxLlaXQoiBPEM45" # API Key
# apisecret <- "ty2ENuHzHEuj3MnXdvsJX4eWRrPQEjcyeNIqD8ihmwSRm94uWg" #API Secret
# token <- "2474791495-mRJQmYMaSB37pGfZ8R40H0pB1iCBfREDz8e6iEN" #Access Token
# tokensecret <- "wtzRikxrSoViWUU5K8ZoFoflTyIL7iUdBXViYIAEUBl31" #Access token secret

# setup_twitter_oauth(apikey, apisecret, token, tokensecret)
# I don't think the cache is saved yet, I might need to re-enter this every time

# Setting Working Directory ----------------------------------------------------
# Working directory is the folder that contains atmos groups' latest estimates
# for Washington state
#dir <- paste0("C:/Users/RGan/Google Drive/CSU/wild_fire/washington/",
#               "smoke_data/created_pm_estimates")

# Git path on pc (relative path still not working)
dir <- paste0("./washington/smoke")
setwd(dir)
getwd()
list.files() # check the files

# Import and data management of dataframes -------------------------------------

# Import the wrf-chem data frame
wrf_chem <- paste0('WRF_Chem_PM25_wash2012.csv')
wrf_chem_pm <- read_csv(wrf_chem)

# Rename all columns to denote wrf estimates
#x <- colnames(wrf_chem_pm[, 4:126])
#x2 <- colnames(wrf_chem_pm[, 1:3])
#x3 <- paste('wrf', x, sep = '')
#x4 <- c(x2, x3)
#colnames(wrf_chem_pm) <- c(x4)
# all renamed
summary(wrf_chem_pm)

# Import wrf_chem no fire data frame
wrf_nf <- paste0('WRF-Chem_nofire_wash2012.csv')
wrf_nf_pm <- read_csv(wrf_nf)

# Rename all columns to denote wrf estimates
#x <- colnames(wrf_nf_pm[, 4:126])
#x2 <- colnames(wrf_nf_pm[, 1:3])
#x3 <- paste('wrf_nf', x, sep = '')
#x4 <- c(x2, x3)
#colnames(wrf_nf_pm) <- c(x4)
# all renamed
summary(wrf_nf_pm)

# wrf smoke estatimate (wrf-chem estimates minus wrf-nofire estimates)
wrf_smk <- wrf_chem_pm[, 4:126] - wrf_nf_pm[, 4:126]
# setting minimum value to 0 if <0
wrf_smk[wrf_smk < 0] <- 0
summary(wrf_smk)
grid_id <- wrf_chem_pm[,1:3]

# final estimates of grid pm due to smoke
wrf_smk_pm <- cbind(grid_id, wrf_smk)

# rename variables to denote this is the estimated smoke from WRF-Chem model
x <- colnames(wrf_smk_pm[, 4:126])
x2 <- colnames(wrf_smk_pm[, 1:3])
x3 <- paste('wrf_smk', x, sep = '')
x4 <- c(x2, x3)
colnames(wrf_smk_pm) <- c(x4)

summary(wrf_smk_pm)

# Import Geoweightridgeregression and background csv's
geo_reg <- paste0('GeoWeightedRidgeRegression_PM2.5_wash2012.csv')
geo_pm <- read_csv(geo_reg)

background <- paste0("Background_PM2.5_wash2012.csv")
background_pm <- read_csv(background)

# create estimates of geo_smk 
geo_smk <- geo_pm[, 4:126] - background_pm[, 4:126]
# set all values <0 to 0
geo_smk[geo_smk < 0] <- 0
# grid id, lat and long
grid_id <- geo_pm[,1:3]

# final estimates of grid pm due to smoke
geo_smk_pm <- cbind(grid_id, geo_smk)

# rename variables to denote this is the estimated smoke from geo-backgrnd model
x <- colnames(geo_smk_pm[, 4:126])
x2 <- colnames(geo_smk_pm[, 1:3])
x3 <- paste('geo_smk', x, sep = '')
x4 <- c(x2, x3)
colnames(geo_smk_pm) <- c(x4)

summary(geo_smk_pm)

# Import Kriging values
krig <- paste0("Kriging_PM25_wash2012.csv")
krig_pm <- read_csv(krig)
# Rename all columns to denote wrf estimates
x <- colnames(krig_pm[, 4:126])
x2 <- colnames(krig_pm[, 1:3])
x3 <- paste('krig', x, sep = '')
x4 <- c(x2, x3)
colnames(krig_pm) <- c(x4)
summary(krig_pm)

# merge two datasets together
wash_pm_df <- full_join(wrf_smk_pm, geo_smk_pm, 
                        by = c('WRFGRID_ID', 'Longitude', 'Latitude'))
glimpse(wash_pm_df)

# read in shape files of Bonne's and Will's smoke grid -------------------------
# mac directory
# dir <- paste0('/Users/ryangan/Google Drive/CSU/wild_fire/shape_files/',
#              'wash_grid_shapefile')
# pc directory
shp_dir <- paste0('C:/Users/RGan/Google Drive/CSU/wild_fire/shape_files/',
                  'wash_grid_shapefile')

smoke_grid <- readOGR(dsn = shp_dir, layer = "wash_grid")
summary(smoke_grid) # has 1107 here
plot(smoke_grid) # check out the grid
ssplot
# set projection string same as county polygon projectons (nad83)
#nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
#crs(smoke_grid) <- nad83

# Joining pm, temp, and planetary bound layer to the shapefile grid
smoke_grid@data <- merge(smoke_grid@data, wash_pm_df, by = 'WRFGRID_ID')
summary(smoke_grid)
# read US County Shapefile Boundary 2012 ---------------------------------------\

# pc directory (I need to really figure out relaitve paths)
dir2 <- paste0('C:/Users/RGan/Google Drive/CSU/wild_fire/shape_files/',
               'us_census_shapes/tl_2012_us_zcta510')

us_zip_2012 <- readOGR(dsn = dir2, layer = 'tl_2012_us_zcta510')
summary(us_zip_2012)

# read in zip file of CHARS zips
# mac
# zip_file <- paste0("/Users/ryangan/Google Drive/CSU/wild_fire/washington/",
#                   "smoke_data/created_pm_estimates/wash_zip_2012.csv")

zip_file <- paste0("C:/Users/RGan/Google Drive/CSU/wild_fire/washington/",
                   "smoke_data/created_pm_estimates/wash_zip_2012.csv")

chars_zip_2012 <- read_csv(zip_file)
chars_zip_2012 <- chars_zip_2012[,2:3]
chars_zip_2012$ZIPCODE <- as.character(chars_zip_2012$ZIPCODE)

# check on zip code 99998, appears to be in germany? is it a army base?
# removing anyways
chars_zip_2012 <- filter(chars_zip_2012, ZIPCODE != '99998')

# limit to just washington state zipcodes
wash_zip_map <- us_zip_2012[us_zip_2012$ZCTA5CE10 %in% chars_zip_2012$ZIPCODE,]
summary(wash_zip_map)
plot(smoke_grid)
plot(wash_zip_map, add=T)
# looks like they overlay pretty well

# output zipcodes from washington zipcode map to bind values to
wa_zip <- data.frame(wash_zip_map@data$ZCTA5CE10) 
wa_zip_list <- list(wa_zip)
wa_zip <- rename(wa_zip, ZIPCODE = wash_zip_map.data.ZCTA5CE10)

#------------------------------------------------------------------------------
# creation of raster and estimating zipcode-level meteorological values ---------
# create some vectors/dataframes before starting the loop

# vector of dates for July 1st through October 31st, 2012
date_2012 <- data.frame(seq(as.Date('2012-07-01'), as.Date('2012-10-31'), 
                            by = 'day'))
summary(date_2012)

# create a raster of the wrf grid polygons
ras_grid <- raster(ncol= 41, nrow =27) # create raster with dimensions of the wrf_grid
extent(ras_grid) <- extent(smoke_grid) # make sure the extents of the raster cover polygon

# Estimating Zip PM ------------------------------------------------------------

# WRF-Chem smoke estimate loop -------------------------------------------------
# create dataframe to fill
wrf_smk_zip_date_df <- data_frame(date = NA, ZIPCODE = NA, wrf_smk_pm_mean = NA,
               wrf_smk_pm_max = NA, wrf_smk_pm_min = NA, wrf_smk_pm_median = NA)

# start time of procedure
ptm <- proc.time()
# loop start to estimate zip pm by day 
for( i in 4:126){
  
  # set raster name to find in shapefile
  rasterize_column_name <- colnames(wash_pm_df)[i]
  
  # polygon needs to come before raster
  smoke_raster <- rasterize(smoke_grid, ras_grid, rasterize_column_name) 

  # extract zip estimates of PM from the raster
  wrf_smk_pm_mean <- extract(smoke_raster, wash_zip_map, fun = mean, na.rm=T)
  wrf_smk_pm_max  <- extract(smoke_raster, wash_zip_map, fun = max, na.rm=T)
  wrf_smk_pm_min <- extract(smoke_raster, wash_zip_map, fun = min, na.rm=T)
  wrf_smk_pm_median <- extract(smoke_raster, wash_zip_map, fun = median, na.rm=T)
  
  # take the sept date row, -3 since column 1 is grid id and  2:3 are the lat lon
  date <- date_2012[i-3,] 
  
  specific_date_df <- cbind(date, wa_zip, wrf_smk_pm_mean, wrf_smk_pm_max, 
                            wrf_smk_pm_min, wrf_smk_pm_median)
  # iteration of dataframe update for each day (need dplyr package)
  # i might want to bind columns
  wrf_smk_zip_date_df <- bind_rows(wrf_smk_zip_date_df, specific_date_df)
}

# end time
(proc.time() - ptm)/3600
# tweet to let me know it's done
tweet("@RyanWGan R script is done running") 

wrf_smk_zip_date_df <- filter(wrf_smk_zip_date_df, !is.na(date))
summary(wrf_smk_zip_date_df)
tail(wrf_smk_zip_date_df)


# Geo smk estimate loop --------------------------------------------------------
str(wash_pm_df[,127:249])

# create dataframe to fill
geo_smk_zip_date_df <- data_frame(date = NA, ZIPCODE = NA, geo_smk_mean = NA,
                       geo_smk_max = NA, geo_smk_min = NA, geo_smk_median = NA)

# start time of procedure
ptm <- proc.time()


# loop start to estimate zip pm by day ----------------------------------------
for( i in 127:249){
  
  # set raster name to find in shapefile
  rasterize_column_name <- colnames(wash_pm_df)[i]
  
  # polygon needs to come before raster
  smoke_raster <- rasterize(smoke_grid, ras_grid, rasterize_column_name) 

  # extract zip estimates of PM from the raster
  geo_smk_mean <- extract(smoke_raster, wash_zip_map, fun = mean, na.rm=T)
  geo_smk_max  <- extract(smoke_raster, wash_zip_map, fun = max, na.rm=T)
  geo_smk_min <- extract(smoke_raster, wash_zip_map, fun = min, na.rm=T)
  geo_smk_median <- extract(smoke_raster, wash_zip_map, fun = median, na.rm =T)

  # take the date row, -126 to get right dates
  date <- date_2012[i-126,] 
  
  specific_date_df <- cbind(date, wa_zip, geo_smk_mean, geo_smk_max, 
                            geo_smk_min, geo_smk_median)
  # iteration of dataframe update for each day (need dplyr package)
  # i might want to bind columns
  geo_smk_zip_date_df <- bind_rows(geo_smk_zip_date_df, specific_date_df)
}

geo_smk_zip_date_df <- filter(geo_smk_zip_date_df, !is.na(date))
summary(geo_smk_zip_date_df)
tail(geo_smk_zip_date_df)

# end time
(proc.time() - ptm)/3600

# Creating final dataset (join all smoke files together) and output ------------

# note: smk_pm is the difference from WRF-Chem and WRF-Chem no fire
smoke_df <- right_join(wrf_smk_zip_date_df, geo_smk_zip_date_df,
                              by = c("date", "ZIPCODE")) 

summary(smoke_df)

# write pm_zip_date_df of jeffs values to permanent file and merge in with temp
write <- paste0('wash_smoke_zip.csv')
write_csv(smoke_df, write)
?write_csv



