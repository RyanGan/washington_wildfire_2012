#-------------------------------------------------------------------------------
#     Title: Washington population-weighted smoke PM2.5 by day and zip
#     Author: Ryan Gan                                                                   
#     Date Created: 6/16/16   Date Modified: 6/28/16                                                   
#     R version: 3.3.3                                                       
#-------------------------------------------------------------------------------

# Note: This is general code that could be submitted in batches to handle 
#       multiple imputs (dataframes)

# Note 8/8/16: I think the matrix approach actually runs pretty quick; doesn't
#              need to be submitted in batches

# load libraries ---------------------------------------------------------------
library(dplyr) # data manipulation package 
library(tidyr)
library(readr)


# Setting Working Directory ----------------------------------------------------
#dir <- paste0("C:/Users/RGan/Google Drive/CSU/wild_fire/washington/",
#               "smoke_data/created_pm_estimates")

# relative path 
dir <- paste0("./washington/smoke")
setwd(dir)
getwd()
list.files()

# Import and prepare smoke PM 2.5 data for loop --------------------------------

# These files never change and can take place outside the loop
# Import population weight csv 
pop_path <- paste0('wash_popdens2010.csv')
pop_grid <- read_csv(pop_path)

# rename missing column
colnames(pop_grid) <- c('WRFGRID_ID', 'Longitude', 'Latitude', 'pop_density')
summary(pop_grid)

# calculate the estimated population for each grid (multiply density by grid area)
population_grid <- pop_grid %>% mutate(pop_n = pop_density *(15^2)) 
summary(population_grid)

# Import the file that contains the proportion of each grid that overlaps a zip
zip_grid_path <- paste0('zip_wrfgrid_proportion.csv')
zip_grid_proportion <- read_csv(zip_grid_path)

# Importing files that I would like to loop through
# Geo weighted regresssion (Atmos model of choice)
geo_wt_path <- paste0("GeoWeightedRidgeRegression_PM2.5_wash2012.csv")
geo_wt <- read_csv(geo_wt_path)

# Global Ridge Regression
global_path <- paste0("GlobalRidgeRegression_PM2.5_wash2012.csv")
global_reg <- read_csv(global_path)

# Kriging 
krig_path <- paste0("Kriging_PM2.5_wash2012.csv")
krig <- read_csv(krig_path)

# Background PM2.5
back_path <- paste0("Background_PM2.5_wash2012.csv")
background <- read_csv(back_path)

# WRF-Chem
wrf_path <- paste0("WRF-Chem_PM2.5_wash2012.csv")
wrf <- read_csv(wrf_path)

# WRF-nofire
wrf_nf_path <- paste0("WRF-Chem_nofire_wash2012.csv")
wrf_nf <- read_csv(wrf_nf_path)

# WRF temperature
wrf_temp_path <- paste0("WRF-Chem_2mTemperature_wash2012.csv")
wrf_temp <- read_csv(wrf_temp_path)

# WRF planetary bound layer
wrf_pbl_path <- paste0("WRF-Chem_PBL_wash2012.csv")
wrf_pbl <- read_csv(wrf_pbl_path)

# Dataframes of PM2.5 attributed to wildfire smoke -----------------------------
# _smk indicates that we somehow account for background levels 
# in the case of WRF models, the WRF-nofire model is subtracted off
# in other cases, the background pm is subtracted off

head(wrf[, 1:10])
head(wrf_nf[, 1:10])

# estimating WRF-Chem smoke model
wrf_smk <- wrf[, 4:126] - wrf_nf[, 4:126]
# set all values <0 to 0
wrf_smk[wrf_smk < 0] <- 0
# grid id, lat and long
grid_id <- wrf[,1:3]
# final estimates of grid pm due to smoke
wrf_smk_pm <- cbind(grid_id, wrf_smk)
head(wrf_smk_pm[, 1:10])

# Geo weight, global, and krig dataframes subtracting off background pm2.5
# Geo-weighted regression smoke
geo_smk <- geo_wt[, 4:126] - background[, 4:126]
# set all values <0 to 0
geo_smk[geo_smk < 0] <- 0
# grid id, lat and long
grid_id <- geo_wt[,1:3]
# final estimates of grid pm due to smoke
geo_smk_pm <- cbind(grid_id, geo_smk)
head(geo_smk_pm[, 1:10])

# global smoke
global_smk <- global_reg[, 4:126] - background[, 4:126]
global_smk[global_smk < 0] <- 0
global_smk_pm <- cbind(grid_id, global_smk)
head(global_smk_pm[ ,1:10])

# krig smoke
krig_smk <- krig[, 4:126] - background[, 4:126]
krig_smk[krig_smk <0] <- 0
krig_smk_pm <- cbind(grid_id, krig_smk)
head(krig_smk_pm[, 1:10])

# Setting up dataframes for loop -----------------------------------------------
# create a list of the dataframes I want to loop through
df_list <- list(wrf = wrf, wrf_nf = wrf_nf, wrf_smk_pm = wrf_smk_pm, 
                geo_wt = geo_wt, global_reg = global_reg, krig = krig,
                background = background, geo_smk_pm = geo_smk_pm, 
                global_smk_pm = global_smk_pm, krig_smk_pm = krig_smk_pm, 
                wrf_temp = wrf_temp, wrf_pbl = wrf_pbl)

df_name <- c('wrf', 'wrf_nf', 'wrf_smk_pm', 'geo_wt', 'global_reg', 'krig', 
             'background', 'geo_smk_pm', 'global_smk_pm', 'krig_smk_pm', 
             'wrf_temp', 'wrf_pbl')


# General approach to producing population-weighted zipcode-specific PM2.5 -----

# The best thing to do is mutiply the population grid by the concentration
# matrices first, which I believe should give you the population weighted
# concentrations for each grid (dim should be 1107 by 126 I think). Then take
# that matrix and multiply by zipcode by grid matrix to get the population
# weighted concentration for each grid, which I can then divide by
# the zip_pop_matrix to get the zip code specific population weighted average.


# zip and wrf grid overlap matrix ----------------------------------------------
zip_grid_to_matrix <- zip_grid_proportion[ , 2:1108]
zip_grid_matrix <- matrix(as.numeric(unlist(zip_grid_to_matrix)), 
                          nrow=nrow(zip_grid_to_matrix))
dim(zip_grid_matrix) # (dimension of matrix 595 x 1107)


# creating population grid matrix ----------------------------------------------
# matrix is a 1107 row, 1 column matrix that contains the grid estimated number
# of people
summary(population_grid)
population_to_matrix <- population_grid[, 5]
# Set NAs of population to 0
population_to_matrix$pop_n <- ifelse(is.na(population_to_matrix$pop_n), 0,
                                     population_to_matrix$pop_n) 

summary(population_to_matrix)

# covert to matrix
pop_grid_matrix <- matrix(as.numeric(unlist(population_to_matrix)), 
                          nrow=nrow(population_to_matrix))
dim(pop_grid_matrix) # (1107 x 1 dimension matrix)

# Matrix algebra that can be done outside the list
# Multiply the pop_grid matrix by the zip_grid matrix to get a sum of
# the population in each zipcode
zip_pop_matrix <-  zip_grid_matrix %*% pop_grid_matrix
dim(zip_pop_matrix) # 595 x 1 matrix (column is population for each zip code)

# convert zip_pop_matrix to a vector to use to divide later in the loop
zip_pop_vector <- as.vector(zip_pop_matrix)

# Convert zip population matrix to a vector (easier to apply formula col by col)
# easiest to create a vector to multiply each column (date) of the matrix
grid_population_vector <- as.vector(pop_grid_matrix)
tail(grid_population_vector)

# output zipcode column from zip_grid_proportion for naming purposes later
zipcode <- zip_grid_proportion[,1]
head(zipcode)

# start timer
start <- proc.time()

# set the dataframe of interest to a general dataframe to loop through
for(i in 1:length(df_list)){

daily_pm_grid  <- data.frame(df_list[[i]]) # easier to use double brackets so 
# X comes before each date for each dataframe, then I can remove X when I tidy
data_frame_name <- df_name[i]

# Matrix Multiplication --------------------------------------------------------

# For each day, I need to multiply the PM2.5 concentration for each zip code 

# convert dataframes to matrix (need to conver to matrix and remove variables like
# zip code and lat long)
# daily PM 2.5 concentration for each grid 
daily_pm_to_matrix <- daily_pm_grid[, 4:126]


pm_matrix <- matrix(as.numeric(unlist(daily_pm_to_matrix)), 
                    nrow=nrow(daily_pm_to_matrix))
dim(pm_matrix) # 1107 (pm val in wrf_grid) by 123 (days) matrix

# Multiply the matrix of zip_grid proportion by the PM concentration matrix
# This matrix contains the summed PM2.5 concentrations for each zip code for
# each day
zip_grid_wt_pm_matrix <- zip_grid_matrix %*% pm_matrix

dim(zip_grid_wt_pm_matrix) # 595 x 123 matrix (595 zipcodes by 123 days)

# multiply the population vector by the pm concentration matrix for each day
# (column in the matrix)

pm_pop_matrix <- diag(grid_population_vector) %*% pm_matrix 
dim(pm_pop_matrix) # 1107 (grid) x 123 (days)

# now I want to multiply the pm_pop_matrix by the zip_gird matrix
zip_grid_wt_pm_matrix <-  zip_grid_matrix %*% pm_pop_matrix
# this gives me a matrix of the sum of values for each zipcode for each day 
dim(zip_grid_wt_pm_matrix) # 595 (summed pm2.5 in each zipcodes) x 123 (days)

# now I think each daily value of this matrix needs to be divided by
# the summed population of each zip code
# need to multiply by the inverse of the zip_pop_vector for each column 
# since you cannot divide with matrix algebra
zip_pop_and_grid_wt_pm <- diag(1/zip_pop_vector) %*% zip_grid_wt_pm_matrix 
dim(zip_pop_and_grid_wt_pm) # 595 (population wted avg of PM2.5 ) x 123 (days)
# this should work
summary(zip_pop_and_grid_wt_pm)
# There are two zipcodes with 0 people living in it, therefore there will be
# 2 missing values for each day as dividing by 0 is not possible


# bind the zipcode column in with the matrix (I think this works)
zip_pm_conc <- cbind(zipcode, zip_pop_and_grid_wt_pm)

# now give every other column the header of the date of the geo_wt value
x <- colnames(daily_pm_grid[, 4:126 ])
x # check date column names
x2 <- c('ZIPCODE', x) # concate with zipcode column name
x2
# assign column names to matrix
colnames(zip_pm_conc) <- c(x2)

head(zip_pm_conc)

# create environment dataframe with name of original dataset hia is based off
matrix_name <- paste(data_frame_name, 'df', sep = '_')
assign(matrix_name, zip_pm_conc)

# write permanent dataset
write_path <- paste('./zip_population_weighted_pm/', matrix_name, '.csv', sep = '')
write.csv(matrix_name, file = write_path)


} # end loop

stop <- proc.time() - start
stop

# very fast loop, much easier than my extract function

# I think this might work and be more efficent than my extract raster loop
# it still needs to be tested and confirmed against some of my older datasets

# subset zipcode 98858 # row 124 i think, column 84 for the 21st of september

# Creation of dataframe to join with health estimates --------------------------

# take each dataframe and create one large dataframe of zip code, date, and then
# each population weighted value
tidy_loop <- list(wrf_df = wrf_df, wrf_nf_df = wrf_nf_df, 
                  wrf_smk_pm_df = wrf_smk_pm_df,
                  geo_wt_df = geo_wt_df, global_reg_df = global_reg_df, 
                  krig_df = krig_df, background_df = background_df,
                  geo_smk_pm_df = geo_smk_pm_df, global_smk_pm_df = global_smk_pm_df, 
                  krig_smk_pm_df = krig_smk_pm_df, wrf_temp_df = wrf_temp_df, 
                  wrf_pbl_df = wrf_pbl_df)


pm_name <- c('wrf_pm', 'wrf_nf_pm', 'wrf_smk_pm', 'geo_wt_pm', 'global_reg_pm', 
             'krig_pm', 'background_pm', 'geo_smk_pm', 'global_smk_pm', 'krig_smk_pm', 
             'wrf_temp', 'wrf_pbl')

# empty dataframe
wash_pm_pop_wt_2012 <- data.frame(matrix(vector(), 73185, 14, # rows, columns
                                  dimnames = list(c(), c("ZIPCODE", "date",
                                  pm_name))), stringsAsFactors = F)
# probably not efficient to fill cols 1 and 2 on each loop, but eh, it's fast anyways

for(k in 1:length(tidy_loop)){
  df_to_tidy <- data.frame(tidy_loop[[k]])
  pm_col_name <- pm_name[k]

  wash_zip_pm_pop_wt <- df_to_tidy %>% gather(date, pm_method, -ZIPCODE) %>% 
    arrange(ZIPCODE, date)
  # convert character to date
  wash_zip_pm_pop_wt$date <- as.Date(wash_zip_pm_pop_wt$date, "X%Y%m%d")
  # rename the column 'pm_method' to the stored pm_col_name
  colnames(wash_zip_pm_pop_wt) <- c("ZIPCODE", "date", pm_col_name)
  
  
  # final dataset
  wash_pm_pop_wt_2012[, 1] <- wash_zip_pm_pop_wt[, 1]
  wash_pm_pop_wt_2012[, 2] <- wash_zip_pm_pop_wt[, 2]
  wash_pm_pop_wt_2012[, k+2] <- wash_zip_pm_pop_wt[, 3]
  
} # end loop

summary(wash_pm_pop_wt_2012) # 246 missing values are in the 2 zips with 0 people
head(wash_pm_pop_wt_2012)

df_check <- wash_pm_pop_wt_2012 %>% filter(ZIPCODE == 98858 & date == '2012-09-21')
df_check

write_path <- paste0('./zip_population_weighted_pm/zip_pm_to_merge_with_chars.csv')
write_csv(wash_pm_pop_wt_2012, write_path)

# looks good; write premanent file to merge with health data

# Calculation checks -----------------------------------------------------------

which(colnames(geo_smk_pm_df)== 'X20120921')
check <- geo_smk_pm_df[124, 84]
check
# this is the geo_smk_pm value on sept 21st for zipcode 98858
# value of 107.7 population weighted average for zipcode 98858 on Sept 21st
# my old way of estimating prodced a geo_smk mean of 98.698, and max of 113.2,
# and min of 78.9

# Checking the population wted estimate values with the observed values
# Using Spokane Monroe station and zip 99205
zip <- filter(geo_wt_df, ZIPCODE == 99205)

spokane_check <- zip[, 60:95]
spokane_check

# these estimates are very close to the ground site measures from 09/2012
# WRF Grid 1042 covers all of this zipcode; check proportion
zip_99205_proportion <- zip_grid_proportion %>% filter(ZIPCODE == 99205)
zip_99205_proportion[, c(1015:1017, 1042:1045)]

# check 98858
zip_98858_proportion <- zip_grid_proportion %>% filter(ZIPCODE == 98858)
grid_overlap_98858 <- zip_98858_proportion[, c(663:668, 690:694, 717:721,
                                               743:748)]

overlap_vector_98858 <- data.frame(t(grid_overlap_98858))


# find values from geo_wt_df for the same grid boxes on 9/21
geo_grid <- geo_wt[c(662:667, 689:693, 716:720, 742:747), c(1,86)]
geo_grid

# find population in each grid
pop_grid_921 <- population_grid[c(662:667, 689:693, 716:720, 742:747), c(1,5)]

# bind all columns together
zip_99858_pop_pm <- cbind(overlap_vector_98858, geo_grid, pop_grid_921)

pop_wt_avg_99858 <- sum(zip_99858_pop_pm[, 1] * zip_99858_pop_pm[, 3] *
                    zip_99858_pop_pm[, 5]) / sum(zip_99858_pop_pm[, 1] * zip_99858_pop_pm[, 5])

pop_wt_avg_99858 # 114.1497

# check with the value in the actual dataframe
which(colnames(geo_wt_df)== 'X20120921')
check <- geo_wt_df[124, 84]
check # same value, 114.1497

# code works
