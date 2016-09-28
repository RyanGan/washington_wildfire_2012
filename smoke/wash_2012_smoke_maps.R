#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#     Title: Creating visuals of the wrf grids over washington for july 1 to
#            october 31, 2012
#     Purpose: Nice sequence maps for presentations
#     Author: Ryan Gan
#     Date Created: 6/20/16   Date Modified: 6/20/16
#     R version: 3.2.3
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# libraries used
library(dplyr)
library(ggmap)
library(ggplot2)
library(rgdal) # package for shape files
library(sp)
library(raster) # need for crs (but masks select in dplyr and extract in tidyr)
library(maptools) # need for fortify by region
library(rgeos)
library(readr)

# set working directory
getwd()
# relative path
setwd('./washington/smoke')
list.files()

# read csv smoke files
wrf_chem <- paste0('WRF_Chem_PM25_wash2012.csv')
wrf_chem_pm <- read_csv(wrf_chem)

# Rename all columns to denote wrf estimates
x <- colnames(wrf_chem_pm[, 4:126])
x2 <- colnames(wrf_chem_pm[, 1:3])
x3 <- paste('wrf', x, sep = '')
x4 <- c(x2, x3)
colnames(wrf_chem_pm) <- c(x4)
# all renamed
summary(wrf_chem_pm)

# read in shape files of Bonne's and Will's smoke grid -------------------------
# pc
# grid_dir <- paste0("C:/Users/RGan/Google Drive/CSU/wild_fire/shape_files/",
#                   "wash_grid_shapefile")
# mac
grid_dir <- paste0('/Users/ryangan/Google Drive/CSU/wild_fire/shape_files/',
                    'wash_grid_shapefile')

smoke_grid <- readOGR(dsn = grid_dir, layer = "wash_grid")
summary(smoke_grid) # has 1107 here
plot(smoke_grid) # check out the grid

# output the id to merge
smoke_grid@data$id <- rownames(smoke_grid@data)
smoke_grid_points <- fortify(smoke_grid, region = 'id')
str(smoke_grid_points)
str(wrf_chem_pm)
wrf_chem_pm <- mutate(wrf_chem_pm, id = as.character(WRFGRID_ID))

# merge in wrf_meteor values where WRFGRID_ID == id
smoke_grid_df <- smoke_grid_points %>% full_join(wrf_chem_pm, by = 'id')
smoke_grid_df[ , 134] <- NULL

# might change high value bound
smoke_grid_df <- smoke_grid_df %>% filter(!is.na(wrf20120901)) %>%
                 mutate_each(funs(ifelse( . > 250, 250, .)),
                 wrf20120701:wrf20121031)

summary(smoke_grid_df)

# set projection string same as county polygon projectons (nad83)
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
crs(smoke_grid) <- nad83

# fortify smoke_grid shp to data frame by wrfgrid id (need)
smoke_data <- fortify(smoke_grid, region = 'WRFGRID_ID')
summary(as.factor(smoke_data$id))

# Creating maps ----------------------------------------------------------------

# Set base map layer
# find max and min values of raster grid
xmn=min(wrf_chem_pm[,2]) # min longitude
xmx=max(wrf_chem_pm[,2]) # max longitude
ymn=min(wrf_chem_pm[,3]) # min latitude
ymx=max(wrf_chem_pm[,3]) # max latitude


bbox <- c(-125, 45, -117.0, 50)

washington_map <- get_map(location= bbox, source = 'stamen',
                          maptype = 'toner', crop = TRUE)

# create loop to add pm values for each day
# Date labels for title
date_2012 <- data.frame(seq(as.Date('2012-07-01'), as.Date('2012-10-31'),
                            by = 'day'))

setwd('./map_images') # set directory to save maps
getwd()
# note projection seems a bit off.
# map loop 
for(i in 11:133){ # start of loop
  date <- date_2012[i-10,]
  # print all maps
  ggmap(washington_map) + geom_polygon(data = smoke_grid_df, aes(x = long, y = lat,
                          group = id, fill = smoke_grid_df[i]), alpha = 0.6) +
    scale_fill_gradient(expression('WRF-Chem PM'[2.5]),
                        low = 'white', high = 'red',
                        # limits looks like it works to set same scale
                        limits= c(0,250)) +
    ggtitle(date) + labs(x = "Longitude", y = "Latitude")
  # close out print loop
  ggsave(paste(i,'pdf', sep = '.'))
} # end loop
