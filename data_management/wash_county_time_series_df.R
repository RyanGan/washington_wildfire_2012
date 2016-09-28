# ------------------------------------------------------------------------------
# Title: Creation of county-level time-series dataframe for Washington 2012
# Author: Ryan Gan
# Date Created: 9/28/16
# ------------------------------------------------------------------------------

# Load libraries -----
library(tidyverse)

# Import 2012 CHARS confidential dataframe with outcome ---- 

# Note: file is saved on my work computer or the secured CSU server
# create read path
read_path <- paste0("C:/Users/RGan/Documents/CSU/Wild Fire/",
           "Washington St CHARS Data/confidential_data/chars_2012_confidential.csv")

chars_conf_df_2012 <- read_csv(read_path)

# Join CHARS data with names of Washington counties 

# Create two vectors for CHARS county code to bind in with CHARS data
COUNTYRES <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
               "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
               "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
               "33", "34", "35", "36", "37", "38", "39", "99")

wa_county_name <- c("not_wa_residence", "Adams", "Asotin", "Benton", "Chelan", 
            "Clallam", "Clark", "Columbia", "Cowlitz", "Douglas", "Ferry", "Franklin",
            "Garfield", "Grant", "Grays Harbor", "Island", "Jefferson", "King",
            "Kitsap", "Kittitas", "Klickitat", "Lewis", "Lincoln", "Mason", "Okanogan", 
            "Pacific", "Pend Oreille", "Pierce", "San Juan", "Skagit", "Skamania",
            "Snohomish", "Spokane", "Stevens", "Thurston", "Wahkiakum", "Walla Walla",
            "Whatcom", "Whitman", "Yakima", "Unknown")
  
chars_county <- data.frame(cbind(COUNTYRES, wa_county_name))
# convert the variables to characters to make merges easier
chars_county$COUNTYRES <- as.character(chars_county$COUNTYRES)
chars_county$wa_county_name <- as.character(chars_county$wa_county_name)

# create a working dataset 'chars_2012'
chars_time_series_2012 <- chars_conf_df_2012 %>% 
  # join in the washington county name based on CHARS county identifier
  full_join(chars_county, by = "COUNTYRES") %>% 
  # filter to urgent and ER visits
  filter(ADM_TYPE == 1 | ADM_TYPE == 2) %>% 
  # group by county of residence and date
  group_by(wa_county_name, )
  




# mport Washington County 