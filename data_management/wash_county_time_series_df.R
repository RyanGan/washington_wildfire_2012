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
summary(chars_conf_df_2012)

# Join CHARS data with names of Washington counties 

# Create two vectors for CHARS county code to bind in with CHARS data
COUNTYRES <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
               "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
               "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
               "33", "34", "35", "36", "37", "38", "39", "99")

county <- c("not_wa_residence", "Adams", "Asotin", "Benton", "Chelan", 
            "Clallam", "Clark", "Columbia", "Cowlitz", "Douglas", "Ferry", "Franklin",
            "Garfield", "Grant", "Grays Harbor", "Island", "Jefferson", "King",
            "Kitsap", "Kittitas", "Klickitat", "Lewis", "Lincoln", "Mason", "Okanogan", 
            "Pacific", "Pend Oreille", "Pierce", "San Juan", "Skagit", "Skamania",
            "Snohomish", "Spokane", "Stevens", "Thurston", "Wahkiakum", "Walla Walla",
            "Whatcom", "Whitman", "Yakima", "Unknown")
  
chars_county <- data.frame(cbind(COUNTYRES, county))
# convert the variables to characters to make merges easier
chars_county$COUNTYRES <- as.character(chars_county$COUNTYRES)
chars_county$county <- as.character(chars_county$county)

# Import population weighted county PM2.5
pm_path <- paste0("C:/Users/RGan/Documents/git_local_repos/wildfire/",
                  "wildfire_washington/smoke/pm_data/wa_county_pop_wt_pm.csv")

county_pm <- read_csv(pm_path)

summary(as.factor(county_pm$county))

# Note: Think about how to work in patient id; single or multiobs?

# create a working dataset 'chars_2012'
chars_2012_county_time_series_df <- chars_conf_df_2012 %>% 
  # filter to 2012 dates (prevents parsing failures later on)
  filter(admit_date_impute >= "2012-01-01" & admit_date_impute <= "2012-12-31") %>% 
  # join in the washington county name based on CHARS county identifier
  full_join(chars_county, by = "COUNTYRES") %>% 
  # filter to urgent and ER visits
  filter(ADM_TYPE == 1 | ADM_TYPE == 2) %>% 
  # rename 'admit_date_impute' to 'date'
  rename(date = admit_date_impute) %>% 
  # group by county of residence and date
  group_by(county, date) %>% 
  # sum up each primary diagnosis for each outcome for each day for each county
  summarise(n_obs = n(), resp_n = sum(resp1), asthma_n = sum(asthma1), 
            pneum_n = sum(pneum1), acute_bronch_n = sum(acute_bronch1), 
            copd_n = sum(copd1),copd_ex_n = sum(copd_ex1), cvd_n = sum(cvd1), 
            ihd_n = sum(ihd1), arrythmia_n = sum(arrhythmia1), hf_n = sum(hf1), 
            cereb_vas_n = sum(cereb_vas1), mi_n = sum(mi1), 
            broken_arm_n = sum(broken_arm1), ra_n = sum(ra1)) %>% 
  # join daily county estimates of pm2.5
  full_join(county_pm, by = c('date', 'county'))
  
summary(chars_2012_county_time_series_df)

# write permanent dataframe

write_path <- paste0("C:/Users/RGan/Documents/git_local_repos/wildfire/",
  "wildfire_washington/analysis/analysis_data/wa_2012_county_time_series.csv")

write_csv(chars_2012_county_time_series_df, write_path)
