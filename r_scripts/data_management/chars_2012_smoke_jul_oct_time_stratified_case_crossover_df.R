# ------------------------------------------------------------------------------
# Title: Case-crossover dataframe creation (only patients with single obs)
#        and joining with county- and zip-level PM2.5 values. 
# Author: Ryan Gan
# Date Created: 6/29/2016   Date Modified: 10/3/16               
# ------------------------------------------------------------------------------

# Notes ----
# Notes 8/16/16: I updated smoke data with population weighted averages for zip
# This code also assumed only one visit, so I remove the people with multiple
# admissions for the outcome

# Notes 10/3/16: I'm retaining the county assignment of the observation so I
# can join the county population weighted averages as well for comparison
# and to analyze morbidity similar to Rish

# Libraries ----
# load libraries
library(tidyverse)
library(lubridate) # working with date


# Read in smoke data created in loop -------------------------------------------
getwd()
# read in zipcode level populatoin-weighted pm
read_path <- paste0('./data/pm_data/zip_pm_to_merge_with_chars.csv')

zip_smoke <- read_csv(read_path) 

# read in county level population-weighted pm
read_path2 <- paste0('./data/pm_data/wa_county_pop_wt_pm.csv')

county_smoke <- read_csv(read_path2)
# descriptives of the two smoke datasets
summary(county_smoke)
summary(zip_smoke)

# Zipcode PM2.5 estimates
# create lag variables that take smoke values from n previous days for zipcodes
zip_smoke_w_lag <- zip_smoke %>% arrange(ZIPCODE, date) %>%
  # group by zipcode
  group_by(ZIPCODE) %>% 
  # wrf
  mutate(wrf_pm_lag1 = lag(wrf_pm, 1, order_by = ZIPCODE), 
    wrf_pm_lag2 = lag(wrf_pm, 2, order_by = ZIPCODE),
    wrf_pm_lag3 = lag(wrf_pm, 3, order_by = ZIPCODE),
    wrf_pm_lag4 = lag(wrf_pm, 4, order_by = ZIPCODE),
    wrf_pm_lag5 = lag(wrf_pm, 5, order_by = ZIPCODE),
    # wrf no fire lag
    wrf_nf_pm_lag1 = lag(wrf_nf_pm, 1, order_by = ZIPCODE),
    wrf_nf_pm_lag2 = lag(wrf_nf_pm, 2, order_by = ZIPCODE),
    wrf_nf_pm_lag3 = lag(wrf_nf_pm, 3, order_by = ZIPCODE),
    wrf_nf_pm_lag4 = lag(wrf_nf_pm, 4, order_by = ZIPCODE),
    wrf_nf_pm_lag5 = lag(wrf_nf_pm, 5, order_by = ZIPCODE),
    # wrf_smk_pm
    wrf_smk_pm_lag1 = lag(wrf_smk_pm, 1, order_by = ZIPCODE),
    wrf_smk_pm_lag2 = lag(wrf_smk_pm, 2, order_by = ZIPCODE),
    wrf_smk_pm_lag3 = lag(wrf_smk_pm, 3, order_by = ZIPCODE),
    wrf_smk_pm_lag4 = lag(wrf_smk_pm, 4, order_by = ZIPCODE),
    wrf_smk_pm_lag5 = lag(wrf_smk_pm, 5, order_by = ZIPCODE),
    # geo weighted pm
    geo_wt_pm_lag1 = lag(geo_wt_pm, 1, order_by = ZIPCODE),
    geo_wt_pm_lag2 = lag(geo_wt_pm, 2, order_by = ZIPCODE),
    geo_wt_pm_lag3 = lag(geo_wt_pm, 3, order_by = ZIPCODE),
    geo_wt_pm_lag4 = lag(geo_wt_pm, 4, order_by = ZIPCODE),
    geo_wt_pm_lag5 = lag(geo_wt_pm, 5, order_by = ZIPCODE),
    # global reg pm
    global_reg_pm_lag1 = lag(global_reg_pm, 1, order_by = ZIPCODE),
    global_reg_pm_lag2 = lag(global_reg_pm, 2, order_by = ZIPCODE),
    global_reg_pm_lag3 = lag(global_reg_pm, 3, order_by = ZIPCODE),
    global_reg_pm_lag4 = lag(global_reg_pm, 4, order_by = ZIPCODE),
    global_reg_pm_lag5 = lag(global_reg_pm, 5, order_by = ZIPCODE),
    # krig pm
    krig_pm_lag1 = lag(krig_pm, 1, order_by = ZIPCODE),
    krig_pm_lag2 = lag(krig_pm, 2, order_by = ZIPCODE),
    krig_pm_lag3 = lag(krig_pm, 3, order_by = ZIPCODE),
    krig_pm_lag4 = lag(krig_pm, 4, order_by = ZIPCODE),
    krig_pm_lag5 = lag(krig_pm, 5, order_by = ZIPCODE),   
    # background pm
    background_pm_lag1 = lag(background_pm, 1, order_by = ZIPCODE),
    background_pm_lag2 = lag(background_pm, 2, order_by = ZIPCODE),
    background_pm_lag3 = lag(background_pm, 3, order_by = ZIPCODE),
    background_pm_lag4 = lag(background_pm, 4, order_by = ZIPCODE),
    background_pm_lag5 = lag(background_pm, 5, order_by = ZIPCODE),   
    # geo_smk_pm 
    geo_smk_pm_lag1 = lag(geo_smk_pm, 1, order_by = ZIPCODE),
    geo_smk_pm_lag2 = lag(geo_smk_pm, 2, order_by = ZIPCODE),
    geo_smk_pm_lag3 = lag(geo_smk_pm, 3, order_by = ZIPCODE),
    geo_smk_pm_lag4 = lag(geo_smk_pm, 4, order_by = ZIPCODE),
    geo_smk_pm_lag5 = lag(geo_smk_pm, 5, order_by = ZIPCODE),
    # global smk pm
    global_smk_pm_lag1 = lag(global_smk_pm, 1, order_by = ZIPCODE),
    global_smk_pm_lag2 = lag(global_smk_pm, 2, order_by = ZIPCODE),
    global_smk_pm_lag3 = lag(global_smk_pm, 3, order_by = ZIPCODE),
    global_smk_pm_lag4 = lag(global_smk_pm, 4, order_by = ZIPCODE),
    global_smk_pm_lag5 = lag(global_smk_pm, 5, order_by = ZIPCODE),
    # krig smk pm
    krig_smk_pm_lag1 = lag(krig_smk_pm, 1, order_by = ZIPCODE),
    krig_smk_pm_lag2 = lag(krig_smk_pm, 2, order_by = ZIPCODE),
    krig_smk_pm_lag3 = lag(krig_smk_pm, 3, order_by = ZIPCODE),
    krig_smk_pm_lag4 = lag(krig_smk_pm, 4, order_by = ZIPCODE),
    krig_smk_pm_lag5 = lag(krig_smk_pm, 5, order_by = ZIPCODE),
    # temp
    wrf_temp_lag1 = lag(wrf_temp, 1, order_by = ZIPCODE),
    wrf_temp_lag2 = lag(wrf_temp, 2, order_by = ZIPCODE),
    wrf_temp_lag3 = lag(wrf_temp, 3, order_by = ZIPCODE),
    wrf_temp_lag4 = lag(wrf_temp, 4, order_by = ZIPCODE),
    wrf_temp_lag5 = lag(wrf_temp, 5, order_by = ZIPCODE)) %>% 
  # ungroup by zip
  ungroup(ZIPCODE) %>% 
  # attach a zip indicator for each smoke variable
  setNames(paste(colnames(.), "zip", sep="_")) %>% 
  # remove the '_zip' from the zipcode and date variable 
  rename(ZIPCODE = ZIPCODE_zip, date = date_zip)

# # check 
# check <- zip_smoke_w_lag %>% 
#   select(ZIPCODE, date, geo_smk_pm_zip, geo_smk_pm_lag1_zip,
#          geo_smk_pm_lag2_zip, geo_smk_pm_lag3_zip) # looks good

# County PM2.5 estimates
# create lag variables that take smoke values from n previous days for county
county_smoke_w_lag <- county_smoke %>% arrange(county, date) %>%
  group_by(county) %>% 
  # wrf
  mutate(wrf_pm_lag1 = lag(wrf_pm, 1, order_by = county), 
    wrf_pm_lag2 = lag(wrf_pm, 2, order_by = county),
    wrf_pm_lag3 = lag(wrf_pm, 3, order_by = county),
    wrf_pm_lag4 = lag(wrf_pm, 4, order_by = county),
    wrf_pm_lag5 = lag(wrf_pm, 5, order_by = county),
    # wrf no fire lag
    wrf_nf_pm_lag1 = lag(wrf_nf_pm, 1, order_by = county),
    wrf_nf_pm_lag2 = lag(wrf_nf_pm, 2, order_by = county),
    wrf_nf_pm_lag3 = lag(wrf_nf_pm, 3, order_by = county),
    wrf_nf_pm_lag4 = lag(wrf_nf_pm, 4, order_by = county),
    wrf_nf_pm_lag5 = lag(wrf_nf_pm, 5, order_by = county),
    # wrf_smk_pm
    wrf_smk_pm_lag1 = lag(wrf_smk_pm, 1, order_by = county),
    wrf_smk_pm_lag2 = lag(wrf_smk_pm, 2, order_by = county),
    wrf_smk_pm_lag3 = lag(wrf_smk_pm, 3, order_by = county),
    wrf_smk_pm_lag4 = lag(wrf_smk_pm, 4, order_by = county),
    wrf_smk_pm_lag5 = lag(wrf_smk_pm, 5, order_by = county),
    # geo weighted pm
    geo_wt_pm_lag1 = lag(geo_wt_pm, 1, order_by = county),
    geo_wt_pm_lag2 = lag(geo_wt_pm, 2, order_by = county),
    geo_wt_pm_lag3 = lag(geo_wt_pm, 3, order_by = county),
    geo_wt_pm_lag4 = lag(geo_wt_pm, 4, order_by = county),
    geo_wt_pm_lag5 = lag(geo_wt_pm, 5, order_by = county),
    # global reg pm
    global_reg_pm_lag1 = lag(global_reg_pm, 1, order_by = county),
    global_reg_pm_lag2 = lag(global_reg_pm, 2, order_by = county),
    global_reg_pm_lag3 = lag(global_reg_pm, 3, order_by = county),
    global_reg_pm_lag4 = lag(global_reg_pm, 4, order_by = county),
    global_reg_pm_lag5 = lag(global_reg_pm, 5, order_by = county),
    # krig pm
    krig_pm_lag1 = lag(krig_pm, 1, order_by = county),
    krig_pm_lag2 = lag(krig_pm, 2, order_by = county),
    krig_pm_lag3 = lag(krig_pm, 3, order_by = county),
    krig_pm_lag4 = lag(krig_pm, 4, order_by = county),
    krig_pm_lag5 = lag(krig_pm, 5, order_by = county),   
    # background pm
    background_pm_lag1 = lag(background_pm, 1, order_by = county),
    background_pm_lag2 = lag(background_pm, 2, order_by = county),
    background_pm_lag3 = lag(background_pm, 3, order_by = county),
    background_pm_lag4 = lag(background_pm, 4, order_by = county),
    background_pm_lag5 = lag(background_pm, 5, order_by = county),   
    # geo_smk_pm 
    geo_smk_pm_lag1 = lag(geo_smk_pm, 1, order_by = county),
    geo_smk_pm_lag2 = lag(geo_smk_pm, 2, order_by = county),
    geo_smk_pm_lag3 = lag(geo_smk_pm, 3, order_by = county),
    geo_smk_pm_lag4 = lag(geo_smk_pm, 4, order_by = county),
    geo_smk_pm_lag5 = lag(geo_smk_pm, 5, order_by = county),
    # global smk pm
    global_smk_pm_lag1 = lag(global_smk_pm, 1, order_by = county),
    global_smk_pm_lag2 = lag(global_smk_pm, 2, order_by = county),
    global_smk_pm_lag3 = lag(global_smk_pm, 3, order_by = county),
    global_smk_pm_lag4 = lag(global_smk_pm, 4, order_by = county),
    global_smk_pm_lag5 = lag(global_smk_pm, 5, order_by = county),
    # krig smk pm
    krig_smk_pm_lag1 = lag(krig_smk_pm, 1, order_by = county),
    krig_smk_pm_lag2 = lag(krig_smk_pm, 2, order_by = county),
    krig_smk_pm_lag3 = lag(krig_smk_pm, 3, order_by = county),
    krig_smk_pm_lag4 = lag(krig_smk_pm, 4, order_by = county),
    krig_smk_pm_lag5 = lag(krig_smk_pm, 5, order_by = county),
    # temp
    wrf_temp_lag1 = lag(wrf_temp, 1, order_by = county),
    wrf_temp_lag2 = lag(wrf_temp, 2, order_by = county),
    wrf_temp_lag3 = lag(wrf_temp, 3, order_by = county),
    wrf_temp_lag4 = lag(wrf_temp, 4, order_by = county),
    wrf_temp_lag5 = lag(wrf_temp, 5, order_by = county), order_by = county) %>% 
  # ungroup county
  ungroup(county) %>% 
  # attach a zip indicator for each smoke variable
  setNames(paste(colnames(.), "county", sep="_")) %>% 
  # remove the '_zip' from the zipcode and date variable 
  rename(county = county_county, date = date_county)

# check 
# check <- county_smoke_w_lag %>% 
#    select(county, date, geo_smk_pm_county, geo_smk_pm_lag1_county,
#           geo_smk_pm_lag2_county, geo_smk_pm_lag3_county) # looks good

# Infile the Permanent Cleaned CHARS 2012 DataFrame ----------------------------
# Put this file on the atmos server ASAP
read <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/',
               'confidential_data/chars_2012_confidential.csv')
# read in chars cleaned data
chars_2012_conf_df <- read_csv(read)
# check data
glimpse(chars_2012_conf_df)

# find how many admissions overall were during the july 1 to oct31 timeframe
n_period <- chars_2012_conf_df %>% 
  filter(ADM_DATE >= "2012-07-01" & ADM_DATE <= "2012-10-31") %>% 
  nrow()

n_period


# look up admit type, may want to subset to specifc admit
# ADM_TYPE variable: 1 = emergency, 2 = urgent, 3 = elective, 4 = newborn
# 5 = trauma, 9 = info not available
summary(as.factor(chars_2012_conf_df$ADM_TYPE))
# when I run analyses, I may want to limit only to emergency or urgent

# Adding in county names based on the COUNTYRES variable in CHARS data
# COUNTYRES values can be found in the CHARS data key
# Join CHARS data with names of Washington counties 

## NOTE 10/3/16: I should actually join these county names in the code cleaning
# and creating variables for the CHARS dataset

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

# join county name in to CHARS data
# create a working dataset 'chars_2012'
chars_2012_to_loop <- chars_2012_conf_df %>% 
  # filter to 2012 dates (prevents parsing failures later on)
  filter(admit_date_impute >= "2012-01-01" & admit_date_impute <= "2012-12-31") %>% 
  # join in the washington county name based on CHARS county identifier
  full_join(chars_county, by = "COUNTYRES")

summary(chars_2012_to_loop)

# Case-crossover datasets ------------------------------------------------------


# set path to write permanent files
wd_path <- paste0('./analysis/analysis_data')
setwd(wd_path)
# check working directory
getwd()

glimpse(chars_2012_to_loop)

var_list <- c('resp1', 'asthma1', 'copd_ex1', 'copd1', 'pneum1', 'acute_bronch1',
              'cvd1', 'mi1', 'arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1', 'ra1',
              'broken_arm1')
# remove non-trauma, resp1 and pneum (way too big for this loop)
# var_list <- c('arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1')
# still to large to efficiently make datasets on this keyboard. Need to refine.

start <- Sys.time()
for(j in var_list){ # begin first loop of variable names (outcomes)

# Case-Crossover loop ----------------------------------------------------------

outcome_col <- which(colnames(chars_2012_to_loop) == j) # use to keep outcome var

outcome_id <- chars_2012_to_loop %>%
              filter(chars_2012_to_loop[[j]] == 1) %>% # jth outcome
              filter(admit_date_impute >= '2012-07-01' & 
                     admit_date_impute <= '2012-10-31') %>% 
              filter(STATERES == 'WA') %>% # limit to washington 
              arrange(admit_date_impute) %>%
              # commenting out id as I have patientid now  
              # mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
              select(PATIENTID, (outcome_col), # keep in bracket for outcome var num
                    admit_date_impute, DIS_DATE, ADM_TYPE, LENSTAYD, ZIPCODE, 
                    COUNTYRES, county, AGE, SEX, sex_num, race_nhw, age_cat) %>%
              mutate(date_admit = admit_date_impute,
                     date_discharge = DIS_DATE,
                     length_stay = LENSTAYD) %>%
              arrange(PATIENTID, admit_date_impute) %>% 
              # generate a enumerated variable to indicate number of obs by patid
              group_by(PATIENTID) %>% 
              mutate(obs_num = seq_along(PATIENTID)) %>% 
              select(-admit_date_impute, -DIS_DATE, -LENSTAYD)

# I want to remove people with multiple visits
# find people with multiple visits and make an indicator
outcome_count <- outcome_id %>% group_by(PATIENTID) %>% 
  summarise(obs = n()) %>% mutate(multi_obs = ifelse(obs > 1, 1, 0))

# merge an indicator of multi obs back in to outcome_id and check the patients
# with multiple visits and limit to patients who only come in once
single_visits <- outcome_id %>% left_join(outcome_count, by = 'PATIENTID') %>% 
  filter(multi_obs == 0)

# for asthma: 1765 - 1638
# lost 127 observations with multiple visits

outcome_col2 <- which(colnames(single_visits) == j) # use to keep outcome var

# create dataset to populate
id_date_df <- data_frame()

# begin second loop to create counterfactual observations for each case subject
        for (i in 1:nrow(single_visits)){
         # code dates for each id up to two months before and after the event (56 days)
         # note 10/10/16, trying the entire referent periods of 126
         date <- seq(as.Date(single_visits[[i, 12]] - 126), 
                     # consider every day
                     as.Date(single_visits[[i, 12]] + 126), by = '1 week') 

         # covariates to preserve
         covariate <- single_visits[i, ]%>% 
                select(PATIENTID, (outcome_col2), ZIPCODE, COUNTYRES, county, 
                       ADM_TYPE, date_admit, multi_obs, date_discharge, 
                       length_stay, AGE, age_cat, sex_num, race_nhw)
         # replicate covariates length of counterfactual dates
         cov_df <- do.call("bind_rows", replicate(length(date), covariate, 
                                                  simplify = F))
        
         # bind unique id and date of the year with covariates
         id_date <- data_frame(date) %>% bind_cols(cov_df)
         # iteration which binds rows of unique ids
         id_date_df <- bind_rows(id_date_df, id_date)
                                    } # end inner loop

# join with outcome
outcome_casecross <- id_date_df %>% 
  mutate(date_admit = as.Date(date_admit, '%Y-%m-%d')) %>%
  mutate(outcome = ifelse(date_admit == date, 1, 0)) %>%
  # filter to July 1st to Oct 31st
  filter(date >= "2012-07-1" & date <= "2012-10-31") %>% 
  # join with zip-level pm estimates 
  left_join(zip_smoke_w_lag, by = c("date", "ZIPCODE")) %>%
  # join with county-level pm estimates
  left_join(county_smoke_w_lag, by = c("date", "county")) %>% 
  # create variables
  mutate(day = as.factor(weekdays(date)),
    day_admit = as.factor(weekdays(date_admit)),
    month_smk = month(date),
    month_admit = month(date_admit),
    los = as.numeric(date_discharge - date_admit), 
    season_admit = ifelse(date_admit >= "2012-06-22" &  date_admit <= "2012-09-22",
                          "summer",
                   ifelse(date_admit >= "2012-09-23" & date_admit <= "2012-12-21",
                          "fall", "other")),
    season_smk = ifelse(date >= "2012-06-22" &  date <= "2012-09-22", "summer",
                   ifelse(date >= "2012-09-23" & date <= "2012-12-21", "fall",
                          "other"))) %>%
  arrange(PATIENTID, date) # order by id and date

# checks
#glimpse(outcome_casecross)
#which(colnames(outcome_casecross)=='geo_wt_pm_county')
#check <- outcome_casecross[, c(1:16, 20, 87, 151:157)]

# Create a permanent case-cross over dataset
file_name <- paste(j, 'jul_to_oct_time_strat_casecross.csv', sep = '_')

# write permanent dataset
write_csv(outcome_casecross, file_name)

      } # End of the overall loop

# sweet this works

total_time <- Sys.time() - start
total_time


