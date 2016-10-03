# ------------------------------------------------------------------------------
# Title: Join of smoke data and CHARS data for 2012 
#        and data management/ dataframe creation
# Author: Ryan Gan
# Date Created: 6/29/2016   Date Modified: 8/15/16               
# ------------------------------------------------------------------------------

# Notes 8/16/16: I updated smoke data with population weighted averages for zip
# To do: I need to join the unique identifier to CHARS data

# Notes 10/3/16: I'm retaining the county assignment of the observation so I
# can join the county population weighted averages as well for comparison
# and to analyze morbidity similar to Rish. This code still needs work.

# Libraries ----
# load libraries
library(tidyverse)
library(lubridate) # working with date


# Read in smoke data created in loop -------------------------------------------
getwd()
# read in zipcode level populatoin-weighted pm
read_path <- paste0('./smoke/pm_data/zip_pm_to_merge_with_chars.csv')

zip_smoke <- read_csv(read_path) 

# read in county level population-weighted pm
read_path2 <- paste0('./smoke/pm_data/wa_county_pop_wt_pm.csv')

county_smoke <- read_csv(read_path2)
# descriptives of the two smoke datasets
summary(county_smoke)
summary(zip_smoke)

# Zipcode PM2.5 estimates
# create lag variables that take smoke values from n previous days for zipcodes
zip_smoke_w_lag <- zip_smoke %>% arrange(ZIPCODE, date) %>%
  # wrf
  mutate(wrf_pm_lag1 = lag(wrf_pm, 1), 
    wrf_pm_lag2 = lag(wrf_pm, 2),
    wrf_pm_lag3 = lag(wrf_pm, 3),
    wrf_pm_lag4 = lag(wrf_pm, 4),
    wrf_pm_lag5 = lag(wrf_pm, 5),
    # wrf no fire lag
    wrf_nf_pm_lag1 = lag(wrf_nf_pm, 1),
    wrf_nf_pm_lag2 = lag(wrf_nf_pm, 2),
    wrf_nf_pm_lag3 = lag(wrf_nf_pm, 3),
    wrf_nf_pm_lag4 = lag(wrf_nf_pm, 4),
    wrf_nf_pm_lag5 = lag(wrf_nf_pm, 5),
    # wrf_smk_pm
    wrf_smk_pm_lag1 = lag(wrf_smk_pm, 1),
    wrf_smk_pm_lag2 = lag(wrf_smk_pm, 2),
    wrf_smk_pm_lag3 = lag(wrf_smk_pm, 3),
    wrf_smk_pm_lag4 = lag(wrf_smk_pm, 4),
    wrf_smk_pm_lag5 = lag(wrf_smk_pm, 5),
    # geo weighted pm
    geo_wt_pm_lag1 = lag(geo_wt_pm, 1),
    geo_wt_pm_lag2 = lag(geo_wt_pm, 2),
    geo_wt_pm_lag3 = lag(geo_wt_pm, 3),
    geo_wt_pm_lag4 = lag(geo_wt_pm, 4),
    geo_wt_pm_lag5 = lag(geo_wt_pm, 5),
    # global reg pm
    global_reg_pm_lag1 = lag(global_reg_pm, 1),
    global_reg_pm_lag2 = lag(global_reg_pm, 2),
    global_reg_pm_lag3 = lag(global_reg_pm, 3),
    global_reg_pm_lag4 = lag(global_reg_pm, 4),
    global_reg_pm_lag5 = lag(global_reg_pm, 5),
    # krig pm
    krig_pm_lag1 = lag(krig_pm, 1),
    krig_pm_lag2 = lag(krig_pm, 2),
    krig_pm_lag3 = lag(krig_pm, 3),
    krig_pm_lag4 = lag(krig_pm, 4),
    krig_pm_lag5 = lag(krig_pm, 5),   
    # background pm
    background_pm_lag1 = lag(background_pm, 1),
    background_pm_lag2 = lag(background_pm, 2),
    background_pm_lag3 = lag(background_pm, 3),
    background_pm_lag4 = lag(background_pm, 4),
    background_pm_lag5 = lag(background_pm, 5),   
    # geo_smk_pm 
    geo_smk_pm_lag1 = lag(geo_smk_pm, 1),
    geo_smk_pm_lag2 = lag(geo_smk_pm, 2),
    geo_smk_pm_lag3 = lag(geo_smk_pm, 3),
    geo_smk_pm_lag4 = lag(geo_smk_pm, 4),
    geo_smk_pm_lag5 = lag(geo_smk_pm, 5),
    # global smk pm
    global_smk_pm_lag1 = lag(global_smk_pm, 1),
    global_smk_pm_lag2 = lag(global_smk_pm, 2),
    global_smk_pm_lag3 = lag(global_smk_pm, 3),
    global_smk_pm_lag4 = lag(global_smk_pm, 4),
    global_smk_pm_lag5 = lag(global_smk_pm, 5),
    # krig smk pm
    krig_smk_pm_lag1 = lag(krig_smk_pm, 1),
    krig_smk_pm_lag2 = lag(krig_smk_pm, 2),
    krig_smk_pm_lag3 = lag(krig_smk_pm, 3),
    krig_smk_pm_lag4 = lag(krig_smk_pm, 4),
    krig_smk_pm_lag5 = lag(krig_smk_pm, 5),
    # temp
    wrf_temp_lag1 = lag(wrf_temp, 1),
    wrf_temp_lag2 = lag(wrf_temp, 2),
    wrf_temp_lag3 = lag(wrf_temp, 3),
    wrf_temp_lag4 = lag(wrf_temp, 4),
    wrf_temp_lag5 = lag(wrf_temp, 5)) %>% 
  # attach a zip indicator for each smoke variable
  setNames(paste(colnames(.), "zip", sep="_")) %>% 
  # remove the '_zip' from the zipcode and date variable 
  rename(ZIPCODE = ZIPCODE_zip, date = date_zip)

# check 
summary(zip_smoke_w_lag) # looks good

# County PM2.5 estimates
# create lag variables that take smoke values from n previous days for county
county_smoke_w_lag <- county_smoke %>% arrange(county, date) %>%
  # wrf
  mutate(wrf_pm_lag1 = lag(wrf_pm, 1), 
    wrf_pm_lag2 = lag(wrf_pm, 2),
    wrf_pm_lag3 = lag(wrf_pm, 3),
    wrf_pm_lag4 = lag(wrf_pm, 4),
    wrf_pm_lag5 = lag(wrf_pm, 5),
    # wrf no fire lag
    wrf_nf_pm_lag1 = lag(wrf_nf_pm, 1),
    wrf_nf_pm_lag2 = lag(wrf_nf_pm, 2),
    wrf_nf_pm_lag3 = lag(wrf_nf_pm, 3),
    wrf_nf_pm_lag4 = lag(wrf_nf_pm, 4),
    wrf_nf_pm_lag5 = lag(wrf_nf_pm, 5),
    # wrf_smk_pm
    wrf_smk_pm_lag1 = lag(wrf_smk_pm, 1),
    wrf_smk_pm_lag2 = lag(wrf_smk_pm, 2),
    wrf_smk_pm_lag3 = lag(wrf_smk_pm, 3),
    wrf_smk_pm_lag4 = lag(wrf_smk_pm, 4),
    wrf_smk_pm_lag5 = lag(wrf_smk_pm, 5),
    # geo weighted pm
    geo_wt_pm_lag1 = lag(geo_wt_pm, 1),
    geo_wt_pm_lag2 = lag(geo_wt_pm, 2),
    geo_wt_pm_lag3 = lag(geo_wt_pm, 3),
    geo_wt_pm_lag4 = lag(geo_wt_pm, 4),
    geo_wt_pm_lag5 = lag(geo_wt_pm, 5),
    # global reg pm
    global_reg_pm_lag1 = lag(global_reg_pm, 1),
    global_reg_pm_lag2 = lag(global_reg_pm, 2),
    global_reg_pm_lag3 = lag(global_reg_pm, 3),
    global_reg_pm_lag4 = lag(global_reg_pm, 4),
    global_reg_pm_lag5 = lag(global_reg_pm, 5),
    # krig pm
    krig_pm_lag1 = lag(krig_pm, 1),
    krig_pm_lag2 = lag(krig_pm, 2),
    krig_pm_lag3 = lag(krig_pm, 3),
    krig_pm_lag4 = lag(krig_pm, 4),
    krig_pm_lag5 = lag(krig_pm, 5),   
    # background pm
    background_pm_lag1 = lag(background_pm, 1),
    background_pm_lag2 = lag(background_pm, 2),
    background_pm_lag3 = lag(background_pm, 3),
    background_pm_lag4 = lag(background_pm, 4),
    background_pm_lag5 = lag(background_pm, 5),   
    # geo_smk_pm 
    geo_smk_pm_lag1 = lag(geo_smk_pm, 1),
    geo_smk_pm_lag2 = lag(geo_smk_pm, 2),
    geo_smk_pm_lag3 = lag(geo_smk_pm, 3),
    geo_smk_pm_lag4 = lag(geo_smk_pm, 4),
    geo_smk_pm_lag5 = lag(geo_smk_pm, 5),
    # global smk pm
    global_smk_pm_lag1 = lag(global_smk_pm, 1),
    global_smk_pm_lag2 = lag(global_smk_pm, 2),
    global_smk_pm_lag3 = lag(global_smk_pm, 3),
    global_smk_pm_lag4 = lag(global_smk_pm, 4),
    global_smk_pm_lag5 = lag(global_smk_pm, 5),
    # krig smk pm
    krig_smk_pm_lag1 = lag(krig_smk_pm, 1),
    krig_smk_pm_lag2 = lag(krig_smk_pm, 2),
    krig_smk_pm_lag3 = lag(krig_smk_pm, 3),
    krig_smk_pm_lag4 = lag(krig_smk_pm, 4),
    krig_smk_pm_lag5 = lag(krig_smk_pm, 5),
    # temp
    wrf_temp_lag1 = lag(wrf_temp, 1),
    wrf_temp_lag2 = lag(wrf_temp, 2),
    wrf_temp_lag3 = lag(wrf_temp, 3),
    wrf_temp_lag4 = lag(wrf_temp, 4),
    wrf_temp_lag5 = lag(wrf_temp, 5)) %>% 
  # attach a zip indicator for each smoke variable
  setNames(paste(colnames(.), "county", sep="_")) %>% 
  # remove the '_zip' from the zipcode and date variable 
  rename(county = county_county, date = date_county)

# check 
summary(county_smoke_w_lag) # looks good

# Infile the Permanent Cleaned CHARS 2012 DataFrame ----------------------------
# Put this file on the atmos server ASAP
read <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/',
               'confidential_data/chars_2012_confidential.csv')
# read in chars cleaned data
chars_2012_conf_df <- read_csv(read)
# check data
glimpse(chars_2012_conf_df)

# look up admit type, may want to subset to specifc admit
# ADM_TYPE variable: 1 = emergency, 2 = urgent, 3 = elective, 4 = newborn
# 5 = trauma, 9 = info not available
summary(as.factor(chars_2012_conf_df$ADM_TYPE))
# when I run analyses, I may want to limit only to emergency or urgent
# best to retan the adm_type variable 
summary(chars_2012_conf_df$SEQ_NO_ENC)
glimpse(chars_2012_conf_df$SEQ_NO_ENC)

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


# Case-crossover dataframe creation --------------------------------------------

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

  j <- "asthma1"
# Outcome-specific dataframe loop ----------------------------------------------
  # prepares the dataframe for creation of case-crossover df
outcome_col <- which(colnames(chars_2012_to_loop) == j) # use to keep outcome var

outcome_id <- chars_2012_to_loop %>%
  filter(chars_2012_to_loop[[j]] == 1) %>% # jth outcome
  filter(admit_date_impute >= '2012-07-01' & 
         admit_date_impute <= '2012-10-31') %>% 
  filter(STATERES == 'WA') %>% # limit to washington 
  arrange(PATIENTID, admit_date_impute) %>% 
  mutate(date_admit = admit_date_impute, date_discharge = DIS_DATE,
    join_date = date_admit, length_stay = LENSTAYD) %>% 
  # generate a enumerated variable to indicate number of obs by patid
  group_by(PATIENTID) %>% 
  mutate(vis_num = seq_along(PATIENTID),
    # find last visit of those with multiple visits
    last_admit_date = lag(date_admit),
    next_admit_date = lead(date_admit),
    # days between visits
    days_between_visit = date_admit - last_admit_date,
    days_between_admit = ifelse(is.na(days_between_visit), 0, days_between_visit),
    # find people that moved
    last_zip = lag(ZIPCODE, order_by = vis_num),
    moved = ifelse(vis_num == 1, 0, 
            ifelse(ZIPCODE == last_zip, 0, 1))) %>% 
  ungroup() %>% # ungroup to avoid any weird grouping problems
  # changing id to row number as I have patientid now  
  mutate(obs_num = seq(1, nrow(.), by = 1)) %>% # create observation number
  select(obs_num, PATIENTID, vis_num, (outcome_col), # keep in bracket
    STAYTYPE, date_admit, date_discharge, last_admit_date, days_between_admit,
    next_admit_date, join_date, length_stay, ADM_TYPE, ZIPCODE, COUNTYRES, county,
    moved, AGE, SEX, sex_num, race_nhw, age_cat) 

# check person with multiple visits
patid_1350539_vis <- filter(outcome_id, PATIENTID == 1350539)

# how many people with asthma have multiple visits?
outcome_count <- outcome_id %>% group_by(PATIENTID) %>%
 summarise(obs = n()) %>% mutate(multi_obs = ifelse(obs > 1, 1, 0))
 
 # xtabs(~multi_obs, outcome_count) # 100
 # 100/(1538+100) # 6% of obs

# merge an indicator of multi obs back in to outcome_id and check the patients
# with multiple visits
multiple_visits <- outcome_id %>% left_join(outcome_count, by = 'PATIENTID') 
# as I suspected, some people move
# check to make sure there is a multiple visits indicator variable
patid_1350539_vis <- filter(multiple_visits, PATIENTID == 1350539)

# output patient id of people that moved 
# (I'm going to exclude them from analysis for now)
move_id <- multiple_visits %>% filter(moved == 1) %>% select(PATIENTID, moved)  

# merge in and exclude people who move
outcome_id_all_vis <- multiple_visits %>% select(-moved) %>% # remove moved var
  left_join(move_id, by = "PATIENTID") %>% 
  filter(is.na(moved)) %>% # keep those who don't move
  select(-moved, -obs_num) # remove variables that don't have a purpose

# check patient 1350539 as they have 6 visits

# check missing age (why do I have age_cat var not missing?)
# missing_age <- outcome_id_all_vis %>% filter(is.na(AGE))
# Ahh, they are all outpatient, so I need to use age_impute. Age cat is correct

# dataframe to loop through.
# contains only first visit for each patient; assumes age, sex, race don't change.
outcome_id_for_loop <- outcome_id_all_vis %>% group_by(PATIENTID) %>% 
  filter(row_number() == 1) # vis_num == 1 would have worked too

# Seperate out variables I'd like to vary for each person, (i.e. admission date)
id_vary_vars <- outcome_id %>% 
  select(PATIENTID, join_date, date_admit, date_discharge, length_stay)

# keep outcome var location for naming later
outcome_col2 <- which(colnames(outcome_id_for_loop) == j) 

# code dates for each date during the observation period; outside the obs-sp loop
date <- seq(as.Date("2012-07-01"), as.Date("2012-10-31"), by = "1 day") 
# 123 days

# create empty dataset to populate
id_date_df <- data_frame()

# begin second loop to create counterfactual observations for each patient/subject
  for (i in 1:nrow(outcome_id_for_loop)){
   # covariates to preserve
    covariate <- outcome_id_for_loop[i, ] %>% 
      select(PATIENTID, STAYTYPE, ADM_TYPE,(outcome_col2), ZIPCODE, COUNTYRES,
             county, AGE, age_cat, SEX, sex_num, race_nhw)
   
   # replicate covariates length of counterfactual dates
    cov_df <- do.call("bind_rows", replicate(length(date), covariate, 
                                            simplify = F))
   
   # bind unique id and date of the year with covariates
    id_date <- cov_df %>% bind_cols(data_frame(date)) 
   
   # iteration which binds rows of unique ids
    id_date_df <- bind_rows(id_date_df, id_date)
    
                                    } # end inner loop

# begin third loop to retain information of patients/subjects with multi visits
for (meow in 1:nrow(outcome_id_all_vis)){
  # covariates to repeat from each observation
  meow <- 284
  covariate2 <- outcome_id[meow, ]
  if(covariate2$multi == 0){
    cov_df <- do.call("bind_rows", replicate(length(date), covariate2, 
                                            simplify = F))  
  }
}

# join with outcome
outcome_casecross <- id_date_df %>% 
  left_join(id_vary_vars, by = c("PATIENTID", "date" = "join_date")) %>% 
  mutate(outcome = ifelse(is.na(date_admit), 0, 1)) %>% 
  left_join(smoke_w_lag, by = c("ZIPCODE", "date")) %>%
  #create variables
  mutate(day = as.factor(weekdays(date)),
         day_admit = as.factor(weekdays(date_admit)),
         month_smk = month(date),
         month_admit = month(date_admit),
         los = as.numeric(date_discharge - date_admit)) %>%
  arrange(PATIENTID, date) # order by id and date

summary(as.factor(outcome_casecross$outcome))
# check multiobservation person
patid_1350539 <- filter(outcome_casecross, PATIENTID == 1350539) %>%  
  arrange(date)

# code works, but I have duplicate observations. I need to figure out how to deal
# with that

# Create a permanent case-cross over dataset
file_name <- paste(j, 'jul_to_oct_casecross.csv', sep = '_')

# write permanent dataset
write_csv(outcome_casecross, file_name)


      } # End of the overall loop

# sweet this works

total_time <- Sys.time() - start
total_time


