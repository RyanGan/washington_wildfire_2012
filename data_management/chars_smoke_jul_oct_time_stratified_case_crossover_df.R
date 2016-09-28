# ------------------------------------------------------------------------------
# Title: Join of smoke data and CHARS data for 2012 
#        and data management/ dataframe creation
# Author: Ryan Gan
# Date Created: 6/29/2016   Date Modified: 8/15/16               
# ------------------------------------------------------------------------------

# Notes 8/16/16: I updated smoke data with population weighted averages for zip
# This code also assumed only one visit, so I remove the people with multiple
# admissions for the outcome

# load libraries
library(dplyr) # data manipulation package (might use plyr instead)
library(tidyr) # data manipulation package (might not use)
library(readr) # reads csv
library(lubridate) # working with date


# Read in smoke data created in loop -------------------------------------------
getwd()
read_path <- paste0('C:/Users/RGan/Google Drive/CSU/wildfire/washington/',
                    'smoke/zip_population_weighted_pm/',
                    'zip_pm_to_merge_with_chars.csv')

smoke <- read_csv(read_path) 

summary(smoke)
head(smoke)
# create lag variables that take smoke values from n previous days
smoke_w_lag <- smoke %>% arrange(ZIPCODE, date) %>%
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
    wrf_temp_lag5 = lag(wrf_temp, 5))

# check 
summary(smoke_w_lag) # looks good

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


# Case-crossover datasets ------------------------------------------------------

# set path to write permanent files
wd_path <- paste0('./washington/case_crossover_dataframes')

setwd(wd_path)
getwd()
glimpse(chars_2012_conf_df)

var_list <- c('resp1', 'asthma1', 'copd_ex1', 'copd1', 'pneum1', 'acute_bronch1',
              'cvd1', 'mi1', 'arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1', 'ra1',
              'broken_arm1')
# remove non-trauma, resp1 and pneum (way too big for this loop)
# var_list <- c('arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1')
# still to large to efficiently make datasets on this keyboard. Need to refine.

start <- Sys.time()
for(j in var_list){ # begin first loop of variable names (outcomes)

# Case-Crossover loop ----------------------------------------------------------

outcome_col <- which(colnames(chars_2012_conf_df) == j) # use to keep outcome var

outcome_id <- chars_2012_conf_df %>%
              filter(chars_2012_conf_df[[j]] == 1) %>% # jth outcome
              filter(admit_date_impute >= '2012-07-01' & 
                     admit_date_impute <= '2012-10-31') %>% 
              filter(STATERES == 'WA') %>% # limit to washington 
              arrange(admit_date_impute) %>%
              # commenting out id as I have patientid now  
              # mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
              select(PATIENTID, (outcome_col), # keep in bracket for outcome var num
                    admit_date_impute, DIS_DATE, ADM_TYPE,
                    LENSTAYD, ZIPCODE, AGE, SEX, sex_num, race_nhw, age_cat) %>%
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

# 1765 - 1638
# lost 127 observations with multiple visits

outcome_col2 <- which(colnames(single_visits) == j) # use to keep outcome var

# create dataset to populate
id_date_df <- data_frame()

# begin second loop to create counterfactual observations for each case subject
        for (i in 1:nrow(single_visits)){
         # code dates for each id up to two months before and after the event
         date <- seq(as.Date(single_visits[[i, 10]] - 56), 
                     # consider every day
                     as.Date(single_visits[[i, 10]] + 56), by = '1 week') 

         # covariates to preserve
         covariate <- single_visits[i, ]%>% 
                select(PATIENTID, (outcome_col2), ZIPCODE, ADM_TYPE, date_admit, 
                       multi_obs, date_discharge, length_stay, AGE, age_cat, 
                       sex_num, race_nhw)
         # replicate covariates length of counterfactual dates
         cov_df <- do.call("bind_rows", replicate(length(date), covariate, 
                                                  simplify = F))
        
         # bind unique id and date of the year with covariates
         id_date <- data_frame(date) %>% bind_cols(cov_df)
         # iteration which binds rows of unique ids
         id_date_df <- bind_rows(id_date_df, id_date)
                                    } # end inner lop
  
# join with outcome
outcome_casecross <- id_date_df %>% 
                     mutate(date_admit = as.Date(date_admit, '%Y-%m-%d')) %>%
                     mutate(outcome = ifelse(date_admit == date, 1, 0)) %>%
                     left_join(smoke_w_lag, by = c("date", "ZIPCODE")) %>%
                    # create variables
                     mutate(day = as.factor(weekdays(date)),
                            day_admit = as.factor(weekdays(date_admit)),
                            month_smk = month(date),
                            month_admit = month(date_admit),
                            los = as.numeric(date_discharge - date_admit)) %>%
                     arrange(PATIENTID, date) # order by id and date

# Create a permanent case-cross over dataset
file_name <- paste(j, 'jul_to_oct_casecross.csv', sep = '_')

# write permanent dataset
write_csv(outcome_casecross, file_name)


      } # End of the overall loop

# sweet this works

total_time <- Sys.time() - start
total_time


