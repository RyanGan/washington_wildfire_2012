#-------------------------------------------------------------------------------
#     Title: Import and basic data management of confidential CHARS 2015 dataset
#     Author: Ryan Gan                                                                   
#     Date Created: 12/28/2016                                               
#-------------------------------------------------------------------------------

# Important note to self: CHARS confidential data can only be used on secured
# servers and should be locked up. Once all data mangement is done and 
# case-crossover data sets are created, I'll delete CHARS conf data off this
# computer and keep it only on the secured drive

# I split this chunk of code off as the base data management of CHARS data
# Coding for outcomes is in a seperate code chunk.

# Notes: I didn't categorize or clean race variable since I haven't used it

# libraries --------------------------------------------------------------------
library(tidyverse)
# load haven to read SAS files
library(haven)

# importing and binding datasets -----------------------------------------------
getwd()
list.files("./data")
# chars 2015 inpatient file; n obs = 648004
chars_in_2015 <- read_sas("./data/chr2015inpat.sas7bdat") 
# chars 2015 outpatient files; n obs = 108837
chars_out_2015 <- read_sas("./data/chr2015obs.sas7bdat")
# revisit file
revisit_id <- read_sas("./data/confrevisit2011_2015.sas7bdat")

# checking on what is in each dataframe
glimpse(chars_in_2015)
glimpse(chars_out_2015)
glimpse(revisit_id)

# most variables are the same and I can bind/append two datasets together
chars_2015_in_out <- bind_rows(chars_in_2015, chars_out_2015)

# bind in patient id variable
chars_2015_conf_df <- revisit_id %>% # merge in patient id variable
  left_join(chars_2015_in_out, by = c('SEQ_NO_ENC', 'STAYTYPE')) %>% 
  # filter/remove observations that don't have a diagnosis
  filter(!is.na(DIAG1)) %>% 
  # some information on date admit, age, etc are contained in different 
  # variables for the outpatient/observation visits
  mutate(age_impute = ifelse(is.na(AGE), AGE_O, AGE),
        weekend_impute = ifelse(is.na(AWEEKEND), AWEEKEND_O, AWEEKEND),
        admit_date_impute = as.Date(ifelse(is.na(ADM_DATE), 
                            as.character(ADMF_DATE), # needs to be char 
                            as.character(ADM_DATE)), # needs to be char
                            format = "%Y-%m-%d"), # supply date formate
        lenstay_days_impute = ifelse(is.na(LENSTAYD), LENSTAYD_O, LENSTAYD),
        # make age category
        age_cat = ifelse(age_impute < 15, 0,
                  ifelse(age_impute >= 15 & age_impute < 65, 1,
                  ifelse(age_impute >= 65, 2, NA)))) %>% 
        # filter to 2015 dates only
        filter(admit_date_impute <= "2015-12-31" & 
               admit_date_impute >= "2015-01-01")


# write permanent csv ----------------------------------------------------------
write_csv(chars_2015_conf_df, "./data/chars_2012_confidential_no_outcomes.csv") 

# basic data checks based on the wragling script above ----
glimpse(chars_2015_conf_df)

glimpse(chars_2015_conf_df$age_impute)
# check dates
summary(chars_2015_conf_df$ADM_DATE)
# earliest date of admit 2013-06-18 and most recent 2016-03-04
# I also have 74,651 missing admission dates; likely contained in ADMF_DATE

# imputing for date leaves only 790 missing now
summary(chars_2015_conf_df$admit_date_impute)
# note most of my checks were performed on 2012 data; 2015 data very similar
# except ICD9 switches to ICD10 in October. Likely a bigger issue for coding 
# outcomes

# count of personal ids
pat_id_counts <- chars_2015_conf_df %>%  group_by(PATIENTID) %>% 
  summarise(count = n()) # 546,765 is the number of unique patients

summary(pat_id_counts)
hist(pat_id_counts$count)

check <- pat_id_counts %>% filter(count == 40)
check

id <- chars_2015_conf_df %>% filter(PATIENTID == 1144844)

summary(id)
