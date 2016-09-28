#-------------------------------------------------------------------------------
#     Title: Import and basic data management of confidential CHARS 2012 dataset
#     Author: Ryan Gan                                                                   
#     Date Created: 2/17/2015      Date Modified: 8/16/16                                              
#-------------------------------------------------------------------------------

# Important note to self: CHARS confidential data can only be used on secured
# servers and should be locked up. Once all data mangement is done and 
# case-crossover data sets are created, I'll delete CHARS conf data off this
# computer and keep it only on the secured drive

# I split this chunk of code off as the base data management of CHARS data
# Coding for outcomes is in a seperate code chunk.

# NOTE for 8/16/16: Merged in PATIENTID based on SEQ_NO_ENC and STAYTYPE

# libraries used ---------------------------------------------------------------
library(haven) # use haven package, more efficient for big SAS files
library(dplyr)
library(tidyr)
library(readr)

# setting working directories and importing datasets ---------------------------
# set working directory to smoke data
setwd("C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/confidential_data")
getwd()
list.files()

# import SAS files from Wash state ---------------------------------------------
# import inpatient
file_1 <- paste0("C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/",
              "confidential_data/chr2012inpat.sas7bdat")
chars_inpat_2012 <- read_sas(file_1)

# important note about SEQ_NO_ENC (encrypted)-----------------------------------
# SEQ_NO_ENC (encrypted sequence number) need to be linked with subject ID since it's encrypted

# import observation
file_2 <- paste0("C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/",
                 "confidential_data/chr2012obs.sas7bdat")
chars_out_2012 <- read_sas(file_2)
# both files are imported

# data cleaning and management -------------------------------------------------
# view data
glimpse(chars_inpat_2012)
glimpse(chars_out_2012)

# looks like all variables ar ethe same and I can bind/append two datasets together
chars_2012_in_out <- bind_rows(chars_inpat_2012, chars_out_2012)
summary(chars_2012_in_out)

# Import and merge in PATIENTID variable ---------------------------------------
conf_path <- paste0("C:/Users/RGan/Documents/CSU/Wild Fire/",
                    "Washington St CHARS Data/confidential_data/",
                    "confrevisit2011_2015.sas7bdat")
  
# read revisit patient id file
revisit_id <- read_sas(conf_path)

# need to merge on staytype and seq_no_enc
# make sure both are same format
glimpse(revisit_id) 
glimpse(chars_2012_in_out)
# seq to char
chars_2012_in_out$SEQ_NO_ENC <- as.character(chars_2012_in_out$SEQ_NO_ENC)
revisit_id$SEQ_NO_ENC <- as.character(revisit_id$SEQ_NO_ENC) 
# stay type to char
chars_2012_in_out$STAYTYPE <- as.character(chars_2012_in_out$STAYTYPE)
revisit_id$STAYTYPE <- as.character(revisit_id$STAYTYPE)

# merge by seq_no_enc
chars_2012_conf_df <- chars_2012_in_out %>% # merge in patient id variable
                      left_join(revisit_id, by = c('SEQ_NO_ENC', 'STAYTYPE'))

summary(chars_2012_conf_df) # looks okay; join with CHARS datamanage script

# count of personal ids
pat_id_counts <- chars_2012_conf_df %>%  group_by(PATIENTID) %>% 
                 summarise(count = n()) 


summary(pat_id_counts)
hist(pat_id_counts$count)

check <- pat_id_counts %>% filter(count == 33)
check

id <- chars_2012_conf_df %>% filter(PATIENTID == 5012307)

summary(id)

# note regarding AVG_LOS variable: all outpatient/observation claims will be 
# missing this variable (n=102,024), as well as some inpatient
xtabs(~STAYTYPE, data = chars_2012_conf_df)

# I want to output the zipcodes to know what to link on and to limite my shape
# file
chars_zip_2012 <- filter(chars_2012_conf_df, STATERES == 'WA') %>% 
                  group_by(ZIPCODE) %>% summarise(count = n()) 

# set permanent cvs file of 2012 chars zip
write.csv(chars_zip_2012, file = paste0("C:/Users/RGan/Documents/CSU/Wild Fire/",
                          "Washington_Smoke_Data/wash_zip_2012.csv"))

# making sure variables are in the right functional form
# STAYTYPE = 1: inpatient claims
# STAYTYPE = 2: outpatient claims
# set staytype as numeric variable
chars_2012_conf_df$STAYTYPE <- as.numeric(chars_2012_conf_df$STAYTYPE) 
# HOSPITAL as.factor (some have leading 0s and I want to preserve these)
chars_2012_conf_df$HOSPITAL <- as.factor(chars_2012_conf_df$HOSPITAL) 
# ZIPCODE and Zip +4 as factor
chars_2012_conf_df$ZIPCODE <- as.factor(chars_2012_conf_df$ZIPCODE) 
chars_2012_conf_df$ZIPPLUS4 <- as.factor(chars_2012_conf_df$ZIPPLUS4) 
# how many people lack a +4 zip?
658696/744343 # 88% of sample missing 4 dig postal code, not a helpful var
# STATERES
chars_2012_conf_df$STATERES <- as.factor(chars_2012_conf_df$STATERES)
xtabs(~STATERES, data = chars_2012_conf_df)
# COUNTYRES
chars_2012_conf_df$COUNTYRES <- as.factor(chars_2012_conf_df$COUNTYRES)
xtabs(~COUNTYRES, data= chars_2012_conf_df)
#COUNTRY
chars_2012_conf_df$COUNTRY <- as.factor(chars_2012_conf_df$COUNTRY)
xtabs(~COUNTRY, data = chars_2012_conf_df) # most US, some out of country
#6181 missing Age
# SEX
chars_2012_conf_df$SEX <- as.factor(chars_2012_conf_df)
xtabs(~SEX, data = chars_2012_conf_df)
# ADM_DATE (admission date); looks to be in appropriate date format (YYYY-MM-DD)
str(chars_2012_conf_df$ADM_DATE)
# ADM_HR and DIS_HR should be numeric
chars_2012_conf_df$ADM_HR <- as.numeric(chars_2012_conf_df$ADM_HR)
chars_2012_conf_df$DIS_HR <- as.numeric(chars_2012_conf_df$DIS_HR)
# ADM_TYPE admission type should be facor
chars_2012_conf_df$ADM_TYPE <- as.factor(chars_2012_conf_df$ADM_TYPE)
# ADM_SRC
chars_2012_conf_df$ADM_SRC <- as.factor(chars_2012_conf_df$ADM_SRC)
# STATUS
chars_2012_conf_df$STATUS <- as.factor(chars_2012_conf_df$STATUS)
# PAYER1
chars_2012_conf_df$PAYER1 <- as.factor(chars_2012_conf_df$PAYER1)
# leave DIAG (diagnoses) as character for now; makes it easier to code outcomes
# convert Present on arival POA to factor
chars_2012_conf_df$POA1 <- as.factor(chars_2012_conf_df$POA1)
chars_2012_conf_df$POA2 <- as.factor(chars_2012_conf_df$POA2)
chars_2012_conf_df$POA3 <- as.factor(chars_2012_conf_df$POA3)
chars_2012_conf_df$POA4 <- as.factor(chars_2012_conf_df$POA4)
chars_2012_conf_df$POA5 <- as.factor(chars_2012_conf_df$POA5)
chars_2012_conf_df$POA6 <- as.factor(chars_2012_conf_df$POA6)
chars_2012_conf_df$POA7 <- as.factor(chars_2012_conf_df$POA7)
chars_2012_conf_df$POA8 <- as.factor(chars_2012_conf_df$POA8)
chars_2012_conf_df$POA9 <- as.factor(chars_2012_conf_df$POA9)
# outlier as factor
chars_2012_conf_df$OUTLIER <- as.factor(chars_2012_conf_df$OUTLIER)
# exclude as factor
chars_2012_conf_df$EXCLUDE <- as.factor(chars_2012_conf_df$EXCLUDE)
# DIS_DATE_PARTIAL
summary(as.factor(chars_2012_conf_df$DIS_DATE_PARTIAL))
# 563 missing discharge dates. these same 563 missing discharge dates have partial 
# discharge dates
check <- filter(chars_2012_conf_df, is.na(DIS_DATE)) # check missing dates
xtabs(~ DIS_DATE_PARTIAL, data = check)
# will leave discharge date partial as character now

# leave ECODE variables as character for now too as it's easier to id outcomes
chars_2012_conf_df$SEQ_NO_ENC <- as.factor((chars_2012_conf_df$SEQ_NO_ENC))
# NOTE about variable SEQ_NO_ENC -----------------------------------------------
# there are 2 seq no enc, when there should not be, must be based on in/out pat
summary(as.factor(chars_2012_conf_df$SEQ_NO_ENC))

# convert sex to factor
chars_2012_conf_df$SEX <- as.factor(chars_2012_conf_df$SEX)
# create numeric sex variable, F=1, M=0
chars_2012_conf_df <- mutate(chars_2012_conf_df, sex_num = ifelse(SEX== 'F',1,0))

xtabs(~ SEX + sex_num, chars_2012_conf_df)
# ------------------------------------------------------------------------------
# Race Variable
# coding race variable
xtabs(~chars_2012_conf_df$RACE_WHT)
# create a categorical race/ethnicity variable
# remove previous race_cat var
chars_2012_conf_df$race_cat <- NULL

# coding R (refused to answer) and U (unknown) as missing
chars_2012_conf_df$race_cat[chars_2012_conf_df$RACE_WHT == 'U' | 
                              chars_2012_conf_df$RACE_WHT == 'R' |
                              # 99 as missing for now so I can view in X tabs
                            chars_2012_conf_df$RACE_WHT == ' '] <- 99 
# check to make sure they are missing for all races
print(xtabs(~ chars_2012_conf_df$race_cat + chars_2012_conf_df$RACE_WHT)) 
print(xtabs(~ chars_2012_conf_df$race_cat + chars_2012_conf_df$RACE_BLK))
print(xtabs(~ chars_2012_conf_df$race_cat + chars_2012_conf_df$RACE_AMI))
print(xtabs(~ chars_2012_conf_df$race_cat + chars_2012_conf_df$RACE_ASI))
print(xtabs(~ chars_2012_conf_df$race_cat + chars_2012_conf_df$RACE_HAW))
# it looks coding for the missing indicators in RACE_WHT gets the rest of the 
# reported race variables too not HISPANICE ethnicity though. 

# coding race non-hispanic white as ref
chars_2012_conf_df$race_cat[chars_2012_conf_df$RACE_WHT == 'Y' &
                           chars_2012_conf_df$HISPANIC == 'N' & 
                           chars_2012_conf_df$RACE_BLK == 'N' &
                           chars_2012_conf_df$RACE_AMI == 'N' & 
                           chars_2012_conf_df$RACE_ASI == 'N' &
                           chars_2012_conf_df$RACE_HAW == 'N'] <- NA 

# non-hispanic white coding
chars_2012_conf_df$race_cat[chars_2012_conf_df$RACE_WHT == 'Y' & 
                           chars_2012_conf_df$HISPANIC == 'N' & # might need to consider != 'Y' instead of 'N'
                           chars_2012_conf_df$RACE_BLK == 'N' &
                           chars_2012_conf_df$RACE_AMI == 'N' & 
                           chars_2012_conf_df$RACE_ASI == 'N' &
                           chars_2012_conf_df$RACE_HAW == 'N'] <- 0 # non-hisp white as ref

summary(as.factor(chars_2012_conf_df$race_cat))
# Black 
chars_2012_conf_df$race_cat[chars_2012_conf_df$RACE_WHT == 'N' & 
                           chars_2012_conf_df$HISPANIC == 'N' &
                           chars_2012_conf_df$RACE_BLK == 'Y' &
                           chars_2012_conf_df$RACE_AMI == 'N' & 
                           chars_2012_conf_df$RACE_ASI == 'N' &
                           chars_2012_conf_df$RACE_HAW == 'N'] <- 1 # black

print(xtabs(~ chars_2012_conf_df$HISPANIC + chars_2012_conf_df$RACE_WHT)) 
# some report multiple races. do I code as other? 

# hispanic ethnicity
chars_2012_conf_df$race_cat[chars_2012_conf_df$HISPANIC == 'Y'] <- 2 # hispanic 
# I think if they report Y to hispanic ethnicity, I will classify as such, regardless of race

# what about hispanic white? need to see how many folks are not assigned a race category
print(xtabs(~ chars_2012_conf_df$HISPANIC + chars_2012_conf_df$RACE_WHT))

# some overlap of people both reporting hispanic and white
chars_2012_conf_df$race_cat[chars_2012_conf_df$RACE_WHT == 'N' & 
                           chars_2012_conf_df$HISPANIC == 'N' &
                           chars_2012_conf_df$RACE_BLK == 'N' &
                           chars_2012_conf_df$RACE_AMI == 'Y' | 
                           chars_2012_conf_df$RACE_ASI == 'Y' |
                           chars_2012_conf_df$RACE_HAW == 'Y'] <- 3 # american indian
# asian, HAW other
# check race category
print(xtabs(~ chars_2012_conf_df$race_cat + chars_2012_conf_df$RACE_BLK))

# make a race binary, nhw or other

chars_2012_conf_df$race_nhw <- ifelse(chars_2012_conf_df$RACE_WHT == 'Y' &
                                     chars_2012_conf_df$HISPANIC == 'N', 1, 0)

# modify race_nhw to have missing values
chars_2012_conf_df$race_nhw[chars_2012_conf_df$RACE_WHT == 'U' | 
                           chars_2012_conf_df$RACE_WHT == 'R' |
                           chars_2012_conf_df$RACE_WHT == ' '] <- NA

print(xtabs(~ chars_2012_conf_df$race_nhw + chars_2012_conf_df$race_cat))
print(xtabs(~ chars_2012_conf_df$race_nhw + chars_2012_conf_df$RACE_WHT +
              chars_2012_conf_df$HISPANIC)) # var seems okay

# variables need to be either numeric or factor to analyze


# check variables via contengency tables (particularly race)
print(xtabs(~ chars_2012_conf_df$RACE_WHT + chars_2012_conf_df$race_cat))
print(xtabs(~ chars_2012_conf_df$HISPANIC + chars_2012_conf_df$race_cat))
summary(chars_2012_conf_df$race_cat)
# set 99 race to missing
# modify race_nhw to have missing values
chars_2012_conf_df$race_cat[chars_2012_conf_df$race_cat == 99] <- NA
summary(chars_2012_conf_df$race_cat)

# end coding for race variable -------------------------------------------------

# check/fix missing data when able ---------------------------------------------
# some missingness in data might be due to appending
check <- filter(chars_2012_conf_df, is.na(ADM_DATE)) # check missing dates
xtabs(~ ADMF_DATE, data = check)
# same could be true for missing age
check <- filter(chars_2012_conf_df, is.na(AGE))
xtabs(~ AGE_O, data = check)

# check age (n missing = 6181)
xtabs(~is.na(AGE) + AGE_O, data= chars_out_2012)
summary(is.na(chars_out_2012$ADM_DATE))
head(chars_inpat_2012)
# likely the same 6181 people missing values for AGE, AWEEKEND, ADM_DATE
# fix missing age by filling in missing values with values from AGE_O
# don't overwrite original variable, create imputed
# convert ADM_DATE to character first 
chars_2012_conf_df$ADM_DATE <- as.character(chars_2012_conf_df$ADM_DATE)
chars_2012_conf_df$ADMF_DATE <- as.character(chars_2012_conf_df$ADMF_DATE)
glimpse(chars_2012_conf_df)

chars_2012_conf_df <- mutate(chars_2012_conf_df , 
                age_impute = ifelse(is.na(AGE), AGE_O, AGE),
                weekend_impute = ifelse(is.na(AWEEKEND), AWEEKEND_O, AWEEKEND),
                admit_date_impute = ifelse(is.na(ADM_DATE), ADMF_DATE, ADM_DATE),
            lenstay_days_impute = ifelse(is.na(LENSTAYD), LENSTAYD_O, LENSTAYD))
# NOTE 5/11/16 impute length of stay in days

# check length of stay
los_check <- filter(chars_2012_conf_df, !is.na(LENSTAYD_O))

t.test(los_check$LENSTAYD_O, los_check$LENSTAYD)
summary(as.factor(chars_2012_conf_df$DIS_DATE))

# should be no more missing values for AGE, AWEEKEND, and ADM_DATE
summary(chars_2012_conf_df)
# convert admit dates back to date format
chars_2012_conf_df$ADM_DATE <- as.Date(chars_2012_conf_df$ADM_DATE, format = "%Y-%m-%d")
chars_2012_conf_df$ADMF_DATE <- as.Date(chars_2012_conf_df$ADMF_DATE, "%Y-%m-%d")
chars_2012_conf_df$admit_date_impute <- as.Date(chars_2012_conf_df$admit_date_impute, "%Y-%m-%d")

xtabs(~is.na(ADM_DATE) + is.na(ADMF_DATE), data = chars_2012_conf_df)

# Age Categorical Variable -----------------------------------------------------
# Creating Age Category variable (<15 = 0, 15 to 64 = 1, 65+ = 2)
summary(chars_2012_conf_df$age_impute)
hist(chars_2012_conf_df$age_impute)

chars_2012_conf_df <- mutate(chars_2012_conf_df, 
                             age_cat = ifelse(age_impute < 15, 0,
                                       ifelse(age_impute >= 15 & age_impute < 65, 1,
                                       ifelse(age_impute >= 65, 2, NA))))
xtabs(~ age_cat + age_impute, chars_2012_conf_df)
summary(chars_2012_conf_df)


# Creating a Permanent DataFrame -----------------------------------------------
# write a permanent chars confidential dataset

path <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/',
               'confidential_data/chars_2012_confidential_no_outcomes.csv')

write_csv(chars_2012_conf_df, path) 

