# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Title: Join of categorical smoke data and CHARS data for 2012 
#        and data management/ dataframe creation
# Author: Ryan Gan
# Date Created: 5/20/2016               Date: Modified: 5/20/2016
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# load libraries
library(dplyr) # data manipulation package (might use plyr instead)
library(tidyr) # data manipulation package (might not use)
library(readr) # reads csv
library(lubridate) # working with date

# Read in smoke data created in loop -------------------------------------------
read_path <- paste0('C:/Users/RGan/Google Drive/CSU/wild_fire/washington/',
                     'smoke_data/created_pm_estimates/hms_smoke_2012.csv')

smoke <- read.csv(read_path) # read_r was giving me parsing failures, using 
                             # read.csv function instead
summary(smoke)
check_rows <- smoke[39278:39528, ] # rows giving me problems with read_csv

smoke$date_exposure <- as.Date(smoke$date_exposure, format = "%Y-%m-%d") 

glimpse(smoke)

# create a variable that takes the name of the smoke column with maximum value 
smoke <- smoke %>% 
         mutate(smoke_exposure = ifelse(smk_binary_perc > 0, 1, 0),
                smoke_var = colnames(smoke[, 4:7])[
                max.col(smoke[,4:7],ties.method="first")],
                date = date_exposure)

# convert character vars to numeric
smoke$smoke_cat[smoke$smoke_var == 'smk_no_perc'] <- 0
smoke$smoke_cat[smoke$smoke_var == 'smk_low_perc'] <- 1
smoke$smoke_cat[smoke$smoke_var == 'smk_mid_perc'] <- 2
smoke$smoke_cat[smoke$smoke_var == 'smk_high_perc'] <- 3

# check
xtabs(~ smoke_var + smoke_cat, smoke)
xtabs(~ smoke_exposure + smoke_cat, smoke) # smk_cat and smk_exp don't perfectly
# overlap. This is probalby because some zipcodes mostly no impacted by smoke
# get coded as smoke_cat 0

# Infile the Permanent Cleaned CHARS 2012 DataFrame ----------------------------
read <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/',
               'confidential_data/chars_2012_confidential.csv')
# read in chars cleaned data
chars_2012_conf_df <- read_csv(read)
# remove the row column (select doesn't work with raster package loaded)
chars_2012_conf_df[ ,1] <- NULL 

glimpse(chars_2012_conf_df)

# Merge two datasets for smoke exposure for each record in CHARS --------------- 
chars_2012_smk <- chars_2012_conf_df %>% 
                  # filter to just 2012
                  filter(admit_date_impute >= '2012-01-01' & 
                         admit_date_impute <= '2012-12-31') %>%
                  mutate(date_exposure = admit_date_impute) %>% # make date var to merge
                  right_join(smoke, by = c('date_exposure', 'ZIPCODE')) %>% 
                  # removing missing var rows
                  filter(!is.na(smk_binary_perc) & !is.na(asthma1))

summary(chars_2012_smk)

# creation of asthma primary dx case-crossover dataframe
xtabs(~smoke_cat + smoke_exposure, chars_2012_smk)
xtabs(~ asthma1, chars_2012_smk)
xtabs(~ pneum_bronch1, chars_2012_smk) # 82k events; slow loop; estimate 16h to run
xtabs(~ copd_ex1, chars_2012_smk)
xtabs(~ arrhythmia1, chars_2012_smk)
xtabs(~ cereb_vas1, chars_2012_smk) # probably a couple hours
xtabs(~ ihd1, chars_2012_smk)
xtabs(~ hf1, chars_2012_smk)

summary(glm(asthma1 ~ smoke_exposure + AGE + SEX, family = 'poisson', data = chars_2012_smk))
summary(glm(copd_ex1 ~ smoke_exposure + AGE + SEX, family = 'poisson', data = chars_2012_smk))
summary(glm(mi1 ~ smoke_exposure + AGE + SEX, family = 'poisson', data = chars_2012_smk))
summary(glm(resp1 ~ smoke_exposure + AGE + SEX, family = 'poisson', data = chars_2012_smk))
summary(glm(ihd1 ~ smoke_exposure + AGE + SEX, family = 'poisson', data = chars_2012_smk))


# Case-crossover datasets ------------------------------------------------------

# set path to write permanent files
wd_path <- paste0('C:/Users/RGan/Google Drive/CSU/wildfire',
                  '/washington/case_crossover_dataframes')

setwd(wd_path)
getwd()

var_list <- c('asthma1', 'copd_ex1', 'mi1', 'resp1', 
              'pneum_bronch1', 'arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1')
# remove non-trauma, resp1 and pneum (way too big for this loop)
# var_list <- c('arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1')
# still to large to efficiently make datasets on this keyboard. Need to refine.

start <- Sys.time()
for(j in var_list){ # begin first loop of variable names (outcomes)

# Case-Crossover loop ----------------------------------------------------------
outcome_col <- which(colnames(chars_2012_conf_df) == j) # use to keep outcome var

outcome_id <- chars_2012_conf_df %>%
              filter(chars_2012_conf_df[[j]] == 1) %>% # jth outcome
              filter(admit_date_impute >= '2012-01-01' & 
                     admit_date_impute <= '2012-12-31') %>% 
              filter(STATERES == 'WA') %>% # limit to washington 
              arrange(admit_date_impute) %>%
              mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
              select(id, (outcome_col), # keep in bracket for outcome var num
                     admit_date_impute, DIS_DATE, 
                     LENSTAYD, ZIPCODE, AGE, SEX, sex_num, race_nhw, age_cat) %>%
              mutate(date_admit = admit_date_impute,
                     date_discharge = DIS_DATE,
                     length_stay = LENSTAYD) %>%
              select(-admit_date_impute, -DIS_DATE, -LENSTAYD)

outcome_col2 <- which(colnames(outcome_id) == j) # use to keep outcome var

# create dataset to populate
id_date_df <- data_frame(id = NA, ZIPCODE = NA, AGE = NA, 
                         age_cat = NA, sex_num = NA, race_nhw = NA, date_admit = NA, 
                         date_discharge = NA, length_stay = NA, date = NA)

# begin second loop to create counterfactual observations for each case subject
        for (i in 1:nrow(outcome_id)){
         # code dates for each id up to two month before and one month after the event
         date <- seq(as.Date(outcome_id[[i, 9]] - 56), 
                     as.Date(outcome_id[[i, 9]] + 56), by = '1 week')

         # covariates to preserve
         covariate <- filter(outcome_id, id == i) %>% 
                      select(id, (outcome_col2), ZIPCODE, date_admit, date_discharge, 
                             length_stay, AGE, age_cat, sex_num, race_nhw)
         # replicate covariates length of counterfactual dates
         cov_df <- do.call("bind_rows", replicate(length(date), covariate, simplify = F))
        
         # bind unique id and date of the year with covariates
         id_date <- data_frame(date) %>% bind_cols(cov_df)
         # iteration which binds rows of unique ids
         id_date_df <- bind_rows(id_date_df, id_date)
                                    } # end inner lop

# join with outcome
outcome_casecross <- filter(id_date_df, !is.na(id)) %>% 
                     mutate(date_admit = as.Date(date_admit, '%Y-%m-%d')) %>%
                     mutate(outcome = ifelse(date_admit == date, 1, 0)) %>%
                     left_join(smoke, by = c("date", "ZIPCODE")) %>%
                    # create variables
                     mutate(day = as.factor(weekdays(date)),
                            day_admit = as.factor(weekdays(date_admit)),
                            month_smk = month(date),
                            month_admit = month(date_admit),
                            los = as.numeric(date_discharge - date_admit)) %>%
                     arrange(id, date) # order by id and date

# Create a permanent case-cross over dataset
file_name <- paste(j, 'categorical_smk_casecross.csv', sep = '_')

# write permanent dataset
write_csv(outcome_casecross, file_name)


      } # End of the overall loop

# sweet this works

total_time <- Sys.time() - start
total_time



