# ------------------------------------------------------------------------------
# Title: Analysis of Case-Cross Over Data Sets With Full Year of Smoke Data
# Author: Ryan Gan
# Date Created: 5/24/2016               Date: Modified: 5/24/2016
# ------------------------------------------------------------------------------

# Load libraries ---------------------------------------------------------------
library(readr) # for importing csv files
library(dplyr)
library(survival) # for conditional logistic regression
library(ggplot2) # graphics
library(lme4) # mixed model package
library(splines)

# Set working directory and read in files --------------------------------------
# Relative path still not working for some reason (define full path)
# pc path
wd_path <- paste0("C:/Users/RGan/Google Drive/CSU/wildfire/washington/",
                  "case_crossover_dataframes")
# mac path
# wd_path <- paste0("/Users/ryangan/Google Drive/CSU/wildfire/washington/",
#                  "case_crossover_dataframes")

setwd(wd_path)
getwd()
# Check files in the directory
list.files()

# Infile case-crossover dataframes ---------------------------------------------
# Dataframes made in 'chars_3month_binary_smoke_may2016 script
# resp exacerbations
resp_casecross <- read_csv("resp1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# asthma
asthma_casecross <- read_csv("asthma1_categorical_smk_casecross.csv", 
                   col_types = cols(smk_binary_perc = col_double(),
                   smk_no_perc = col_double(), smk_low_perc = col_double(),
                   smk_mid_perc = col_double(), 
                   smk_high_perc = col_double())) %>% # delete missing values
                   filter(!is.na(smk_binary_perc))
                     
summary(asthma_casecross)
# copd exacerbations
copd_ex_casecross <- read_csv("copd_ex1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(),
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# pneum or bronchitis
pneum_bronch_casecross <- read_csv("pneum_bronch1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# arrhythmia
arrhythmia_casecross <- read_csv("arrhythmia1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# cerebral vascular
cereb_vas_casecross <- read_csv("cereb_vas1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# heart failure
hf_casecross <- read_csv("hf1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# ischemic heart disease
ihd_casecross <- read_csv("ihd1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# myo infarc
mi_casecross <- read_csv("mi1_categorical_smk_casecross.csv", 
                     col_types = cols(smk_binary_perc = col_double(),
                     smk_no_perc = col_double(), smk_low_perc = col_double(),
                     smk_mid_perc = col_double(), 
                     smk_high_perc = col_double())) %>% # delete missing values
                     filter(!is.na(smk_binary_perc))

# Analyses ---------------------------------------------------------------------

# Any Respiratory --------------------------------------------------------------
xtabs(~ resp1 + outcome, resp_casecross) # 47223 events over the year

# dataframe of asthma counts by date
date_counts <- group_by(resp_casecross, date) %>%
               summarise(n_resp = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_resp)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure  +
                strata(id), resp_casecross)
summary(mod)

# Asthma -----------------------------------------------------------------------
xtabs(~ asthma1 + outcome, asthma_casecross) # 5145 events over the year

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + as.factor(month_smk) + 
                strata(id), asthma_casecross)
summary(mod)

# non-linear mixed model (does it produce similar results?)
# glm model
glm_mod <- glm(outcome ~ smoke_exposure + as.factor(month_smk),
               family = 'binomial'(link = 'logit'), data = asthma_casecross)

summary(glm_mod) # when adjusting for month, it produces the same estimates :)

# binomial mixed effects
m <- glmer(outcome ~ smoke_exposure + as.factor(month_smk) + (1 | id), data = asthma_casecross,
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m) # similar results again

# can I add a spline?
# dataframe of asthma counts by date
date_counts <- group_by(asthma_casecross, date) %>%
               summarise(n_asthma = sum(outcome))

# plot
ggplot(date_counts, aes(x = date, y = n_asthma)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# estimate spline curve
spl <- bs(date_counts$date,degree=3, df=5)

spline_mod <- glm(n_asthma ~ spl, date_counts, family=quasipoisson)
summary(spline_mod)
pred <- predict(spline_mod,type="response")

plot(date_counts$date, date_counts$n_asthma, ylim=c(0,30), pch=19, cex=0.2, 
     col=grey(0.6), main="Flexible cubic spline model",
     ylab="Daily number of Asthma Hospitalizations",
     xlab="Date")
lines(date_counts$date,pred,lwd=2)

# try adding spline to mixed model instead of month factor
m <- glmer(outcome ~ smoke_exposure + ns(date, 5) + (1 | id), data = asthma_casecross,
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m) # similar results again, and kind of fun. I like this model
# I could use this for county level count data too

# Asthma length of stay
# density of asthma length of stay
ggplot(asthma_casecross, aes(x = length_stay)) + geom_density() 
# account for length of stay
asthma_los <- asthma_casecross %>% 
                mutate(at_risk = ifelse(outcome == 1 |
                                 date_discharge <= date_exposure, 1, 0)) %>%
                filter(at_risk == 1)
head(asthma_los)

# accounting for length of stay helps quite a bit with the point estimate
mod <- clogit(outcome ~ smoke_exposure +  
                strata(id), asthma_los)
summary(mod)

# glm model with length of stay
glm_mod <- glm(outcome ~ smoke_exposure ,
               family = 'binomial'(link = 'logit'), data = asthma_los)
summary(glm_mod) 

# poisson
glm_mod <- glm(outcome ~ smoke_exposure ,
               family = 'poisson'(link = 'log'), data = asthma_los)
summary(glm_mod) 

# binary random effects model with a spline and length of stay
m <- glmer(outcome ~ smoke_exposure + ns(date, 5) + (1 | id), data = asthma_los,
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m) # similar results again, and kind of fun. I like this model

# COPD Exacerbation ------------------------------------------------------------
xtabs(~ copd_ex1 + outcome, copd_ex_casecross) # 7485 events over the year


# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + as.factor(month_smk) + 
                strata(id), copd_ex_casecross)
summary(mod)

# smoke category
mod <- clogit(outcome ~ as.factor(smoke_cat) + as.factor(month_smk) + 
                strata(id), copd_ex_casecross)
summary(mod)

xtabs(~ outcome + smoke_exposure, copd_ex_casecross)


# mixed model with spline
# modeling temporal trend
# dataframe of asthma counts by date
date_counts <- group_by(copd_ex_casecross, date) %>%
               summarise(n_copd_ex= sum(outcome))
# plot
ggplot(date_counts, aes(x = date, y = n_copd_ex)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# estimate spline curve
spl <- bs(date_counts$date,degree=3, df=4)

spline_mod <- glm(n_copd_ex ~ spl, date_counts, family=quasipoisson)
summary(spline_mod)
pred <- predict(spline_mod,type="response")

plot(date_counts$date, date_counts$n_copd_ex, ylim=c(0,50), pch=19, cex=0.2, 
     col=grey(0.6), main="Flexible cubic spline model",
     ylab="COPD Exacerbation Hospitalizations",
     xlab="Date")
lines(date_counts$date,pred,lwd=2)


# mixed model
m <- glmer(outcome ~ smoke_exposure + ns(date, 5) + (1 | id), data = copd_ex_casecross,
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m)


# recovery period (everyone has 1 month)
copd_ex_recovery <- mutate(copd_ex_casecross, 
                           days_diff = date_exposure - date_admit,
                       keep = ifelse(days_diff > 0 & days_diff <= 28, 0,
                              ifelse(days_diff < 0 & days_diff >= -28, 0, 1))) %>% 
                       filter(keep == 1) %>% 
                       filter(month_admit >= 3 & month_admit <= 11)

# conditionial logistic model
mod <- clogit(outcome ~ smoke_exposure + 
                strata(id), copd_ex_recovery)
summary(mod)

# accounting for recovery period via discharge date ----------------------------
# check distribution of length_stay
ggplot(copd_ex_casecross, aes(x = length_stay)) + geom_density() 
# recovery period based on length of stay
copd_ex_los <- copd_ex_casecross %>% 
                mutate(at_risk = ifelse(outcome == 1 |
                                 date_discharge <= date_exposure, 1, 0)) %>%
                filter(at_risk == 1) %>% 
                filter(month_admit >= 3 & month_admit <= 11)

date_check <- copd_ex_los %>% select(outcome, date_admit, date_discharge,
                                     date_exposure, at_risk)

head(date_check, n = 20L)
xtabs(~ outcome, copd_ex_los)

# conditionial logistic model
mod <- clogit(outcome ~ as.factor(smoke_exposure) + as.factor(month_smk) +
                strata(id), copd_ex_los)
summary(mod)

# mixed model
m <- glmer(outcome ~ smoke_exposure + ns(date, 4) + (1 | id), data = copd_ex_los,
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m)


# Pneumonia or Bronchitis
# dataframe of asthma counts by date
date_counts <- group_by(pneum_bronch_casecross, date) %>%
               summarise(n_pneum_bronch = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_pneum_bronch)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + as.factor(month_smk) +
                strata(id), pneum_bronch_casecross)
summary(mod)

# Cerebro vascular
date_counts <- group_by(cereb_vas_casecross, date) %>%
               summarise(n_cereb_vas = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_cereb_vas)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + 
                strata(id), cereb_vas_casecross)
summary(mod)

# IHD
# dataframe of asthma counts by date
date_counts <- group_by(ihd_casecross, date) %>%
               summarise(n_ihd = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_ihd)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + as.factor(month_smk) +
                strata(id), ihd_casecross)
summary(mod)


# Arrhythmia
# dataframe of asthma counts by date
date_counts <- group_by(arrhythmia_casecross, date) %>%
               summarise(n_arrhythmia = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_arrhythmia)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + 
                strata(id), arrhythmia_casecross)
summary(mod)

# Heart Failure
# dataframe of asthma counts by date
date_counts <- group_by(hf_casecross, date) %>%
               summarise(n_hf = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_hf)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + as.factor(month_smk) +
                strata(id), hf_casecross)
summary(mod)

# MI
# dataframe of asthma counts by date
date_counts <- group_by(mi_casecross, date) %>%
               summarise(n_mi = sum(outcome))

# plot of events over time
ggplot(date_counts, aes(x = date, y = n_mi)) +
       geom_point() +
       geom_smooth() +
       scale_size_area()

# estimate spline curve
spl <- bs(date_counts$date, degree=3, df=7)

spline_mod <- glm(n_mi ~ spl, date_counts, family=quasipoisson)
summary(spline_mod)
pred <- predict(spline_mod,type="response")

plot(date_counts$date, date_counts$n_mi, ylim=c(0,50), pch=19, cex=0.2, 
     col=grey(0.6), main="Flexible cubic spline model",
     ylab="MI Hospitalizations",
     xlab="Date")
lines(date_counts$date,pred,lwd=2)


# conditional logistic model
mod <- clogit(outcome ~ smoke_exposure + as.factor(month_smk) +
                strata(id), mi_casecross)
summary(mod)

# using length of stay
mi_los <- mi_casecross %>% 
          mutate(at_risk = ifelse(outcome == 1 |
                           date_discharge <= date_exposure, 1, 0)) %>%
          filter(at_risk == 1) %>% 
          filter(month_admit >= 3 & month_admit <= 11)

date_check <- mi_los %>% select(id, outcome, date_admit, date_discharge,
                                     date_exposure, los, at_risk) %>%
                         filter(los >= 7)

mod <- clogit(outcome ~ smoke_exposure + strata(id), mi_los)
summary(mod)

# mixed model
m <- glmer(outcome ~ as.factor(smoke_cat) + ns(date, 7) + (1 | id), data = mi_los,
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m)

