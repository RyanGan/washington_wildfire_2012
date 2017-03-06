# ------------------------------------------------------------------------------
# Title: Analysis of Case-Cross Over Dataframes with data from July to October
# Author: Ryan Gan
# Date Created: 6/29/17            
# ------------------------------------------------------------------------------

# Load libraries ---------------------------------------------------------------
library(readr) # for importing csv files
library(dplyr)
library(survival) # for conditional logistic regression
library(ggplot2) # graphics
library(lme4) # mixed model package
library(splines)

# Set working directory and read in files --------------------------------------
# Relative path 
wd_path <- paste0("./washington/case_crossover_dataframes")

setwd(wd_path)
getwd()
# Check files in the directory
list.files()

# Infile case-crossover dataframes ---------------------------------------------
# Dataframes made in 'chars_3month_binary_smoke_may2016 script
# resp exacerbations
resp_casecross <- read_csv("resp1_jul_to_oct_casecross.csv")
# asthma
asthma_casecross <- read_csv("asthma1_jul_to_oct_casecross.csv")
# copd 
copd_casecross <- read_csv("copd1_jul_to_oct_casecross.csv")
# copd exacerbations
copd_ex_casecross <- read_csv("copd_ex1_jul_to_oct_casecross.csv")
# pneum or bronchitis
pneum_casecross <- read_csv("pneum1_jul_to_oct_casecross.csv")
# acute bronchitis
acute_bronch_casecross <- read_csv("acute_bronch1_jul_to_oct_casecross.csv")
# cvd
cvd_casecross <- read_csv("cvd1_jul_to_oct_casecross.csv")
# arrhythmia
arrhythmia_casecross <- read_csv("arrhythmia1_jul_to_oct_casecross.csv")
# cerebral vascular
cereb_vas_casecross <- read_csv("cereb_vas1_jul_to_oct_casecross.csv")
# heart failure
hf_casecross <- read_csv("hf1_jul_to_oct_casecross.csv")
# ischemic heart disease
ihd_casecross <- read_csv("ihd1_jul_to_oct_casecross.csv")
# myo infarc
mi_casecross <- read_csv("mi1_jul_to_oct_casecross.csv")
# RA
ra_casecross <- read_csv("ra1_jul_to_oct_casecross.csv")
# broken arm
broken_arm_casecross <- read_csv("broken_arm1_jul_to_oct_casecross.csv")

# Analyses ---------------------------------------------------------------------
# look up admit type, may want to subset to specifc admit
# ADM_TYPE variable: 1 = emergency, 2 = urgent, 3 = elective, 4 = newborn
# 5 = trauma, 9 = info not available

# Any Respiratory --------------------------------------------------------------
summary(as.factor(resp_casecross$ADM_TYPE))
xtabs(~ resp1 + outcome, resp_casecross) # 10795 events over the timeframe

# ggplot way to check shape
ggplot(resp_casecross, aes(x = wrf_pm, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw() # mostly linear

# conditional logistic model with wrf smk model
mod <- clogit(outcome ~ wrf_pm + 
                strata(PATIENTID), resp_casecross)
summary(mod)

# conditional logistic model with will's geo - background model
mod <- clogit(outcome ~ geo_smk_mean +
                strata(id), resp_casecross)
summary(mod)

# recovery time dataframe
resp_recovery <- resp_casecross %>% 
               mutate(recov = date - date_admit,
                      at_risk = ifelse(recov <= 21 & recov > 0 , 0, 1)) %>%
               filter(at_risk == 1)

# conditional logistic model with wrf smk model
mod <- clogit(outcome ~ wrf_smk_pm_mean  +
                strata(id), resp_recovery)
summary(mod)

# conditional logistic model with will's geo - background model
mod <- clogit(outcome ~ geo_smk_mean +
                strata(id), resp_recovery)
summary(mod)

# Asthma -----------------------------------------------------------------------
xtabs(~ asthma1 + outcome, asthma_casecross) # 1538 events over the timeframe
xtabs(~ outcome + ADM_TYPE, asthma_casecross) # 1276 asthma events are ED
glimpse(asthma_casecross)

# ggplot way to check shape
ggplot(asthma_casecross, aes(x = geo_wt_pm, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()

# increase by 10 units
asthma10 <- asthma_casecross %>% 
  mutate(wrf_10 = wrf_pm/10, wrf_smk_10 = wrf_smk_pm/10, geo_10 = geo_wt_pm/10,
         geo_smk_10 = geo_smk_pm/10) %>% 
  filter(!is.na(geo_wt_pm) & ADM_TYPE == 1)

# conditional logistic model with wrf smk model
mod <- clogit(outcome ~ wrf_smk_pm + strata(PATIENTID), asthma10)
summary(mod)

mod10 <- clogit(outcome ~ geo_10  + strata(PATIENTID), asthma10)
summary(mod10)


# 10 unit increase in WRF estimate
exp(0.004*10)
# lower bound
exp((0.004*10) - (1.96*10*0.0012))
# uppper bound
exp((0.004*10) + (1.96*10*0.0012))

# conditional logistic model with will's geo - background model
mod <- clogit(outcome ~ geo_smk_mean + strata(id), asthma10)
summary(mod)
# 10 unit increase
mod10 <- clogit(outcome ~ geo_10 + strata(id), asthma10)
summary(mod10)

# association
exp(0.0074*10)
# lower bound
exp((0.0074*10) - (1.96*10*0.0022))
# uppper bound
exp((0.0074*10) + (1.96*10*0.0022))

# recovery time dataframe with some extra terms for polynomial and piecewise model
asthma_recovery <- asthma_casecross %>% 
               mutate(recov = date - date_admit,
                      # 2 week recovery
                      at_risk = ifelse(recov <= 14 & recov > 0 , 0, 1),
                      ln_geo = ifelse(geo_smk_mean == 0, log(geo_smk_mean + 0.005),
                                      log(geo_smk_mean)),
                      geo_smk_sq = geo_smk_mean*geo_smk_mean,
                      geo_smk_cube = (geo_smk_mean)^3,
                      geo_smk_quad = (geo_smk_mean)^4,
                      geo_cat = ifelse(geo_smk_mean < 5, 0,
                              ifelse(geo_smk_mean >= 5 & geo_smk_mean < 25, 1,
                              ifelse(geo_smk_mean >= 25 & geo_smk_mean < 50, 2,
                              ifelse(geo_smk_mean >= 50 & geo_smk_mean < 100, 3,
                              ifelse(geo_smk_mean >= 100 & 
                                       geo_smk_mean < 250, 4,NA))))),
                      # piecewise knot at 100 
                      # interaction term, approach will likely have split
                      geo_int = ifelse(geo_smk_mean <= 100, 0, 1),
                      geo_response_int = geo_smk_mean*geo_int,
                      # piecewise 2nd approach
                      geo_smk2 = ifelse(geo_smk_mean <= 100, 0,
                                 geo_smk_mean - 100)) %>%
               filter(at_risk == 1 & !is.na(geo_smk_mean)) 

summary(asthma_recovery)

# conditional logistic model with wrf smk model
mod <- clogit(outcome ~ wrf_smk_pm_mean  +
                strata(id), asthma_recovery)
summary(mod)

# log predictor
mod <- clogit(outcome ~ ln_geo  +
                strata(id), asthma_recovery)
summary(mod)

# is the relationship with asthma and smoke pm linear?
# shape plot 
pr_outcome <- asthma_recovery %>% filter(!is.na(geo_smk_mean))

# shape of curve using loess
plot(pr_outcome$geo_smk_mean, 
     predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean)))

test <- predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean))

# ggplot way to check shape
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()

# Asthma plot ------------------------------------------------------------------

# Histogram of PM2.5
ggplot(asthma_recovery, aes(x=geo_smk_mean)) +
  geom_histogram(binwidth = 1)

ggplot(asthma_recovery, aes(x=ln_geo)) +
  geom_histogram(binwidth = 0.2)

# predicted probability shape of ln_geo
ggplot(asthma_recovery, aes(x = ln_geo, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()


# bin prevalence of outcomes
summary(asthma_recovery$geo_smk_mean)
summary(asthma_recovery$wrf_smk_pm_mean)


# labels of average for cuts
labs <- c(1, 2, 3, 4, 5, 6, 7, 8 , 9, 10,
         11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 40, 
         50, 60, 70, 80, 90, 100, 110, 150, 200, 270)

asthma_pm_bin <- asthma_recovery %>% 
  mutate(geo_pm_bin = cut(geo_smk_mean, c(-0.01, 1, 2, 3, 4, 5, 6, 7, 8 , 9, 10,
         11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 40, 
         50, 60, 70, 80, 90, 100, 110, 150, 200, 270), labels = labs),
         wrf_pm_bin = cut(wrf_smk_pm_mean, c(-0.01, 10, 20, 30, 40, 50, 60, 70, 80,
         90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 250, 300, 350,
         400, 450, 520)))

# check bins
table <- xtabs(~outcome + geo_pm_bin, asthma_pm_bin)
table
prop.table(table, 2)

# binned proportion added to plot
asthma_geo_bin <- asthma_pm_bin %>% group_by(geo_pm_bin) %>% 
  summarise(denom = n(), n_asthma = sum(outcome), prop_asthma = n_asthma/denom) %>% 
  mutate(geo_smk_mean = as.numeric(substr(geo_pm_bin, 1, 10)),
         samp_20 = ifelse(denom <= 20, 1, 0),
         denom_size = ifelse(denom >= 100, 100, denom)) # indicator of small denominator

summary(asthma_geo_bin)
# plot showing shape as well as binned proportion of asthma events
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  geom_point(data= asthma_geo_bin, aes(x=geo_smk_mean, y = prop_asthma,
                                       color = denom_size)) +
  scale_color_continuous(limits=c(1, 100), low="red", high="blue") +
  theme_bw()


# conditional logistic model with will's geo - background model
mod <- clogit(outcome ~ geo_smk_mean + geo_smk_sq +
                strata(id), asthma_recovery)
summary(mod)

# cubed term isn't much better; Quad term doesn't fit well either
asthma_quad_mod <- clogit(outcome ~ geo_smk_mean + geo_smk_sq + 
                   geo_smk_cube + geo_smk_quad + strata(id), asthma_recovery)

summary(asthma_quad_mod)

# try category
mod <- clogit(outcome ~ as.factor(geo_cat) + strata(id), asthma_recovery)

summary(mod)

# piecewise model probably best fit
# piecewise with two terms
piecewise_mod <- clogit(outcome ~ geo_smk_mean + geo_smk2 +
                strata(id), asthma_recovery)
summary(piecewise_mod) 

# best model
pm <- seq(from = 0, to = 250, by = 10)
pm_asthma_piecewise <- data_frame(pm) %>% 
  mutate(odds_ratio = ifelse(pm <= 100, exp(pm * 0.013134),
                             exp((pm * 0.013134)+((pm-100)*-0.016018))))
# need to find a way to work in 95% CIs. Need contrasts or formula
# probably need to calculate by formula since clogit isn't compatible with
# contrast packaged
# odds ratio plot
ggplot(pm_asthma_piecewise, aes(x=pm, y=odds_ratio)) +
  geom_point() +
  theme_bw()
# might not be approapriate to interpret on very hight end of scale, 
# also consider using maximum likelihood to identify knot based on data (likley
# won't change much)

# piecewise interaction (this creates a discontinutiy in the relationship)
piecewise_mod2 <- clogit(outcome ~ geo_smk_mean + geo_response_int + geo_int +
                strata(id), asthma_recovery)

summary(piecewise_mod2) # big gap in this model


# COPD Exacerbations -----------------------------------------------------------
# Association WRF
mod <- clogit(outcome ~ wrf_smk_pm_mean + strata(id), copd_ex_casecross)
summary(mod)
# association
exp(-0.0086*10)
# 95% CI bound
exp((-0.0086*10) - (1.96*10*0.0029))
exp((-0.0086*10) + (1.96*10*0.0029))

# Geo
mod <- clogit(outcome ~ geo_smk_mean + strata(id), copd_ex_casecross)
summary(mod)
# association
exp(0.0046*10)
# 95% CI bound
exp((0.0046*10) - (1.96*10*0.0025))
exp((0.0046*10) + (1.96*10*0.0025))

# SE around a 10 unit increase
copd_10unit <- copd_ex_casecross %>% mutate(geo_10 = geo_smk_mean/10, 
                                            geo_100 = geo_smk_mean/100)


# 1 unit
mod1 <- clogit(outcome ~ geo_smk_mean + strata(id), copd_10unit)
summary(mod1)
# 10 unit
mod10 <- clogit(outcome ~ geo_10 + strata(id), copd_10unit)
summary(mod10)
# 100 unit
mod100 <- clogit(outcome ~ geo_100 + strata(id), copd_10unit)
summary(mod100)

# the SE needs to also be multipled by the unit

# shape plot (removing missing geo_smk values)
pr_outcome <- copd_ex_casecross %>% filter(!is.na(geo_smk_mean))

# shape of curve using loess
plot(pr_outcome$geo_smk_mean, 
     predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean)))
# ggplot
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()
# exacerbations may follow a squared term

# program in the recovery period
copd_ex_recovery <- copd_ex_casecross %>% 
  mutate(recov = date - date_admit,
         # 2 week recovery
         at_risk = ifelse(recov <= 21 & recov > 0 , 0, 1),
         geo_smk_sq = (geo_smk_mean)^2,
         geo_smk_cube = (geo_smk_mean)^3,
         geo_binary = ifelse(geo_smk_mean >= 100, 1, 0)) %>%
  filter(at_risk == 1)

# check shape again
pr_outcome <- copd_ex_recovery %>% filter(!is.na(geo_smk_mean))
# shape of curve using loess
plot(pr_outcome$geo_smk_mean, 
     predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean)))

# fit just linear term
copd_mod <- clogit(outcome ~ geo_smk_mean + strata(id), 
                   data = copd_ex_recovery)
summary(copd_mod)

# fit squared term model (a little better fit)
copd2_mod <- clogit(outcome ~ geo_smk_mean + geo_smk_sq + strata(id), 
                    data = copd_ex_recovery)
summary(copd2_mod)

# cubed
copd3_mod <- clogit(outcome ~ geo_smk_mean + geo_smk_sq + geo_smk_cube + 
                      strata(id), data = copd_ex_recovery)
summary(copd3_mod)

# plot shape of ORs
pm_copd <- data_frame(pm) %>% 
  mutate(odds_ratio = exp((pm * -0.003772) + (pm * 0.00007522)))
# need to find a way to work in 95% CIs. Need contrasts or formula
# probably need to calculate by formula since clogit isn't compatible with
# contrast packaged

# odds ratio plot
ggplot(pm_copd, aes(x=pm, y=odds_ratio)) +
  geom_point() +
  theme_bw()

# plot looks weird...

# try categorical
copd_mod <- clogit(outcome ~ geo_binary + 
                      strata(id), data = copd_ex_recovery)
summary(copd_mod)
# might be no association with copd exacerbations

# Pneumonia or Bonchitis -------------------------------------------------------
# something is wrong with this dataset or the resp  dataset (probably resp dataset)
# sample size is all messed up, need to check
xtabs(~outcome , pneum_bronch_casecross)

pneum_bronch10 <- pneum_bronch_casecross %>% mutate(wrf_10 = wrf_smk_pm_mean/10,
                                        geo_10 = geo_smk_mean/10) %>% 
            filter(!is.na(geo_smk_mean))

# models
# WRF Chem
pb_mod <- clogit(outcome ~ wrf_10 + strata(id), data = pneum_bronch10 )
summary(pb_mod)

# Geo
pb_mod <- clogit(outcome ~ geo_10 + strata(id), data = pneum_bronch10 )
summary(pb_mod)


# I actually think I'd like to split this outcome in to seperate outcomes
# shape plot (removing missing geo_smk values)
pr_outcome <- pneum_bronch_casecross %>% filter(!is.na(geo_smk_mean))

# ggplot
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()

# Stroke or cerebrovas disease -------------------------------------------------
stroke10 <- cereb_vas_casecross %>% mutate(wrf_10 = wrf_smk_pm_mean/10,
                                           geo_10 = geo_smk_mean/10) %>% 
            filter(!is.na(geo_smk_mean))

# models
# WRF Chem
stroke_mod <- clogit(outcome ~ wrf_10 + strata(id), data = stroke10 )
summary(stroke_mod)

# Geo
stroke_mod <- clogit(outcome ~ geo_10 + strata(id), data = stroke10 )
summary(stroke_mod)

cereb_vas_recovery <- cereb_vas_casecross %>% 
  mutate(recov = date - date_admit,
         # 2 week recovery
         at_risk = ifelse(recov <= 14 & recov > 0 , 0, 1),
         geo_smk_sq = (geo_smk_mean)^2,
         geo_smk_cube = (geo_smk_mean)^3,
         geo_binary = ifelse(geo_smk_mean >= 10, 1, 0),
         geo_cat = ifelse(geo_smk_mean < 10, 0,
                   ifelse(geo_smk_mean >= 10 & geo_smk_mean < 50, 1,
                   ifelse(geo_smk_mean >= 50 & geo_smk_mean < 100, 2,
                   ifelse(geo_smk_mean >= 100, 3, NA))))) %>%
  filter(at_risk == 1)

# plot shape
pr_outcome <- cereb_vas_recovery %>% filter(!is.na(geo_smk_mean))

# shape of curve using loess
plot(pr_outcome$geo_smk_mean, 
     predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean)))
# that is a very odd shape...

# ggplot
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()

# models
mod <- clogit(outcome ~ as.factor(geo_cat) +
                strata(id), cereb_vas_recovery)
summary(mod)

# Myocardial Infarction --------------------------------------------------------
# Note:July 1st, 2016 Squared term looks like the best fit. See code below
xtabs(~ mi1 + outcome, mi_casecross) # 3253 events over the timeframe

# MI
mi10 <- mi_casecross %>% mutate(wrf_10 = wrf_smk_pm_mean/10,
                                           geo_10 = geo_smk_mean/10) %>% 
            filter(!is.na(geo_smk_mean))

# models
# WRF Chem
mi_mod <- clogit(outcome ~ wrf_10 + strata(id), data = mi10 )
summary(mi_mod)

# Geo
mi_mod <- clogit(outcome ~ geo_10 + strata(id), data = mi10 )
summary(mi_mod)


# is the relationship with myo infarc and smoke pm linear?
# shape plot (removing missing geo_smk values)
pr_outcome <- mi_recovery %>% filter(!is.na(geo_smk_mean))

# shape of curve using loess
plot(pr_outcome$geo_smk_mean, 
     predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean)))

# ggplot
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()

# same pattern with WRF?
ggplot(mi_casecross, aes(x = wrf_smk_pm_mean, y = outcome)) + 
  geom_smooth(se=T) + 
  theme_bw()
  
glimpse(mi_casecross)

# conditional logistic model with wrf smk model
mod <- clogit(outcome ~ wrf_smk_pm_mean  +
                strata(id), mi_casecross)
summary(mod)

# conditional logistic model with will's geo - background model
mod <- clogit(outcome ~ geo_smk_mean +
                strata(id), mi_casecross)
summary(mod)

# recovery time dataframe
mi_recovery <- mi_casecross %>% 
               mutate(recov = date - date_admit,
                    # 3 week recovery
                    at_risk = ifelse(recov <= 21 & recov > 0 , 0, 1),
                    geo_cat = ifelse(geo_smk_mean < 2, 0,
                              ifelse(geo_smk_mean >= 2 & geo_smk_mean < 50, 1,
                              ifelse(geo_smk_mean >= 50 & geo_smk_mean < 150, 2,
                              ifelse(geo_smk_mean >= 150, 3, NA)))),
                    geo_binary_50 = ifelse(geo_smk_mean >= 50, 1, 0),
                    geo_smk_sq = geo_smk_mean*geo_smk_mean) %>%
# check to make sure code doing what you want
#               select(id, date_admit, date_discharge, date, geo_smk_mean,
#                                               outcome, recov, at_risk) 
               filter(at_risk == 1 & !is.na(geo_smk_mean))

# looks like lag variable didn't get worked in, will need to go back to merge
# geo smk
mi_mod <- clogit(outcome ~ geo_smk_mean + geo_smk_sq +
                strata(id), mi_recovery)
summary(mi_mod) # squared term might be useful

pm <- seq(from = 0, to = 250, by = 10)
pm_effect_estimate <- data_frame(pm) %>% 
    mutate(odds_ratio = exp((pm * 0.008423) + (pm * 0.00006619)))
# need to find a way to work in 95% CIs. Need contrasts or formula
# probably need to calculate by formula since clogit isn't compatible with
# contrast packaged

# odds ratio plot
ggplot(pm_effect_estimate, aes(x=pm, y=odds_ratio)) +
  geom_point() +
  theme_bw()

# smoke cat based on plot
mod <- clogit(outcome ~ as.factor(geo_cat) +
                strata(id), mi_recovery)
summary(mod)

# binary
mod <- clogit(outcome ~ geo_binary_50 +
                strata(id), mi_recovery)
summary(mod)

# MI plot

# bin prevalence of outcomes
summary(mi_recovery$geo_smk_mean)
summary(mi_recovery$wrf_smk_pm_mean)


# labels of average for cuts
labs <- c(1, 2, 3, 4, 5, 6, 7, 8 , 9, 10,
         11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 40, 
         50, 60, 70, 80, 90, 100, 110, 150, 200, 270)

mi_pm_bin <- mi_recovery %>% 
  mutate(geo_pm_bin = cut(geo_smk_mean, c(-0.01, 1, 2, 3, 4, 5, 6, 7, 8 , 9, 10,
         11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 40, 
         50, 60, 70, 80, 90, 100, 110, 150, 200, 270), labels = labs))

# check bins
table <- xtabs(~outcome + geo_pm_bin, mi_pm_bin)
table
prop.table(table, 2)

# binned proportion added to plot
mi_geo_bin <- mi_pm_bin %>% group_by(geo_pm_bin) %>% 
  summarise(denom = n(), n_mi = sum(outcome), prop_mi = n_mi/denom) %>% 
  mutate(geo_smk_mean = as.numeric(substr(geo_pm_bin, 1, 10)),
         samp_20 = ifelse(denom <= 20, 1, 0),
         denom_size = ifelse(denom >= 50, 50, denom)) # indicator of small denominator

summary(mi_geo_bin)
# plot showing shape as well as binned proportion of asthma events
ggplot(mi_recovery, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  geom_point(data= mi_geo_bin, aes(x=geo_smk_mean, y = prop_mi,
                                       color = denom_size)) +
  scale_color_continuous(limits=c(0, 50), low="red", high="blue") +
  theme_bw()
  

# Heart Failure ----------------------------------------------------------------
# Stroke or cerebrovas disease -------------------------------------------------
hf10 <- hf_casecross %>% mutate(wrf_10 = wrf_smk_pm_mean/10,
                                           geo_10 = geo_smk_mean/10) %>% 
            filter(!is.na(geo_smk_mean))

# models
# WRF Chem
hf_mod <- clogit(outcome ~ wrf_10 + strata(id), data = hf10 )
summary(hf_mod)

# Geo
hf_mod <- clogit(outcome ~ geo_10 + strata(id), data = hf10 )
summary(hf_mod)

pr_outcome <- hf_casecross %>% filter(!is.na(geo_smk_mean))

# shape of curve using loess
plot(pr_outcome$geo_smk_mean, 
     predict(loess(pr_outcome$outcome ~ pr_outcome$geo_smk_mean)))

# ggplot
ggplot(pr_outcome, aes(x = geo_smk_mean, y = outcome)) + 
  geom_smooth(se = T) +
  theme_bw()

mod <- clogit(outcome ~ geo_smk_mean +
                strata(id), hf_casecross)
summary(mod)

# Arrhythmia -------------------------------------------------------------------
arrhythmia10 <- arrhythmia_casecross %>% mutate(wrf_10 = wrf_smk_pm_mean/10,
                                           geo_10 = geo_smk_mean/10) %>% 
            filter(!is.na(geo_smk_mean))

# models
# WRF Chem
arrhythmia_mod <- clogit(outcome ~ wrf_10 + strata(id), data = arrhythmia10 )
summary(arrhythmia_mod)

# Geo
arrhythmia_mod <- clogit(outcome ~ geo_10 + strata(id), data = arrhythmia10 )
summary(arrhythmia_mod)