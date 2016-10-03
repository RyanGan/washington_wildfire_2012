# ------------------------------------------------------------------------------
# Title: Washington 2012 Time-Series Analysis
# Author: Ryan Gan
# Date Created: 9/28/16
# ------------------------------------------------------------------------------

# Library
library(tidyverse)
library(splines) # needed to model asthma trend over time

# Load county time series data
path <- paste0("./analysis/analysis_data/wa_2012_county_time_series.csv")

wash_ts_df <- read_csv(path)
# check descriptives
summary(wash_ts_df)

# missing values in sparsely populated counties. set to 0
check_missing <- filter(wash_ts_df, is.na(n_obs))
# mutate each to fill in missing values
check2 <- check_missing %>% mutate_each(funs(new = ifelse(is.na(.), 0, .)), 
                                        n_obs:wrf_pbl)
summary(check2)

# plot asthma events for all counties
ggplot(wash_ts_df, aes(y =asthma_n, x = date)) +
  geom_jitter() +
  facet_wrap(~county)

# Aggregated Asthma Events for all of Washington and Smoke PM ------------------
wash_aggregate_df <- wash_ts_df %>% 
  # set missing asthma values to 0, and also for smoke values, set to 0
  mutate_each(funs(wo_miss = ifelse(is.na(.), 0, .)), n_obs:wrf_pbl) %>% 
  group_by(date) %>% 
  summarise(n_resp = sum(resp_n_wo_miss), n_asthma = sum(asthma_n_wo_miss), 
            n_cvd = sum(cvd_n_wo_miss), avg_wrf_smk = mean(wrf_smk_pm_wo_miss, na.rm = T),
            avg_geo_smk = mean(geo_smk_pm_wo_miss, na.rm = T), 
            avg_temp = mean(wrf_temp, na.rm = T))

summary(wash_aggregate_df)

# asthma trend over the year for all of washington
asthma_trend_plot <- ggplot(wash_aggregate_df, aes(x = date, y = n_asthma)) + 
  geom_jitter()
plot(asthma_trend_plot)

# fit spline model 
spl_yr <- bs(wash_aggregate_df$date,degree=3, df=7) # spline for week/weekend trends

# model n asthma events with a spline with 7 df (knotches)
model1 <- glm(n_asthma ~ spl_yr, wash_aggregate_df, family=quasipoisson) 
summary(model1)

# compute predicted number of asthma events from this model
pred1 <- predict(model1,type="response")

plot(wash_aggregate_df$date, wash_aggregate_df$n_asthma,
     ylim=c(0, 40),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily Number of Asthma ER/Urgent",
     xlab="Date")
lines(wash_aggregate_df$date, pred1, lwd=2)

# check residuals
res1 <- residuals(model1,type="response")

plot(wash_aggregate_df$date, res1,ylim=c(-15,15),pch=19,cex=0.4,col=grey(0.6),
     main="Residuals over time",ylab="Residuals (observed-fitted)",xlab="Date")
abline(h=1,lty=2,lwd=2)

# models for association between smoke pm and daily asthma count 
# unadjusted model
model_unadj <- glm(n_asthma ~ avg_geo_smk, wash_aggregate_df, family=quasipoisson)
summary(model_unadj)

# controlling for seasonality with spline
model_adj <- update(model_unadj,.~.+spl)
summary(model_adj)
# appears to be an association with asthma and wildfire smoke on the aggregate

# Subset to July 1st to October 31st dates -------------------------------------
wash_jul_oct <- wash_aggregate_df %>% 
  filter(date >= "2012-07-01" & date <= "2012-10-31" )
# check stats
summary(wash_jul_oct)

# look at trend over Jul to oct
ggplot(wash_jul_oct, aes(x = date, y = n_asthma)) + 
  geom_jitter() 

# fit spline to summer shape 
spl_summer <- bs(wash_jul_oct$date,degree=3, df=3) # spline for week/weekend trends

# model n asthma events with a spline with 7 df (knotches)
summer_model1 <- glm(n_asthma ~ spl_summer, wash_jul_oct, family=quasipoisson) 
summary(summer_model1)

# compute predicted number of asthma events from this model
pred1 <- predict(summer_model1,type="response")

plot(wash_jul_oct$date, wash_jul_oct$n_asthma,
     ylim=c(0, 40),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily Number of Asthma ER/Urgent",
     xlab="Date")
lines(wash_jul_oct$date, pred1, lwd=2)

# check residuals
res1 <- residuals(summer_model1,type="response")

plot(wash_jul_oct$date, res1,ylim=c(-15,15),pch=19,cex=0.4,col=grey(0.6),
     main="Residuals over time",ylab="Residuals (observed-fitted)",xlab="Date")
abline(h=1,lty=2,lwd=2)

# models for association between smoke pm and daily asthma count 
# unadjusted model
summer_model_unadj <- glm(n_asthma ~ avg_geo_smk + avg_temp, wash_jul_oct, family=quasipoisson)
summary(summer_model_unadj)

# controlling for seasonality with spline
summer_model_adj <- update(summer_model_unadj,.~.+ spl_summer)
summary(summer_model_adj)

# going to try and limit the yearly spline to just jul 1 and oct 31
dates_2012 <- wash_aggregate_df %>% select(date) %>% as.vector()
jul_oct_spline <- cbind(dates_2012, spl_yr)  
# subset spl_yr values 183 to 305  
jul_oct_spline <- spl_yr[183:305, ]
  
summer_model_adj_yr <- update(summer_model_unadj, .~ . + jul_oct_spline)
summary(summer_model_adj_yr)
# this doesn't help

asthma_trend_plot <- ggplot(wash_ts_df, aes(x = date, y = asthma_n)) + 
  geom_jitter() +
  facet_wrap(~ county)

print(asthma_trend_plot)

# try time-series in spokane county
spokane <- wash_ts_df %>% filter(county == "Spokane" & !is.na(geo_smk_pm)) 

spokane_plot <- ggplot(spokane, aes(x = date, y = asthma_n)) + 
  geom_jitter() 

print(spokane_plot)

# not sure a spline would help because there doesn't look to be any seasonal trends
spl <- bs(spokane$date,degree=2,df=2) # spline for week/weekend trends

# model n asthma events with a spline with 7 df (knotches)
model1 <- glm(asthma_n ~ spl, spokane, family=quasipoisson) 
summary(model1)

# compute predicted number of asthma events from this model
pred1 <- predict(model1,type="response")

plot(spokane$date, spokane$asthma_n,
     ylim=c(0, 10),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily Number of Admissions",
     xlab="Date")
lines(spokane$date, pred1, lwd=2)

# check residuals
res1 <- residuals(model1,type="response")

plot(spokane$date, res1,ylim=c(-15,15),pch=19,cex=0.4,col=grey(0.6),
     main="Residuals over time",ylab="Residuals (observed-fitted)",xlab="Date")
abline(h=1,lty=2,lwd=2)

# models for association between smoke pm and daily asthma count 
# unadjusted model
model_unadj <- glm(asthma_n ~ geo_smk_pm, spokane, family=quasipoisson)
summary(model_unadj)

# controlling for seasonality with spline
model_adj <- update(model_unadj,.~.+spl)
summary(model_adj)

