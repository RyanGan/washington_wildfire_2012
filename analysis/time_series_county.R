# ------------------------------------------------------------------------------
# Title: Washington 2012 Time-Series Analysis
# Author: Ryan Gan
# Date Created: 9/28/16
# ------------------------------------------------------------------------------

# Library
library(tidyverse)
library(splines) # needed to model asthma trend over time


# Load county time series data
path <- paste0("C:/Users/RGan/Documents/git_local_repos/wildfire/",
  "wildfire_washington/analysis/analysis_data/wa_2012_county_time_series.csv")

wash_ts_df <- read_csv(path)

# check descriptives
summary(wash_ts_df)

# asthma trend over time
asthma_trend_plot <- ggplot(wash_ts_df, aes(x = date, y = asthma_n)) + 
  geom_jitter() +
  facet_wrap(~ county)

print(asthma_trend_plot)

# try time-series in spokane county
spokane <- wash_ts_df %>% filter(county == "King" & !is.na(geo_smk_pm)) 

spokane_plot <- ggplot(spokane, aes(x = date, y = asthma_n)) + 
  geom_jitter() 

print(spokane_plot)

# spline model -----------------------------------------------------------------
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

# models for association between smoke pm and daily asthma count ---------------
# unadjusted model
model_unadj <- glm(asthma_n ~ geo_smk_pm, spokane, family=quasipoisson)
summary(model_unadj)

# controlling for seasonality with spline
model_adj <- update(model_unadj,.~.+spl)
summary(model_adj)

