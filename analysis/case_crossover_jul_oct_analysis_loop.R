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

# Set working directory and read in files --------------------------------------
# Relative path 
wd_path <- paste0("./analysis/analysis_data")

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

# dataframe list
df_list <- list(resp_casecross, asthma_casecross, copd_casecross, copd_ex_casecross, 
  pneum_casecross, acute_bronch_casecross, cvd_casecross, arrhythmia_casecross,
  cereb_vas_casecross, hf_casecross, ihd_casecross, mi_casecross, 
  ra_casecross, broken_arm_casecross)


# Producing conditional logit model estimates loop -----------------------------
for(i in 1:length(df_list)){

  df_to_loop <- data.frame(df_list[i])
  outcome <- colnames(df_to_loop[3])
  
  # extract covariates from dataframe
  covariates_df <- df_to_loop[, c(1:14, 25:26, 77:86)]
  # extract pm values and divide by 10 and ordered
  pm_estimates_df <- df_to_loop[, c(15, 27:31, 16, 32:36, 17, 37:41, 18, 42:46,
                                    19, 47:51, 20, 52:56, 22, 62:66, 23, 67:71,
                                    24, 72:76)]/10  # create 10 unit increases
  # bind columns back together
  df_analysis <- cbind(covariates_df, pm_estimates_df) %>% 
    filter(!is.na(wrf_pm)) %>% 
    # limit to emergency or urgent care
    filter(ADM_TYPE == 1 | ADM_TYPE == 2) #%>%  
    # mutate(recov = date - date_admit, 
    #        at_risk = ifelse(recov <= 21 & recov > 0 , 0, 1)) %>%
    # filter(at_risk == 1)
  
  # second loop to run a model for each pm estimation method
  
  # empty matrix
  point_estimates <- matrix(nrow = 54, ncol = 8, byrow = T)
  
  colnames(point_estimates) <- c('pm_method', 'n', 'n_events', 'odds_ratio', 
                                 'lower95', 'upper95', 'se', 'p_val')
    
    for(j in 27:80){
      
      var_name <- colnames(df_analysis[j])
      
      # conditional logistic regression model
      mod <- clogit(outcome ~ df_analysis[, j] + wrf_temp + strata(PATIENTID), 
                    df_analysis)

      # populate matrix
      row_n <- j-26

      point_estimates[row_n, 1] <- var_name
      point_estimates[row_n, 2] <- mod$n
      point_estimates[row_n, 3] <- mod$nevent
      # odds ratio
      point_estimates[row_n, 4] <- round(exp(summary(mod)$coefficient[1,1]), 3)

      # 95% lower bound
      point_estimates[row_n, 5] <- round(exp((summary(mod)$coefficient[1,1]) -
                                        1.96*(summary(mod)$coefficient[1,3])), 3)
      # 95% upper bound
      point_estimates[row_n, 6] <- round(exp((summary(mod)$coefficient[1,1]) +
                                        1.96*(summary(mod)$coefficient[1,3])), 3)
      # standard error
      point_estimates[row_n, 7] <- round(summary(mod)$coefficient[1,3], 4)
      # p val
      point_estimates[row_n, 8] <- round(summary(mod)$coefficient[1,5], 4)
  
      # save point estimates as a dataframe
      point_est_df <- data.frame(point_estimates)
      assign(paste(outcome, 'point_est', sep = '_'), point_est_df) 
    }
}

output <- 
  matrix(paste("Content", LETTERS[1:16]), 
         ncol=4, byrow = TRUE)


htmlTable(output,
          header =  paste(c("1st", "2nd",
                            "3rd", "4th"), "header"),
          rnames = paste(c("1st", "2nd",
                           "3rd", "4th"), "row"),
          rgroup = c("Group A",
                     "Group B"),
          n.rgroup = c(2,2),
          cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
          n.cgroup = c(2,2), 
          caption="Basic table with both column spanners (groups) and row groups",
          tfoot="&dagger; A table footer commment")
