#-------------------------------------------------------------------------------
#     Title: Creation of outcome variables for confidential CHARS 2012 dataset
#     Author: Ryan Gan                                                                   
#     Date Created: 7/6/16                                                
#-------------------------------------------------------------------------------

# Load Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)

# Read in permanent CHARs dataset ----------------------------------------------
path <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/Washington St CHARS Data/',
               'confidential_data/chars_2012_confidential_no_outcomes.csv')

chars_2012_conf_df <- read_csv(path) 

glimpse(chars_2012_conf_df)
# parsing failure at row 738152; check it out
parse_check <- chars_2012_conf_df[738152, ]
 # Patient ID is missing; might not be a problem
glimpse(parse_check)


# use cms.gov to look up icd9 codes if you need help

# Creating vectors of outcome claims -------------------------------------------
# import chars diagnosis code key
diag_read <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/',
                    'Washington St CHARS Data/MedicareDiagCodeV30.csv')

icd9_key <- read.csv(diag_read)
icd9_key$X <- NULL
summary(icd9_key)
head(icd9_key)

# sort by icd9 code add row variable 
icd9_key <- arrange(icd9_key, DiagV30) %>%
            mutate(n = as.numeric(row.names(icd9_key)))

# now I should be able to output the DiagV30 column by row number 
# or search for key terms in LonagDesc

## All Non-Trauma Causes 001-799 -----------------------------------------------

# I commented out this section since it's way too many outcomes
#which(icd9_key$DiagV30 == '0010') # row 1
#which(icd9_key$DiagV30 == '7999') # row 9580

#non_trauma_icd9 <- filter(icd9_key, n >= 1 & n <= 9580) %>%
#                   select(DiagV30)
# convert to vector
#non_trauma_icd9 <- as.vector(as.matrix(non_trauma_icd9))   

# # make indicator of non-trauma-icd9
# chars_2012_conf_df$non_trauma1 <- ifelse(chars_2012_conf_df$DIAG1 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma2 <- ifelse(chars_2012_conf_df$DIAG2 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma3 <- ifelse(chars_2012_conf_df$DIAG3 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma4 <- ifelse(chars_2012_conf_df$DIAG4 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma5 <- ifelse(chars_2012_conf_df$DIAG5 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma6 <- ifelse(chars_2012_conf_df$DIAG6 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma7 <- ifelse(chars_2012_conf_df$DIAG7 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma8 <- ifelse(chars_2012_conf_df$DIAG8 %in% non_trauma_icd9 , 1, 0)
# chars_2012_conf_df$non_trauma9 <- ifelse(chars_2012_conf_df$DIAG9 %in% non_trauma_icd9 , 1, 0)
# 
# # sum up the indicators
# chars_2012_conf_df$non_trauma_sum <- (chars_2012_conf_df$non_trauma1 + chars_2012_conf_df$non_trauma2 + 
#                                       chars_2012_conf_df$non_trauma3 + chars_2012_conf_df$non_trauma4 +
#                                       chars_2012_conf_df$non_trauma5 + chars_2012_conf_df$non_trauma6 +
#                                       chars_2012_conf_df$non_trauma7 + chars_2012_conf_df$non_trauma8 +
#                                       chars_2012_conf_df$non_trauma9)
# chars_2012_conf_df$non_trauma_dx <- ifelse(chars_2012_conf_df$non_trauma_sum> 0, 1, 0)
# 
# xtabs(~ non_trauma1, chars_2012_conf_df) # most claims for non-trauma

# Coding Respiratory Disease ICD-9 Outcomes ------------------------------------

# All Respiratory Diseases 460 to 519 ------------------------------------------
which(icd9_key$DiagV30 == '460') # row 5067 (cold) start of resp outcomes
# last icd9 code in resp disease is 519.9 (unsp dis of resp sys)
which(icd9_key$DiagV30 == '5199') # row 5321 end of resp outcomes

resp_icd9 <- filter(icd9_key, n >= 5067 & n <= 5321) %>%
              select(DiagV30)
# convert to vector
resp_icd9 <- as.vector(as.matrix(resp_icd9))   

# CHARS indicator of resp_icd9
chars_2012_conf_df$resp1 <- ifelse(chars_2012_conf_df$DIAG1 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp2 <- ifelse(chars_2012_conf_df$DIAG2 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp3 <- ifelse(chars_2012_conf_df$DIAG3 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp4 <- ifelse(chars_2012_conf_df$DIAG4 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp5 <- ifelse(chars_2012_conf_df$DIAG5 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp6 <- ifelse(chars_2012_conf_df$DIAG6 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp7 <- ifelse(chars_2012_conf_df$DIAG7 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp8 <- ifelse(chars_2012_conf_df$DIAG8 %in% resp_icd9, 1, 0)
chars_2012_conf_df$resp9 <- ifelse(chars_2012_conf_df$DIAG9 %in% resp_icd9, 1, 0)

# sum up the indicators
chars_2012_conf_df$resp_sum <- (chars_2012_conf_df$resp1 + chars_2012_conf_df$resp2 + 
                          chars_2012_conf_df$resp3 + chars_2012_conf_df$resp4 +
                          chars_2012_conf_df$resp5 + chars_2012_conf_df$resp6 +
                          chars_2012_conf_df$resp7 + chars_2012_conf_df$resp8 +
                          chars_2012_conf_df$resp9)
chars_2012_conf_df$resp_dx <- ifelse(chars_2012_conf_df$resp_sum> 0, 1, 0)

# priimary dx of any resp outcome
xtabs(~ resp1, chars_2012_conf_df)
50112/(50112+694231)

# Asthma, ICD-9 493 ------------------------------------------------------------
# try asthma 493 to 49392; identify rows with following code
which(icd9_key$DiagV30 == '49300') # start of asthma is row 5206
which(icd9_key$DiagV30 == '49392') # end of asthma is row 5219

# limit just to asthma code and just the diagnosis column
icd9_check <- filter(icd9_key, n >= 5206 & n <= 5219) 
icd9_check

asthma_icd9 <- filter(icd9_key, n >= 5206 & n <= 5219) %>%
                 select(DiagV30)

asthma_icd9 <- as.vector(as.matrix(asthma_icd9))   

xtabs(~asthma_icd9 )

# now can I make a new variable, asthma1, that indicates an asthma claim?
chars_2012_conf_df$asthma1 <- ifelse(chars_2012_conf_df$DIAG1 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma2 <- ifelse(chars_2012_conf_df$DIAG2 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma3 <- ifelse(chars_2012_conf_df$DIAG3 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma4 <- ifelse(chars_2012_conf_df$DIAG4 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma5 <- ifelse(chars_2012_conf_df$DIAG5 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma6 <- ifelse(chars_2012_conf_df$DIAG6 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma7 <- ifelse(chars_2012_conf_df$DIAG7 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma8 <- ifelse(chars_2012_conf_df$DIAG8 %in% asthma_icd9, 1, 0)
chars_2012_conf_df$asthma9 <- ifelse(chars_2012_conf_df$DIAG9 %in% asthma_icd9, 1, 0)

# sum up the asthma indicators
chars_2012_conf_df$asthma_sum <- (chars_2012_conf_df$asthma1 + chars_2012_conf_df$asthma2 + 
                            chars_2012_conf_df$asthma3 + chars_2012_conf_df$asthma4 +
                            chars_2012_conf_df$asthma5 + chars_2012_conf_df$asthma6 +
                            chars_2012_conf_df$asthma7 + chars_2012_conf_df$asthma8 +
                            chars_2012_conf_df$asthma9)
chars_2012_conf_df$asthma_dx <- ifelse(chars_2012_conf_df$asthma_sum > 0, 1, 0)

# check if the binary asthma_dx code aligns with the sum

asthma_dx_check <- table(chars_2012_conf_df$asthma_dx, chars_2012_conf_df$asthma_sum)
asthma_dx_check

summary(chars_2012_conf_df)

# seems to do the same thing, but both codes could be incorrect. i should check
# the number of asthma claims in diagnoses 1 (5174 claims)
asthma_claims <- subset(chars_2012_conf_df, DIAG1 %in% asthma_icd9)
asthma_claims$DIAG1 <- as.factor(asthma_claims$DIAG1)
# check asthma claims subset
summary(asthma_claims$DIAG1)

# number of asthma claims; first convert claims to as.factor
chars_2012_conf_df$asthma_dx <- as.factor(chars_2012_conf_df$asthma_dx)

summary(chars_2012_conf_df$asthma_dx)
# percentage of claims that are asthma claims
(39720/(39720+704623)) * 100

rm(asthma_claims) # remove to save space
# 5.3% of claims in Washington in 2012 were related to asthma

# Pneumonia ICD-9 code 480-486 -------------------------------------------------
# id the rows of interest
which(icd9_key$DiagV30 == '4800') # row 5149, start pneumonia
which(icd9_key$DiagV30 == '486') # row 5183, end pneumonia
# code df 
pneumonia_icd9 <- filter(icd9_key, n >= 5149 & n <= 5183) %>%
                  select(DiagV30)
# convert to vector
pneumonia_icd9 <- as.vector(as.matrix(pneumonia_icd9))

# make indicator of pneumonia
chars_2012_conf_df$pneum1 <- ifelse(chars_2012_conf_df$DIAG1 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum2 <- ifelse(chars_2012_conf_df$DIAG2 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum3 <- ifelse(chars_2012_conf_df$DIAG3 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum4 <- ifelse(chars_2012_conf_df$DIAG4 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum5 <- ifelse(chars_2012_conf_df$DIAG5 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum6 <- ifelse(chars_2012_conf_df$DIAG6 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum7 <- ifelse(chars_2012_conf_df$DIAG7 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum8 <- ifelse(chars_2012_conf_df$DIAG8 %in% pneumonia_icd9 , 1, 0)
chars_2012_conf_df$pneum9 <- ifelse(chars_2012_conf_df$DIAG9 %in% pneumonia_icd9 , 1, 0)

# sum up the indicators
chars_2012_conf_df$pneum_sum <- (chars_2012_conf_df$pneum1 + chars_2012_conf_df$pneum2 + 
                                 chars_2012_conf_df$pneum3 + chars_2012_conf_df$pneum4 +
                                 chars_2012_conf_df$pneum5 + chars_2012_conf_df$pneum6 +
                                 chars_2012_conf_df$pneum7 + chars_2012_conf_df$pneum8 +
                                 chars_2012_conf_df$pneum9)

chars_2012_conf_df$pneum_dx <- ifelse(chars_2012_conf_df$pneum_sum> 0, 1, 0)

# primary diagnosis
xtabs(~ pneum1, chars_2012_conf_df)
# any diagnosis
xtabs(~ pneum_dx, chars_2012_conf_df)

# Acute bronchitis ICD-9 codes 466 to 466.19 -----------------------------------
which(icd9_key$DiagV30 == '4660') # row 5090, start acute bronch
which(icd9_key$DiagV30 == '46619') # row 5092, end acute bronch

# code df of acute bronchitis codes
acute_bronch_icd9 <- filter(icd9_key, n >= 5090 & n <= 5092) %>% select(DiagV30)
# convert to vector
acute_bronch_icd9 <- as.vector(as.matrix(acute_bronch_icd9))  

# code variables of acute bronchitis
chars_2012_conf_df <- mutate(chars_2012_conf_df, 
  acute_bronch1 = ifelse(DIAG1 %in% acute_bronch_icd9, 1, 0),
  acute_bronch2 = ifelse(DIAG2 %in% acute_bronch_icd9, 1, 0),
  acute_bronch3 = ifelse(DIAG3 %in% acute_bronch_icd9, 1, 0),
  acute_bronch4 = ifelse(DIAG4 %in% acute_bronch_icd9, 1, 0),
  acute_bronch5 = ifelse(DIAG5 %in% acute_bronch_icd9, 1, 0),
  acute_bronch6 = ifelse(DIAG6 %in% acute_bronch_icd9, 1, 0),
  acute_bronch7 = ifelse(DIAG7 %in% acute_bronch_icd9, 1, 0),
  acute_bronch8 = ifelse(DIAG8 %in% acute_bronch_icd9, 1, 0),
  acute_bronch9 = ifelse(DIAG9 %in% acute_bronch_icd9, 1, 0),
  acute_bronch_sum = (acute_bronch1 + acute_bronch2 + acute_bronch3 + 
                      acute_bronch4 +acute_bronch5 + acute_bronch6 + 
                      acute_bronch7 + acute_bronch8 +acute_bronch9), 
  acute_bronch_dx = ifelse(acute_bronch_sum > 0, 1, 0))

# primary diagnosis (2769 events)
xtabs(~ acute_bronch1, chars_2012_conf_df)
# any diagnosis
xtabs(~ acute_bronch_dx, chars_2012_conf_df)
# check coding outcome
check <- filter(chars_2012_conf_df, acute_bronch1 == 1)
xtabs(~DIAG1 + acute_bronch1, check)

# COPD, ICD9 490 to 492, 494, and 496 ------------------------------------------
copd_icd9 <- c('490', '4910','4911','49120','49121','49122','4918','4919', '4920',
             '4928', '4940', '4941', '496') 

# now can I make a new variable, copdn, that indicates an asthma claim?
chars_2012_conf_df$copd1 <- ifelse(chars_2012_conf_df$DIAG1 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd2 <- ifelse(chars_2012_conf_df$DIAG2 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd3 <- ifelse(chars_2012_conf_df$DIAG3 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd4 <- ifelse(chars_2012_conf_df$DIAG4 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd5 <- ifelse(chars_2012_conf_df$DIAG5 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd6 <- ifelse(chars_2012_conf_df$DIAG6 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd7 <- ifelse(chars_2012_conf_df$DIAG7 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd8 <- ifelse(chars_2012_conf_df$DIAG8 %in% copd_icd9, 1, 0)
chars_2012_conf_df$copd9 <- ifelse(chars_2012_conf_df$DIAG9 %in% copd_icd9, 1, 0)

# sum up the copd indicators
chars_2012_conf_df$copd_sum <- (chars_2012_conf_df$copd1 + chars_2012_conf_df$copd2 + 
                                    chars_2012_conf_df$copd3 + chars_2012_conf_df$copd4 +
                                    chars_2012_conf_df$copd5 + chars_2012_conf_df$copd6 +
                                    chars_2012_conf_df$copd7 + chars_2012_conf_df$copd8 +
                                    chars_2012_conf_df$copd9)
chars_2012_conf_df$copd_dx <- ifelse(chars_2012_conf_df$copd_sum > 0, 1, 0)

# check copd coding
xtabs(~copd_dx, chars_2012_conf_df)

58541/685802 # 8.5% of claims for copd

8542/685802 # 1.2% of claims for primary copd

copd_claims <- filter(chars_2012_conf_df, DIAG1 %in% copd_icd9)
copd_claims$DIAG1 <- as.factor(copd_claims$DIAG1)
# check asthma claims subset
summary(copd_claims$DIAG1)
rm(copd_claims) # remove to save space

# might want to consider just looking at 491.21, chronic bronch with acute exacerbation

# codes: 491.21, 491.22, 494.1
copd_exacerbation <- c('49121','49122','4941')

# copd_exacerbation
chars_2012_conf_df$copd_ex1 <- ifelse(chars_2012_conf_df$DIAG1 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex2 <- ifelse(chars_2012_conf_df$DIAG2 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex3 <- ifelse(chars_2012_conf_df$DIAG3 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex4 <- ifelse(chars_2012_conf_df$DIAG4 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex5 <- ifelse(chars_2012_conf_df$DIAG5 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex6 <- ifelse(chars_2012_conf_df$DIAG6 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex7 <- ifelse(chars_2012_conf_df$DIAG7 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex8 <- ifelse(chars_2012_conf_df$DIAG8 %in% copd_exacerbation, 1, 0)
chars_2012_conf_df$copd_ex9 <- ifelse(chars_2012_conf_df$DIAG9 %in% copd_exacerbation, 1, 0)

# sum up the copd indicators
chars_2012_conf_df$copd_ex_sum <- (chars_2012_conf_df$copd_ex1 + chars_2012_conf_df$copd_ex2 + 
                                  chars_2012_conf_df$copd_ex3 + chars_2012_conf_df$copd_ex4 +
                                  chars_2012_conf_df$copd_ex5 + chars_2012_conf_df$copd_ex6 +
                                  chars_2012_conf_df$copd_ex7 + chars_2012_conf_df$copd_ex8 +
                                  chars_2012_conf_df$copd_ex9)
chars_2012_conf_df$copd_ex_dx <- ifelse(chars_2012_conf_df$copd_ex_sum > 0, 1, 0)

# check
xtabs(~ copd_ex1 , chars_2012_conf_df)
7891/736452

# # Common cold ------------------------------------------------------------------
# # Commenting out cold code
# cold_sinusitis_icd9 <- c('460', '4600', '4611', '4612', '4613', '4618', '4619')
# 
# cold_claims <- filter(chars_2012_conf_df, DIAG1 == '460' | DIAG2 == '460') # not many common cold claims
# # stick to cold and sinusitis
# # now can I make a new variable, cold_sinusitisn, that indicates an asthma claim?
# chars_2012_conf_df$cold_sinusitis1 <- ifelse(chars_2012_conf_df$DIAG1 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis2 <- ifelse(chars_2012_conf_df$DIAG2 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis3 <- ifelse(chars_2012_conf_df$DIAG3 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis4 <- ifelse(chars_2012_conf_df$DIAG4 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis5 <- ifelse(chars_2012_conf_df$DIAG5 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis6 <- ifelse(chars_2012_conf_df$DIAG6 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis7 <- ifelse(chars_2012_conf_df$DIAG7 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis8 <- ifelse(chars_2012_conf_df$DIAG8 %in% cold_sinusitis_icd9, 1, 0)
# chars_2012_conf_df$cold_sinusitis9 <- ifelse(chars_2012_conf_df$DIAG9 %in% cold_sinusitis_icd9, 1, 0)
# 
# # sum up the cold_sinusitis indicators
# chars_2012_conf_df$cold_sinusitis_sum <- (chars_2012_conf_df$cold_sinusitis1 + chars_2012_conf_df$cold_sinusitis2 + 
#                                   chars_2012_conf_df$cold_sinusitis3 + chars_2012_conf_df$cold_sinusitis4 +
#                                   chars_2012_conf_df$cold_sinusitis5 + chars_2012_conf_df$cold_sinusitis6 +
#                                   chars_2012_conf_df$cold_sinusitis7 + chars_2012_conf_df$cold_sinusitis8 +
#                                   chars_2012_conf_df$cold_sinusitis9)
# chars_2012_conf_df$cold_sinusitis_dx <- ifelse(chars_2012_conf_df$cold_sinusitis_sum > 0, 1, 0)
# 
# 
# xtabs(~ cold_sinusitis_dx, chars_2012_conf_df)
# 748/743595 # very rare outcome
# 
# # cold_sinusitis check
# cold_claims <- filter(chars_2012_conf_df, DIAG1 %in% cold_sinusitis_icd9)
# cold_claims$DIAG1 <- as.factor(cold_claims$DIAG1)
# # check asthma claims subset
# summary(cold_claims$DIAG1)
# rm(cold_claims)
# 
# # coding allergic rhinitis due other allergen and cause unspecified
# allergic_rhin <- c('4778', '4779')
# allergy_claims <- filter(chars_2012_conf_df, DIAG1 %in% allergic_rhin)
# allergy_claims$DIAG1 <- as.factor(allergy_claims$DIAG1)
# # very few claims (probably not worth looking at)

# Cardiovascular Diseases (general CVD), ICD-9: 390 to 459 ---------------------
which(icd9_key$DiagV30 == '390') # starts at 4593, rheumatic fever
which(icd9_key$DiagV30 == '4599') # ends at 5066, unsp circulatory disorder

# code df of stroke codes 
# code check
icd9_check <- filter(icd9_key, n >= 4593 & n <= 5066)
icd9_check # check codes

cvd_icd9 <- filter(icd9_key, n >= 4593 & n <= 5066) %>%
                  select(DiagV30)
# convert to vector
cvd_icd9 <- as.vector(as.matrix(cvd_icd9)) 

# make indicator of cvd_icd9
chars_2012_conf_df$cvd1 <- ifelse(chars_2012_conf_df$DIAG1 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd2 <- ifelse(chars_2012_conf_df$DIAG2 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd3 <- ifelse(chars_2012_conf_df$DIAG3 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd4 <- ifelse(chars_2012_conf_df$DIAG4 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd5 <- ifelse(chars_2012_conf_df$DIAG5 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd6 <- ifelse(chars_2012_conf_df$DIAG6 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd7 <- ifelse(chars_2012_conf_df$DIAG7 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd8 <- ifelse(chars_2012_conf_df$DIAG8 %in% cvd_icd9 , 1, 0)
chars_2012_conf_df$cvd9 <- ifelse(chars_2012_conf_df$DIAG9 %in% cvd_icd9 , 1, 0)

# sum up the indicators
chars_2012_conf_df$cvd_sum <- (chars_2012_conf_df$cvd1 + chars_2012_conf_df$cvd2 + 
                               chars_2012_conf_df$cvd3 + chars_2012_conf_df$cvd4 +
                               chars_2012_conf_df$cvd5 + chars_2012_conf_df$cvd6 +
                               chars_2012_conf_df$cvd7 + chars_2012_conf_df$cvd8 +
                               chars_2012_conf_df$cvd9)

chars_2012_conf_df$cvd_dx <- ifelse(chars_2012_conf_df$cvd_sum> 0, 1, 0)

xtabs(~ cvd1, chars_2012_conf_df)

# Ischemic heart diseases, ICD-9 codes 410-413 ---------------------------------
which(icd9_key$DiagV30 == '41000') # starts at 4656, acute myo infarc
which(icd9_key$DiagV30 == '4139') # ends 4693, other us angina pectoris

# code df of ihd codes (includes mi)
ihd_icd9 <- filter(icd9_key, n >= 4656 & n <= 4693)
ihd_icd9 # check codes

ihd_icd9 <- filter(icd9_key, n >= 4656 & n <= 4693) %>% select(DiagV30)
# convert to vector
ihd_icd9 <- as.vector(as.matrix(ihd_icd9))  

# make indicator of ihd_icd9
chars_2012_conf_df$ihd1 <- ifelse(chars_2012_conf_df$DIAG1 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd2 <- ifelse(chars_2012_conf_df$DIAG2 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd3 <- ifelse(chars_2012_conf_df$DIAG3 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd4 <- ifelse(chars_2012_conf_df$DIAG4 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd5 <- ifelse(chars_2012_conf_df$DIAG5 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd6 <- ifelse(chars_2012_conf_df$DIAG6 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd7 <- ifelse(chars_2012_conf_df$DIAG7 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd8 <- ifelse(chars_2012_conf_df$DIAG8 %in% ihd_icd9 , 1, 0)
chars_2012_conf_df$ihd9 <- ifelse(chars_2012_conf_df$DIAG9 %in% ihd_icd9 , 1, 0)

# sum up the indicators
chars_2012_conf_df$ihd_sum <- (chars_2012_conf_df$ihd1 + chars_2012_conf_df$ihd2 + 
                                       chars_2012_conf_df$ihd3 + chars_2012_conf_df$ihd4 +
                                       chars_2012_conf_df$ihd5 + chars_2012_conf_df$ihd6 +
                                       chars_2012_conf_df$ihd7 + chars_2012_conf_df$ihd8 +
                                       chars_2012_conf_df$ihd9)

chars_2012_conf_df$ihd_dx <- ifelse(chars_2012_conf_df$ihd_sum> 0, 1, 0)

xtabs(~ ihd1, chars_2012_conf_df) # 11682 primary events

# Arrhythmias ICD-9 code 427 ---------------------------------------------------
which(icd9_key$DiagV30 == '4270') # starts at 4780, acute myo infarc
which(icd9_key$DiagV30 == '4279') # ends 4793, other us angina pectoris

# code df of arrhyth codes 
icd9_check <- filter(icd9_key, n >= 4780 & n <= 4793)
icd9_check # check codes

arrhythmia_icd9 <- filter(icd9_key, n >= 4780 & n <= 4793) %>%
                   select(DiagV30)
# convert to vector
arrhythmia_icd9 <- as.vector(as.matrix(arrhythmia_icd9))

# make indicator of arrhythmia_icd9
chars_2012_conf_df$arrhythmia1 <- ifelse(chars_2012_conf_df$DIAG1 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia2 <- ifelse(chars_2012_conf_df$DIAG2 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia3 <- ifelse(chars_2012_conf_df$DIAG3 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia4 <- ifelse(chars_2012_conf_df$DIAG4 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia5 <- ifelse(chars_2012_conf_df$DIAG5 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia6 <- ifelse(chars_2012_conf_df$DIAG6 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia7 <- ifelse(chars_2012_conf_df$DIAG7 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia8 <- ifelse(chars_2012_conf_df$DIAG8 %in% arrhythmia_icd9 , 1, 0)
chars_2012_conf_df$arrhythmia9 <- ifelse(chars_2012_conf_df$DIAG9 %in% arrhythmia_icd9 , 1, 0)

# sum up the indicators
chars_2012_conf_df$arrhythmia_sum <- (chars_2012_conf_df$arrhythmia1 + 
                                      chars_2012_conf_df$arrhythmia2 + 
              chars_2012_conf_df$arrhythmia3 + chars_2012_conf_df$arrhythmia4 +
              chars_2012_conf_df$arrhythmia5 + chars_2012_conf_df$arrhythmia6 +
              chars_2012_conf_df$arrhythmia7 + chars_2012_conf_df$arrhythmia8 +
              chars_2012_conf_df$arrhythmia9)

chars_2012_conf_df$arrhythmia_dx <- ifelse(chars_2012_conf_df$arrhythmia_sum> 0, 1, 0)

xtabs(~ arrhythmia_dx, chars_2012_conf_df)

# Heart Failure, ICD-9 428 -----------------------------------------------------
which(icd9_key$DiagV30 == '4280') # starts at 4794, congestive heart failure
which(icd9_key$DiagV30 == '4289') # ends at 4808, heart failure

# code df of hf codes 
# code check
icd9_check <- filter(icd9_key, n >= 4794 & n <= 4808)
icd9_check # check codes

hf_icd9 <- filter(icd9_key, n >= 4794 & n <= 4808) %>%
                   select(DiagV30)
# convert to vector
hf_icd9 <- as.vector(as.matrix(hf_icd9))  

# make indicator of hf_icd9
chars_2012_conf_df$hf1 <- ifelse(chars_2012_conf_df$DIAG1 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf2 <- ifelse(chars_2012_conf_df$DIAG2 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf3 <- ifelse(chars_2012_conf_df$DIAG3 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf4 <- ifelse(chars_2012_conf_df$DIAG4 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf5 <- ifelse(chars_2012_conf_df$DIAG5 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf6 <- ifelse(chars_2012_conf_df$DIAG6 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf7 <- ifelse(chars_2012_conf_df$DIAG7 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf8 <- ifelse(chars_2012_conf_df$DIAG8 %in% hf_icd9 , 1, 0)
chars_2012_conf_df$hf9 <- ifelse(chars_2012_conf_df$DIAG9 %in% hf_icd9 , 1, 0)

# sum up the indicators
chars_2012_conf_df$hf_sum <- (chars_2012_conf_df$hf1 + chars_2012_conf_df$hf2 + 
                                 chars_2012_conf_df$hf3 + chars_2012_conf_df$hf4 +
                                 chars_2012_conf_df$hf5 + chars_2012_conf_df$hf6 +
                                 chars_2012_conf_df$hf7 + chars_2012_conf_df$hf8 +
                                 chars_2012_conf_df$hf9)

chars_2012_conf_df$hf_dx <- ifelse(chars_2012_conf_df$hf_sum> 0, 1, 0)

xtabs(~ hf1, chars_2012_conf_df)

# Cerebrovascular Diseases, ICD-9 430-438 --------------------------------------
# Consider refining outcome definition to be more specific
which(icd9_key$DiagV30 == '430') # starts at 4823, brain hemorage
which(icd9_key$DiagV30 == '4389') # ends at 4891, late effects of cerevas dis

# code df of stroke codes 
# code check
icd9_check <- filter(icd9_key, n >= 4823 & n <= 4891)
icd9_check # check codes

cereb_vas_icd9 <- filter(icd9_key, n >= 4823 & n <= 4891) %>%
                  select(DiagV30)
# convert to vector
cereb_vas_icd9 <- as.vector(as.matrix(cereb_vas_icd9)) 

# make indicator of cereb_vas_icd9
chars_2012_conf_df$cereb_vas1 <- ifelse(chars_2012_conf_df$DIAG1 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas2 <- ifelse(chars_2012_conf_df$DIAG2 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas3 <- ifelse(chars_2012_conf_df$DIAG3 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas4 <- ifelse(chars_2012_conf_df$DIAG4 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas5 <- ifelse(chars_2012_conf_df$DIAG5 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas6 <- ifelse(chars_2012_conf_df$DIAG6 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas7 <- ifelse(chars_2012_conf_df$DIAG7 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas8 <- ifelse(chars_2012_conf_df$DIAG8 %in% cereb_vas_icd9 , 1, 0)
chars_2012_conf_df$cereb_vas9 <- ifelse(chars_2012_conf_df$DIAG9 %in% cereb_vas_icd9 , 1, 0)

# sum up the indicators
chars_2012_conf_df$cereb_vas_sum <- (chars_2012_conf_df$cereb_vas1 + chars_2012_conf_df$cereb_vas2 + 
                                        chars_2012_conf_df$cereb_vas3 + chars_2012_conf_df$cereb_vas4 +
                                        chars_2012_conf_df$cereb_vas5 + chars_2012_conf_df$cereb_vas6 +
                                        chars_2012_conf_df$cereb_vas7 + chars_2012_conf_df$cereb_vas8 +
                                        chars_2012_conf_df$cereb_vas9)

chars_2012_conf_df$cereb_vas_dx <- ifelse(chars_2012_conf_df$cereb_vas_sum> 0, 1, 0)

xtabs(~ cereb_vas1, chars_2012_conf_df)

# Myocardial Infarction ICD-9 code 410.00 - 410.92 -----------------------------
summary(as.factor(chars_2012_conf_df$DIAG1))
# it looks like there is initial episodes of care and subsequent episoes
# might nee to split these out (looks like if it ends in 2 then subsequent)
mi_icd9 <- c("41000", "41001", "41002", "41010", "41011", "41012", "41020", 
             "41021", "41022", "41030", "41031", "41032", "41040", "41041",
             "41042", "41050", "41051", "41052", "41060", "41061", "41062",
             "41070", "41071", "41072", "41080", "41081", "41082", "41090",
             "41091", "41092")
mi_icd9 # vector of MI icd-9 codes

# variables indicating MI claim
chars_2012_conf_df$mi1 <- ifelse(chars_2012_conf_df$DIAG1 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi2 <- ifelse(chars_2012_conf_df$DIAG2 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi3 <- ifelse(chars_2012_conf_df$DIAG3 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi4 <- ifelse(chars_2012_conf_df$DIAG4 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi5 <- ifelse(chars_2012_conf_df$DIAG5 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi6 <- ifelse(chars_2012_conf_df$DIAG6 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi7 <- ifelse(chars_2012_conf_df$DIAG7 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi8 <- ifelse(chars_2012_conf_df$DIAG8 %in% mi_icd9, 1, 0)
chars_2012_conf_df$mi9 <- ifelse(chars_2012_conf_df$DIAG9 %in% mi_icd9, 1, 0)

# sum up the mi indicators
chars_2012_conf_df$mi_sum <- (chars_2012_conf_df$mi1 + chars_2012_conf_df$mi2 + 
                        chars_2012_conf_df$mi3 + chars_2012_conf_df$mi4 +
                        chars_2012_conf_df$mi5 + chars_2012_conf_df$mi6 +
                        chars_2012_conf_df$mi7 + chars_2012_conf_df$mi8 +
                        chars_2012_conf_df$mi9)
chars_2012_conf_df$mi_dx <- ifelse(chars_2012_conf_df$mi_sum > 0, 1, 0)

# check if the binary mi_dx code aligns with the sum

mi_dx_check <- table(chars_2012_conf_df$mi_dx, chars_2012_conf_df$mi_sum)
mi_dx_check

summary(as.factor(chars_2012_conf_df$mi1))
# seems to do the same thing, but both codes could be incorrect. i should check
# the number of mi claims in diagnoses 1 (10,459 claims)
mi_claims <- subset(chars_2012_conf_df, DIAG1 %in% mi_icd9)
# check asthma claims subset
summary(as.factor(mi_claims$DIAG1))
# very few with 2 designation indicating subsequent visit, but I still might
# need to remove due to possible misclassification

# create an MI coding that excludes subsequent episodes
mi_initialcare_icd9 <- c("41000", "41001", "41010", "41011", "41020", "41021", 
                         "41030", "41031", "41040", "41041", "41050", "41051", 
                         "41060", "41061", "41070", "41071", "41080", "41081",
                         "41090", "41091")
# make indicator of initial mi
chars_2012_conf_df$mi_prime1 <- ifelse(chars_2012_conf_df$DIAG1 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime2 <- ifelse(chars_2012_conf_df$DIAG2 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime3 <- ifelse(chars_2012_conf_df$DIAG3 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime4 <- ifelse(chars_2012_conf_df$DIAG4 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime5 <- ifelse(chars_2012_conf_df$DIAG5 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime6 <- ifelse(chars_2012_conf_df$DIAG6 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime7 <- ifelse(chars_2012_conf_df$DIAG7 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime8 <- ifelse(chars_2012_conf_df$DIAG8 %in% mi_initialcare_icd9 , 1, 0)
chars_2012_conf_df$mi_prime9 <- ifelse(chars_2012_conf_df$DIAG9 %in% mi_initialcare_icd9 , 1, 0)

# sum up the mi indicators
chars_2012_conf_df$mi_prime_sum <- (chars_2012_conf_df$mi_prime1 + chars_2012_conf_df$mi_prime2 + 
                              chars_2012_conf_df$mi_prime3 + chars_2012_conf_df$mi_prime4 +
                              chars_2012_conf_df$mi_prime5 + chars_2012_conf_df$mi_prime6 +
                              chars_2012_conf_df$mi_prime7 + chars_2012_conf_df$mi_prime8 +
                              chars_2012_conf_df$mi_prime9)
chars_2012_conf_df$mi_prime_dx <- ifelse(chars_2012_conf_df$mi_prime_sum > 0, 1, 0)

# check if the binary mi_dx code aligns with the sum
xtabs(~chars_2012_conf_df$mi_prime_dx + chars_2012_conf_df$mi_prime_sum)

# check difference in n for mi_dx vs mi_prime_dx
xtabs(~chars_2012_conf_df$mi_prime_dx + chars_2012_conf_df$mi_dx)



summary(chars_2012_conf_df)


# Coding in a outcomes that should not be associated with smoke ----------------

# Broken Arm ICD-9 813 ---------------------------------------------------------
which(icd9_key$DiagV30 == '81300') # starts at 10094, fracture of radius and ulna
which(icd9_key$DiagV30 == '81393') # ends at 10140, late effects of cerevas dis

# code df of broken arms
# code check
icd9_check <- filter(icd9_key, n >= 10094 & n <= 10140)
icd9_check # check codes

broken_arm_icd9 <- filter(icd9_key, n >= 10094 & n <= 10140) %>%
                   select(DiagV30)

# convert to vector
broken_arm_icd9 <- as.vector(as.matrix(broken_arm_icd9)) 

# code variables of broken arm
chars_2012_conf_df <- mutate(chars_2012_conf_df, 
  broken_arm1 = ifelse(DIAG1 %in% broken_arm_icd9, 1, 0),
  broken_arm2 = ifelse(DIAG2 %in% broken_arm_icd9, 1, 0),
  broken_arm3 = ifelse(DIAG3 %in% broken_arm_icd9, 1, 0),
  broken_arm4 = ifelse(DIAG4 %in% broken_arm_icd9, 1, 0),
  broken_arm5 = ifelse(DIAG5 %in% broken_arm_icd9, 1, 0),
  broken_arm6 = ifelse(DIAG6 %in% broken_arm_icd9, 1, 0),
  broken_arm7 = ifelse(DIAG7 %in% broken_arm_icd9, 1, 0),
  broken_arm8 = ifelse(DIAG8 %in% broken_arm_icd9, 1, 0),
  broken_arm9 = ifelse(DIAG9 %in% broken_arm_icd9, 1, 0),
  broken_arm_sum = (broken_arm1 + broken_arm2 + broken_arm3 + 
                      broken_arm4 +broken_arm5 + broken_arm6 + 
                      broken_arm7 + broken_arm8 +broken_arm9), 
  broken_arm_dx = ifelse(broken_arm_sum > 0, 1, 0))

# primary diagnosis (2769 events)
xtabs(~ broken_arm1, chars_2012_conf_df)
# any diagnosis
xtabs(~ broken_arm_dx, chars_2012_conf_df)
# check coding outcome
check <- filter(chars_2012_conf_df, broken_arm1 == 1)
xtabs(~DIAG1 + broken_arm1, check)

# Rheumatoid arthritis ICD-9 714 -----------------------------------------------------
which(icd9_key$DiagV30 == '7140') # 7782
which(icd9_key$DiagV30 == '7149') # 7792

# code df of broken arms
# code check
icd9_check <- filter(icd9_key, n >= 7782 & n <= 7792)
icd9_check # check codes

ra_icd9 <- filter(icd9_key, n >= 7782 & n <= 7792) %>%
                   select(DiagV30)

# convert to vector
ra_icd9 <- as.vector(as.matrix(ra_icd9)) 

# code variables of broken arm
chars_2012_conf_df <- mutate(chars_2012_conf_df, 
  ra1 = ifelse(DIAG1 %in% ra_icd9, 1, 0),
  ra2 = ifelse(DIAG2 %in% ra_icd9, 1, 0),
  ra3 = ifelse(DIAG3 %in% ra_icd9, 1, 0),
  ra4 = ifelse(DIAG4 %in% ra_icd9, 1, 0),
  ra5 = ifelse(DIAG5 %in% ra_icd9, 1, 0),
  ra6 = ifelse(DIAG6 %in% ra_icd9, 1, 0),
  ra7 = ifelse(DIAG7 %in% ra_icd9, 1, 0),
  ra8 = ifelse(DIAG8 %in% ra_icd9, 1, 0),
  ra9 = ifelse(DIAG9 %in% ra_icd9, 1, 0),
  ra_sum = (ra1 + ra2 + ra3 + ra4 +ra5 + ra6 + ra7 + ra8 +ra9), 
  ra_dx = ifelse(ra_sum > 0, 1, 0))

# primary diagnosis (2769 events)
xtabs(~ ra1, chars_2012_conf_df)
# any diagnosis
xtabs(~ ra_dx, chars_2012_conf_df)
# check coding outcome
check <- filter(chars_2012_conf_df, ra1 == 1)
xtabs(~DIAG1 + ra1, check)


# end outcome coding for ICD-9 codes -------------------------------------------

summary(chars_2012_conf_df)

# one person is missing a patient ID, likely same person SEQ_NO 2012079960
glimpse(parse_check)

miss_check <- filter(chars_2012_conf_df, is.na(PATIENTID))
glimpse(miss_check)
# Yes same person. Missing only 1 patient ID out of thousands is really good. 

# Creating a Permanent DataFrame -----------------------------------------------
# write a permanent chars confidential dataset
write_path <- paste0('C:/Users/RGan/Documents/CSU/Wild Fire/',
       'Washington St CHARS Data/confidential_data/chars_2012_confidential.csv')

write_csv(chars_2012_conf_df, write_path)



