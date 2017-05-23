# Association between smoke exposure and hospital admissions during the 2012 wildfire season in Washington
## Created and maintained by: Ryan Gan
## Created on: 9/28/2016
## Last Major Update: 5/23/2016

If you have any questions or issues regarding this repository, please contact me via GitHub or the cooresponding author contact in the manuscript below.

### Overview

This GitHub repository contains R code and most files relevant to the NASA wildfire smoke project and is specific to our work on assessing the relationship between wildfire smoke exposure and hospital-related morbidity in Washington State during the 2012 wildfire season. This Git repository contains code and some data used in the manuscript *Comparison of wildfire smoke estimation methods and associations with cardiopulmonary-related hosptial admissions* found online and opensource here: http://onlinelibrary.wiley.com/doi/10.1002/2017GH000073/full. (Link will be updated once final version comes out).

#### Quick Method Outline

The general aim of this project was to assess if there was any association between increasing smoke measured by particulate matter 2.5 ug/m^3 or less (PM2.5) and cardiopulmonary emergency department or urgent care visits in the state of Washington during the 2012 wildfire season. 

The general approach was to use observations from sattellite, ground measurements, and chemical weather models to assess PM2.5 due to wildfire smoke at a given location. We then population-weighted these estimates to the ZIP code level as this was the finest spatial resolution available to us for our health data. We then created time-stratified case-cross over dataframes for each cardiopulmonary outcome of interest. We then compared within subject variability of PM2.5 due to wildfire smoke on admission days for the outcome event compared to referent days where the individual was assumed not to have an event. 

All code to carry out this method outline is included herein. The code and data used to create the original estimates of wildfire smoke PM2.5 can be found at the Colorado State University Atmospheric Science Department's data repository: https://dspace.library.colostate.edu/handle/10217/179811

*Note: This project uses protected health information (i.e. admission date of a patient and reported ZIP code) covered by the Health Information Portability and Accountability Act. Therefore, Washington Comprehensive Hospital Abstract Reporting System (CHARS) data are not available due to data use agreements with Washington State Health Department.*

### Folder Descriptions

This repository has two main folders: data and r_scripts. 

#### r_scripts 

This folder contains three folders of scripts for the R statistical computing language. 

**/r_scripts/analysis/**
1. washington_results_zip_manuscript.Rmd: *This is an R markdown file that contains the code used to produce all the results in the manuscript. The html output is also included in this folder.*

**/r_scripts/data_management/**
1. chars_2012_data_management.R: *This R file contains general exploratory statistics for the CHARS 2012 dataset and creates age and race categories*.
2. chars_2012_outcome_data_management.R: *This R file uses the .csv file created in chars_2012_data_management.R and creates binary cardiopulmonary outcomes using ICD-9 codes.*
3. chars_2012_smoke_jul_oct_time_stratified_case_crossover.R: *This R file uses the cardiopulmonary outcomes identified and creates time-stratified case-crossover dataframes for each outcome of interest and then joins in smoke estimates. These dataframes are used in the results markdown file, but cannot be included on this GitRepo due to HIPAA requirements.*

**/r_scripts/smoke/**
1. proportion_intersection_zip_wrfgrid_wash_2012.R: *This R file calculates the proportion intersection between the WRF-Grid and Washington ZIP code shape files. The proportion intersection is then used to calculate the ZIP code population-weighted smoke estimates.*
2. zip_population_weighted_estimates_wash_2012.R: *This R file calculates the population-weighted smoke PM2.5 estimates for each ZIP code on each day in the smoke season. Estimates produced by this script are then joined to the case-crossover dataframes.*
 
 
