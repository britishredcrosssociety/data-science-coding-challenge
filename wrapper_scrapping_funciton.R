# Wrapper Function to Download and embed ONS Data
#    ONS Local Authority boundaries
#    Data housed in raw-data directory
#    All data used is available under the Open Government Licence v3.0, from the ONS Census 2021.
#    Open Government Licence v3.0 - https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
# Version - 1
# Date - 13/10/2023
# Author - A R Vaughan
# 
# Data Analysed in the Shiny App is from the Office for National Statistics.
# Source: Office for National Statistics licensed under the Open Government Licence v.3.

# load Library's
library(magrittr)
library(tidyverse)

# source script 1 - load ONS data from web (Accommodation + Central Heating)
source("./scripts/ONS_Raw_Data_Downloader.R")

# source script 2 - load Local Authority Shape data
source("./scripts/ONS_Local_Authority_Boundary_extracter_v1.r")

# source script 3 - clean and merge raw ONS data
source("./scripts/census_data_merge.r")

# source script 4 - inital analysis of merged ONS data
source("./scripts/Census_Analysis_1_v1.r")






