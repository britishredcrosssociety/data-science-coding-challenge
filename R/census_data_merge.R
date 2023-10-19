# Function to Extract and merge Census 2021 data from Bulk
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
library(tidyverse)
library(httr)
library(devtools)
library(purrr)

#LSOA level merge data files
# define directory search query and return files
link <- "./data-raw/LSOA"
files <- list.files(link,pattern = ".csv")

# load raw Cenus data frames to list
tmp <- paste0(link,"/",files) %>%
  lapply(.,read_csv)

# identify shared naming
names <- intersect(colnames(tmp[[1]]), colnames(tmp[[2]]))

# left join data frames together
tmp2 <- Reduce(function(x, y)
  left_join(x, y, by = names), tmp)

# export merge data
tmp2 %>% saveRDS(.,file="./data/census_2021_lsoa_raw_data.rds")

# clean workspace
rm(link,files,tmp,tmp2, names)


#---2. Local Authority merge data files
# define directory search query
link <- "./data-raw/LTLA"
files <- list.files(link,pattern = ".csv")

# load raw Cenus data frames to list
tmp <- paste0(link,"/",files) %>%
  lapply(.,read_csv)

# identify shared naming
names <- intersect(colnames(tmp[[1]]), colnames(tmp[[2]]))

# left join data frames together
tmp2 <- Reduce(function(x, y)
  left_join(x, y, by = names), tmp)

# export merge data
tmp2 %>% saveRDS(.,file="./data/census_2021_ltla_raw_data.rds")

# clean workspace
rm(link,files,tmp,tmp2, names)