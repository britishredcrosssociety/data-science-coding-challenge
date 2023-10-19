#' Analysis 1 - Local Authority and LSOA data merges
#'    Data housed in data directory
#'    All data used is available under the Open Government Licence v3.0, from the ONS Census 2021.
#'    Open Government Licence v3.0 - https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#' Version - 1
#' Date - 13/10/2023
#' Author - A R Vaughan
#' 
#' Data Analysed in the Shiny App is from the Office for National Statistics.
#' Source: Office for National Statistics licensed under the Open Government Licence v.3.

# load Library's
library(tidyverse)
library(openair)
library(purrr)
library(janitor)

# load merged data sets and clean
tmp <- readRDS("./data/census_2021_lsoa_raw_data.rds") #LSOA level data
tmp %<>% clean_names() %>%
  select(!date) %>% select(-matches(c("total","code")))

tmp2 <- readRDS("./data/census_2021_ltla_raw_data.rds") #Local Authority level data
tmp2 %<>% clean_names() %>%
  select(!date) %>% select(-matches(c("total","code")))

#--- Analysis one LSOA data
a1 <- tmp %>% gather(variable,value,-geography) %>%
  mutate(type=NA) %>%
  mutate(type=replace(type,grepl("accommodation",variable),"accommodation"),
         type=replace(type,grepl("heating",variable),"heating")) %>%
  mutate(variable = variable %>%
           gsub("accommodation_type_","",.) %>%
           gsub("type_of_central_heating_in_","",.))

# convert values from count to percentage
a2 <- a1 %>% group_by(geography,type) %>%
  group_modify(~mutate(., percentage = value/sum(value)*100)) %>%
  ungroup() %>%
  filter(!value==0)

# load LSOA to ward link data and merge, returning all distinct values
WD <- read_csv("./data-raw/LSOA_to_Ward_Lookup.csv")
WD <- WD %>% select(LSOA21NM,WD22NM) %>%
  rename(geography=LSOA21NM,ward=WD22NM) %>%
  distinct(geography,.keep_all = T)

#join by geography markers (Ward)
a2 <- a2 %>% left_join(.,WD,by="geography")

# load LSOA to Local Authority link data and merge
LA <- read_csv("./data-raw/LSOA_to_LTLA_Lookup.csv")
LA <- LA %>% select(LSOA21NM,LAD22NM) %>%
  rename(geography=LSOA21NM,local_authority=LAD22NM) %>%
  distinct(geography,.keep_all = T)

#join by geography markers (Local Authority)
a2 <- a2 %>% left_join(.,LA,by="geography")

# load Local Authority to Regional/Country link data and merge
RL <- read_csv("./data-raw/LTLA_to_Regional_Lookup.csv")
RL <- RL %>% select(LAD21NM,RGN21NM,CTRY21NM,) %>%
  rename(local_authority=LAD21NM,
         region=RGN21NM,
         country=CTRY21NM) %>%
  distinct(local_authority,.keep_all = T)

a2 <- a2 %>% left_join(.,RL,by="local_authority")

#--- Analysis two LTLA data
b1 <- tmp2 %>% gather(variable,value,-geography) %>%
  mutate(type=NA) %>%
  mutate(type=replace(type,grepl("accommodation",variable),"accommodation"),
         type=replace(type,grepl("heating",variable),"heating")) %>%
  mutate(variable = variable %>%
           gsub("accommodation_type_","",.) %>%
           gsub("type_of_central_heating_in_","",.))

# convert values from count to percentage
b2 <- b1 %>% group_by(geography,type) %>%
  group_modify(~mutate(., percentage = value/sum(value)*100)) %>%
  ungroup() %>%
  filter(!value==0) %>%
  rename(local_authority=geography)

#join by geography markers (Region and country)
b2 <- b2 %>% left_join(.,RL,by="local_authority")

#export cleaning datasets to RDS format
a2 %>% saveRDS(.,file="./data/LSOA/census_2021_LSOA_data.rds") #LSOA level merge
b2 %>% saveRDS(.,file="./data/LTLA/census_2021_LTLA_data.rds") #Local Authority level merge

#clean workspace
rm(tmp,tmp2,a1,a2,b1,b2)
rm(WD,LA,RL)


