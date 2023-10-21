# Function to Download and embed ONS Data
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
library(rvest)
library(httr)

# test working directory for pre-existing data
tmp <- list.files("./data-raw/LSOA")

#test for LSOA data, if False Run
if(length(tmp)==0){
  
  # Define the URL of the ZIP file to download
  zip_url1 <- "https://www.nomisweb.co.uk/output/census/2021/census2021-ts044.zip"
  zip_url2 <- "https://www.nomisweb.co.uk/output/census/2021/census2021-ts046.zip"
  
  # Define the local file path where you want to save the ZIP file
  zip_file1 <- "/data-raw/LSOA/tmp1.zip"
  zip_file2 <- "/data-raw/LSOA/tmp2.zip"
  
  # Download the ZIP file
  download.file(zip_url1, zip_file1, mode = "wb")
  download.file(zip_url2, zip_file2, mode = "wb")
  
  # Extract the contents of the ZIP file to a target directory
  target_directory <- "./data-raw/LSOA"
  unzip(zip_file1, exdir = target_directory)
  unzip(zip_file2, exdir = target_directory)
  
  # Delete non LSOA files
  tmp_rf <- list.files("./data-raw/LSOA",pattern = ".csv")
  tmp_rf <- tmp_rf[-which(grepl("-lsoa",tmp_rf)==T)] %>%
    paste0("./data-raw/LSOA/",.)
  file.remove(tmp_rf)
  
  #remove Meta folder
  unlink("./data-raw/LSOA/metadata", recursive = TRUE)
  
}

#test for LTLA data, if False Run
if(length(tmp)==0){
  
  # Define the URL of the ZIP file to download
  zip_url1 <- "https://www.nomisweb.co.uk/output/census/2021/census2021-ts044.zip"
  zip_url2 <- "https://www.nomisweb.co.uk/output/census/2021/census2021-ts046.zip"
  
  # Define the local file path where you want to save the ZIP file
  zip_file1 <- "./data-raw/LTLA/tmp1.zip"
  zip_file2 <- "./data-raw/LTLA/tmp2.zip"
  
  # Download the ZIP file
  download.file(zip_url1, zip_file1, mode = "wb")
  download.file(zip_url2, zip_file2, mode = "wb")
  
  # Extract the contents of the ZIP file to a target directory
  target_directory <- "./data-raw/LTLA"
  unzip(zip_file1, exdir = target_directory)
  unzip(zip_file2, exdir = target_directory)
  
  # Delete non LSOA files
  tmp_rf <- list.files("./data-raw/LTLA",pattern = ".csv")
  tmp_rf <- tmp_rf[-which(grepl("-ltla",tmp_rf)==T)] %>%
    paste0("./data-raw/LTLA/",.)
  file.remove(tmp_rf)
  
  #remove Meta folder
  unlink("./data-raw/LTLA/metadata", recursive = TRUE)
  
}
