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
tmp <- list.files("./data/LA_boundary")

#test for data, if False Run
if(length(tmp)==0){
  
  # Define the URL of the ZIP file to download
  zip_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_Dec_2021_GB_BFC_2022/FeatureServer/replicafilescache/LAD_Dec_2021_GB_BFC_2022_8993715418694210345.zip"
  
  # Define the local file path where you want to save the ZIP file
  zip_file <- "./data/LA_boundary/tmp.zip"
  
  # Download the ZIP file
  download.file(zip_url, zip_file, mode = "wb")
  
  # Extract the contents of the ZIP file to a target directory
  target_directory <- "./data/LA_boundary"
  unzip(zip_file, exdir = target_directory)
  
  # Delete the downloaded ZIP file
  file.remove(zip_file)
  
}
