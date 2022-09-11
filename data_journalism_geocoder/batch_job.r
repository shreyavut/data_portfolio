#essential libraries
library(tidyverse)
library(janitor)
library(lubridate)
#additional libraries 
library(sf)
library(tigris)
library(tidycensus)
library(censusxy)


## census api key
census_api_key("82c33dd2db316b78367d3476bcb90d2abe65a69a", install = "TRUE", overwrite = TRUE)


police_shootings <- read_csv("data/data-police-shootings-master/fatal-police-shootings-data.csv") %>%
  filter(!is.na(longitude)) 

## ok so it seems like we can only do one lat long at a time so... for loop? idk lets try

## first create empty data frame

police_shootings_sf <- tibble()

#for (row_number in 1:nrow(police_shootings)) {
  
### NOTE: This will only produce the first 6,000 records. For the last 410 records, we will need to replace the "1:6000" chunk with "6001:6410. 
for (row_number in 1:6000) {

  #this makes a dataframe for each
  row_df<- police_shootings %>%
    slice(row_number)
  
  #store lat and long values
  longitude <- row_df$longitude
  latitude <- row_df$latitude
  census_results <- cxy_geography(longitude, latitude, benchmark = "Public_AR_Current",
                                  vintage = "ACS2019_Current") 
  
  if (!is.null(census_results)) {
  
  census_results <- census_results %>%
    select(Census.Tracts.GEOID) %>%
    clean_names()
  
  row_df <- row_df %>%
    bind_cols(census_results) 
  
  
  #binding some rows
  police_shootings_sf <- police_shootings_sf %>%
    bind_rows(row_df) 
  
  print(paste0("finished ", row_number, " ", Sys.time()))
  
    if (row_number%%500 == 0) {
      filepath <- paste0("data/new_geocoded_results_", row_number, ".rds")
      write_rds(police_shootings_sf, filepath)
      police_shootings_sf <- as_tibble()
      
  
    }
  } else { 
    
    print(paste0("No geocode found", row_number, " ", Sys.time()))
    
    }
}
