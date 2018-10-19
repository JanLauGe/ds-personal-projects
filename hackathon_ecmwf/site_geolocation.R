library(tidyverse)
library(jsonlite)

setwd('C:/Users/Laurens/Dropbox/projects/ecmwf/code/ReadingBus/Data')

# Getting locations from claims data
claims <- read_csv('busClaims.csv')

claimsloc <- claims %>%
  {.$`Place of Event`}

# Frequency of accidents by location
claimsloc %>%
  table %>%
  sort(decreasing = TRUE)

# Function to find location via google maps api
geocodeAdddress <- function(location) {
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, location, "&sensor=false", sep = ""))
  x <- fromJSON(url)
  if (x$status == "OK") {
    out <- c(x$results[1,]$geometry$location$lng,
             x$results[1,]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}


# Test the function
geocodeAdddress(paste0(claimsloc[1], ', Reading'))

# Run for all unique elements
all_locs <- lapply(
  X = unique(claimsloc)[1:10],
  FUN = function(x) 
    geocodeAdddress(paste0(x, ', Reading')))


# Reshape into data frame
claimsloc_geo <- data_frame(
  name = colnames(all_locs),
  lat = all_locs[2,],
  lon = all_locs[1,])

# Join back onto claims
bla <- left_join(claims,
          claimsloc_geo,
          by = c('Place of Event' = 'name'))
