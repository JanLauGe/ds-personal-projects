library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)

# Get Places -------------------------------------------------------------------

# Get list of places to investigate
clublist <- read.csv('clublist.csv')
clublist$query <- paste0(
  gsub(pattern = ' ', replacement = '+', x = clublist$Name), '+',
  gsub(pattern = ' ', replacement = '+', x = clublist$Location))

# Function for place info
PlaceAPI <- function(
  search_string) {

  # Hard-coded URLs
  search_url <- 'https://maps.googleapis.com/maps/api/place/textsearch/json'
  search_key <- read_file('APIkey')
  
  # Call API for place info
  search_info <- httr::GET(
    url = search_url,
    query = list(
      query = search_string,
      key = search_key
    )
  )
  # Stop if no response received
  stop_for_status(search_info)
  # Convert raw result to row
  search_id <- search_info$content %>%
    rawToChar %>%
    fromJSON %>%
    .$results %>%
    .$place_id

  if(is.null(search_id)){
    search_id <- 'Not found'
  }else{
    return(search_id)
  }
}

# Get Place IDs
place_ids <- NULL
for(i in (length(place_ids) + 1):nrow(clublist)) {
  place_ids[i] <- PlaceAPI(clublist$query[i])
}
# save the results
clublist %<>% cbind(place_ids)
write_csv(clublist, path = 'clublist_withids.csv')

# Get Place Reviews ------------------------------------------------------------

# Function for place info
ReviewAPI <- function(
  place_id) {

  # Hard-coded URLs
  search_url <- 'https://maps.googleapis.com/maps/api/place/details/json'
  search_key <- read_file('APIkey')
  # Actual API call
  search_reviews <- httr::GET(
    url = search_url,
    query = list(
      key = search_key,
      placeid = place_id
    )
  )
  # Stop if no response received
  stop_for_status(search_reviews)
  # Convert raw result to row
  reviews <- search_reviews$content %>%
    rawToChar %>%
    fromJSON %>%
    .$result %>%
    .$reviews
}







