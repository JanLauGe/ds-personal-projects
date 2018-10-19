
# Function for place info
getCoordinatesFromPostcode <- function(
  postcode) {

  # Hard-coded URLs
  search_url <- 'http://api.postcodes.io/postcodes/'
  
  # Call API for place info
  search_info <- httr::GET(
    url = paste0(search_url,postcode))
  
  # if not found or other error
  if (search_info$status_code != 200){
    out <- c('x' = NA, 'y' = NA)
  
  }else{
    
    # Convert raw result to row
    search_id <- search_info$content %>%
      rawToChar %>%
      fromJSON

    x <- search_id$result$longitude
    y <- search_id$result$latitude
    
    out <- c('x' = x, 'y' = y)
  }
  return(out)
}