
# Function for place info
getCoordinatesFromPostcode <- function(
  postcode) {

  # Hard-coded URLs
  search_url <- 'http://api.postcodes.io/postcodes/'
  
  # Call API for place info
  search_info <- httr::GET(
    url = paste0(search_url,postcode))
  
  # Stop if no response received
  stop_for_status(search_info)
  
  # Convert raw result to row
  search_id <- search_info$content %>%
    rawToChar %>%
    fromJSON

  if(is.null(search_id$result)){
    out <- c(NA, NA)
  }else{
    x <- search_id$result$longitude
    y <- search_id$result$latitude
    
    out <- c(x, y)
  }
  return(out)
}