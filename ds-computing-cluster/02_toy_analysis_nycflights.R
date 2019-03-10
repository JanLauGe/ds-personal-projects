
library(tidyverse)
library(magrittr)
library(nycflights13)
library(randomForest)


train_punctuality_model <- function(
  airline_tag,
  flights,
  ntree = 50
) {
  flights_airline <- flights %>%
    # select relevant flights
    filter(carrier == airline_tag) %>%
    # exclude flight time variables
    select(-arr_time, -air_time, -dep_time, -dep_delay) %>%
    # exclude high-cardinality variables
    select(-dest, -tailnum) %>%
    # transform variables for model fitting
    mutate_if(.p = is.character, .f = as.factor) %>%
    drop_na()
  
  x <- select(flights_airline, -arr_delay)
  y <- pluck(flights_airline, "arr_delay") %>% replace_na(0)
  
  rf_model <- randomForest(x = x, y = y, ntree = ntree)
  return(rf_model)
}


# create output vector
punctuality_models <- list()
# timed loop over airlines
time_single_thread <- system.time(
  for (i in seq_along(airlines[["carrier"]])) {
    # train model
    punctuality_models[[i]] <- train_punctuality_model(
      airline_tag = airlines[['carrier']][[i]],
      flights = flights)
  }
)
