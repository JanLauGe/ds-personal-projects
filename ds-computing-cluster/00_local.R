
library(tidyverse)
library(randomForest)
library(beepr)
library(tictoc)

# our dataset
library(nycflights13)

# run preprocessing pipeline
flights_cleaned <- flights %>%
  # exclude flight time variables
  select(-arr_time, -air_time, -dep_time, -dep_delay) %>%
  # exclude high-cardinality variables
  select(-dest, -tailnum) %>%
  # transform variables for model fitting
  mutate_if(.p = is.character, .f = as.factor) %>%
  drop_na()


airline_tags <- airlines[["carrier"]]
results <- list()
# start timer
tic()
# start loop
for (i in seq_along(airline_tags)) {
  
  # which airline tag is the loop at?
  airline_tag <- airline_tags[[i]]
  message("currently at carrier: ", airline_tag)
  
  # select relevant flights and separate into X and y
  X <- flights_cleaned %>%
    filter(carrier == airline_tag) %>%
    select(-arr_delay)
  y <- flights_cleaned %>%
    filter(carrier == airline_tag) %>%
    pluck("arr_delay") %>%
    replace_na(0)
  
  rf_model <- randomForest(x = X, y = y, ntree = 5)
  results[[i]] <- rf_model
}

message("Done!")
beep(sound = 3)
toc()