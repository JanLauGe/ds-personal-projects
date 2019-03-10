
library(tidyverse)
library(magrittr)
library(ssh)
library(aws.ec2)
library(future)
library(furrr)
library(nycflights13)
library(randomForest)
source("funs_cluster.R")

# set parameters
aws_ami <- "ami-06485bfe40a86470d"
aws_type <- "t2.micro"

aws_config()
cluster <- start_cluster(n_nodes = 3, ami = "ami-0fdaff7e1d44af00b")
cluster_nodes <- use_cluster(cluster, username = "laurens.geffert")


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


punctuality_models <- future_map(
  .x = airlines[["carrier"]],
  .f = train_punctuality_model,
  flights = flights
)
