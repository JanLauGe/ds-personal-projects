
library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)
library(caret)
library(doParallel)

setwd('D:/ubuntu/projects/task_octopusenergy/')

# Load data from files --------------------------------------------

ko_forecast <- read_csv('data/forecast_data.csv') %>%
  # standardise time stamp
  mutate(ts = timeUTC,
         # reformat windgust to number
         windGust = as.numeric(windGust),
         # na seems to signify zeros
         windGust = case_when(is.na(windGust) ~ 0,
                              TRUE ~ windGust),
         # same for precipAccumulation
         precipAccumulation = as.numeric(precipAccumulation),
         precipAccumulation = case_when(is.na(precipAccumulation) ~ 0,
                                        TRUE ~ precipAccumulation),
         visibility = case_when(is.na(visibility) ~ 0,
                                TRUE ~ visibility),
         precipType = case_when(is.na(precipType) ~ 'none',
                                TRUE ~ precipType),
         rain = ifelse(precipType == 'rain', 1, 0),
         snow = ifelse(precipType == 'snow', 1, 0),
         sleet = ifelse(precipType == 'sleet', 1, 0)) %>%
  # drop superfluous variables
  select(-timeUTC, -precipType, -icon, -summary, -day_offset)

ko_radiation <- read_csv('data/kia-ora-radiation.csv') %>%
  transmute(ts = as.POSIXct(timeUTC, tz='UTC'),
            radiation = radiation) 

ko_production <- read_csv('data/kia-ora.csv') %>%
  transmute(ts = utc %>% as.POSIXct(tz='UTC'),
            production = production_kWh)

ko_prices <- read_csv('data/prices.csv')

# Exploratory data analysis --------------------------------------

# Histogram of radiation
ko_radiation %>%
  ggplot(aes(x=radiation)) +
  geom_histogram(bins=30)

# Histogram of production
ko_production %>%
  ggplot(aes(x=production)) +
  geom_histogram(bins=30)

# Join radiation and production.
# We are loosing some records here, but it's only a few dozen,
# so I will ignore this for now
ko_data <- inner_join(
  ko_radiation,
  ko_production,
  by='ts')

# How does radiation match up to production?
# Hypothesis: "Shotgun" pattern, because low radiation will
# neccessarily mean low prodiction, while high (potential) radiation
# can lead to high production, but only if weather conditions are
# favourable
ko_data %>%
  ggplot(aes(x=radiation, y=production)) +
  geom_point(alpha=0.3)
# Hypothesis roughly confirmed, however, observations with medium
# radiation have proportionately low prodiction. Is there a non-linear
# relationship between radiation and production?

ko_data <- inner_join(
  ko_data,
  ko_forecast,
  by='ts')

# Get overview of data
summary(ko_data)
# Some missing values in visibility, rest seems okay

ko_train <- ko_data %>%
  filter(!is.na(production)) %>%
  select(-ts) %>%
  as.matrix()


# Model fitting ----------------------------------------------------

# candidate hyperparameters
xgb_grid <- expand.grid(
  nrounds = 1000,
  max_depth = c(5, 10),
  eta = c(0.5, 0.1, 0.01),
  gamma = c(0, 2, 5, 10),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.5)
# cross-validation settings
xgb_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  verboseIter = TRUE)

# setup parallel cluster
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# fit model
model1 <- train(
  production ~ .,
  data = ko_train,
  trControl = xgb_control,
  tuneGrid = xgb_grid,
  method = 'xgbTree')

# save model object
write_rds(model1, path = 'model1.rds')

#Stop parallel cluster
stopCluster(cl)


# Evaluation -------------------------------------------------

# get prediction for training set
xgb_pred <- cbind(
  predicted = predict(model1, newdata=xgb_train),
  actual = xgb_train$production) %>%
  as.data.frame()

# Plot predicted vs actual
ggplot(xgb_pred, aes(x=actual, y=predicted)) +
  geom_point()

# Get variable importance
varImp(model1)

# Generate predictions for holdout
ko_pred <- cbind(
  ts = ko_data %>% 
    filter(is.na(production)) %>% 
    select(ts),
  predicted = predict(model1, 
    newdata=ko_data %>% 
    filter(is.na(production))))

# Plot timeline of predicted production  
ggplot(ko_pred, aes(x=ts, y=predicted)) +
  geom_line() +
  geom_smooth()



