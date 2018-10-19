
# Setup
library(devtools)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(caret)

set.seed(12345)
session_info()

# Load data
# setwd('C:/local/ecmwf/')
train_raw <- read_csv('data/train.csv',
                      na = c('', 'NA'),
                      col_types = 'icDdddddd')

# Transformation
train_clean <- train_raw %>%
  mutate(weekday = weekdays(date) %>%
                   ordered(levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
         month = month(date, 
                       label = TRUE),
         region = str_replace(region, 
                              pattern = 'E1200000',
                              replacement = 'R'))

# Get lead days as well!

# EDA --------------------------------------------------------------------------
glimpse(train_clean)
pollutants <- c('O3', 'PM10', 'PM25', 'NO2', 'T2M')

# Plot pollutants over time
train_clean %>%
  select(one_of(pollutants), date, region) %>%
  gather(key = metric, value = measurement, -date, -region) %>%
  ggplot(aes(x = date, 
             y = measurement, 
             group = interaction(metric, region), 
             colour = metric)) +
  geom_path() +
  facet_wrap(~metric, scales = 'free_y')

# Plot by day of the week
train_clean %>%
  select(one_of(pollutants), weekday, region) %>%
  gather(key = metric, value = measurement, -weekday, -region) %>%
  ggplot(aes(x = weekday, 
             y = log(measurement), 
             fill = metric)) +
  geom_boxplot() +
  facet_wrap(~region)
  # Some weekly pattern:
  # NO2 and PM10 lower on Sundays, O3 higher

# Plot by day of the week
train_clean %>%
  select(one_of(pollutants), weekday, region) %>%
  gather(key = metric, value = measurement, -weekday, -region) %>%
  ggplot(aes(x = weekday, 
             y = measurement, 
             fill = metric)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = 'free_y')


# Modelling --------------------------------------------------------------------

train_complete <- train_clean %>%
  filter(complete.cases(.)) %>%
  select(-Id)

# Dummify factors
train_dummy <- train_complete %>%
  mutate(weekday = as.character(weekday) %>% as.factor,
         month = as.character(month) %>% as.factor) %>% 
  {predict(dummyVars(mortality_rate ~ ., data = .), 
           newdata = .)} %>%
  as.data.frame()
  
# Split into predictors and response
train_x <- train_dummy %>% 
  select(-date) %>%
  mutate_all(funs(as.numeric)) %>%
  as.matrix()
train_y <- train_complete$mortality_rate

# # Run model
# model0 <- glmnet::cv.glmnet(
#   x = train_x,
#   y = train_y,
#   nfolds = 10,
#   type.measure = 'mse',
#   alpha = 0)

tuneGrid <- expand.grid(
  alpha = seq(from = 0, to = 1, length.out = 20),
  # alpha = c(0.0001, 0.001, 0.01, 0.1, 0.15, seq(0.2, 1, length = 9)),
  lambda = c(0.0000001, 0.0000003, 0.000001, 0.000003, 0.00001, 0.00003, 
             seq(from = 0.0001, to = 0.01, length.out = 20)))
tuneControl <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 5)

model_fit_1 <- train(
  x = train_x,
  y = train_y,
  method = 'glmnet',
  metric = 'RMSE',
  tuneGrid = tuneGrid,
  trControl = tuneControl)


# Predict ----------------------------------------------------------------------

test_raw <- read_csv('data/test.csv')

test_clean <- test_raw %>%
  select(-Id) %>%
  mutate(weekday = weekdays(date) %>%
         ordered(levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
         month = month(date, 
                       label = TRUE),
         region = str_replace(region, 
                              pattern = 'E1200000',
                              replacement = 'R'))
                              
# Create dummy variables
test_dummy <- test_clean %>%
  mutate(weekday = as.character(weekday) %>% as.factor,
         month = as.character(month) %>% as.factor) %>% 
         {predict(dummyVars(~ ., data = .), 
                  newdata = .)} %>%
  as.data.frame() %>%
  select(-date)
  


# Get predicted values
test_prediction <- data_frame(
  Id = test_raw$Id,
  mortality_rate = predict(model_fit_1, newdata = test_dummy))

write_csv(test_prediction, 'C:/local/ecmwf/output/prediction_02_feature_engineering.csv')
