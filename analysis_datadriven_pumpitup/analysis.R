# also check https://github.com/MattBrown88/Pump-it-Up-XGBoost-Ensemble/blob/master/Water_solution%20-%20xgboost%2045.R


# setup
setwd('D:/ubuntu/projects/pump_it_up')
set.seed(12345)

library(tidyverse)
library(magrittr)
library(lubridate)
library(forcats)
library(Metrics)
library(caret)
library(leaflet)
library(doParallel)

# setup parallel cluster
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# load data
raw_y_train <- read_csv('inputs/Training_set_labels.csv')
raw_x_train <- read_csv('inputs/Training_set_values.csv')
raw_x_test <- read_csv('inputs/Test_set_values.csv')


# Data transformation ----------------------------------------------------------

# change y labels
raw_y_train %<>%
  mutate(label = fct_recode(status_group,
                            'pump1' = 'functional',
                            'pump2' = 'non functional',
                            'pump3' = 'functional needs repair')) %>%
  select(id, label)
# but save lookup for later
ylabels <- c(
  'pump1' = 'functional',
  'pump2' = 'non functional',
  'pump3' = 'functional needs repair')

# combine test and train features
# so they can be transformed together
raw_x <- bind_rows(
  cbind(raw_x_train, set = 'train'),
  cbind(raw_x_test, set = 'test'))

# join labels to features
raw <- left_join(raw_x, 
                 raw_y_train,
                 by = 'id')

#glimpse(raw)
#summary(raw)

# transform data
source('fun_convertRawData.R')
xgb_data <- convertRawData(raw)  
glimpse(xgb_data)


# plot map of wells

# pairwise plot of numerical variables

#Modified construction year so that it starts at 0 and counts up 
#Set missing gps_height values to the median of gps_height
#data$gps_height[data$gps_height==0]=median(data$gps_height[data$gps_height>0])

# Pre-processing ---------------------------------------------------------------

# impute missing values
xgb_data <- xgb_data %>%
  preProcess(method = 'medianImpute') %>%
  predict(newdata = xgb_data)

# get training set
xgb_train <- xgb_data %>%
  filter(!is.na(label))

# backup pre-processed data
write_rds(
  x = xgb_train,
  path = 'workspace/training_data_preprocessed')
# restore backup
xgb_train <- read_rds(path = 'workspace/training_data_preprocessed')


# Modelling --------------------------------------------------------------------

# candidate hyperparameters
xgb_grid <- expand.grid(
  nrounds = c(100,300),
  max_depth = 8,#c(4, 8, 12),
  eta = c(0.01, 0.001),#, 0.0001),
  gamma = 1,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 0.5)
# cross-validation settings
xgb_control <- trainControl(
  summaryFunction = multiClassSummary,
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  classProbs = TRUE,
  verboseIter = TRUE)

# fit model
xgb_model <- train(
  label ~ .,
  data = xgb_train,
  trControl = xgb_control,
  tuneGrid = xgb_grid,
  metric='Accuracy',
  method = 'xgbTree')

#Stop parallel cluster
stopCluster(cl)



