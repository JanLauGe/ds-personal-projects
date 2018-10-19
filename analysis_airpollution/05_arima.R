
# Setup
library(devtools)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(caret)
library(xgboost)


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
         year = year(date),
         region = str_replace(region, 
                              pattern = 'E1200000',
                              replacement = 'R'))

# Get lead days!

# Get gliding window averages

# Use long-term trend

# Try prophet to forecast data time series?

# EDA --------------------------------------------------------------------------
glimpse(train_clean)
pollutants <- c('O3', 'PM10', 'PM25', 'NO2', 'T2M')

# Plot pollutants over time
train_clean %>%
  select(one_of(pollutants), mortality_rate, date, region) %>%
  gather(key = metric, value = measurement, -date, -region) %>%
  ggplot(aes(x = date, 
             y = measurement, 
             group = interaction(metric, region), 
             colour = metric)) +
  geom_path() +
  facet_grid(metric ~ region, scales = 'free_y')

# Plot by day of the week
train_clean %>%
  select(one_of(pollutants), mortality_rate, weekday, region) %>%
  gather(key = metric, value = measurement, -weekday, -region) %>%
  ggplot(aes(x = weekday, 
             y = measurement, 
             fill = metric)) +
  geom_boxplot() +
  facet_grid(metric ~ region, scales = 'free_y')
  # facet_wrap(~region)
  # Some weekly pattern:
  # NO2 and PM10 lower on Sundays, O3 higher

# Impute missing values
train_impute <- train_clean %>%
  group_by(region, weekday, month) %>%
  mutate(mean_PM25 = mean(PM25, na.rm = TRUE),
         mean_PM10 = mean(PM10, na.rm = TRUE),
         mean_NO2 = mean(NO2, na.rm = TRUE),
         mean_O3 = mean(O3, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(PM25 = ifelse(is.na(PM25), mean_PM25, PM25),
         PM10 = ifelse(is.na(PM10), mean_PM10, PM10),
         NO2 = ifelse(is.na(NO2), mean_NO2, NO2),
         O3 = ifelse(is.na(O3), mean_O3, O3))%>%
  select(-mean_PM25, -mean_PM10, -mean_NO2, -mean_O3)

# Get lagged averages
train_impute %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(predate = lag(O3, k = 3)) %>%
  ungroup()


# ARIMA ------------------------------------------------------------------------

# Find overall trend
ann_mor_data <- train_impute %>%
  mutate(year = year - min(year)) %>%
  group_by(region, year) %>%
  dplyr::summarise(mean_mortality = mean(mortality_rate)) %>%
  ungroup

ann_mor_data %>%
  ggplot(aes(x = year, y = mean_mortality)) +
  geom_path() + facet_wrap(~region)
  # Overall mortality rates are falling!

# Let's try to capture this trend
# so that we can predict the likely overall mortality
# in future years
ann_mor_model <- ann_mor_data %>%
  mutate(year = year - min(year)) %>%
  lm(formula = mean_mortality ~ year)

ann_mor_newdata <- data.frame(
  region = rep('r1'),
  year = c(0,1,2,3,4,5,6,7))

predict(ann_mor_model, 
        newdata = ann_mor_newdata)





train_impute %>%
  ggplot(aes(x = date, y = mortality_rate)) +
  geom_path() +
  facet_wrap(~region)

train_ts <- train_impute %>%
  filter(region == 'R1') %>%
  select(mortality_rate) %>%
  unlist %>% set_names(NULL) %>%
  ts(start = 1, frequency = 7)

library(forecast)
train_arima <- auto.arima(train_ts)

train_arima_forecast <- forecast.Arima(train_arima, h = 200)
plot.forecast(train_arima_forecast)

HoltWinters(train_ts)

library(forecast)
fit <- ets(train_ts)
fc <- forecast(fit)
plot(fc)

y <- msts(train_ts, seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit)
plot(fc)



# Modelling --------------------------------------------------------------------
train_complete <- train_impute %>%
  dplyr::filter(complete.cases(.)) %>%
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

# Train glmnet
tuneGrid <- expand.grid(
  alpha = seq(from = 0, to = 1, length.out = 20),
  # alpha = c(0.0001, 0.001, 0.01, 0.1, 0.15, seq(0.2, 1, length = 9)),
  lambda = c(0.0000001, 0.0000003, 0.000001, 0.000003, 0.00001, 0.00003, 
             seq(from = 0.0001, to = 0.01, length.out = 20)))
tuneControl <- trainControl(
  method = 'repeatedcv',
  number = 5,
  repeats = 1)

model_fit_3 <- train(
  x = train_x,
  y = train_y,
  method = 'glmnet',
  metric = 'RMSE',
  tuneGrid = tuneGrid,
  trControl = tuneControl)

# Train xgboost
tuneGrid <- expand.grid(
  nrounds = c(500),
  max_depth = c(5, 8, 12),
  eta = c(0.3, 0.1, 0.03, 0.01, 0.003, 0.001),
  colsample_bytree = c(0.6, 0.8, 1), 
  gamma = 1,
  min_child_weight = 1, 
  subsample = 1)
tuneControl <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = 'all',
  allowParallel = TRUE)

tree_model_3 <- train(
  x = train_x,
  y = train_y,
  method = 'xgbTree',
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
  mortality_rate = predict(tree_model_3, newdata = test_dummy))

write_csv(test_prediction, 'C:/Users/Laurens/Dropbox/projects/ecmwf_kaggle/prediction_04_xgboost.csv')
