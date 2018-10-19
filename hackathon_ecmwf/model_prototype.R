
library(tidyverse)
library(magrittr)
library(lubridate)
library(forcats)
library(caret)
library(stringr)

setwd('C:/Users/Laurens/Dropbox/projects/ecmwf/code/ReadingBus/Data/')
claims <- read_csv('busClaims.csv')
apiKey <- readline('Enter API key:')

# Data Cleaning ------------------------------------------------------------------------------------------

# Get useful route names
routes <- c('2','3','4','5','6','7','9','11','12','13','14','15','16','17',
  '19a','19b','19c','21','22','23','24','25','26','27','28','33','500') %>%
  factor

claims %<>%
  mutate(
    route = Route %>%
      # Take part before slash
      str_split('/') %>%
      lapply(FUN = function(x) {x[[1]]}) %>%
      unlist,
    # Remove trailing spaces
    route = route %>%
      str_split(boundary('word')) %>%
      lapply(FUN = function(x) {x[[1]]}) %>%
      unlist,
    # Remove leading zeros
    route = route %>%
      str_split('^[0]{1,9}') %>%
      lapply(FUN = function(x) {x[[length(x)]]}) %>%
      unlist %>% factor,
    route = route %>% 
      fct_recode(
        `2` = '2A',
        `4` = 'X4',
        `6` = '6A',
        `27` = '29')) %>%
  # Create proper date field
  mutate(dates = `Accident Date` %>% as.Date(format = '%d/%m/%y')) %>%
  # Only claims in 2015 and 2016
  #filter(year(dates) > 2014 & year(dates) < 2017) %>%
  # Only valid routes
  filter(route %in% routes) %>%
  # Make bad flag
  mutate(claimed = 1) %>% 
  select(one_of('dates', 'route', 'claimed'))


# Create model data frame --------------------------------------------------------------------------------

modeldata <- data_frame(dates = seq(ymd('2001-01-01'), ymd('2016-12-31'), by = 'days')) %>%
  # Add routes
  cbind(setNames(lapply(routes, function(x) x=NA), routes)) %>%
  # One row per route per day
  gather(key = route, value = claim, -dates) %>%
  # Set claims to zero
  mutate(claim = 0) %>%
  # Merge claims data
  full_join(claims, by = c('dates', 'route')) %>%
  # Get number of claims per day per route
  group_by(dates, route) %>%
  summarise(claims = sum(claim, na.rm = TRUE) + sum(claimed, na.rm = TRUE)) %>%
  # Make factor binary numeric
  mutate(claims = ifelse(claims > 0, 1, 0)) %>% 
  ungroup

# Plot bad flag over time
modeldata %>%
  filter(claims == 'yes') %>%
  ggplot(aes(x = dates)) +
  geom_histogram(binwidth = 1)

# Get weather data
envidata_orig <- read_rds('D:/projects/ecmwf/evidata.rds') %>%
  mutate(dates = timestamp %>% as.Date) %>%
  unique
  
# envidata <- full_join(
#   # Get different time stamps
#   envidata_orig %>%
#     filter(hour(timestamp) == 0 | hour(timestamp) == 1) %>%
#     set_colnames(str_c('v00h_', colnames(.))),
#   envidata_orig %>%
#     filter(hour(timestamp) == 6 | hour(timestamp) == 7) %>%
#     set_colnames(str_c('v06h_', colnames(.))), 
#   by = c('v00h_dates' = 'v06h_dates')) %>%
#   full_join(
#     envidata_orig %>%
#       filter(hour(timestamp) == 12 | hour(timestamp) == 13) %>%
#       set_colnames(str_c('v12h_', colnames(.))),
#     by = c('v00h_dates' = 'v12h_dates')) %>%
#   full_join(
#     envidata_orig %>%
#       filter(hour(timestamp) == 18 | hour(timestamp) == 19) %>%
#       set_colnames(str_c('v18h_', colnames(.))),
#     by = c('v00h_dates' = 'v18h_dates')) %>%

envidata <- envidata_orig %>%
  dplyr::select(dates) %>% unique %>%
  # 6 am data
  left_join(
    envidata_orig %>%
      filter(hour(timestamp) == 6 | hour(timestamp) == 7) %>%
      set_colnames(str_c('v06h_', colnames(.))) %>%
      dplyr::select(-v06h_timestamp),
    by = c('dates' = 'v06h_dates')) #%>%
  # # 12 pm data
  # left_join(
  #   envidata_orig %>%
  #     filter(hour(timestamp) == 12 | hour(timestamp) == 13) %>%
  #     set_colnames(str_c('v12h_', colnames(.))) %>%
  #     dplyr::select(-v12h_timestamp),
  #   by = c('dates' = 'v12h_dates')) %>%
  # # 6 pm data
  # left_join(
  #   envidata_orig %>%
  #     filter(hour(timestamp) == 18 | hour(timestamp) == 19) %>%
  #     set_colnames(str_c('v18h_', colnames(.))) %>%
  #     dplyr::select(-v18h_timestamp),
  #   by = c('dates' = 'v18h_dates'))

modeldata <- modeldata %>%
  # Join to original bus data
  left_join(envidata, by = c('dates' = 'dates')) %>%
  # Change flag variable
  mutate(claims = ifelse(claims > 0, 'yes', 'no') %>% factor,
         # Change route variable
         route = route %>% factor,
         # Get weekday
         weekday = dates %>% weekdays %>% factor,
         year = dates %>% year %>% factor) %>%
  # Keep only complete cases
  filter(complete.cases(.))

  
# Data subsets and dummy variables ----------------------------------------------------------------------

set.seed(2345)
kfolds <- createFolds(modeldata$claims, k = 3)

# Get test and train dataset
data_train <- modeldata[-kfolds[[3]],] %>%
  dplyr::select(-matches('timestamp'), -matches('dates'))
data_test <- modeldata[kfolds[[3]],] %>%
  select(-matches('timestamp'), -matches('dates'))

# # Create dummify formula
# dummify <- dummyVars(claims ~ ., data = data_train)
# # And apply it to train and test set
# data_train_dummy <- predict(dummify, newdata = data_train)
# data_train_claims <- data_train$claims
# data_train <- data_train %>% select(-claims)
# 
# data_test_dummy <- predict(dummify, newdata = data_test)
# data_test_claims <- data_test$claims


# Run the model ----------------------------------------------------------------------------------------

# Train glmnet model
mod_tune <- expand.grid(
  alpha = c(0, 0.5, 1), 
  lambda = c(0.000001, 0.00003)) 
mod_eval <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1,
  classProbs = TRUE,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary)
mod_train <- caret::train(
  claims ~ .,
  data = data_train,
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  tuneGrid = mod_tune,
  trControl = mod_eval)


write_rds(mod_train, path = 'D:/projects/ecmwf/trained_big.rds')
varImp(mod_train) %>% plot(scale = FALSE)
plot(varImp(mod_train, scale = FALSE))
     
mod_pred <- predict(mod_train, newdata = data_test, type = 'prob')
mod_pred_val <- bind_cols(mod_pred, data_test) %>% select(yes, claims)

ModelMetrics::auc(actual = mod_pred_val$claims, predicted = mod_pred_val$yes)

library(AUC)
roc(mod_pred_val$yes, mod_pred_val$claims) %>% plot


