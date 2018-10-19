
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
         month = month(date, label = TRUE),
         doy = yday(date),
         year = year(date),
         region = str_replace(region, 
                              pattern = 'E1200000',
                              replacement = 'R'))

# EDA
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
  geom_path()
  # Strong seasonal pattern!
  # Let's look at this averaged over years

train_clean %>%
  select(one_of(pollutants), doy, year, region) %>%
  gather(key = metric, value = measurement, -doy, -year, -region) %>%
  ggplot(aes(x = doy,
             y = measurement,
             #group = interaction(metric, year, region),
             colour = metric)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(level = 0.99) +
  facet_wrap(~metric, scales = 'free_y')

# Plot by day of the week
train_clean %>%
  select(one_of(pollutants), weekday, region) %>%
  gather(key = metric, value = measurement, -weekday, -region) %>%
  ggplot(aes(x = weekday, 
             y = measurement, 
             fill = metric)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = 'free_y')
  # Weak weekly pattern,
  # Somewhat lower higher O3 and lower NO2 on Sundays





# Plot pollutant by region
train_raw %>%
  select(-Id, -date) %>%
  gather(key = metric, value = measurement, -region) %>%
  ggplot(aes(x = region, y = measurement)) +
  geom_boxplot() +
  facet_wrap(~metric)


# Tidy up data
train_clean <- train_raw %>%
  filter(complete.cases(.))


# Split into predictors and response
train_x <- train_clean %>% 
  select(-Id, -region, -date, -mortality_rate) %>%
  mutate_all(funs(as.numeric)) %>%
  as.matrix()
train_y <- train_clean$mortality_rate

# Run model
model0 <- glmnet::cv.glmnet(
  x = train_x,
  y = train_y,
  nfolds = 10,
  type.measure = 'mse',
  alpha = 0)


tuneGrid <- expand.grid(
  alpha = seq(.05, 1, length = 5),
  lambda = seq(from = 0.00001, to = 0.001, length.out = 10))
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

test_raw <- read_csv('test.csv')

test_clean <- test_raw %>%
  select(-Id, -region, -date) %>%
  as.matrix()

# Get predicted values
test_prediction <- data_frame(
  Id = test_raw$Id,
  mortality_rate = predict(model0, newx = test_clean)[,1])

write_csv(test_prediction, 'C:/local/ecmwf/output/prediction_01_simple_glmnet.csv')
