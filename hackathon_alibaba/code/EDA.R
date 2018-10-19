
library(here)
library(tidyverse)
library(magrittr)

here()

df_city <- read_csv('data/CityData.csv')

df_train <- read_csv('data/ForecastDataforTraining_201712.csv')
df_holdout <- read_csv('data/ForecastDataforTesting_201712.csv')
df_validation <- read_csv('data/In_situMeasurementforTraining_201712.csv')


df_train %>%
  filter(date_id == 1) %>%
  

