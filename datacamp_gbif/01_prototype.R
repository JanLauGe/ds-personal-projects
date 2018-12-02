install.packages('rgbif')
library(rgbif)

occ_data <- occ_search(scientificName = "Ursus americanus", limit = 50)

# raster data
# should include reference to this course:
# https://www.datacamp.com/courses/spatial-analysis-in-r-with-sf-and-raster


# FROM: https://www.tidyverse.org/articles/2018/11/parsnip-0-0-1/
library(parsnip)
library(tidymodels)

set.seed(12345)
split <- initial_split(mtcars, props = 9/10)
car_train <- training(split)
car_test  <- testing(split)

# Let’s preprocess these data to center and scale the predictors. We’ll use a basic recipe to do this:
car_rec <- 
  recipe(mpg ~ ., data = car_train) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep(training = car_train, retain = TRUE)

# The processed versions are:
train_data <- juice(car_rec)
test_data  <- bake(car_rec, car_test)



# define computational engine
lm_car_model <- 
  car_model %>%
  set_engine("lm")
lm_car_model

# can use either formula or matrix input
lm_fit <-
  lm_car_model %>%
  fit(mpg ~ ., data = car_train)

# or
lm_car_model %>%
  fit_xy(x = select(car_train, -mpg), y = select(car_train, mpg))