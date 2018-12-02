#install.packages('rgbif')
library(rgbif)


# DATA PREP ====================================================================

# Cute finces with funny beaks don't only occur in the Galapagos islands
# (https://www.datacamp.com/courses/statistical-thinking-in-python-part-2)

# convert scientific (Latin) name to
speciesKey <- rgbif::name_backbone('Loxia scotica')$speciesKey

# Using rgbif by rOpenSci
gbif_response <- occ_search(
  scientificName = "Loxia scotica",
  country = "GB",
  hasCoordinate = TRUE,
  hasGeospatialIssue = FALSE,
  limit = 9999)
# backup to reduce API load
write_rds(
  x = gbif_response,
  path = here::here('data/gbif_response_loxsco.rds')
)
gbif_response <- read_rds(path = here::here('data/gbif_response_loxsco.rds'))

# # convert into dataframe
# data_birds_raw <- data_frame(
#   year = gbif_response %>% names(),
#   data = gbif_response %>% map('data')) %>%
#   unnest()

# look at a random sample of 100 rows
gbif_response$data %>%
  sample_n(100) %>% 
  View()

data_birds_clean <- gbif_response$data %>%
  # TODO: double-check filtes!
  # filter(
  #   # only classified as present
  #   occurrenceStatus == "present" &
  #   # only records with no issues
  #   issues == "" &
  #   # only creative commons license records
  #   str_detect(license, "http://creativecommons.org/")) %>%
  # get year of record from eventDate
  mutate(decade = eventDate %>% 
           ymd_hms() %>% 
           round_date("10y") %>%
           year() %>%
           as.numeric()) %>%
  # exclude records from after 2015
  filter(decade != 2020) %>%
  # retain only relevant variables
  select(decimalLongitude, decimalLatitude, decade)

# latlon crs reference
proj_latlon <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj_ukgrid <- CRS("+init=epsg:27700")

data_birds_bydecade <- data_birds_clean %>%
  group_by(decade) %>% nest() %>%
  filter(decade >= 1970) %>%
  mutate(points = map(
    .x = data, 
    .f = function(x) {
      # make records into spatial points
      SpatialPoints(coords = x, proj4string = proj_latlon)
    } %>%
      # reproject spatial points to match the climate data
      spTransform(CRSobj = proj_ukgrid)))
  #select(decade, points)

# raster data
# should include reference to this course:
# https://www.datacamp.com/courses/spatial-analysis-in-r-with-sf-and-raster
data_climate_bydecade <- read_rds(here::here("data/ukcp09_stacked_rasters.rds"))


# EDA ==========================================================================

# TODO:
# should use same scale
# could do a cheeky gganimate here
# 1970
plot(data_climate_bydecade$raster_stacks[[1]])
#2010
plot(data_climate_bydecade$raster_stacks[[5]])

# combine climate and bird data
df <- data_birds_bydecade %>%
  mutate(
    climate_data_at_record =  map2(
      .x = data_climate_bydecade$raster_stacks,
      .y = data_birds_bydecade$points,
      .f = function(x, y) {raster::extract(x, y) %>% as_data_frame()}
    )
  ) %>%
  # discard spatial points
  select(-points) %>%
  unnest()


# MODELLING ====================================================================

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