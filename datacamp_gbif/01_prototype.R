#install.packages('rgbif')
library(rgbif)
library(here)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)

library(sf)
library(sp)
library(raster)

library(tidymodels)
library(parsnip)

library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)
library(ggthemes)

# DATA PREP ====================================================================

# for reproducibility
set.seed(12345)

# Cute finces with funny beaks don't only occur in the Galapagos islands
# (https://www.datacamp.com/courses/statistical-thinking-in-python-part-2)


# THIS ONLY NEEDS TO RUN ONCE!
# convert scientific (Latin) name to
# speciesKey <- rgbif::name_backbone('Loxia scotica')$speciesKey
# 
# # Using rgbif by rOpenSci
# gbif_response <- occ_search(
#   scientificName = "Loxia scotica",
#   country = "GB",
#   hasCoordinate = TRUE,
#   hasGeospatialIssue = FALSE,
#   limit = 9999)
# # backup to reduce API load
# write_rds(
#   x = gbif_response,
#   path = here::here('data/gbif_response_loxsco.rds')
# )


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

# pick raster cells at random
raster_random_sample <- function(x, y) {
  raster::sampleRandom(
    x = x, 
    size = length(y) * 5,
    cells = TRUE,
    na.rm = TRUE,
    asRaster = FALSE,
    sp = FALSE) %>% 
    as_data_frame()}

# combine climate and bird data
df_presence <- data_birds_bydecade %>%
  mutate(
    presence = 1,
    climate =  map2(
      .x = data_climate_bydecade$raster_stacks,
      .y = data_birds_bydecade$points,
      .f = function(x, y) {raster::extract(x, y) %>% as_data_frame()}
    )
  )

# helper function to extract lat and lon from cell id
get_latlon_from_raster <- function(x, y) {
  xyFromCell(
    object = x,
    cell = y$cell,
    spatial = TRUE) %>%
    spTransform(proj_latlon) %>%
    coordinates() %>%
    as_data_frame() %>%
    transmute(
      decimalLongitude = x,
      decimalLatitude = y)
}

# draw random sample from climate data with similar
# temporal distribution to the bird data
df_nopresence <- data_birds_bydecade %>%
  mutate(
    presence = 0,
    climate = map2(
      .x = data_climate_bydecade$raster_stacks,
      .y = points,
      .f = raster_random_sample),
    # get coordinates of the random sample
    data = map2(
      .x = data_climate_bydecade$raster_stacks,
      .y = climate,
      .f = get_latlon_from_raster),
    # remove the now superfluous cell column from climate data
    climate = map(.x = climate, .f = function(x) {x %>% select(-cell)})
  )


df <- bind_rows(df_presence, df_nopresence) %>%
  # discard spatial points
  select(-points) %>%
  # get into modelling format
  unnest()



# MODELLING ====================================================================

# FROM: https://www.tidyverse.org/articles/2018/11/parsnip-0-0-1/
library(parsnip)
library(tidymodels)
library(glmnet)
library(caret)

# true temporal split as holdout
# TODO: should be stratified by presence / absence
df_modelling <- df[df$decade != "2010",]
df_holdout <- df[df$decade == "2010",]

df_train <- df_modelling %>%
  select(-decade, -decimalLongitude, -decimalLatitude) %>%
  mutate(presence = case_when(
    presence == 1 ~ "presence",
    presence == 0 ~ "absence") %>%
    factor()) %>%
  na.omit()
df_test <- df_holdout %>%
  select(-decade, -decimalLongitude, -decimalLatitude) %>%
  mutate(presence = case_when(
    presence == 1 ~ "presence",
    presence == 0 ~ "absence") %>%
      factor()) %>%
  na.omit()

# CARET
tuneGrid <- expand.grid(
  alpha = seq(0, 1, length = 6),
  lambda = 10 ** seq(-1, -10, by = -.25))
tuneControl <- trainControl(
  method = 'repeatedcv',
  classProbs = TRUE,
  number = 10,
  repeats = 5,
  verboseIter = FALSE,
  summaryFunction = twoClassSummary)

model_fit <- train(
  presence ~ .,
  data = df_train,
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  tuneGrid = tuneGrid,
  trControl = tuneControl)

plot(model_fit)

# combine prediction with validation set
df_eval <- predict(
  object = model_fit,
  newdata = df_test,
  type = "prob") %>%
  pull(1) %>%
  cbind(
    "pred" = ., 
    "obs" = as.numeric(df_test$presence) - 1)

df_eval <- data_frame(
  pred = predict(object = model_fit, newdata = df_test, type = "prob")[,1],
  obs = (as.numeric(df_test$presence) - 1) %>% factor()
)

# get ROC value
roc_auc_vec(estimator = "binary", truth = df_eval$obs, estimate = df_eval$pred)

predict(object = model_fit, newdata = df_test, type = "prob")[,1]

# generate raster prediction
make_pred_raster <- function(climate_raster_stack) {
  # X values to predict with trained model
  X <- climate_raster_stack %>% 
    as.data.frame()
  # calculate modelled probabilities
  pred_values <- predict(object = model_fit, 
                         newdata = X[complete.cases(X), ],
                         type = "prob") %>%
    pull(1)
  
  # copy raster from climate data
  raster_prediction <- climate_raster_stack[[1]]
  # overwrite all values
  raster_prediction@data@values <- NA
  # copy prediction values
  raster_prediction@data@values[complete.cases(X)] <- pred_values
  
  return(raster_prediction)
}


plot_prediction <- function(x) {
  x %>%
    as("SpatialPixelsDataFrame") %>%
    as_data_frame() %>%
    set_colnames(c("Probability", "lon", "lat")) %>%
    ggplot(aes(x = lon, y = lat, fill = Probability)) +  
    geom_tile() +
    coord_equal() +
    theme_map() +
    theme(legend.position = "bottom")
}

prediction <- data_climate_bydecade %>%
  mutate(
    prediction = map(.x = raster_stacks, .f = make_pred_raster),
    plot = map(.x = prediction, .f = plot_prediction)
  )

# TODO:
# could make this into faceted ggplot instead





