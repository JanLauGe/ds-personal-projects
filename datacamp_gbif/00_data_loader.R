
# This collection contains datasets of climate variables derived from the network of UK land surface observations. The data have been interpolated from meteorological station data onto a uniform grid to provide complete and consistent coverage across the UK. The data sets cover the UK at 5 x 5 km resolution and span the period 1910 - 2015. They are available at daily, monthly and annual timescales, as well as long-term averages for the periods 1961 - 1990, 1971 - 2000, and 1981 - 2010. Baseline averages are also available at 25 x 25 km resolution to match the UKCP09 climate change projections.
# 
# The primary purpose of this data resource is to encourage and facilitate research into climate change impacts and adaptation. The datasets have been created by the Met Office with financial support from the Department for Environment, Food and Rural Affairs (Defra) and are promoted within the UK Climate Projections (UKCP09). The UKCP09 report The climate of the UK and recent trends uses these gridded data sets to describe UK climatoloagies and regional trends.
# 
# Citable as:Met Office; Hollis, D.; McCarthy, M. (2017): UKCP09: Met Office gridded and regional land surface climate observation datasets. Centre for Environmental Data Analysis, date of citation. http://catalogue.ceda.ac.uk/uuid/87f43af9d02e42f483351d79b3d6162a


library(here)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)

library(sf)
library(sp)
library(raster)

# list all folders
climate_vars <- list.dirs(here::here("data/climate/download/timeseries/"))[-1]
climate_var_names <- climate_vars %>% basename

# list csv file names
climate_var_files <- map(
  .x = climate_vars,
  .f = list.files,
  include.dirs = FALSE, 
  full.names = TRUE)

# to read in csv files for one variable and make to long format
read_ukcp_var <- function(file_name, var_name) {
  file_content <- read_csv(
    file = file_name,
    col_names = FALSE,
    na = c("-9999"),
    skip = 0
  )
  file_content_long <- file_content %>% 
    select(-X1) %>% 
    t() %>% as_data_frame() %>%
    set_colnames(pull(file_content, 1)) %>%
    gather(key = 'month', value = 'measurement', -easting, -northing) %>%
    mutate(variable = var_name)
  
  return(file_content_long)
}

# to read all files into one dataframe
read_ukcp <- function(file_names, var_name) {
  # read files for var
  var_data <- map_df(
    .x = file_names, 
    .f = read_ukcp_var, 
    var_name = var_name)
  # aggregate by decade
  var_data_summarized <- var_data %>%
    transmute(
      easting = easting,
      northing = northing,
      variable = variable,
      measurement = measurement,
      decade = month %>% 
        fast_strptime(format = "%Y-%m") %>%
        round_date("10y") %>%
        year() %>%
        as.numeric()
    ) %>%
    group_by(easting, northing, variable, decade) %>%
    summarize(measurement = mean(measurement)) %>%
    ungroup()
  
  return(var_data_summarized)
}

# read all files 
ukcp_data <- map2_df(
  .x = climate_var_files,
  .y = climate_var_names,
  .f = read_ukcp
)

# save data
#write_rds(x = ukcp_data, path = here::here("data/ukcp09_by_decade.rds"))
ukcp_data <- read_rds(path = here::here("data/ukcp09_by_decade.rds"))

# shortcuts
proj_ukgrid <- CRS("+init=epsg:27700")
proj_latlong <- CRS("+init=epsg:4326")

# make into nested df
nested_rasters <- ukcp_data %>% 
  group_by(decade, variable) %>% nest() %>%
  # exclude pre-1970 values
  filter(decade %in% c("1970", "1980", "1990", "2000", "2010")) %>%
  # create rasters
  mutate(
    rasters = data %>%
      map(.f = function(x) {
        rasterFromXYZ(xyz = x, crs = proj_ukgrid)
      }
    )
  )

# stack rasters together by decade
stacked_rasters <- nested_rasters %>%
  group_by(decade) %>%
  summarize(
    raster_stacks = list(
      stack(rasters) %>%
        # variables as list names
        set_names(variable)
    )
  )

write_rds(
  x = stacked_rasters,
  path = here::here("data/ukcp09_stacked_rasters.rds")
)










