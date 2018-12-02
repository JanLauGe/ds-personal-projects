
# This collection contains datasets of climate variables derived from the network of UK land surface observations. The data have been interpolated from meteorological station data onto a uniform grid to provide complete and consistent coverage across the UK. The data sets cover the UK at 5 x 5 km resolution and span the period 1910 - 2015. They are available at daily, monthly and annual timescales, as well as long-term averages for the periods 1961 - 1990, 1971 - 2000, and 1981 - 2010. Baseline averages are also available at 25 x 25 km resolution to match the UKCP09 climate change projections.
# 
# The primary purpose of this data resource is to encourage and facilitate research into climate change impacts and adaptation. The datasets have been created by the Met Office with financial support from the Department for Environment, Food and Rural Affairs (Defra) and are promoted within the UK Climate Projections (UKCP09). The UKCP09 report The climate of the UK and recent trends uses these gridded data sets to describe UK climatoloagies and regional trends.
# 
# Citable as:Met Office; Hollis, D.; McCarthy, M. (2017): UKCP09: Met Office gridded and regional land surface climate observation datasets. Centre for Environmental Data Analysis, date of citation. http://catalogue.ceda.ac.uk/uuid/87f43af9d02e42f483351d79b3d6162a


library(here)
library(tidyverse)
library(magrittr)
library(stringr)

library(sf)
library(sp)
library(raster)


# list csv file names
file_names <- list.files(
  path = here::here('data/climate/download/timeseries/mean-temperature/'), 
  include.dirs = FALSE, full.names = TRUE)

# read in csv files and make to long format
read_ukcp09 <- function(file_name) {
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
    gather(key = 'month', value = 'measurement', -easting, -northing)
  return(file_content_long)
}

# read all files into list
ukcp09 <- map_df(.x = file_names, .f = read_ukcp09)


# shortcuts
proj_ukgrid <- CRS("+init=epsg:27700")
proj_latlong <- CRS("+init=epsg:4326")


# test on subset
x <- ukcp09 %>% 
  filter(month == "1910-01")
r <- rasterFromXYZ(x[, c("easting", "northing", "measurement")], crs = "+init=epsg:27700")
plot(r)

bla <- rasterize(file_spatial)
# convert to latlon
bla <- spTransform(r, proj_latlong)

# merge into one spatial dataframe
df_spatial <- reduce(.x = files_spatial, .f = rbind)

write_rds(df_spatial, "data/df_spatial_monthly_mean.rds")

library(lubridate)
df_spatial[1:10,] %>%
  mutate(month = fast_strptime(x = month, format = "Y!-m!*"))

library(dismo)
files <- list.files(
  path = paste(system.file(package="dismo"), '/ex', sep = ''), 
  pattern = 'grd', 
  full.names = TRUE )

predictors <- stack(files)



library(rWBclimate)
UK <- get_model_temp(
  locator = "GBR", 
  type = "mavg", 
  start = 2080, 
  end = 2100)

get_historical_precip(locator = "GBR", "month")
