
library(tidyverse)
library(magrittr)
library(jsonlite)
library(lubridate)
library(assertthat)
library(fs)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
source('utils.R')

# list all files from takeout
fnames <- fs::dir_ls(
  path='data/Semantic Location History',
  type='file',
  recurse=TRUE)

df = tibble()
for (fname in fnames) {
  df <- get_location_from_json(fname) %>%
    bind_rows(df, .)
}

# convert data formats
df %<>%
  mutate(
    time = as_datetime(time / 1e3),
    lat = lat / 1e7,
    lng = lng / 1e7)

# plot coordinates
world <- ne_countries(scale = "medium", returnclass = "sf")
df %>%
  ggplot() +
  geom_sf(data=world) +
  geom_point(aes(x=lng, y=lat), color='red')
  

# convert to sf points
pnts <- df %>%
  st_as_sf(coords = c('lng', 'lat'), crs = st_crs('WGS84'))

x <- st_intersection(pnts, world)

# breakdown of records per country
x %>%
  group_by(sovereignt) %>%
  summarize(n=n()) %>%
  arrange(desc(n))



