library(tidyverse)
library(magrittr)
library(stringr)

library(jsonlite)
library(readxl)
library(httr)

library(sp)
library(rgeos)
library(maps)
library(deldir)
library(maptools)
library(leaflet)

setwd('D:/ubuntu/projects/foodcycle')

# read data
fc_locations <- read_excel('data/FoodCycle Locations.xlsx') %>%
  mutate(Postcode = str_replace(Postcode, ' ', ''))
fc_ranking <- read_csv('data/ranking postcodes.csv') %>%
  mutate(Postcode = str_replace(Postcode, ' ', ''))
fc_locations <- left_join(
  fc_locations, fc_ranking, by='Postcode')


# get locations from postcodes
source('R/getCoordinatesFromPostcode.R')
fc_coordinates <- fc_locations$Postcode %>% 
  str_replace(' ', '') %>%
  purrr::map(getCoordinatesFromPostcode)

# extract lat lon data
fc_locations$x <- sapply(fc_coordinates, function(x) x[[1]] %>% unlist)
fc_locations$y <- sapply(fc_coordinates, function(x) x[[2]] %>% unlist)

# leave out incomplete records
fc_locations_clean <- fc_locations %>%
  filter(!is.na(x) & !is.na(y))
# combine into store locaton dataframe
coords <- data_frame(
  x = fc_locations_clean$x,
  y = fc_locations_clean$y)


# Voroni map -------------------------------------------------------

# get UK shapefile
poly_uk <- map('world', 'uk', 
               col='transparent', 
               fill=TRUE,plot=FALSE)
# source function from file
source('R/makeVoronoiPolygons.R')
# get voronoi polygons
poly_voronoi <- makeVoronoiPolygons(coords, poly_uk)

# make map into polygon
poly_uk <- map2SpatialPolygons(
  poly_uk, IDs=IDs,
  proj4string=CRS("+proj=longlat +datum=WGS84"))

# define projection
proj4string(poly_voronoi) <- proj4string(poly_uk)

# get intersection of uk shapefile with voronoi polygons
poly_voronoi <- gIntersection(poly_uk, poly_voronoi, byid=TRUE)


# Leaflet map ------------------------------------------------------

# make icons
fc_icons <- iconList(
  foodcycle = makeIcon('data/foodcycle-logo.png', 15, 15))


m <- leaflet(data = fc_locations) %>%
  addTiles() %>%
  # add voronoi polygons
  addPolygons(data=poly_voronoi,
              color='green',
              fill='lightgreen',
              stroke=1,
              weight=1) %>%
  # add location of foodcycle projects
  addCircleMarkers(data = fc_locations,
                   lng=~x,
                   lat=~y,
                   radius=5,
                   color='darkgreen',
                   stroke=0,
                   fillOpacity=0.8,
                   popup=~Billing_Address) %>%
  addCircleMarkers(data = fc_locations %>%
                     filter(rank < 10),
                   lng=~x,
                   lat=~y,
                   radius=5,
                   stroke=2,
                   color='white',
                   fillOpacity=1,
                   popup=~Billing_Address) %>%
  addCircleMarkers(data = fc_locations %>%
                     filter(rank > 10),
                   lng=~x,
                   lat=~y,
                   radius=5,
                   color='black',
                   stroke=0,
                   fillOpacity=1,
                   popup=~Billing_Address)
m

library(htmlwidgets)
saveWidget(m, file="m.html")

