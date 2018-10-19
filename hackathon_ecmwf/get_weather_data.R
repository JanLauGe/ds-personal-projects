

library(tidyverse)
library(magrittr)
library(ncdf4)
library(raster)
library(MASS)
library(anytime)

#setwd('/home/ma/majw/Notebooks/data_service_catalog/')

# open the netcdf file
setwd('D:/projects/ecmwf')

load_ncdf <- function(year) {
  
  # Open file
  ncdffile <- nc_open(paste0(year,'.nc'), write=FALSE)

  nvals <- length(ncdffile$dim[[3]]$vals)
  # Number of variables to extract from ncdf
  nvars <- length(ncdffile$var)
  # Output data frame shape
  envidata <- as_data_frame(matrix(rep(0, times = nvals * nvars), ncol = nvars))
  varnames <- NULL
  
  for(i in 1:nvars){
  
    # Get variable
    variable <- ncdffile$var[[i]]
    # Get variable name
    varnames[i] <- variable$name
    # Get values for the entire grid
    valuegrid <- ncvar_get(ncdffile, varid = variable)
  
    # #retrieve latitude and longitude  information
    # lon <- which(valuegrid$dim$lon$vals == 358.50)
    # lat <- which(valuegrid$dim$lat$vals == 51.00)
    # 
    # # Get value of the specific cell we are interested in
    # valuegrid[lon, lat, 1]
    
    envidata[,i] <- valuegrid
  
  }
  
  # Set column names
  envidata <- set_colnames(envidata, varnames)
  
  # Get timestamps
  # # "hours since 1900-01-01 00:00:0.0"
  envidata[,'timestamps'] <- variable$dim[[3]]$vals %>%
    as.numeric %>%
    as_data_frame %>%
    mutate(value = as.POSIXct(value * 60 * 60, origin="1900-01-01"))
  
  oldnames <- c("fg10","sd","sf","tcc","u10","v10","t2m","d2m","e","mn2t","tco3","tp","timestamps")
  
  if(all(colnames(envidata) == oldnames)){
    newnames <- c(
      'windgusts',
      'snowdepth',
      'snowfall',
      'cloudcover',
      'wind1',
      'wind2',
      'temperature',
      'dewpoint',
      'evaporation',
      'mintemp',
      'ozone',
      'precipitation',
      'timestamp'
    )
    envidata <- set_colnames(envidata, newnames)
  }else{
    error('columns not matching')
  }
  
  
  # fg10 = windgusts
  # sd = snowdepth
  # sf = snowfall
  # tcc = cloudcover
  # u10 = wind1
  # v10 = wind2
  # t2m = temperature
  # d2m = dewpoint
  # e = evaporation
  # mn2t = mintemp
  # tco3 = ozone
  # tp = precipitation
  
  return(envidata)
}


# Get data for all years
years <- seq(2001, 2016, 1)

for(year in years){
  thisyear <- load_ncdf(year)
  if(exists('total')){
    total <- bind_rows(total, thisyear)
  }else{
    total <- thisyear
  }
}

# Check the result
head(total)

write_csv(total, path = 'D:/projects/ecmwf/evidata.csv')
write_rds(total, path = 'D:/projects/ecmwf/evidata.rds')

