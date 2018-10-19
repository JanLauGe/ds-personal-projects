

## Saved xlsb as csv within LibreOffice

## Load Callouts
callouts <- read.csv( "callouts.csv"  )
## Load Claims 
claims <- read.csv( "claims.csv" ) 

claims$Date1 <- as.Date( claims$Date.File.Opened, "%m/%d/%Y" )
callouts$Date1 <- as.Date( callouts$Date, "%m/%d/%Y" )

########## WEATHER DATA #######################################################

## Step 1: install netCDF library and dependencies
##https://gist.github.com/perrette/cd815d03830b53e24c82

## Step 2: install R package
install.packages( "ncdf4" )
library( ncdf4 )

## Step 3:download the data
## http://apps.ecmwf.int/datasets/data/interim-full-daily/levtype=sfc/requests/netcdf/58bb040cf7414d9ae22150e5/

######### PLAY WITH DATA ##########

## ## test data
## ##ncname <- '/home/fiona/Dropbox/climate_datahack/_grib2netcdf-atls18-95e2cf679cd58ee9b4db4dd119a05a8d-Ptsd3d.nc'

## ## temp, rainfall, snow for 2015 and 2016
## ncname1 <- '/home/fiona/Dropbox/climate_datahack/_grib2netcdf-atls01-95e2cf679cd58ee9b4db4dd119a05a8d-utU_9m.nc'

## ## ## rainfall and snow 3 hours only 
## ## ncname <- '/home/fiona/Downloads/_grib2netcdf-atls20-95e2cf679cd58ee9b4db4dd119a05a8d-3yKM0W.nc' 

## ## rainfall and snow first 12 hours
## ncname2 <- '/home/fiona/Downloads/_grib2netcdf-atls00-95e2cf679cd58ee9b4db4dd119a05a8d-LvzZFs.nc' 
## ## rainfall and snow second 12 hours 
## ncname3 <- '/home/fiona/Downloads/_grib2netcdf-atls20-95e2cf679cd58ee9b4db4dd119a05a8d-o5xRzD.nc' 




## ## print the dataset info 
## ncin <- nc_open( ncname ) 
## print( ncin ) 

## ## get the latitude and longitude 
## lon <- ncvar_get(ncin,"longitude")
## nlon <- dim(lon)
## head(lon)
## lat <- ncvar_get(ncin,"latitude",verbose=F)
## nlat <- dim(lat)
## head(lat)

## ## work out the index of the latitude and longitude of reading 
## rlat <- 52## actual 51.45 ## use 51.75 [52]
## rlon <- 2## actual 0.98 ## use 0.75 [2]

## ## extract the temperature 
## rtemps <- ncvar_get( ncin, varid = "t2m", start = c(rlon, rlat, 1 ), count = c(1,1,730))
## rtempsC <- rtemps - 273.15 ## convert to C
## plot( rtempsC ) ## looks good

## rsf <- ncvar_get( ncin, varid = "sf", start = c(rlon, rlat, 1 ), count = c(1,1,730))
## rrain <- ncvar_get( ncin, varid = "tp", start = c(rlon, rlat, 1 ), count = c(1,1,730))
## head( rsf )
## head( rrain ) ## something is wrong with these


## plot( rsf )
## plot( rrain )

################## final data ############################

rlat <- 52## actual 51.45 ## use 51.75 [52]
rlon <- 2## actual 0.98 ## use 0.75 [2]

### temperature
ncin <- nc_open( ncname1 )
rtemps <- ncvar_get( ncin, varid = "t2m", start = c(rlon, rlat, 1 ), count = c(1,1,730))
rtempsC <- rtemps - 273.15 ## convert to C

## snow and precip
ncin <- nc_open( ncname2 ) ## first 12 
rsf1 <- ncvar_get( ncin, varid = "sf", start = c(rlon, rlat, 1 ), count = c(1,1,730))
rrain1 <- ncvar_get( ncin, varid = "tp", start = c(rlon, rlat, 1 ), count = c(1,1,730))

ncin <- nc_open( ncname3 ) ## first 12 
rsf2 <- ncvar_get( ncin, varid = "sf", start = c(rlon, rlat, 1 ), count = c(1,1,730))
rrain2 <- ncvar_get( ncin, varid = "tp", start = c(rlon, rlat, 1 ), count = c(1,1,730))

rsf <- rsf1 + rsf2
rrain <- rrain1 + rrain2

weather <- data.frame( temp = rtempsC, snowfall = rsf, precip = rrain )
save( weather , file = "weather_reading_2015and2016.Rdata" )
