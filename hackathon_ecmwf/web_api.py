#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

server = ECMWFDataServer(url="https://api.ecmwf.int/v1", 
                         key="", 
                         email="laurensgeffert@gmail.com")

# Retrieve data in netCDF format
server.retrieve({
        'stream': "oper",
        'levtype': "sfc",
        'param': "167",
        'dataset': "interim",
        'step': "0",
        'grid': "0.5/0.5",
        'area': "90/-180/-90/179.5",
        'time': "00/06/12/18",
        'date': "2014-07-01/to/2014-07-31",
        'type': "an",
        'class': "ei",
        'format': "netcdf",
        'target': "C:\\Users\\Laurens\\test.nc"
    })

#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ei",
    "dataset": "interim",
    "date": "2001-01-01/to/2001-12-31",
    "expver": "1",
    "grid": "0.5/0.5",
    'area': "51.5/-1/51.5/-1",
    "levtype": "sfc",
    "param": "49.128/141.128/144.128/164.128/165.128/166.128/167.128/168.128/182.128/202.128/206.128/228.128",
    # step 0 for analysis data
    "step": "6/12", 
    "stream": "oper",
    # 0, 6, 12, 18 for analysis data
    "time": "00:00:00/12:00:00", 
    # "an" for analysis data
    "type": "fc", 
    'format': "netcdf",
    "target": "D:\\projects\\ecmwf\\2001.nc",
})