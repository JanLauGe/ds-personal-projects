#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

server = ECMWFDataServer(url="https://api.ecmwf.int/v1", 
                         key=input("Enter Web API key: "), 
                         email="laurensgeffert@gmail.com")

years = range(2002, 2017, 1)
for year in years:
    server.retrieve({
        "class": "ei",
        "dataset": "interim",
        "date": str(year) + "-01-01/to/" + str(year) + "-12-31",
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
        "target": "D:\\projects\\ecmwf\\" + str(year) + ".nc",
    })