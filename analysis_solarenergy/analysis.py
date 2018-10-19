# -*- coding: utf-8 -*-
"""
Created on Thu Sep  7 14:05:47 2017

@author: Laurens
"""

# Required libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
sns.set()

# Change working directory
%cd D:\ubuntu\projects\task_Octopusenergy

# Load data from csvs
ko_radiation = pd.read_csv('data/kia-ora-radiation.csv')
ko_weather = pd.read_csv('data/forecast_data.csv')
ko_output = pd.read_csv('data/kia-ora.csv')
ko_prices = pd.read_csv('data/prices.csv')

ko_weather.info()
ko_weather['summary'].value_counts(dropna=False)
ko_weather.describe()

ko_radiation.plot(kind='hist')
ko_radiation.plot()

# Convert data types
ko_radiation['timeUTC'] = pd.to_datetime(ko_radiation['timeUTC'])


_ = plt.hist(ko_output['production_kWh'])
_ = plt.xlabel('production (kWh)')
_ = plt.ylabel('count')
plt.show()
