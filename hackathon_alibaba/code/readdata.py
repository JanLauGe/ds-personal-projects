#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sat Jan 20 09:59:32 2018

@author: alex
"""


import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
#import matplotlib.pyplot as plt
#from sklearn import linear_model, datasets

from sklearn import linear_model

input_file = "training.csv"
input_file1 = "city.csv"
input_file2 = "insitu.csv"
input_file3 = "test.csv"

# comma delimited is the default
train = pd.read_csv(input_file, header = 0)
city = pd.read_csv(input_file1, header = 0)
insitu = pd.read_csv(input_file2, header = 0)
test = pd.read_csv(input_file3, header = 0)