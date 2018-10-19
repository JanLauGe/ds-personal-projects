# Accident and break-down prediction for Reading Buses

## Inspiration
We were impressed with the approach of Reading Buses to make their data open and available. 
We wanted to use their data to show that open data creates win-win situations between the public sector, 
the private sector, and civil society. It was our ambition to create a real, value-adding product for 
Reading Buses using ECMWF data. 

## What it does
Our model predicts probability of accidents for buses on bus lines in response to weather conditions.
Key data sets we combined included line routes and Claims data from Reading buses alongside Weather 
data from the MARS archive in particular data sets relating to Total Column Ozone, Precipitation, 
Sonw fall and Evaporation.

## How we built it
We used data on bus collisions from 2001 to 2016 and ECMWF data from MARS extracted via the python API. 
We started off with exploratory data analysis in R and plotly (included as html).
Models were built in R using generalised linear models with lasso and ridge regression, 
cross-validation for hold-out test and validation datasets, and plots.
We identified the key words of our challege to be “predicting breakdowns”. Following this on a qualitative 
level we brainstormed a series of questions using the S.Mm.A.R.T (specific, Measures, Motivation, Actions, Routes) 
acrynom. Questions we asked included where were major claims occuring? What are the quantities and measures of the Claims? 
Etc. We then identified data sets that we could combine to help us answer these questions and form hypotheses.

## Challenges we ran into
Cleaning the bus data was challenging, and it would have been nice to get the MARS data directly into R.
However, the python API did a great job in the end! Perhaps we should come back to the dataset and write an
R package implementation of the Web API library?

## Accomplishments that we're proud of
Built a model with an out-of-sample AUC of 0.76! AUC is a measure of predictive accuracy, 
with 0.5 being random and 1 being a perfect prediction. 0.76 indicates a pretty decent result for a real-world, 
large-scale dataset like this. Quite a bit better than we had initially anticipated! 


## Team name & members
Predicting Accidents for Reading Buses

* Fiona Grimson - Fiona.grimson@gmail.com
* Rajesh Sydlon - rajesh.sydlon@gmail.com
* Laura Castrillo - Castrillo@gmail.com
* Mo - mtarky@outlook.com
* Gordon Rates - wegiangb@hotmail.co.uk
* Laurens Geffert - laurensgeffert@gmail.com


## Lessons learnt
*"How to extract MARS data via the python API, and how to deal with NetCDF format data in R.
If we could start all over again we would force John to stay with us the weekend ;)
More of his domain expertise on the could have benefited a lot!"* -Laurens

*"I learnt a lot about how to use data to back up hypotheses. Coming from a strategic background, working alongside data scientists gave me my first taste of hands-on cleaning of data and using data i nreal time to inform (or invalidate) hypotheses."* - Rajesh

*"Weather dataset can be very varied and correlate to even bus crashes."*  - Gordon

*"There is so much potential for interdisciplinary collaborations. Everybody had a different background and skills which were all important in getting the analysis done!"* - Fiona


## Future developments
We are pretty confident that Reading Buses will be interested in using our model. 
We are handing over code and model, and hope that there will be opportunities to build on this work in the future! 
Other data can be included to improve the model, such as bus operations, special events in Reading, and time of the day. We are interested in learning more about developing this work in the Reading buses innovator programme, testing our work in small scale pilots within Reading buses and working with the Finance, Engineering and Marketing teams to build economic models around our products. We also brainstormed a number of ideas around both the customer experience and acquisition of more customers.

**Additional Data sets...**
- More weather data, perhaps of high spatial resolution
- Including events like Reading Festival in the model
- More bus operations data and passenger surveys

**Expand predictive analytics within Reading Buses...**
- Scheduling engineers 
- Optimising customer experience ( information about delays )

**Write an R package for accessing ECMWF data? (rOpenSci)**
