# Modelling species distributions with R

This is a first prototype for a datacamp project. The attached code produces the datasets that will be used in the project and outlines the general workflow that students would follow (although it will be cut down and simplified significantly).


## Motivation

There are three basic ideas to the project:

1) introduce students to web APIs in general and those accessible through the
rOpenSci initiative in particular as a data source of data science projects.

2) teach the application of model building and hyperparameter tuning with caret,
using regularised glms as an example

3) illustrate how the tidyverse approach, especially purrr::map, can be used
to quickly apply the same method to a range of datasets and hence create
a variety of outputs that can be compared to one another for insight.


## Data Sources

1) Climatic Data

This collection contains datasets of climate variables derived from the network of UK land surface observations. The data have been interpolated from meteorological station data onto a uniform grid to provide complete and consistent coverage across the UK. The data sets cover the UK at 5 x 5 km resolution and span the period 1910 - 2015. They are available at daily, monthly and annual timescales, as well as long-term averages for the periods 1961 - 1990, 1971 - 2000, and 1981 - 2010. Baseline averages are also available at 25 x 25 km resolution to match the UKCP09 climate change projections.

The primary purpose of this data resource is to encourage and facilitate research into climate change impacts and adaptation. The datasets have been created by the Met Office with financial support from the Department for Environment, Food and Rural Affairs (Defra) and are promoted within the UK Climate Projections (UKCP09). The UKCP09 report The climate of the UK and recent trends uses these gridded data sets to describe UK climatoloagies and regional trends.

Citable as:Met Office; Hollis, D.; McCarthy, M. (2017): UKCP09: Met Office gridded and regional land surface climate observation datasets. Centre for Environmental Data Analysis, date of citation. http://catalogue.ceda.ac.uk/uuid/87f43af9d02e42f483351d79b3d6162a


2) Species Data

GBIF—the Global Biodiversity Information Facility—is an international network and research infrastructure funded by the world’s governments and aimed at providing anyone, anywhere, open access to data about all types of life on Earth.

Here, GBIF data is accessed via the rgbif package, developed by the rOpenSci initiative.

rOpenSci fosters a culture that values open and reproducible research using shared data and reusable software. We do this by:
* Creating technical infrastructure in the form of carefully vetted, staff- and community-contributed R software tools that lower barriers to working with scientific data sources on the web
* Creating social infrastructure through a welcoming and diverse community
* Making the right data, tools and best practices more discoverable
* Building capacity of software users and developers and fostering a sense of pride in their work
* Promoting advocacy for a culture of data sharing and reusable software.

rOpenSci is a non-profit initiative founded in 2011 by Karthik Ram, Scott Chamberlain, and Carl Boettiger to make scientific data retrieval reproducible. Over the past seven years we have developed an ecosystem of open source tools, we run annual unconferences, and review community developed software.
