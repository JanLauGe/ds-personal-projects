
# Get started:
# www.tidyverse.org


# I took some RStudio example data
# devtools::install_github('rstudio/EDAWR')
library(EDAWR)
pollution

library(nycflights13)
flights



# readr  -----------------------------------------------------------------------
# cheatsheet: https://rawgit.com/rstudio/cheatsheets/master/data-import.pdf

flights
# without readr: export import...
system.time(write.csv(flights, '~/projects/personal/tidyverse-pitch/baseR.csv'))
system.time(df_baseR <- read.csv('~/projects/personal/tidyverse-pitch/baseR.csv'))

head(df_baseR) # what is X???
str(df_baseR)  # why factors???

# it takes a lot of arguments and effort to do it properly:
read.csv(
  file = '~/projects/personal/tidyverse-pitch/baseR.csv',
  stringsAsFactors = FALSE,
  row.names = 1)

# with readr
library(here)
library(readr)
here()
system.time(write_csv(flights, here('readR.csv')))
system.time(df_readr <- read_csv(here('readR.csv')))

# so pretty! with additional info!
df_readr

# btw, also works really well with dates etc!
df_readr$time_hour[1:10]



# tibble  ----------------------------------------------------------------------

# data frames are annoying...
df_baseR
# Whoa!! 
# Where did my history go?
# What are the column names?


library(tibble)
df <- as_tibble(df_baseR)
df # this looks much nicer!
glimpse(df) # and this can be very helpful for tibbles with many columns

class(df_readr)



# magrittr ---------------------------------------------------------------------

# initialise some data
x <- runif(n = 10, min = -1, max = +1)
x <- c(x[1:9], NA)
y <- 2

# base R way
sum(exp(which.max(log(abs(x), base = 5))), y)

library(magrittr)
sum(x,y)
x %>% sum(y)

x
abs(x)
log(10, 5)
10 %>% log(5)

# the magrittr way
x %<>% 
  abs() %>% 
  log(base = 5) %>% 
  which.max() %>%
  exp() %>%
  sum(y)

df <- iris
df <- df[df$Species == 'asd',]

  
# ADVANTAGES
# - You'll sequence of operations from left to right, instead of inside and out
# - You'll avoid nested function calls
# - You'll minimize the need for local variables and function definitions
# - You'll make it easy to add steps anywhere in the sequence of operations.
# - It works really well with dplyr!!

# SIDENOTE: Other pipes
# %<>%    -  Compound assignment operator
# %$%     -  Exposition operator
# %T>%    -  Tee operator

# Tal Galili
# https://www.r-statistics.com/2014/08/simpler-r-coding-with-pipes-the-present-and-future-of-the-magrittr-package/
# Revolutions R
# https://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html
# datacamp
# https://www.datacamp.com/community/tutorials/pipe-r-tutorial
# ...



# dplyr ------------------------------------------------------------------------
# cheatsheet: https://ugoproto.github.io/ugo_r_doc/dplyr.pdf

# some baseR operations are illegible
pollution[pollution$city == 'London', c('size', 'amount')]

# dplyrs "grammar for data manipulation" reads pretty much like english
library(dplyr)
pollution %>%
  filter(city == 'London') %>%
  select('size', 'amount')

# surprise, you don't even need to quote column names
pollution %>%
  filter(city == 'London') %>%
  select(size, amount)

# there is other funky functions as well:
# mutate makes new columns
pollution %>%
  # calculate ppb (parts per billion)
  mutate(
    ppb = amount * 1e6,
    ppt = amount * 1e9,
    pps = amount * 1e12)

# summarise aggregates data from groups
pollution %>%
  # calculate total parts per million
  group_by(city) %>%
  summarize(total = sum(amount))



# tidyr ------------------------------------------------------------------------
# cheatsheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

pollution

# what if we want this?
#
#       city  large  small
# 1  Beijing    121    56
# 2   London     22    16
# 3 New York     23    14

# reshaping data can be tedious in base
pollution_small <- pollution[pollution$size == 'small', c('city', 'amount')]
pollution_large <- pollution[pollution$size == 'large', c('city', 'amount')]
colnames(pollution_large) <- c('city', 'large')
colnames(pollution_small) <- c('city', 'small')
pollution_small; pollution_large
merge(pollution_large, pollution_small, by = 'city')

# this is MUCH better!
library(tidyr)
spread(pollution, key = size, value = amount)

# map
storms

# OTHER TIDYVERSE PACKAGES:
# stringr     - for strings
# forcats     - for factors
# lubridate   - for dates
# purrr       - for lists
#
# RESOURCES:
# A gRadual intRoduction to the tidyverse - https://tidyverse-intro.github.io/
# Learn the tidyverse - https://www.tidyverse.org/learn/



# THE TIDYVERSE IN ACTION ======================================================

library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)

# bigger storms dataset ships with dplyr

# Everybody has heard about Hurrican Katrina
storms %>%
  filter(name == 'Katrina' & year == 2005) %>%
  # get proper timestamps
  mutate(
    date = str_c(year, '/', month, '/', day),
    timestamp = str_c(date, ' ', hour, ':00:00') %>% ymd_hms()) %>%
  # plot evolution
  ggplot(aes(x = timestamp, y = wind, colour = pressure, size = pressure)) + 
  geom_path()

# EDA - did storms get stronger overtime?
storms %>%
  group_by(year, name) %>%
  summarise_at(
    .vars = c('category', 'wind', 'pressure'), 
    .funs = max) %>%
  ggplot(aes(x = year, y = wind, group = name)) +
  geom_point(alpha = .2)
  ggtitle('storm wind speed over time')

# non-unique names!
# which ones???
storms %>%
  select(name, year) %>%
  group_by(name) %>%
  summarise(nyears = n_distinct(year)) %>%
  arrange(desc(nyears))


set.seed(123)
stormsample <- storms %>%
  # make a unique ID
  mutate(id = group_indices(., name, year)) %>%
  # sample some storms (pseudo)-randomly
  filter(id %in% sample(.$id, size = 9))

stormsample %>%
  mutate(
    date = str_c(year, '/', month, '/', day),
    timestamp = str_c(date, ' ', hour, ':00:00') %>% ymd_hms()) %>%
  # plot evolution
  ggplot(aes(x = timestamp, y = wind, colour = pressure)) +
  geom_path() +
  facet_wrap('name', scales = 'free_x')


#' Additional resources:
#' - tidy data paper: https://www.jstatsoft.org/article/view/v059i10


#' Managing many models with R
#' Nested      (tidyr)
#' Functional  (purrr)
#' Models      (broom)
#' 
#' https://www.youtube.com/watch?v=rz3_FDVt9eg



# function to fit a model to iris data
fit_model <- function(df) {
  model <- lm(
    formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
    data = df)
  return(model)
}

df_nested <- iris %>%
  group_by(Species) %>%
  nest() %>%
  mutate(
    model = map(.x = data, .f = fit_model)
  )

df_nested$model[1]



# SNOWFLAKE ====================================================================

# connect to snowflake
library(dplyr.snowflakedb)
connection_parameters <- c(
  Driver = "/opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib", 
  ACCOUNT = "nmc", 
  SERVER = "yourserver", 
  UID = "youruser", 
  PWD = "yourpassword", 
  WAREHOUSE = "yourwarehouse", 
  DATABASE = "yourdatabase", 
  SCHEMA = "PUBLIC", 
  SSL = "on")

sf_jdbc_jar <- "/opt/snowflake/snowflakeodbc/lib/universal/snowflake-jdbc-3.6.13.jar"
options(dplyr.jdbc.classpath = sf_jdbc_jar)
con_sf <- dplyr.snowflakedb::src_snowflakedb(
  user = connection_parameters["UID"][[1]], 
  password = connection_parameters["PWD"][[1]], 
  account = connection_parameters["ACCOUNT"][[1]], 
  host = connection_parameters["SERVER"][[1]], 
  opts = list(
    warehouse = connection_parameters["WAREHOUSE"][1], 
    db = connection_parameters["DATABASE"][1], 
    schema = connection_parameters["SCHEMA"][1], 
    ssl = connection_parameters["SSL"][1]))

# get data from R to snowflake
sf_tab <- copy_to(con_sf, iris, mode = 'append')

sf_tab %>%
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  mutate(new = Sepal.Length * Petal.Length) %>%
  collect()

# dplyr logic on snowflake tables
db_list_tables(con_sf$con)

tbl(con_sf, LOOKUP_SEGMENT)

# Read more:
# Snowflake blog post - https://www.snowflake.com/blog/integrating-the-snowflake-data-warehouse-with-r-via-dplyr/



# Use feather to exchange data with python
library(feather)
write_feather(
  x = storms,
  path = here('storms.feather'))
