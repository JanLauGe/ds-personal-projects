
library(tidyverse)
library(magrittr)
library(jsonlite)
library(lubridate)
library(assertthat)
library(fs)


### SINGLE FILE EXAMPLE --------------------------------------------------------

# read json
fname <- 'data/Semantic Location History/2014/2014_MAY.json'

json <- fromJSON(
  txt=fname,
  simplifyVector=TRUE,
  flatten=FALSE)

# check JSON sub-lists look okay
assert_that({
  assertthat::are_equal(length(json[[1]]), 2)
  assertthat::has_name(json[[1]], list('placeVisit', 'activitySegment'))
})

# get sub-elements with place and activity data
df_places <- json %>%
  pluck(1) %>%
  pluck('placeVisit') %>%
  jsonlite::flatten(recursive=F) %>%
  tibble()
df_activities <- json %>%
  pluck(1) %>%
  pluck('activitySegment') %>%
  jsonlite::flatten(recursive=F) %>%
  tibble()

# helper function
get_addr_last_row <- function(string) {
  string %>%
    str_split('\\n', simplify = TRUE) %>%
    last()
}

# get country and timestamps
df <- bind_rows(
  df_places %>% mutate(
    id = row_number(),
    type = 'place',
    country = location.address %>% map_chr(get_last_row)),
  df_activities %>% mutate(
    id = row_number() + nrow(df_places),
    type = 'activity',
    country_start = startLocation.address %>% map_chr(get_last_row),
    country_end = endLocation.address %>% map_chr(get_last_row))) %>%
  select(
    id,
    type,
    country,
    country_start,
    country_end,
    duration.startTimestampMs,
    duration.endTimestampMs) %>%
  transmute(
    id,
    counrty = coalesce(country, country_start, country_end),
    time_start = as_datetime(as.numeric(duration.startTimestampMs)/1e3),
    time_end = as_datetime(as.numeric(duration.endTimestampMs)/1e3)) %>%
  drop_na() %>%
  arrange(time_start)
  
sample_n(df, 20)


### PROGRAMMATIC ---------------------------------------------------------------

# helper function
get_addr_last_row <- function(string) {
  string %>%
    str_split('\\n', simplify = TRUE) %>%
    last()
}

# single file function
extract_cntry_from_json <- function(fnames) {
  
  message(str_glue('now processing {fname}'))
  
  # read json file
  json <- fromJSON(
    txt=fname,
    simplifyVector=TRUE,
    flatten=FALSE)
  
  # check JSON sub-lists look okay
  assert_that({
    assertthat::are_equal(length(json[[1]]), 2)
    assertthat::has_name(json[[1]], list('placeVisit', 'activitySegment'))
  })
  
  # get sub-element with place data
  df_places <- json %>%
    pluck(1) %>%
    pluck('placeVisit') %>%
    jsonlite::flatten(recursive=F) %>%
    tibble() %>%
    mutate(
      id = row_number(),
      type = 'place',
      country = location.address %>% map_chr(get_last_row))
  
  # get sub-element with activity data
  df_activities <- json %>%
    pluck(1) %>%
    pluck('activitySegment') %>%
    jsonlite::flatten(recursive=F) %>%
    tibble()
  
  # edge case: no activity addresses
  if (
    'startLocation.address' %in% names(df_activities) &
    'endLocation.address' %in% names(df_activities)) {
    
    df_activities %<>%
      mutate(
        id = row_number() + nrow(df_places),
        type = 'activity',
        country_start = startLocation.address %>% map_chr(get_last_row),
        country_end = endLocation.address %>% map_chr(get_last_row))
    
    df <- bind_rows(
      df_places,
      df_activities) %>%
      mutate(country = coalesce(country, country_start, country_end))
      
  } else {
    df <- df_places
  }
  
  # get country and timestamps from raw
  df %<>% select(
    id,
    type,
    country,
    duration.startTimestampMs,
    duration.endTimestampMs)
    
  # formatting
  df %<>%
    transmute(
      id,
      country,
      time_start = as_datetime(as.numeric(duration.startTimestampMs)/1e3),
      time_end = as_datetime(as.numeric(duration.endTimestampMs)/1e3)) %>%
    drop_na() %>%
    arrange(time_start)
  
  return(df)
}



# list all files from takeout
fnames <- fs::dir_ls(
  path='data/Semantic Location History',
  type='file',
  recurse=TRUE)

df = tibble()
for (fname in fnames) {
  df <- extract_cntry_from_json(fname) %>%
    bind_rows(df, .)
}

df %>%
  group_by(country) %>%
  summarize(n = n()) %>%
  arrange(desc(n))