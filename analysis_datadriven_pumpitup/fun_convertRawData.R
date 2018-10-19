
convertRawData <- function(raw) {
  #' This function converts raw input data into a form 
  #' that is suitable as model input
  #'
  #' Input: raw
  #' a data frame with raw features and labels from read.csv
  #' 
  #' Output: tidy_x 
  #' a data frame with proper formatting,
  #' dummified factorial variables,
  #' and explicit NAs
  
  # transform data types as appropriate
  x <- raw %>%
    mutate(
      # convert date to date format
      date_recorded = as.Date(date_recorded),
      # convert codes from integer to factor
      region_code = as.factor(region_code),
      district_code = as.factor(district_code),
      # convert true/false to binary variables
      permit = as.logical(permit) %>% as.integer,
      public_meeting = as.logical(public_meeting) %>% as.integer) %>%
    # convert strings to factors
    mutate_if(.predicate = is.character, 
              .funs = funs(as.factor))
  
  
  # replace missing values with na:
  # height zero seems to indicate missing value
  x[x$gps_height == 0,'gps_height'] <- NA
  x[x$longitude == 0,'longitude'] <- NA
  x[x$latitude == 0,'latitude'] <- NA
  # # empty strings seems to indicate missing value
  # x[x$funder == '','funder'] <- NA
  # x[x$installer == '','installer'] <- NA
  # x[x$subvillage == '','subvillage'] <- NA
  # x[x$scheme_name == '', 'scheme_name'] <- NA
  # x[x$scheme_management == '', 'scheme_management'] <- NA
  
  # Missing values in construction year.
  # Would it be more reasonable to use the min or the median?
  # summary(x$construction_year[x$construction_year != 0])
  x[x$construction_year == 0, 'construction_year'] <- 2000
  
  # infer age of pump
  x %<>% mutate(
    # get year recorded from date
    year_recorded = year(date_recorded),
    # get age of pump at date of observation
    age = year_recorded - construction_year,
    # and change year recorded back to a factor
    year_recorded = as.factor(year_recorded),
    # get month recorded from date
    month_recorded = lubridate::month(date_recorded, label = TRUE) %>%
      as.character %>%
      as.factor)
  
  # exclude duplicate variables
  x %<>% select(
    -payment_type,
    -extraction_type_group,
    -quality_group,
    -quantity_group,
    -recorded_by,
    -waterpoint_type)
  
  # some factor variables have too many levels
  high_lvl_factors <- lapply(x, FUN = function(x) length(levels(x)))
  # Exclude factors with more than 10 levels
  x %<>% select(-one_of(which(high_lvl_factors > 21) %>% names))
  
  # all numerical variables
  x_numeric <- x[,lapply(x, class) != 'factor']
  # subset with all remaining categorical variables
  x_factors <- x[,lapply(x, class) == 'factor'] %>%
    # but exclude label
    select(-label)
  # dummify factors ("one-hot-encoding")
  x_dummies <- x_factors %>%
    dummyVars(' ~ .', data = .) %>%
    predict(newdata = x_factors) %>%
    data.frame()
  
  # recombine numeric and dummified data
  tidy_x <- cbind(x_numeric, x_dummies, label = x$label)
  
  return(tidy_x)
}