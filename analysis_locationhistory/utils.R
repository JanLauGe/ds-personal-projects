
# extract location data from takeout JSON
get_location_from_json <- function(fnames) {
  
  message(str_glue('now processing {fname}'))
  
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
  
  # get location from places
  df <- df_places %>%
    transmute(
      type = 'place_main',
      context = 'obs',
      time_start = as.numeric(duration.startTimestampMs),
      lat = location.latitudeE7,
      lng = location.longitudeE7,
      time_end = as.numeric(duration.endTimestampMs)) %>%
    # convert to rows with start and end info
    pivot_longer(starts_with('time')) %>%
    drop_na() %>%
    select(type, context, time=value, lat, lng)
  
  if ('simplifiedRawPath.points' %in% colnames(df_places)) {
    df <- df_places %>%
      unnest(simplifiedRawPath.points) %>%
      transmute(
        type = 'place_raw',
        context = 'obs',
        time = as.numeric(timestampMs),
        lat = latE7,
        lng = lngE7) %>%
      bind_rows(df)
  }
  
  # get location from activities
  df <- df_activities %>%
    transmute(
      id = row_number(),
      type = 'activity_main',
      time_start = as.numeric(duration.startTimestampMs),
      lat_start = startLocation.latitudeE7,
      lng_start = startLocation.longitudeE7,
      time_end = as.numeric(duration.endTimestampMs),
      lat_end = endLocation.latitudeE7,
      lng_end = endLocation.longitudeE7) %>%
    # convert to rows with start and end info
    pivot_longer(time_start:lng_end) %>%
    separate(name, into=c('coordinate', 'context'), sep='_') %>%
    pivot_wider(names_from=coordinate, values_from = value) %>%
    drop_na() %>%
    select(-id) %>%
    bind_rows(df)
  
  if ('waypointPath.waypoints' %in% colnames(df_activities)) {
    df <- df_activities %>%
      unnest(waypointPath.waypoints) %>%
      transmute(
        type = 'activity_waypoints',
        context = 'obs',
        time = as.numeric(duration.startTimestampMs),
        lat = latE7,
        lng = lngE7) %>%
      bind_rows(df)
  }
  
  if ('simplifiedRawPath.points' %in% colnames(df_activities)) {
    df <- df_activities %>%
      unnest(simplifiedRawPath.points) %>%
      transmute(
        type = 'activity_raw',
        context = 'obs',
        time = as.numeric(timestampMs),
        lat = latE7,
        lng = lngE7) %>%
      bind_rows(df)
  }
  
  return(df)
}