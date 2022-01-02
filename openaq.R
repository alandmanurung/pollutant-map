# Get Dataset
get_latest <- function() {
    limit <- 1
    json_file <- paste0(
        "https://u50g7n0cbj.execute-api.us-east-1.amazonaws.com/v2/latest?",
        "limit=", limit,
        "&page=1",
        "&offset=0",
        "&sort=desc",
        "&radius=1000",
        "&country_id=IN",
        "&order_by=lastUpdated",
        "&dumpRaw=false"
    )

    json_data <- fromJSON(file = json_file)
    limit <- json_data$meta$found

    # Re-search the latest datasets
    json_file <- paste0(
        "https://u50g7n0cbj.execute-api.us-east-1.amazonaws.com/v2/latest?",
        "limit=", limit,
        "&page=1",
        "&offset=0",
        "&sort=desc",
        "&radius=1000",
        "&country_id=IN",
        "&order_by=lastUpdated",
        "&dumpRaw=false"
    )

    json_data <- fromJSON(file = json_file)

    create_row <- function(){
        tibble(
            Location = NA,
            Lat = NA,
            Long = NA,
            pm25 = NA,
            um010 = NA,
            um025 = NA,
            um100 = NA,
            pm1 = NA,
            pm10 = NA,
            co = NA,
            o3 = NA,
            no2 = NA,
            so2 = NA,
        )
    }

    stations_df <- create_row()

    for (obs in json_data$results) {
        measurement <- obs$measurements
        tmp_df <- create_row()

        tmp_df$Location <- obs$location
        tmp_df$Lat <- obs$coordinates$latitude
        tmp_df$Long <- obs$coordinates$longitude

        for (objects in measurement) {
            tmp_df[objects$parameter] <- objects$value
        }

        stations_df <- add_row(stations_df, tmp_df)
    }
    stations_df
}

india_aq <- get_latest()
india_aq <- india_aq %>%
  dplyr::select(Lat, Long, pm25, co, no2, so2) %>% 
  filter(!is.na(Lat), !is.na(Long))

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +toWGS84=0,0,0"

india_sf <- st_as_sf(
    india_aq,
    coords = c("Long", "Lat"),
    crs = projcrs
)

# Get Dataset for Date Picking
get_date <- function(startDate, endDate) {
  limit <- 1
  json_file <- paste0(
    "https://u50g7n0cbj.execute-api.us-east-1.amazonaws.com/v2/latest?",
    "limit=", limit,
    "&page=1",
    "&offset=0",
    "&sort=desc",
    "&radius=1000",
    "&country_id=IN",
    "&order_by=lastUpdated",
    "&dumpRaw=false"
  )
  
  json_data <- fromJSON(file = json_file)
  limit <- json_data$meta$found
  
  # Re-search the latest datasets
  json_file <- paste0(
    "https://u50g7n0cbj.execute-api.us-east-1.amazonaws.com/v2/latest?",
    "limit=", limit,
    "&page=1",
    "&offset=0",
    "&sort=desc",
    "&radius=1000",
    "&country_id=IN",
    "&order_by=lastUpdated",
    "&dumpRaw=false"
  )
  
  json_data <- fromJSON(file = json_file)
  
  create_row <- function(){
    tibble(
      Location = NA,
      Lat = NA,
      Long = NA,
      pm25 = NA,
      um010 = NA,
      um025 = NA,
      um100 = NA,
      pm1 = NA,
      pm10 = NA,
      co = NA,
      o3 = NA,
      no2 = NA,
      so2 = NA,
    )
  }
  
  stations_df <- create_row()
  
  for (obs in json_data$results) {
    measurement <- obs$measurements
    tmp_df <- create_row()
    
    tmp_df$Location <- obs$location
    tmp_df$Lat <- obs$coordinates$latitude
    tmp_df$Long <- obs$coordinates$longitude
    
    for (objects in measurement) {
      tmp_df[objects$parameter] <- objects$value
    }
    
    stations_df <- add_row(stations_df, tmp_df)
  }
  stations_df
}