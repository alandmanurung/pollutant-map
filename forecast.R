library(reticulate)
library(rnaturalearth)

# reticulate::py_run_file("retrieve_atm2.py")

unzip("download.netcdf_zip", exdir = "./atm_data")

india <- rnaturalearth::ne_countries(country = "india")
india_states <- rnaturalearth::ne_states(country = "india")

get_polTable <- function(file){
  fc_pm25 <- raster::brick(file, varname = "pm2p5") %>% mask(india)
  fc_pm10 <- raster::brick(file, varname = "pm10") %>% mask(india)
  fc_co <- raster::brick(file, varname = "tcco") %>% mask(india)
  fc_no2 <- raster::brick(file, varname = "tcno2") %>% mask(india)
  fc_so2 <- raster::brick(file, varname = "tcso2") %>% mask(india)
  fc_o3 <- raster::brick(file, varname = "gtco3") %>% mask(india)
  
  polList <- list(
    pm25 = fc_pm25, 
    pm10 = fc_pm10,
    co = fc_co,
    no2 = fc_no2,
    so2 = fc_so2,
    o3 = fc_o3
  )
}

forecastTable <- get_polTable("./atm_data/data.nc")

indices <- format(as.Date(names(forecastTable$co), format = "X%Y.%m.%d"), format = "%d") %>% as.numeric()

find_names <- function(start, end){
  a <- as.Date(start, format = "X%Y.%m.%d")
  b <- as.Date(end, format = "X%Y.%m.%d")
  seq(a, b, by = "days") %>% 
    format("X%Y.%m.%d")
}

fcast_names <- find_names(min(names(forecastTable$pm25)), max(names(forecastTable$pm25)))

forecastTable$pm25 <- stackApply(forecastTable$pm25, indices, fun = mean)
forecastTable$pm10 <- stackApply(forecastTable$pm10, indices, fun = mean)
forecastTable$co <- stackApply(forecastTable$co, indices, fun = mean)
forecastTable$no2 <- stackApply(forecastTable$no2, indices, fun = mean)
forecastTable$so2 <- stackApply(forecastTable$so2, indices, fun = mean)
forecastTable$o3 <- stackApply(forecastTable$o3, indices, fun = mean)

names(forecastTable$pm25) <- fcast_names
names(forecastTable$pm10) <- fcast_names
names(forecastTable$co) <- fcast_names
names(forecastTable$no2) <- fcast_names
names(forecastTable$so2) <- fcast_names
names(forecastTable$o3) <- fcast_names

fc_timeSeries <- function(area){
  
  if (area != "All") {
    maskArea <- subset(india_states, india_states$name == area)
    ts_pm25 <- mask(forecastTable$pm25, maskArea)
    ts_pm10 <- mask(forecastTable$pm10, maskArea)
    ts_co <- mask(forecastTable$co, maskArea)
    ts_no2 <- mask(forecastTable$no2, maskArea)
    ts_so2 <- mask(forecastTable$so2, maskArea)
    ts_o3 <- mask(forecastTable$o3, maskArea)
    
  } else {
    ts_pm25 <- forecastTable$pm25
    ts_pm10 <- forecastTable$pm10
    ts_co <- forecastTable$co
    ts_no2 <- forecastTable$no2
    ts_so2 <- forecastTable$so2
    ts_o3 <- forecastTable$o3
  }
  
  polTS <- tibble(
    id = names(forecastTable$co),
    date = as.Date(id, "X%Y.%m.%d"),
    pm25 = NA,
    pm10 = NA,
    co = NA,
    no2 = NA,
    so2 = NA,
    o3 = NA
  ) %>% 
    column_to_rownames("id")
  
  for (dates in names(forecastTable$co)) {
    polTS[dates, "pm25"] <- subset(ts_pm25, dates) %>% values() %>% mean(na.rm = TRUE) * 1e9
    polTS[dates, "pm10"] <- subset(ts_pm10, dates) %>% values() %>% mean(na.rm = TRUE) * 1e9
    polTS[dates, "co"] <- subset(ts_co, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
    polTS[dates, "no2"] <- subset(ts_no2, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
    polTS[dates, "so2"] <- subset(ts_so2, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
    polTS[dates, "o3"] <- subset(ts_o3, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
  }
  
  polTS %>%
    as_tibble()
}