#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(leaflet)
library(rjson)
library(sf)
library(tidyverse)
library(magrittr)
library(reticulate)
library(raster)
library(ncdf4)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthhires)
library(plotly)
library(rgdal)
library(rgeos)

# Get CDS CAMS Atmospheric Dataset
# reticulate::use_condaenv("r-reticulate")
# reticulate::py_run_file("retrieve_atm.py")

# Load India and states polygons
india <- rnaturalearth::ne_countries(scale = "large", country = "india")
india_states <- rnaturalearth::ne_states(country = "india")
aqi_polygon <- sf::st_as_sf(india_states) %>% select(name)

# Read Monthly Raster Data
pm25 <- raster::brick("cams_atmosphereData.nc", varname = "pm2p5") %>% mask(india)
pm10 <- raster::brick("cams_atmosphereData.nc", varname = "pm10") %>% mask(india)
co <- raster::brick("cams_atmosphereData.nc", varname = "tcco") %>% mask(india)
no2 <- raster::brick("cams_atmosphereData.nc", varname = "tcno2") %>% mask(india)
so2 <- raster::brick("cams_atmosphereData.nc", varname = "tcso2") %>% mask(india)
o3 <- raster::brick("cams_atmosphereData.nc", varname = "gtco3") %>% mask(india)

# Create List of Pollutants
polList <- list(pm25, pm10, co, no2, so2, o3)
names(polList) <- c("pm25", "pm10", "co", "no2", "so2", "o3")

# Create Time Series Table
get_timeSeries <- function(date_start, date_end, area){
    
    if (area != "All") {
        maskArea <- subset(india_states, india_states$name == area)
        ts_pm25 <- mask(pm25, maskArea)
        ts_pm10 <- mask(pm10, maskArea)
        ts_co <- mask(co, maskArea)
        ts_no2 <- mask(no2, maskArea)
        ts_so2 <- mask(so2, maskArea)
        ts_o3 <- mask(o3, maskArea)
        
    } else {
      ts_pm25 <- pm25
      ts_pm10 <- pm10
      ts_co <- co
      ts_no2 <- no2
      ts_so2 <- so2
      ts_o3 <- o3
    }
    
    polTS <- tibble(
        id = names(co),
        date = as.Date(id, "X%Y.%m.%d"),
        pm25 = NA,
        pm10 = NA,
        co = NA,
        no2 = NA,
        so2 = NA,
        o3 = NA
    ) %>% 
        column_to_rownames("id")
    
    for (dates in names(co)) {
        polTS[dates, "pm25"] <- subset(ts_pm25, dates) %>% values() %>% mean(na.rm = TRUE) * 1e9
        polTS[dates, "pm10"] <- subset(ts_pm10, dates) %>% values() %>% mean(na.rm = TRUE) * 1e9
        polTS[dates, "co"] <- subset(ts_co, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
        polTS[dates, "no2"] <- subset(ts_no2, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
        polTS[dates, "so2"] <- subset(ts_so2, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
        polTS[dates, "o3"] <- subset(ts_o3, dates) %>% values() %>% mean(na.rm = TRUE) * 1e5
    }
    
    polTS %>% 
        filter(date >= date_start & date <= date_end) %>% 
      as_tibble()
}

# Choices for Pollutants
pollutants <- c("pm25", "pm10", "co", "no2", "so2", "o3")
pol_vars <- c(
    "PM2.5" = "pm25",
    "PM10" = "pm10",
    "CO" = "co",
    "NO2" = "no2",
    "SO2" = "so2",
    "Ozone" = "o3"
)
pol_aqi <- c(
  "PM2.5" = "pm25",
  "PM10" = "pm10"
)

# Choices for Indian States
source("india_state.R", echo = FALSE)

# Create Forecast Table
source("forecast.R", echo = FALSE)

# Define UI for application that shows the map
ui <- shinyUI(
    navbarPage(
        "Project 17",
            tabPanel(
            "Monthly Mean CAMS Map",
            div(
                class = "outer",
                tags$head(
                    includeCSS("style.css"),
                    tags$style(
                      "#controls_monthly {
                        overflow: auto;
                      }"
                    )
                ),
                leafletOutput("map_monthly", width = "100%", height = "100%"),
                absolutePanel(
                  id = "AQI_Table_monthly",
                  class = "panel panel-default",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 60,
                  left = 20,
                  right = "auto",
                  bottom = "auto",
                  width = 330,
                  height = "auto",
                  
                  h2("Air Quality Index (AQI)"),
                  
                  img(src = "AQI_Table.png", width = "100%"),
                  
                  h4("Air Quality Index were derived from parameters provided by Indian Central Pollution Board Control")
                ),
                absolutePanel(
                    id = "controls_monthly",
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = TRUE,
                    top = 60,
                    left = "auto",
                    right = 20,
                    bottom = "auto",
                    width = 330,
                    height = "auto",
                    
                    h2("Parameter Settings"),
                    
                    switchInput(inputId = "aqi_monthly", value = FALSE, label = "Show AQI"),
                    
                    conditionalPanel(
                      "input.aqi_monthly == true",
                      selectInput("pollutant_monthly_aqi", "Pollutant (AQI)", pol_aqi)
                    ),
                    
                    conditionalPanel(
                      "input.aqi_monthly == false",
                      selectInput("pollutant_monthly", "Pollutant", pol_vars)
                    ),
                    
                    airDatepickerInput(
                        "date_monthly",
                        "Date:",
                        value = "2015-01-01",
                        minDate = "2015-01-01",
                        maxDate = "2020-12-01",
                        view = "months",
                        minView = "months",
                        dateFormat = "yyyy-mm"
                    ),
                    
                    selectInput("state_month", "State", states_choices),
                    
                    uiOutput("mt_stateName"),
                    
                    plotlyOutput("monthlyTS", height = "auto"),
                    
                    h4("Contains modified Copernicus Atmosphere Monitoring Service Information")
                )
            )
        ),
        tabPanel(
            "Forecast CAMS Map",
            div(
                class = "outer",
                tags$head(
                    includeCSS("style.css")
                ),
                leafletOutput("map_forecast", width = "100%", height = "100%"),
                absolutePanel(
                  id = "AQI_Table_forecast",
                  class = "panel panel-default",
                  fixed = TRUE,
                  draggable = TRUE,
                  top = 60,
                  left = 20,
                  right = "auto",
                  bottom = "auto",
                  width = 330,
                  height = "auto",
                  
                  h2("Air Quality Index (AQI)"),
                  
                  img(src = "AQI_Table.png", width = "100%"),
                  
                  h4("Air Quality Index were derived from parameters provided by Indian Central Pollution Board Control")
                ),
                absolutePanel(
                    id = "controls_forecast",
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = FALSE,
                    top = 60,
                    left = "auto",
                    right = 20,
                    bottom = "auto",
                    width = 330, 
                    height = "auto",
                    
                    h2("Parameter Settings"),
                    
                    switchInput(inputId = "aqi_forecast", value = FALSE, label = "Show AQI"),
                    
                    conditionalPanel(
                      "input.aqi_forecast == true",
                      selectInput("pollutant_forecast_aqi", "Pollutant (AQI)", pol_aqi)
                    ),
                    
                    conditionalPanel(
                      "input.aqi_forecast == false",
                      selectInput("pollutant_forecast", "Pollutant", pol_vars)
                    ),
                    
                    airDatepickerInput(
                        "date_forecast",
                        "Date:",
                        value = min(names(forecastTable$pm25)) %>% as.Date(format = "X%Y.%m.%d"),
                        minDate = min(names(forecastTable$pm25)) %>% as.Date(format = "X%Y.%m.%d"),
                        maxDate = max(names(forecastTable$pm25)) %>% as.Date(format = "X%Y.%m.%d"),
                        view = "days",
                        minView = "days",
                        dateFormat = "dd-mm-yyyy"
                    ),
                    
                    selectInput("state_fcast", "State", states_choices),
                  
                    uiOutput("fc_stateName"),
                    
                    plotlyOutput("forecastTS", height = "auto"),
                    
                    h4("Contains modified Copernicus Atmosphere Monitoring Service Information")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create Leaflet Map
  output$map_monthly <- renderLeaflet({
    leaflet(india_states, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Stamen.Toner) %>% 
      setView(lng = 77, lat = 22, zoom = 5) %>% 
      addRasterImage(pm25$X2015.01.01, colors = "viridis")
  })
  
  output$map_forecast <- renderLeaflet({
    leaflet(data = india_states, leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 77, lat = 22, zoom = 5) %>% 
      addRasterImage(forecastTable$pm25[[names(forecastTable$pm25)[1]]], colors = "viridis")
  })
  
  # Create Time Series Plot
  
  output$monthlyTS <- renderPlotly({
    TS_Table <- get_timeSeries("2015-01-01", "2020-12-01", "All")
    
    mTS <- TS_Table %>% 
      pivot_longer(cols = -date, names_to = "pol", values_to = "value") %>% 
      mutate(pol = fct_relevel(pol, c("pm25", "pm10", "co", "no2", "so2", "o3"))) %>% 
      ggplot() +
      geom_line(aes(x = date, y = value, color = pol), show.legend = FALSE) +
      facet_wrap(
        pol ~ .,
        ncol = 1,
        scales = "free_y",
        labeller = labeller(
          pol = c(
            "pm25" = "PM 2.5",
            "pm10" = "PM 10",
            "co" = "Carbon Monoxide (CO)",
            "no2" = "Nitrogen Dioxide (NO2)",
            "so2" = "Sulphur Dioxide (SO2)",
            "o3" = "Ozone"
          ))
      ) + labs(x = "Date", y = "") + theme(legend.position = "none")
    
    ggplotly(mTS)
  })
  
  output$forecastTS <- renderPlotly({
    TS_Table <- fc_timeSeries("All")
    
    mTS <- TS_Table %>% 
      pivot_longer(cols = -date, names_to = "pol", values_to = "value") %>% 
      mutate(pol = fct_relevel(pol, c("pm25", "pm10", "co", "no2", "so2", "o3"))) %>% 
      ggplot() +
      geom_line(aes(x = date, y = value, color = pol), show.legend = FALSE) +
      facet_wrap(
        pol ~ .,
        ncol = 1,
        scales = "free_y",
        labeller = labeller(
          pol = c(
            "pm25" = "PM 2.5",
            "pm10" = "PM 10",
            "co" = "Carbon Monoxide (CO)",
            "no2" = "Nitrogen Dioxide (NO2)",
            "so2" = "Sulphur Dioxide (SO2)",
            "o3" = "Ozone"
          ))
      ) + labs(x = "Date", y = "") + theme(legend.position = "none")
    
    ggplotly(mTS)
  })
  
  # Observer for pollutant concentration input
  
  observe({
      ptn_month <- input$pollutant_monthly
      ptn_month_aqi <- input$pollutant_monthly_aqi
      ptn_fcast <- input$pollutant_forecast
      ptn_fcast_aqi <- input$pollutant_forecast_aqi
      
      polRange <- tibble(
        pollutant = c("pm25", "pm10", "co", "no2", "so2", "o3"),
        btm_range = c(0, 0, 20, 0, 0, 400),
        upr_range = c(600, 800, 300, 2, 3, 800)
      )
      
      pal_month <- colorNumeric(
        "viridis", 
        c(
          polRange[polRange$pollutant == ptn_month, "btm_range"] %>% as.numeric(),
          polRange[polRange$pollutant == ptn_month, "upr_range"] %>% as.numeric()
        ), 
        na.color = rgb(0, 0, 0, 0))
      
      pal_fcast <- colorNumeric(
        "viridis", 
        c(
          polRange[polRange$pollutant == ptn_fcast, "btm_range"] %>% as.numeric(),
          polRange[polRange$pollutant == ptn_fcast, "upr_range"] %>% as.numeric()
        ), 
        na.color = rgb(0, 0, 0, 0))
      
      if (!input$aqi_monthly){
        
        # Monthly Plot
        mapdate <- input$date_monthly
        mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
        
        mapRast_month <- polList[[ptn_month]] %>% subset(mapdate)
        colorData <- values(mapRast_month)
        
        leafletProxy("map_monthly") %>% 
          clearShapes() %>% 
          clearImages() %>% 
          addRasterImage(mapRast_month, colors = "viridis") %>% 
          addLegend(
            "bottomleft", 
            opacity = 1.0,
            pal = pal_month, 
            values = c(
              polRange[polRange$pollutant == ptn_month, "btm_range"] %>% as.numeric(),
              polRange[polRange$pollutant == ptn_month, "upr_range"] %>% as.numeric()
            ), 
            title = ifelse(
              ptn_month %in% c("pm25", "pm10"),
              paste(str_to_upper(ptn_month), "in µg/m^3"),
              paste(str_to_upper(ptn_month), "in µg/cm^2")
              ),
            layerId = "colorLegend",
            na.label = ""
          )

      } else {
        
        # Monthly Plot
        mapdate <- input$date_monthly
        mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
        
        mapRast_month <- polList[[ptn_month_aqi]] %>% subset(mapdate)
        colorData <- values(mapRast_month)
        
        aqiTable <- aqi_polygon %>%
          mutate(
            value = extract(mapRast_month, aqi_polygon) %>% lapply(mean) %>% unlist()
          )
        
        bins <- c(0, 50, 100, 200, 300, 400, 800)
        pal <- colorBin(c("#00b050", "#92d050", "#ffff00", "#ff9900", "#ff0000", "#c00000"), domain = aqiTable$value, bins = bins)
        
        leafletProxy("map_monthly", data = aqiTable) %>% 
          clearImages() %>% 
          addPolygons(
            fillColor = ~pal(value * 1e9),
            fillOpacity = 1.0,
            opacity = 1.0,
            color = "black",
            weight = 1.0,
          )

      }
      
      if (!input$aqi_forecast) {
        # Forecast Plot
        mapdate <- input$date_forecast
        mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
        
        mapRast_forecast <- forecastTable[[ptn_fcast]] %>% subset(mapdate)
        colorData <- values(mapRast_forecast)
        
        leafletProxy("map_forecast") %>% 
          clearImages() %>%
          clearShapes() %>% 
          addRasterImage(mapRast_forecast, colors = "viridis") %>% 
          addLegend(
            "bottomleft", 
            pal = pal_fcast,
            opacity = 1.0,
            values = c(
              polRange[polRange$pollutant == ptn_fcast, "btm_range"] %>% as.numeric(),
              polRange[polRange$pollutant == ptn_fcast, "upr_range"] %>% as.numeric()
            ), 
            title = ifelse(
              ptn_month %in% c("pm25", "pm10"),
              paste(str_to_upper(ptn_fcast), "in µg/m^3"),
              paste(str_to_upper(ptn_fcast), "in µg/cm^2")
            ), 
            layerId = "colorLegend",
            na.label = "",
          )
      } else {
        # Forecast Plot
        mapdate <- input$date_forecast
        mapdate <- paste0("X", format(mapdate, "%Y.%m.%d"))
        
        mapRast_fcast <- forecastTable[[ptn_fcast_aqi]] %>% subset(mapdate)
        colorData <- values(mapRast_fcast)
        
        aqiTable <- aqi_polygon %>%
          mutate(
            value = extract(mapRast_fcast, aqi_polygon) %>% lapply(mean) %>% unlist()
          )
        
        bins <- c(0, 50, 100, 200, 300, 400, 800)
        pal <- colorBin(c("#00b050", "#92d050", "#ffff00", "#ff9900", "#ff0000", "#c00000"), domain = aqiTable$value, bins = bins)
        
        leafletProxy("map_forecast", data = aqiTable) %>%
          clearImages() %>%
          addPolygons(
            fillColor = ~pal(value * 1e9),
            fillOpacity = 1.0,
            opacity = 1.0,
            color = "black",
            weight = 1.0,
          )
      }
      
  })
  
  # Observe for state input in Raster Map

  observe({
     area_month <- input$state_month
     area_fcast <- input$state_fcast
     
     # Change the text in Time Series
     output$mt_stateName <- renderUI({
       h4(id = "mt_stateName", paste("Time Series of", ifelse(area_month != "All", area_month, "All India")))
     })
     
     output$fc_stateName <- renderUI({
       h4(id = "fc_stateName", paste("Time Series of", ifelse(area_fcast != "All", area_fcast, "All India")))
     })
     
     ###### MAP
     # Monthly Map
     if (area_month != "All") {
           area_pol_month <- subset(india_states, india_states$name == area_month)
           area_ext_month <- area_pol_month %>% extent()
  
           leafletProxy("map_monthly", data = area_pol_month) %>%
               removeShape("selectedState") %>%
               flyTo(
                   lng = mean(c(area_ext_month[1], area_ext_month[2])),
                   lat = mean(c(area_ext_month[3], area_ext_month[4])),
                   zoom = 6
               ) %>%
               addPolygons(
                   layerId = "selectedState",
                   color = "red",
                   opacity = 1.0,
                   fillOpacity = 0.05
               )

      } else {
        leafletProxy("map_monthly") %>%
          removeShape("selectedState") %>%
          flyTo(lng = 77, lat = 22, zoom = 5)
      }
     
     # Forecast Map
     if (area_fcast != "All") {
       area_pol_fcast <- subset(india_states, india_states$name == area_fcast)
       
       area_ext_fcast <- area_pol_fcast %>% extent()
       
       leafletProxy("map_forecast", data = area_pol_fcast) %>%
         removeShape("selectedState") %>%
         flyTo(
           lng = mean(c(area_ext_fcast[1], area_ext_fcast[2])),
           lat = mean(c(area_ext_fcast[3], area_ext_fcast[4])),
           zoom = 6
         ) %>%
         addPolygons(
           layerId = "selectedState",
           color = "red",
           opacity = 1.0,
           fillOpacity = 0.05
         )
       
     } else {
       leafletProxy("map_forecast") %>%
         removeShape("selectedState") %>%
         flyTo(lng = 77, lat = 22, zoom = 5)
     }
     
     ###### TIME - SERIES
     
     output$monthlyTS <- renderPlotly({
       TS_Table <- get_timeSeries("2015-01-01", "2020-12-01", area_month)
       
       mTS <- TS_Table %>% 
         pivot_longer(cols = -date, names_to = "pol", values_to = "value") %>% 
         mutate(pol = fct_relevel(pol, c("pm25", "pm10", "co", "no2", "so2", "o3"))) %>% 
         ggplot() +
         geom_line(aes(x = date, y = value, color = pol), show.legend = FALSE) +
         facet_wrap(
           pol ~ .,
           ncol = 1,
           scales = "free_y",
           labeller = labeller(
             pol = c(
               "pm25" = "PM 2.5",
               "pm10" = "PM 10",
               "co" = "Carbon Monoxide (CO)",
               "no2" = "Nitrogen Dioxide (NO2)",
               "so2" = "Sulphur Dioxide (SO2)",
               "o3" = "Ozone"
             ))
         ) + labs(x = "Date", y = "") + theme(legend.position = "none")
       
       ggplotly(mTS)
     })
     
     # Forecast TS
     
     output$forecastTS <- renderPlotly({
       TS_Table <- fc_timeSeries(area_fcast)
       
       mTS <- TS_Table %>% 
         pivot_longer(cols = -date, names_to = "pol", values_to = "value") %>% 
         mutate(pol = fct_relevel(pol, c("pm25", "pm10", "co", "no2", "so2", "o3"))) %>% 
         ggplot() +
         geom_line(aes(x = date, y = value, color = pol), show.legend = FALSE) +
         facet_wrap(
           pol ~ .,
           ncol = 1,
           scales = "free_y",
           labeller = labeller(
             pol = c(
               "pm25" = "PM 2.5",
               "pm10" = "PM 10",
               "co" = "Carbon Monoxide (CO)",
               "no2" = "Nitrogen Dioxide (NO2)",
               "so2" = "Sulphur Dioxide (SO2)",
               "o3" = "Ozone"
             ))
         ) + labs(x = "Date", y = "") + theme(legend.position = "none")
       
       ggplotly(mTS)
     })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
