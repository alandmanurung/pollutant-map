#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            setView(lng = -93.85, lat = 37.45, zoom = 4)
    })

})
