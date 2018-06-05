library(leaflet)
library(shiny)
library(dplyr)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 100, left = 20, width = 300,
                
                sliderInput("rating","Minimum Rating",min = 0, max = 5, step = 0.5, value = 3),
                sliderInput("n_reviews","Minimum Reviews", min =0, max = 200, step = 10, value = 10),
                selectizeInput("category","Filter by Categories",choices = sorted_categories,multiple = T),
                style = "opacity: 0.9; z-index: 1000;"
                
                
                
  )
  
  
)