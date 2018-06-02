library(leaflet)
library(shiny)
library(dplyr)

## good provider maps:  Hydda, carodb.darkmatter, Stamen.Terrain, Stamen.TopOSMFeatures, Wikimedia

yelp_data <- read.csv("~/madison_eats/yelp_df.csv",stringsAsFactors = F)
categ <- gsub("|",",",yelp_data$Categories,fixed = T)
categ_list <- strsplit(categ," , ")
all_categories <- unique(unlist(categ_list))
names(categ_list) <- yelp_data$Name

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 10, width = 300,
                
                  sliderInput("rating","Rating >= ",min = 0, max = 5, step = 0.5, value = 3),
                sliderInput("n_reviews","Minimum Reviews", min =0, max = 200, step = 10, value = 10),
                selectizeInput("category","Filter by Categories",choices = all_categories,multiple = T),
                style = "opacity: 0.9; z-index: 1000;"
                
                

                )
 

)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  

  
  
  filter_rating <- reactive(input$rating)
  filter_review <- reactive(input$n_reviews)
  filter_categ <- reactive(input$category)
  
  output$mymap <- renderLeaflet({


    if (length(filter_categ()) != 0){
    matrix_categ_in_rest <- sapply(filter_categ(), function(x) grepl(x,categ_list,fixed=T))
    cat_in_rest <- apply(matrix_categ_in_rest,1,function(x) any(x))
    new_data <- yelp_data[cat_in_rest,]
    
    new_data <- new_data %>% filter(Rating >= filter_rating(), 
                                     Review_Count >= filter_review())
    }else{
      
      new_data <- yelp_data %>% filter(Rating >= filter_rating(), 
                                       Review_Count >= filter_review())
    }
    

    
    
    leaflet(data = new_data) %>% setView(lng = -89.426010,lat = 43.071695,zoom = 12) %>% 
      addProviderTiles(providers$Wikimedia) %>% 
      addMarkers(~Longitude, ~ Latitude, popup = ~Name)
  })

  
}


shinyApp(ui, server)


