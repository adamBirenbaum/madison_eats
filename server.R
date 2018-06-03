
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
      addMarkers(~Longitude, ~ Latitude, popup = paste0(new_data$Name,":  <br> ",new_data$Rating," &#9733 <br> ",new_data$Review_Count," reviews"))
  })
  
  
}