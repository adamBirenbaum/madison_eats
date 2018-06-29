
library(rvest)
library(dplyr)
library(httr)
library(htmlwidgets)


if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_app <- "/srv/shiny-server/madison_eats/"
}else{
  path_to_app <- "~/api_/srv/shiny-server/madison_eats/"
}



table_url <- "https://en.wikipedia.org/wiki/User:Michael_J/County_table"

aa <- table[[1]]
aa$`Population(2010)`<- as.numeric(gsub(",","",aa$`Population(2010)`))

table <- table_url %>% 
  html() %>% 
  html_nodes(xpath = '/html/body/div[3]/div[3]/div[4]/div/table' ) %>% 
  html_table()

table_df <- table[[1]]
table_df <- table_df[,c(2,4,11,13,14)]
names(table_df) <- c("State","County","Total_Area","Latitude","Longitude")
table_df$Latitude <- gsub("°","",table_df$Latitude)
table_df$Latitude <- gsub("+","",table_df$Latitude,fixed = T)
table_df$Latitude <- as.numeric(table_df$Latitude)

table_df$Longitude <- gsub("°","",table_df$Longitude)
table_df$Longitude <- gsub("+","",table_df$Longitude,fixed = T)
table_df$Longitude <- gsub("–","-",table_df$Longitude,fixed = T)
table_df$Longitude <- as.numeric(table_df$Longitude)


table_df$Total_Area <- as.numeric(gsub(",","",table_df$Total_Area,fixed = T))





#yelp_api_key <- read.table("~/api_key/yelp_api_key.txt",stringsAsFactors = F)[1,1]


yelp_api_key <- readRDS("~/madison_eats/yelp_api_key.RDS")


api_header <- paste0("Bearer ",yelp_api_key)


business_search_by_coord <- function(lat,lon,rad){
  api_header <- paste0("Bearer ",yelp_api_key)
  GET(url = "https://api.yelp.com/v3/businesses/search",
      config = add_headers(Authorization= api_header),
      query=list(latitude=lat,longitude = lon,
                 radius = rad,limit = 50L,term = "restaurants"))
  
}


# 
# left <- -89.841121
# right <- -89.011833
# top <- 43.291498
# bottom <- 42.844576
# 
# width <- seq(from = left, to = right, length.out = 60)
# height <- seq(from = top, to = bottom, length.out = 60)
# all_coords <- expand.grid(width,height)
# names(all_coords) <- c("longitude","latitude")

# length_of_lon <- 81156.40750960271 / 1000  #km / degree
# length_of_lat <- 111098.35781049416 / 1000

table_df$Radius <- sqrt(table_df$Total_Area) / 2 *1.1# km
table_df <- table_df %>% mutate(length_of_Lon = 111.32*cos(Latitude*pi/180),length_of_Lat = 111.098)
table_df <- table_df %>% mutate(left = Longitude - Radius/length_of_Lon, right = Longitude + Radius/length_of_Lon,
                    bottom = Latitude - Radius/length_of_Lat, top = Latitude + Radius/length_of_Lat)





draw_circle_in_center <- function(left, right, top, bottom){
  print(paste0("Recursion number:  ",parts))
  parts <<- parts + 1
  
  absolute_left <- left
  absolute_right <- right
  absolute_top <- top
  absolute_bottom <- bottom
  
  for (i in 1:4){
    print(paste0("Iteration: ",i))
      if (i == 1){
        bottom <- mean(c(top, bottom))
        right <-  mean(c(left, right))
      }else if (i == 2){
        left <- right
        right <- absolute_right
        
        
        #bottom <- mean(c(top, bottom))
        #left <- mean(c(left, right))
      }else if (i == 3){
        right <- left
        left <- absolute_left
        top <- bottom
        bottom <- absolute_bottom
        
        #top <- mean(c(top, bottom))
        #right <- mean(c(left, right))
      }else if (i == 4){
        left <- right
        right <- absolute_right
        #top <- mean(c(top, bottom))
        #left <- mean(c(left, right))
        
      }
    
 

    center_lat <- mean(c(top,bottom))
    rad <- round(((top - center_lat)*110)*1000)
    df <- business_search_by_coord(center_lat,mean(c(left,right)),rad)
    Sys.sleep(1.5)
    df <-content(df, "parsed")
    num_business <- length(df$business)
    print(paste0("Number of Businesses:  ",num_business))
    

    leaf <<- leaf %>%  addRectangles(
      lng1=left, lat1=top,
      lng2=right, lat2=bottom,
      fillColor = "transparent"
    )# %>% addMarkers(lng = mean(c(left,right)),lat = center_lat,label = as.character(num_business))
    leaf

    if (num_business!=0){
      for (j in 1:num_business){
        row_j <<- row_j + 1
        
        aa <-  unlist(df$businesses[[j]])
        rest <- unlist(df$businesses[j])
        yelp_df[row_j,] <<- unname(unlist(sapply(yelp_data_categories,function(x) paste0(aa[names(aa) == x],collapse = " | "))))
        
        
      }
      if (num_business == 50){
       draw_circle_in_center(left, right, top , bottom)
      }
        

      
    }
      

  }
  
  
  
}


county_table <- table_df[3053,]
county_table <- table_df[1341,]

vec <- c(222)
for (i in 1:length(vec)){
  county_table <- table_df[vec[i],]
  

absolute_left <- county_table$left
absolute_right <- county_table$right
absolute_top <- county_table$top
absolute_bottom <- county_table$bottom


  leaf <<-
  leaflet() %>% 
    addTiles() %>% 
  addRectangles(
    lng1=absolute_left, lat1=absolute_top,
    lng2=absolute_right, lat2=absolute_bottom,
    fillColor = "transparent"
  ) 




yelp_data_categories <- c("id", "name", "is_closed", "url", "review_count", 
                          "categories.title",  "rating", "coordinates.latitude", 
                          "coordinates.longitude", "price", "location.display_address1")

yelp_df <- data.frame(ID = character(0), Name = character(0),Is_Closed = logical(0),URL = character(0),Review_Count = character(0),Categories = character(0), Rating = character(0),
                      Latitude = character(0), Longitude = character(0), Price = character(0), Address = character(0),stringsAsFactors = F)


row_j <- 0
parts <- 0
draw_circle_in_center(absolute_left,absolute_right,absolute_top,absolute_bottom)

yelp_df <- distinct(yelp_df,ID,.keep_all = T)
write.csv(yelp_df,paste0("~/madison_eats/data_folder/",county_table$County,"-",county_table$State,"_yelp_df.csv"),row.names = F)

saveWidget(leaf, file=paste0("~/madison_eats/recursive_maps/",county_table$County,"-",county_table$State,"_map.html"))

}

get_coords <- function(left,right,top,bottom){
  
  
}







row_i <- 0
big_zip <- c()
for (z in zips){
  
  d <- business_search_by_zip(z)
  Sys.sleep(2)
  d <- content(d, "parsed")
  num_business <- length(d$business)
  if (num_business == 50){
    big_zip <- c(big_zip,z)
  }
  for (i in 1:num_business){
    row_i <- row_i + 1
    print(row_i)
    aa <-  unlist(d$businesses[[i]])
    rest <- unlist(d$businesses[i])
    yelp_df[row_i,] <- unname(unlist(sapply(yelp_data_categories,function(x) paste0(aa[names(aa) == x],collapse = " | "))))
    
    
  }
  
}


yelp_df <- distinct(yelp_data,ID,.keep_all = T)
write.csv(yelp_df,"~/madison_eats/yelp_df.csv",row.names = F)

