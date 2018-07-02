
library(rvest)
library(dplyr)
library(httr)
library(htmlwidgets)


if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_app <- "/srv/shiny-server/madison_eats/"
}else{
  path_to_app <- "~/api_/srv/shiny-server/madison_eats/"
}

# library(sf)
# shape <- read_sf(dsn = "~/UScounties",layer  = "UScounties")
# county_df <- data.frame(County = shape$NAME, State = shape$STATE_NAME,left = 0, right = 0 , top = 0, bottom = 0,stringsAsFactors = F)
# 
# for (i in 1:3141){
#   county_df$left[i] <- min(shape$geometry[[i]][[1]][[1]][,1])
#   county_df$right[i] <- max(shape$geometry[[i]][[1]][[1]][,1])
#   county_df$top[i] <- max(shape$geometry[[i]][[1]][[1]][,2])
#   county_df$bottom[i] <- min(shape$geometry[[i]][[1]][[1]][,2])
# }
# states <- read.csv("~/states_abbrev.csv",stringsAsFactors = F)
# county_df$State <- sapply(county_df$State,function(x) states$Abbreviation[states$State == x],USE.NAMES = F)
# 
# 
# county_df <- county_df %>% arrange(State) %>% 
#   group_by(County,State) %>% mutate(length_of_Lon = 111.32*cos((mean(c(top,bottom)))*pi/180),length_of_Lat = 111.098,
#   width =abs(left - right)*length_of_Lon, height = abs(top - bottom)*length_of_Lat)
# write.csv(county_df,"~/county_df.csv",row.names = F)

# 
# 
# shape <- st_read(dsn = "~/UScounties",layer  = "UScounties")
# shape_df <- st_centroid(shape)
# shape_df$Latitude <- 0
# shape_df$Longitude <- 0
# 
# 
# for (i in 1:3141){
#   shape_df$Longitude[i] <- shape_df$geometry[[i]][1]
# shape_df$Latitude[i] <- shape_df$geometry[[i]][2]
# 
# }
# 
# shape_df$geometry = NULL
# shape_df <- shape_df[,c(1,2,6,7)]
# names(shape_df)[1:2] <- c("County","State")
# shape_df$County <- as.character(shape_df$County)
# shape_df$State <- as.character(shape_df$State)
# 
# 
# county_centroid_df <- shape_df
# write.csv(county_centroid_df,"~/county_centroid_df.csv",row.names = F)
# 
# county_centroid_df <- read.csv("~/county_centroid_df.csv",stringsAsFactors = F)
# 
# 
# 
# table_url <- "https://en.wikipedia.org/wiki/User:Michael_J/County_table"
# 
# 
# table <- table_url %>%
#   html() %>%
#   html_nodes(xpath = '/html/body/div[3]/div[3]/div[4]/div/table' ) %>%
#   html_table()
# 
# table_df <- table[[1]]
# table_df <- table_df[,c(2,4,11,13,14)]
# names(table_df) <- c("State","County","Total_Area","Latitude","Longitude")
# 
# table_df <- table_df[,c(1,2,3)]
# 
# 
# table_df$Latitude <- mapply(function(st,ct){
#   county_centroid_df$Latitude[county_centroid_df$State == st & county_centroid_df$County == ct][1]
#   
# }, st = table_df$State,table_df$County) %>% unlist() %>% unname()
# 
# table_df$Longitude <- mapply(function(st,ct){
#   county_centroid_df$Longitude[county_centroid_df$State == st & county_centroid_df$County == ct][1]
#   
# }, st = table_df$State,table_df$County) %>% unlist() %>% unname()

# 
# table_df$Latitude <- gsub("°","",table_df$Latitude)
# table_df$Latitude <- gsub("+","",table_df$Latitude,fixed = T)
# table_df$Latitude <- as.numeric(table_df$Latitude)
# 
# table_df$Longitude <- gsub("°","",table_df$Longitude)
# table_df$Longitude <- gsub("+","",table_df$Longitude,fixed = T)
# table_df$Longitude <- gsub("–","-",table_df$Longitude,fixed = T)
# table_df$Longitude <- as.numeric(table_df$Longitude)


#table_df$Total_Area <- as.numeric(gsub(",","",table_df$Total_Area,fixed = T))




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

#table_df$Radius <- sqrt(table_df$Total_Area) / 2 # km
# table_df <- table_df %>% mutate(length_of_Lon = 111.32*cos(Latitude*pi/180),length_of_Lat = 111.098)
# table_df <- table_df %>% mutate(left = Longitude - Radius/length_of_Lon, right = Longitude + Radius/length_of_Lon,
#                     bottom = Latitude - Radius/length_of_Lat, top = Latitude + Radius/length_of_Lat)
# 




draw_circle_in_center <- function(left, right, top, bottom,length_of_lon){
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
    
 
    width <- abs(left - right)*length_of_lon
    height <- abs(top - bottom)*111.098

    center_lat <- mean(c(top,bottom))
    center_lon <- mean(c(left,right))
    scale_constant <- sqrt((height / 2)^2 + (width / 2)^2) / (max(c(height,width)) / 2)
  
    rad <- ceiling((((max(c((top - center_lat)*111.1,(abs(right - center_lon)*length_of_lon))))))*1000 * scale_constant)
    if(rad > 15000){
      draw_circle_in_center(left, right, top , bottom,length_of_lon)
    }
    df <- try(business_search_by_coord(center_lat,center_lon,rad))
    if (inherits(df,"try-error")){
      df <- try(business_search_by_coord(center_lat,center_lon,rad))
    }
    num_calls <<- num_calls + 1
    Sys.sleep(1.5)
    df <-content(df, "parsed")
    num_business <- length(df$business)
    num_business_string <- ifelse(num_business == 50,"50+",as.character(num_business))
    print(paste0("Number of Businesses:  ",num_business))
    

    leaf <<- leaf%>% #addMarkers(data = yelp_df,lat = ~as.numeric(Latitude), lng = ~as.numeric(Longitude)) %>% 
      
      addRectangles(
        lng1=left, lat1=top,
        lng2=right, lat2=bottom,
        fillColor = "transparent",weight = 3,color = "black",opacity = 1
      ) %>% addCircles(lat = center_lat, lng = mean(c(left, right)),radius = rad,group = "Search Region", 
                       weight = 1,opacity = .05) %>% addLabelOnlyMarkers(
                         lat = center_lat, lng = center_lon,label = num_business_string,    labelOptions = leaflet::labelOptions(
                           noHide = TRUE,
                           textOnly = F,
                           opacity = 1
                         ),group = "Number of Restaurants") %>% 
      addLayersControl(overlayGroups = c("Search Region","Number of Restaurants"),options = layersControlOptions(collapsed = F))
    
    # %>% addMarkers(lng = mean(c(left,right)),lat = center_lat,label = as.character(num_business))
    
    leaf

    if (num_business!=0){
      for (j in 1:num_business){
        row_j <<- row_j + 1
        
        aa <-  unlist(df$businesses[[j]])
        rest <- unlist(df$businesses[j])
        yelp_df[row_j,] <<- unname(unlist(sapply(yelp_data_categories,function(x) paste0(aa[names(aa) == x],collapse = " | "))))
        
        
      }
      if (num_business == 50){
       draw_circle_in_center(left, right, top , bottom,length_of_lon)
      }
        

      
    }
      

  }
  
  
  
}

table_df <- read.csv("~/county_df.csv",stringsAsFactors = F)

county_table <- table_df[3053,]
county_table <- table_df[1341,]

vec <- 2992:3063
vec <-vec[vec != 3055]
num_calls_df <- data.frame(County = integer(0), State = integer(0), Num_Calls = integer(0),stringsAsFactors = F)
#num_calls_df <- read.csv("~/num_calls_df.csv",stringsAsFactors = F)
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
    fillColor = "transparent",weight = 3, color = "black",opacity = 1
  ) 




yelp_data_categories <- c("id", "name", "is_closed", "url", "review_count", 
                          "categories.title",  "rating", "coordinates.latitude", 
                          "coordinates.longitude", "price", "location.display_address1")

yelp_df <- data.frame(ID = character(0), Name = character(0),Is_Closed = logical(0),URL = character(0),Review_Count = character(0),Categories = character(0), Rating = character(0),
                      Latitude = character(0), Longitude = character(0), Price = character(0), Address = character(0),stringsAsFactors = F)


row_j <- 0
parts <- 0
num_calls <- 0
draw_circle_in_center(absolute_left,absolute_right,absolute_top,absolute_bottom,county_table$length_of_Lon)

yelp_df <- distinct(yelp_df,ID,.keep_all = T)
write.csv(yelp_df,paste0("~/madison_eats/data_folder/",county_table$County,"-",county_table$State,"_yelp_df.csv"),row.names = F)

saveWidget(leaf, file=paste0("~/madison_eats/recursive_maps/",county_table$County,"-",county_table$State,"_map.html"))

num_calls_df <- rbind(num_calls_df,data.frame(County = county_table$County,State = county_table$State, Num_Calls = num_calls, stringsAsFactors = F))
}


write.csv(num_calls_df, "~/num_calls_df.csv",row.names = F)




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

