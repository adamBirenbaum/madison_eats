
library(rvest)
library(dplyr)
library(httr)

if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_app <- "/srv/shiny-server/madison_eats/"
}else{
  path_to_app <- "~/api_/srv/shiny-server/madison_eats/"
}



zip <- read_html("https://www.zip-codes.com/county/wi-dane.asp#zipcodes")

zips <- zip %>% 
  html_nodes(".label") %>% 
  html_text()


zips <- zips[2:56]
zips <- gsub(".*([0-9]{5})","\\1",zips)



#yelp_api_key <- read.table("~/api_key/yelp_api_key.txt",stringsAsFactors = F)[1,1]

yelp_api_key <- "mCjVhhKVTYYzz-zCblJO0QpbIkBMgi27VcL71CccAXnDrCjAzgGLCpa19AWy9VUk88bfgtIF60LvrWckWaQVGKIYeJcPan-hj4d50NixmOQaMZyk_P3k-TPdgZMgW3Yx"

api_header <- paste0("Bearer ",yelp_api_key)

business_search_by_zip <- function(zip){
  api_header <- paste0("Bearer ",yelp_api_key)
  GET(url = "https://api.yelp.com/v3/businesses/search",
      config = add_headers(Authorization= api_header),
      query=list(location=paste0("zip ",zip),limit = 50L,term = "restaurants"))
  
}

business_search_by_coord <- function(lat,lon,rad){
  api_header <- paste0("Bearer ",yelp_api_key)
  GET(url = "https://api.yelp.com/v3/businesses/search",
           config = add_headers(Authorization= api_header),
           query=list(latitude=lat,longitude = lon,
                      radius = rad,limit = 50L,term = "restaurants"))
  
}



left <- -89.841121
right <- -89.011833
top <- 43.291498
bottom <- 42.844576

width <- seq(from = left, to = right, length.out = 60)
height <- seq(from = top, to = bottom, length.out = 60)
all_coords <- expand.grid(width,height)
names(all_coords) <- c("longitude","latitude")

length_of_lon <- 81156.40750960271
length_of_lat <- 111098.35781049416
lon_spacing <- diff(width)[1] * length_of_lon
lat_spacing <- abs(diff(height)[1]) * length_of_lat
radius_spacing <- round(sqrt((lon_spacing/2)^2 + (lat_spacing/2)^2))
all_coords$radius <- radius_spacing



yelp_data_categories <- c("id", "name", "is_closed", "url", "review_count", 
                          "categories.title",  "rating", "coordinates.latitude", 
                          "coordinates.longitude", "price", "location.display_address1")

yelp_df <- data.frame(ID = character(0), Name = character(0),Is_Closed = logical(0),URL = character(0),Review_Count = character(0),Categories = character(0), Rating = character(0),
                    Latitude = character(0), Longitude = character(0), Price = character(0), Address = character(0),stringsAsFactors = F)





row_i <- 0
big_lat <- c()
big_lon <- c()
k <- 1
for (j in 1:nrow(all_coords)){
  print(k)
  k <- k + 1
  d <- business_search_by_coord(all_coords$latitude[j],all_coords$longitude[j],all_coords$radius[j])
  Sys.sleep(2)
  d <- content(d, "parsed")
  num_business <- length(d$business)
  if (num_business == 50){
    big_lat <- c(big_lat,all_coords$latitude[j])
    big_lon <- c(big_lon, all_coords$longitude[j])
  }
  if (num_business!=0){
    for (i in 1:num_business){
      row_i <- row_i + 1
      print(row_i)
      aa <-  unlist(d$businesses[[i]])
      rest <- unlist(d$businesses[i])
      yelp_df[row_i,] <- unname(unlist(sapply(yelp_data_categories,function(x) paste0(aa[names(aa) == x],collapse = " | "))))
      
      
    }
    
  }else{
    print("zero")
  }

  
}




yelp_df <- distinct(yelp_df,ID,.keep_all = T)





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

