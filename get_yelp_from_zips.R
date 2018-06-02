
library(rvest)
library(dplyr)
library(httr)

zip <- html("https://www.zip-codes.com/county/wi-dane.asp#zipcodes")

zips <- zip %>% 
  html_nodes(".label") %>% 
  html_text()


zips <- zips[2:56]
zips <- gsub(".*([0-9]{5})","\\1",zips)



yelp_api_key <- read.table("~/api_key/yelp_api_key.txt",stringsAsFactors = F)[1,1]


api_header <- paste0("Bearer ",yelp_api_key)

business_search_by_zip <- function(zip){
  api_header <- paste0("Bearer ",yelp_api_key)
  GET(url = "https://api.yelp.com/v3/businesses/",
      config = add_headers(Authorization= api_header),
      query=list(location=paste0("zip ",zip),limit = 50L,term = "restaurants"))
  
}



yelp_data_categories <- c("id", "name", "is_closed", "url", "review_count", 
                          "categories.title",  "rating", "coordinates.latitude", 
                          "coordinates.longitude", "price", "location.display_address1")

yelp_df <- data.frame(ID = character(0), Name = character(0),Is_Closed = logical(0),URL = character(0),Review_Count = character(0),Categories = character(0), Rating = character(0),
                    Latitude = character(0), Longitude = character(0), Price = character(0), Address = character(0),stringsAsFactors = F)


row_i <- 0
for (z in zips){
  
  d <- business_search_by_zip(z)
  Sys.sleep(2)
  d <- content(d, "parsed")
  num_business <- length(d$business)
  
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

