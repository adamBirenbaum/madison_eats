
yelp_data <- read.csv("~/madison_eats/yelp_df.csv",stringsAsFactors = F)
categ <- gsub("|",",",yelp_data$Categories,fixed = T)
categ_list <- strsplit(categ," , ")


# want the most popular tags towards the front... but not fast food
sorted_categories <- names(sort(table(unlist(categ_list)),decreasing = T))
sorted_categories <- sorted_categories[sorted_categories != "Fast Food"]
sorted_categories <- c(sorted_categories, "Fast Food")


names(categ_list) <- yelp_data$Name
