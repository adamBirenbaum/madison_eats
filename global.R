yelp_data <- read.csv("~/madison_eats/yelp_df.csv",stringsAsFactors = F)
categ <- gsub("|",",",yelp_data$Categories,fixed = T)
categ_list <- strsplit(categ," , ")
all_categories <- unique(unlist(categ_list))
names(categ_list) <- yelp_data$Name
