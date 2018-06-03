

yelp_data <- tryCatch(read.csv("~/madison_eats/yelp_df.csv",stringsAsFactors = F),error = function(e) read.csv("/srv/shiny-server/madison_eats/yelp_df.csv"))
categ <- gsub("|",",",yelp_data$Categories,fixed = T)
categ_list <- strsplit(categ," , ")
all_categories <- unique(unlist(categ_list))
names(categ_list) <- yelp_data$Name
