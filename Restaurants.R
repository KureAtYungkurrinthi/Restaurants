##----------------------------------------------------------------------------##
## Required dependencies.
##----------------------------------------------------------------------------##

# Install required packages
install.packages("mongolite")
install.packages("tidyverse")

# Load required library
library(mongolite)
library(tidyverse)

##----------------------------------------------------------------------------##
## Data Wrangling: Loading and tidying the dataset to ensure it is in a clean and usable format.
##----------------------------------------------------------------------------##

# Load data from mongodb
restaurants_collection = mongo(collection="restaurants", db="sample_restaurants", url=Sys.getenv("connection_string"))
neighborhoods_collection = mongo(collection="neighborhoods", db="sample_restaurants", url=Sys.getenv("connection_string"))

# Pull data into local data frame
restaurants = as.data.frame(restaurants_collection$find())
neighborhoods = as.data.frame(neighborhoods_collection$find())

##----------------------------------------------------------------------------##
## Top 10 cuisine in NYC
##----------------------------------------------------------------------------##

# Using mongodb aggregate function to fetch a list of cuisine and their counts
cuisine_counts <- as.data.frame(restaurants_collection$aggregate('[{"$group": {"_id": "$cuisine", "count": {"$sum": 1}}}]'))
cuisine_counts <- cuisine_counts %>%
    rename(cuisine = "_id") %>%
    arrange(desc(count)) # Sort cuisine list from high count to low

# Only keep top 10 most popular cuisine
other_count <- sum(cuisine_counts$count[16:nrow(cuisine_counts)])
other_index <- which(cuisine_counts$cuisine == "Other")
cuisine_counts$count[other_index] <- cuisine_counts$count[other_index] + other_count
cuisine_counts <- cuisine_counts %>%
    head(15) %>%
    arrange(desc(cuisine == "Other"))

# Draw pie chart
pie(cuisine_counts$count, labels = cuisine_counts$cuisine, main = "Top 10 cuisine in NYC")
