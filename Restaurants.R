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

##----------------------------------------------------------------------------##
## Top 15 Most Popular Cuisine in NYC.
##----------------------------------------------------------------------------##

# Using mongodb aggregate function to fetch a list of cuisine and their counts
cuisine_counts <- as.data.frame(restaurants_collection$aggregate('[{"$group": {"_id": "$cuisine", "count": {"$sum": 1}}}]'))
cuisine_counts <- cuisine_counts %>%
    rename(cuisine = "_id") %>%
    arrange(desc(count)) # Sort cuisine list from high count to low

# Only keep top 15 most popular cuisine
other_count <- sum(cuisine_counts$count[16:nrow(cuisine_counts)])
other_index <- which(cuisine_counts$cuisine == "Other")
cuisine_counts$count[other_index] <- cuisine_counts$count[other_index] + other_count
cuisine_counts <- cuisine_counts %>%
    head(15) %>%
    arrange(desc(cuisine == "Other"))

# Draw bar chart
ggplot(cuisine_counts, aes(x = cuisine, y = count, fill = cuisine)) + 
    geom_bar(stat = "identity") + 
    theme_minimal() +
    labs(title = "Top 15 Most Popular Cuisine in NYC",
         x = "Cuisine Type",
         y = "Number of Restaurants") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

# Draw pie chart
pie(cuisine_counts$count, labels = cuisine_counts$cuisine, main = "Top 15 Most Popular Cuisine in NYC")

##----------------------------------------------------------------------------##
## Cuisine Scores Over Time.
##----------------------------------------------------------------------------##

# Fetch the restaurant grades and cuisine from MongoDB
restaurant_scores <- as.data.frame(restaurants_collection$find(fields = '{"restaurant_id": 1, "cuisine": 1, "grades": 1, "_id": 0}')) %>%
    unnest(grades) # Unnest the grades array so that each row represents a single grade

# Convert date to year only
restaurant_scores$year <- year(restaurant_scores$date)

# Calculate the number of restaurants for each cuisine
top_cuisines <- restaurant_scores %>%
    group_by(cuisine) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    top_n(10)

# Filter the data for the top 10 cuisines and calculate average score in each year
restaurant_scores <- restaurant_scores %>%
    filter(cuisine %in% top_cuisines$cuisine) %>%
    group_by(cuisine, year) %>%
    summarise(avg_score = mean(score))

# Plot the line chart
ggplot(restaurant_scores, aes(x = year, y = avg_score, color = cuisine)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Average Score for Top 10 Cuisines Over Time",
         x = "Year",
         y = "Average Score")
