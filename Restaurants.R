##----------------------------------------------------------------------------##
## Required dependencies.
##----------------------------------------------------------------------------##

# Install required packages
install.packages("mongolite")
install.packages("tidyverse")
install.packages("sf")
install.packages("mapview")

# Load required library
library(mongolite)
library(tidyverse)
library(sf)
library(mapview)

##----------------------------------------------------------------------------##
## Data Wrangling: Loading and tidying the dataset to ensure it is in a clean and usable format.
##----------------------------------------------------------------------------##

# Load data from mongodb
restaurants_collection = mongo(collection="restaurants", db="sample_restaurants", url=Sys.getenv("connection_string"))
neighborhoods_collection = mongo(collection="neighborhoods", db="sample_restaurants", url=Sys.getenv("connection_string"))

##----------------------------------------------------------------------------##
## Top 15 Most Popular Cuisine.
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
    labs(title = "Top 15 Most Popular Cuisine",
         x = "Cuisine Type",
         y = "Number of Restaurants") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

# Draw pie chart
pie(cuisine_counts$count, labels = cuisine_counts$cuisine, main = "Top 15 Most Popular Cuisine")

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

##----------------------------------------------------------------------------##
## Rating Distributions.
##----------------------------------------------------------------------------##

# Fetch the restaurant grades from MongoDB
restaurant_scores <- as.data.frame(restaurants_collection$find(fields = '{"restaurant_id": 1, "grades": 1, "_id": 0}')) %>%
    unnest(grades) # Unnest the grades array so that each row represents a single grade

# Convert date to year only
restaurant_scores$year <- year(restaurant_scores$date)

# Show score distribution
ggplot(restaurant_scores) +
    geom_histogram(mapping = aes(x = score), binwidth = 0.5) +
    coord_cartesian(xlim = c(0, 50))

# Show grade distribution
ggplot(restaurant_scores) +
    geom_bar(mapping = aes(x = grade))

# Calculate the number of restaurants for each cuisine
restaurant_scores <- restaurant_scores %>%
    group_by(year) %>%
    summarise(avg_score = mean(score))

# Plot the line chart
ggplot(restaurant_scores, aes(x = year, y = avg_score)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Average Score Over Time",
         x = "Year",
         y = "Average Score")

##----------------------------------------------------------------------------##
## Restaurant Density on Map.
##----------------------------------------------------------------------------##

# Load the restaurant location data
restaurant_locations <- restaurants_collection$find(fields = '{"name": 1, "address.coord": 1, "_id": 0}') %>%
    unnest(address) %>% # Ugly hacks
    unnest_wider(coord, names_sep = "") # split coordinates into latitude and longitude

# Filter coordinates within USA
restaurant_locations_usa <- filter(restaurant_locations, coord1 >= -125, coord1 <= -67, coord2 >= 24, coord2 <= 50) 

# Draw dot in USA
ggplot() + 
    geom_polygon(data = map_data("state"), mapping = aes(long, lat, group = group), fill = "white", colour = "grey50") + 
    coord_quickmap() + 
    geom_point(restaurant_locations_usa, mapping = aes(coord1, coord2), colour = "red")

#Draw over the world
ggplot() + 
    geom_polygon(data = map_data("world"), mapping = aes(long, lat, group = group), fill = "white", colour = "grey50") + 
    coord_quickmap() +
    geom_point(restaurant_locations, mapping = aes(coord1, coord2), colour = "red")
