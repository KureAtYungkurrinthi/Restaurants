## Copyright (C) 2023 Sheng-fan Wang @ Flinders University
## All Rights Reserved

##----------------------------------------------------------------------------##
## Required dependencies.
##----------------------------------------------------------------------------##

# Install required packages
install.packages("mongolite")
install.packages("tidyverse")
install.packages("sf")
install.packages("maps")
install.packages("mapview")
install.packages("modelr")
install.packages("Metrics")

# Load required library
library(mongolite)
library(tidyverse)
library(sf)
library(maps)
library(mapview)
library(modelr)
library(splines)
library(Metrics)

##----------------------------------------------------------------------------##
## Data Wrangling: Loading and tidying the dataset to ensure it is in a clean and usable format.
##----------------------------------------------------------------------------##

# Load data from mongodb
restaurants_collection = mongo(collection="restaurants", db="sample_restaurants", url=Sys.getenv("connection_string"))
#neighborhoods_collection = mongo(collection="neighborhoods", db="sample_restaurants", url=Sys.getenv("connection_string"))

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

# Shorter name for Latin cuisine
latin_index <- which(grepl("Latin", cuisine_counts$cuisine))
cuisine_counts$cuisine[latin_index] <- "Latin"

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

# Shorter name for Latin cuisine
latin_index <- which(grepl("Latin", restaurant_scores$cuisine))
restaurant_scores$cuisine[latin_index] <- "Latin"

# Filter out data for the first and last year
restaurant_scores <- filter(restaurant_scores, year > min(restaurant_scores$year), year < max(restaurant_scores$year))

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

# Get average score per year
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

##----------------------------------------------------------------------------##
## Restaurant Map NYC.
##----------------------------------------------------------------------------##

# Load the restaurant location data
restaurant_locations <- restaurants_collection$find(fields = '{"name": 1, "address.coord": 1, "cuisine": 1, "_id": 0}') %>%
    unnest(address) %>% # Ugly hacks
    unnest_wider(coord, names_sep = "") %>% # split coordinates into latitude and longitude
    filter(coord1 >= -74.2591, coord1 <= -73.7004, coord2 >= 40.4774, coord2 <= 40.9176) # Filter coordinates within NYC

# transform the data frame into an sf object
restaurant_nyc <- st_as_sf(x = na.omit(restaurant_locations), 
                           coords = c("coord1","coord2"),
                           crs = 4326) %>% 
    st_transform(crs = 2263)

# create plot
mapview(
    restaurant_nyc,
    zcol = "cuisine",
    cex = 2,
    legend = FALSE,
    layer.name = "Restaurant Density in NYC")

################################################################################

# Load the restaurant location data
restaurant_locations <- restaurants_collection$find(fields = '{"address.coord": 1, "borough": 1, "cuisine": 1, "_id": 0}') %>%
    unnest(address) %>% # Ugly hacks
    unnest_wider(coord, names_sep = "") %>% # split coordinates into latitude and longitude
    filter(coord1 >= -74.2591, coord1 <= -73.7004, coord2 >= 40.4774, coord2 <= 40.9176) # Filter coordinates within NYC

# Grouping by borough and calculating the most popular cuisine
popular_cuisine <- restaurant_locations %>%
    group_by(borough) %>%
    count(cuisine, sort = TRUE) %>%
    slice(1) %>%
    select(borough, cuisine)

View(popular_cuisine)

##----------------------------------------------------------------------------##
## Data Modelling - supervised machine learning models
##----------------------------------------------------------------------------##
options(na.action = na.warn)

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
restaurant_scores <- na.omit(restaurant_scores) %>%
    filter(cuisine %in% top_cuisines$cuisine) %>%
    filter(score < 100) %>%
    mutate(cuisine = str_replace(cuisine, "Latin \\(Cuban, Dominican, Puerto Rican, South & Central American\\)", "Latin"))

# Predicting score based on cuisine
mod_scores <- lm(score ~ cuisine, data = restaurant_scores)
grid <- restaurant_scores %>% 
    data_grid(cuisine) %>% 
    add_predictions(mod_scores)

ggplot(restaurant_scores, aes(cuisine)) + 
    geom_point(aes(y = score, colour = year)) +
    geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

# Predicting score based on date (highlighting cuisine)
mod1 <- lm(score ~ date + cuisine, data = restaurant_scores)
mod2 <- lm(score ~ date * cuisine, data = restaurant_scores)
grid <- restaurant_scores %>% 
    data_grid(date, cuisine) %>% 
    gather_predictions(mod1, mod2)

ggplot(restaurant_scores, aes(date)) + 
    geom_point(aes(y = score, colour = cuisine)) + 
    geom_line(data = grid, aes(y = pred)) + 
    facet_wrap(~ model)

# Attempt to predict cuisines score based on date
mod3 <- lm(score ~ ns(date, 3) + cuisine, data = restaurant_scores)
grid <- restaurant_scores %>% 
    data_grid(date, cuisine) %>% 
    gather_predictions(mod3)

ggplot(restaurant_scores, aes(date)) +
    geom_point(aes(y = score, colour = cuisine)) +
    geom_line(data = grid, aes(y = pred)) + 
    facet_grid(model ~ cuisine)

# Calculate the RMSE for each model
mod_rmse <- add_predictions(restaurant_scores, mod_scores)
rmse1 <- add_predictions(restaurant_scores, mod1)
rmse2 <- add_predictions(restaurant_scores, mod2)
rmse3 <- add_predictions(restaurant_scores, mod3)

rmse(mod_rmse$score, mod_rmse$pred)
rmse(rmse1$score, rmse1$pred)
rmse(rmse2$score, rmse2$pred)
rmse(rmse3$score, rmse3$pred)

##----------------------------------------------------------------------------##
## Data Modelling - K-means clustering
##----------------------------------------------------------------------------##
ggplot(restaurant_scores, aes(date, score)) + 
    geom_point(aes(col=cuisine))

# Convert dates to numeric and remove NA values
restaurant_scores_clean <- restaurant_scores %>%
    mutate(date = as.numeric(as.Date(date))) %>%
    filter(!is.na(date), !is.na(score))

# apply kmeans
set.seed(100) #prior kmeans() function, set the seed to make k-means results "stable" 
cuisineCluster <- kmeans(restaurant_scores_clean %>% select(2,4), center=10, nstart=200)

#Match the predicted clusters with the original data.
table(cuisineCluster$cluster, restaurant_scores_clean$cuisine)

#Visualise the clusters
#set the cluster to categorical variables
restaurant_scores_clean$Cluster <- cuisineCluster$cluster
restaurant_scores_clean$Cluster <- factor(restaurant_scores_clean$Cluster)

#plot
ggplot(restaurant_scores_clean, aes(date, score)) + 
    geom_point(aes(col=Cluster))
