# Install required packages
install.packages("mongolite")

# Load required library
library(mongolite)

restaurants_collection = mongo(collection="restaurants", db="sample_restaurants", url=Sys.getenv("connection_string"))
neighborhoods_collection = mongo(collection="neighborhoods", db="sample_restaurants", url=Sys.getenv("connection_string"))

restaurants_collection$count()
neighborhoods_collection$count()
