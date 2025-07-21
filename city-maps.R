# Plot cities and zoos
# Logan Estell
# lestell@arizona.edu
# 2025-07-20

require(dplyr)  # data wrangling
require(osmdata)  # city boundaries
require(ggplot2) #data viz
require(ggpubr) #multi-panel plot
require(extrafont) #to use Arial in figures

# Get a polygon for each city
# Add star for zoo location and plus symbols for records from GBIF
# Label plots accordingly

zoos <- read.csv(file = "data/Zoos.csv")

cities <- unique(zoos$city)

city_plots <- list()

for (city_i in 1:length(cities)) {
  # city_i <- 1
  
  # Need city and state abbreviation.
  city_name <- cities[city_i]
state_abbr <- zoos$state[zoos$city == city_name][1]  

# Make a character string we can use with OpenStreetMap data with city and state abbreviation
city_state <- paste0(city_name, ", ", state_abbr)
message(paste0("Creating plot for ", city_state))
city_poly <- osmdata::getbb(place_name = city_state,
                            format_out = "polygon")
# Most queries return a list, and we just want first matrix element when a single polygon is returned, it is already a matrix

if (class(city_poly[1] == "list")) {
  city_poly <- city_poly[[1]]}
}

# Now get GBIF observations for the city

