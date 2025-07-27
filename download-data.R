# Download observation data from GBIF
# Logan Estell
# Based on Jeff Oliver codesin BotanicGardenHotspot Repo
# 2025-07-27

library(sf)


# Libraries
require(dplyr)    # data wrangling
require(osmdata)  # city boundaries from OpenStreetProject
require(sf)       # point filtering for cities
require(rgbif)
source(file = "Functions/query_gbif.R")

# NOT SURE IF NEED TO DO THIS ASK IN NEXT MEETING:
# TODO: some "cities" should actually be multiple bounding boxes
# Palm Desert = Palm Desert + Indian Wells
# Phoenix = Phoenix + Tempe
# El Paso = El Paso + Ciudad Juárez
#TODO: keep city-stats updated based on this

# Load zoo data
zoos <- read.csv(file = "data/zoos.csv")

# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE

# Download data for each zoo, then for the corresponding cities
taxon_keys <- c("Hesperiidae" = 6953,
                "Lycainidae" = 5473,
                "Nymphalidae" = 7017,
                "Papilionidae" = 9417,
                "Pieridae" = 5481,
                "Riodinidae" = 1933999)

for (zoo_i in 1:nrow(zoos)) {
  # Make a nice filename for the data file
  zoo_name <- tolower(x = gsub(pattern = " ", 
                                  replacement = "_",
                                  x = zoos$name[zoo_i]))
  zoo_file <- paste0("data/gbif/", zoo_name, "-obs.csv")
  if (overwrite | !file.exists(zoo_file)) {
    message("***  Downloading data for ", zoos$name[zoo_i])
    # Count number of observations in zoo rectangle, as pagination might be 
    # necessary; actually performs one search per taxonKey value (in this case, 
    # one search per family and returns list with one element for each family)
    zoos_obs <- query_gbif(taxon_keys = taxon_keys,
                             lon_limits = c(zoos$lon_min[zoo_i], 
                                            zoos$lon_max[zoo_i]),
                             lat_limits = c(zoos$lat_min[zoo_i], 
                                            zoos$lat_max[zoo_i]),
                             verbose = TRUE)
    
    write.csv(x = zoos_obs,
              file = zoo_file,
              row.names = FALSE)
  } else {
    message("Skipping download for ", zoos$name[zoo_i], ", already on disk.")
  }
}

# Now download data for the cities
city_state_string <- paste(zoos$city, zoos$state, sep = ", ")
city_state_string <- unique(city_state_string)

for(city_state in city_state_string) {
  # Make a name for filename, have to do it twice to avoid double underscores
  city_name <- tolower(x = gsub(pattern = ", ",
                                replacement = "_",
                                x = city_state))
}

city_name <- gsub(pattern = " ",
                  replacement = "_",
                  x = city_name)
city_file <- paste0("data/gbif/", city_name, "-obs.csv")
if (overwrite | !file.exists(city_file)) {
  message("***  Downloading data for ", city_state)
  city_poly <- osmdata::getbb(place_name = city_state, format_out = "polygon")
  # Most queries return a list, and we just want the first matrix element; when 
  # a single polygon is returned, it is already a matrix
  if (class(city_poly)[1] == "list") {
    city_poly <- city_poly[[1]]
  }
  
  # First find the maximum containing rectangle coordinates and use those for 
  # the GBIF query
  min_lon <- min(city_poly[, 1])
  max_lon <- max(city_poly[, 1])
  min_lat <- min(city_poly[, 2])
  max_lat <- max(city_poly[, 2])
  
  city_obs <- query_gbif(taxon_keys = taxon_keys,
                         lon_limits = c(min_lon, max_lon),
                         lat_limits = c(min_lat, max_lat),
                         verbose = TRUE)
  
  # Convert the polygon to a simple feature for ease of filtering points
  city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")
  
  # Now make the city_obs into a simple feature  
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  city_obs_sf <- sf::st_as_sf(x = city_obs,
                              coords = c("decimalLongitude", "decimalLatitude"),
                              crs = wgs84)
  
  # and use it with sf::st_within; below returns logical vector indicating 
  # whether point is within the polygon; use that vector to select rows from 
  # city_obs that are within the city polygon
  points_within <- sf::st_within(x = city_obs_sf, y = city_sf) %>% lengths > 0
  city_obs <- city_obs[points_within, ]
  write.csv(x = city_obs,
            file = city_file,
            row.names = FALSE)
} else {
  message("Skipping download for ", city_state, ", already on disk.")
}
