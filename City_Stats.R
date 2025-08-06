#Calc city and garden sizes
#Logan Estell
#lestell@arizona.edu
#Derived from Jeff Oliver Botanical Garden Codes
#2025-08-6

require(osmdata) # city boundaries
require(sf) #polygon area calc
require(dplyr) #data wrangling

zoos <- read.csv(file = "Data/Zoos.csv")

#add to the data frame
zoos$zoo_area <- NA
zoos$city_area <- NA

# Iterate over all zoos
for (zoo_i in 1:nrow(zoos)) {
  # start w zoo polygon, calc area, create a simple feature
  zoo_coords <- matrix(data = c(zoos$lon_min[zoo_i], zoos$lat_max[zoo_i], 
                                zoos$lon_max[zoo_i], zoos$lat_max[zoo_i],
                                zoos$lon_max[zoo_i], zoos$lat_min[zoo_i],
                                zoos$lon_min[zoo_i], zoos$lat_min[zoo_i],
                                zoos$lon_min[zoo_i], zoos$lat_max[zoo_i]),
                       nrow = 5, byrow = TRUE)
  zoo_st_poly <- sf::st_polygon(x = list(zoo_coords), dim = "XY")
  
  #For area, we add the CRS and convert to a simple feature
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  zoo_sf <- sf::st_sfc(zoo_st_poly, crs = wgs84)
  
  #We needs to wrap in set_units to get area in km^2
  zoos$zoo_area[zoo_i] <-  units::set_units(sf::st_area(zoo_sf), "km^2")
  
  #get city data
  city_name <- zoos$city[zoo_i]
  state_name <- zoos$state[zoo_i]
  
  city_area <- zoos$city_area[zoos$city == city_name]
  if (any(!is.na(city_area))) {
    # Have a non-missing city area, so just use that value
    city_area <- city_area[!is.na(city_area)][1] # hack
  } else { # Area not yet recorded, so do the calculations now
    city_string <- paste0(city_name, ", ", state_name)
    message(paste0("Running query for ", city_string, " on OSM."))
    city_poly <- osmdata::getbb(place_name = city_string,
                                format_out = "polygon")
    if (class(city_poly)[1] == "list") {
      city_poly <- city_poly[[1]]
    }
    city_st_poly <- sf::st_polygon(x = list(city_poly), dim = "XY")
    city_sf <- sf::st_sfc(city_st_poly, crs = wgs84)
    city_area <- units::set_units(sf::st_area(city_sf), 
                                  "km^2")
  }
  zoos$city_area[zoo_i] <- city_area
}

#write to file
zoos %>%
  select(name, city, state, city_area, zoo_area) %>%
  mutate(city = paste0(city, ", ", state)) %>%
  select(-state) %>%
  mutate(perc = round((zoo_area / city_area) * 100, digits = 4)) %>%
  mutate(city_area = round(city_area, digits = 3),
         zoo_area = round(zoo_area, digits = 3)) %>%
  write.csv(file = "Output/city_stats.csv",
            row.names =FALSE)