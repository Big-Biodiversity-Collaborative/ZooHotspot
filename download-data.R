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
source(file = "functions/query_gbif.R")

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