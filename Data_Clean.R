# Quick check to see how many observations we have
# Logan Estell
# lestell@arizona.edu
# Derived from Jeff Oliver Botanical Garden Codes
# 2025-08-05

library(dplyr)

# Load in Zoo data
zoos <- read.csv(file = "data/zoos.csv")

obs_counts <- data.frame(zoos = zoos$name,
                        zoo_count = NA,
                        city_count = NA)

#tally # of obs of each species
species_obs <- NULL

#only take obs from yr 2000 or later
min_year <- 2000

#TODO: Ask Jeff about this section
# Three species warranted investigation:
# C. columella - turns out that GBIF considers Strymon istapa as subspecies of 
#                C. columella; all records are fine
# E. aveyrana - used to be in Phyciodes; all records are fine
# P. astylalus - only one observation in Tohono Chul; still questionable
sp_to_check <- c("Callicista columella", "Eresia aveyrana", "Papilio astyalus")
check_obs <- NULL

for (zoo_i in 1:nrow(zoos)) {
  #get file names for zoo and cities
  zoo_name <- tolower(x = gsub(pattern = " ",
                               replacement = "_",
                               x = zoos$name[zoo_i]))
  zoo_file <- paste0("Data/GBIF/", zoo_name, "-obs.csv")
  city_state <- paste(zoos[zoo_i],
                      zoos$state[zoo_i],
                      sep = ", ")
  city_name <- tolower(x =gsub(pattern = ", ",
                               replacement = "_",
                               x = city_state))
  city_name <- gsub(pattern = " ",
                    replacement = "_",
                    x = city_name)
  city_file <- paste0("Data/GBIF/", city_name, "-obs.csv")
}

#Read in data
zoos_obs <- read.csv(file = zoo_file)
city_obs <- read.csv(file = city_file)

#Drop rows missing species names and old records
zoo_obs <- zoo_obs %>%
  filter(!is.na(species)) %>%
  filter(year >= min_year)
city_obs <- city_obs %>%
  filter(!is.na(species)) %>%
  filter(year >= min_year)
