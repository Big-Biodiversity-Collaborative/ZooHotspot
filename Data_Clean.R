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
  city_state <- paste(zoos$city[zoo_i],
                      zoos$state[zoo_i],
                      sep = ", ")
  city_name <- tolower(x = gsub(pattern = ", ",
                               replacement = "_",
                               x = city_state))
  city_name <- gsub(pattern = " ",
                    replacement = "_",
                    x = city_name)
  city_file <- paste0("Data/GBIF/", city_name, "-obs.csv")


#Read in data
zoos_obs <- read.csv(file = zoo_file)
city_obs <- read.csv(file = city_file)

#Drop rows missing species names and old records
zoos_obs <- zoos_obs %>%
  filter(!is.na(species)) %>%
  filter(year >= min_year)
city_obs <- city_obs %>%
  filter(!is.na(species)) %>%
  filter(year >= min_year)

# Stash and question ones
ques <- zoos_obs %>%
  bind_rows(city_obs) %>%
  filter(species %in% sp_to_check)
if (nrow(ques) > 0) {
  if (is.null(check_obs)) {
    check_obs <- ques
  } else {
    check_obs <- check_obs %>%
      bind_rows(ques)
  }
}

# Get rid of duplicates of each data set
zoos_obs <- zoos_obs %>%
  distinct(decimalLongitude, decimalLatitude, family, 
           species, year, month, day, .keep_all = TRUE)
city_obs <- city_obs %>%
  distinct(decimalLongitude, decimalLatitude, family, 
           species, year, month, day, .keep_all = TRUE)

obs_counts$zoo_count[zoo_i] <- nrow(zoos_obs)
obs_counts$city_count[zoo_i] <- nrow(city_obs)

# Now do counts for species, keeping year for that tally, too
both_obs <- zoos_obs %>%
  bind_rows(city_obs) %>%
  rename(longitude = decimalLongitude,
         latitude = decimalLatitude) %>%
  distinct(longitude, latitude, family, species, 
           year, month, day, .keep_all = TRUE)

if (is.null(species_obs)) {
  species_obs <- both_obs
} else {
  species_obs <- species_obs %>%
    bind_rows(both_obs)
}
}

# Output table for obs counts
observation_counts <- obs_counts %>%
  left_join(zoos %>% select(name, city, state),
            by = c("zoos" = "name")) %>%
  arrange(state, city)
# observation_counts
write.csv(x = observation_counts,
          file = "output/observation-counts.csv",
          row.names = FALSE)

# Output table for species counts
species_counts <- species_obs %>%
  group_by(family, species) %>%
  summarize(num_obs = n()) %>%
  ungroup()
# nrow(species_counts)
write.csv(x = species_counts,
          file = "output/species-counts.csv",
          row.names = FALSE)

# How many from iNat and eButterfly?
source_counts <- species_obs %>%
  group_by(datasetName) %>%
  summarize(dataset_count = n())
# source_counts
