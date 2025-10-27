# Species richness and diversity calculations for gardens and cities
# Derived from Jeff Oliver's code in BotanicalGardenHotspot Repo
# Logan Estell
# lestell@arizona.edu
# 2025-10-26

require(dplyr)   # data wrangling

# Load zoo data
zoos <- read.csv(file = "data/zoos.csv")
min_obs <- 35

# We only want observations that occurred in or after 2000 
min_year <- 2000

zoos$zoo_richness <- NA
zoos$zoo_diversity <- NA
zoos$city_richness <- NA
zoos$city_diversity <- NA

for (zoo_i in 1:nrow(zoos)) {
  # Get file names for zoo and corresponding city
  zoo_name <- tolower(x = gsub(pattern = " ", 
                                  replacement = "_",
                                  x = zoos$name[zoo_i]))
  zoo_file <- paste0("data/gbif/", zoo_name, "-obs.csv")
  zoo_obs <- read.csv(file = zoo_file)
  # Keep only those records with species name and year >= min_year
  zoo_obs <- zoo_obs %>%
    filter(!is.na(species)) %>%
    filter(year >= min_year)
  
  # Drop duplicates
  zoo_obs <- zoo_obs %>%
    distinct(decimalLongitude, decimalLatitude, family, 
             species, year, month, day, .keep_all = TRUE)
  
  if (nrow(zoo_obs) >= min_obs) {
    # RICHNESS for this zoo
    zoos$zoo_richness[zoo_i] <- length(unique(zoo_obs$species))
    
    # DIVERSITY for this garden (Shannon's Index, H)
    zoos$zoo_diversity[zoo_i] <- zoo_obs %>%
      group_by(species) %>%
      summarize(abun = n()) %>%
      ungroup() %>%
      mutate(total_abun = sum(abun)) %>%
      mutate(p = abun/total_abun) %>%
      mutate(plogp = -1 * (p * log(p))) %>%
      select(plogp) %>%
      sum()
    
    # Grab city info
    city_state <- paste(zoos$city[zoo_i], 
                        zoos$state[zoo_i], 
                        sep = ", ")
    city_name <- tolower(x = gsub(pattern = ", ",
                                  replacement = "_",
                                  x = city_state))
    city_name <- gsub(pattern = " ",
                      replacement = "_",
                      x = city_name)
    city_file <- paste0("data/gbif/", city_name, "-obs.csv")
    
    # Read in data
    city_obs <- read.csv(file = city_file)
    
    # Keep only those records with species name and year >= min_year
    city_obs <- city_obs %>%
      filter(!is.na(species)) %>%
      filter(year >= min_year)
    
    # Drop duplicates
    city_obs <- city_obs %>%
      distinct(decimalLongitude, decimalLatitude, family, 
               species, year, month, day, .keep_all = TRUE)
    
    zoos$city_richness[zoo_i] <- length(unique(city_obs$species))
    
    zoos$city_diversity[zoo_i] <- city_obs %>%
      group_by(species) %>%
      summarize(abun = n()) %>%
      ungroup() %>%
      mutate(total_abun = sum(abun)) %>%
      mutate(p = abun/total_abun) %>%
      mutate(plogp = -1 * (p * log(p))) %>%
      select(plogp) %>%
      sum()
  }
}
# Write to file
zoos %>%
  select(name, city, state, zoo_richness, zoo_diversity, 
         city_richness, city_diversity) %>%
  mutate(perc_richness = zoo_richness/city_richness,
         perc_diversity = zoo_diversity/city_diversity) %>%
  write.csv(file = "output/richness-diversity.csv",
            row.names = FALSE)