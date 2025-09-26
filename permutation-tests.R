# Permutation tests to look at diversity and richness of zoos
# Logan Estell
# lestell@arizona.edu
# 2025-09-26

#Libraries
require(dplyr) #data wrangling
require(tidyr) #data wrangling
require(ggplot2) #dat visualization
require(stringr) #text formatting for plots
require (extrafont) #use of arial in figures

#load data
zoos <- read.csv(file= "Data/Zoos.csv")

#only pull observations from 2000 or later
min_year <-  2000

#drop singletons?
drop_single <- FALSE

#file to save permutation results
perm_test_file <-  "output/perm-test-results.rds"

#force permutation tests even if results exist on disk?
force_perms <- FALSE

if (force_perms | !file.exists(perm_test_file))  {
  #parameters for perm tests
  nreps <-  1000
  min_obs <- 40
  
  perm_tests <- list()
  for (zoo_i in 1:nrow(zoos)) {
    #get file names for garden and cities
    zoo_name <- tolower(x= gsub(pattern = " ",
                                replacement = "_",
                                x = zoos$name[zoo_i]))
    zoo_file <- paste0("data/gbif/", zoo_name, "-obs.csv")
    zoo_obs <- read.csv(file = zoo_file)
    #only keep records with species name and year >= min_year
    zoo_obs <- zoo_obs %>%
      filter(!is.na(species)) %>%
      filter(year >= min_year)
    
    #drop duplicates
    zoo_obs <-zoo_obs %>%
      distinct(decimalLongitude, decimalLatitude, family, 
               species, year, month, day, .keep_all = TRUE)
    
    #drop singletons where appropriate
    if (drop_single) {
      zoo_multiples <- zoo_obs %>%
        group_by(species) %>%
        summarize(num_obs = n()) %>%
        filter(num_obs > 1)
      zoo_obs <- zoo_obs %>%
        filter(species %in% zoo_multiples$species)
    }
    
    # Only proceed if number of zoo observations is large enough
    if (nrow(zoo_obs) >= min_obs) {
      # RICHNESS for this garden
      garden_richness <- length(unique(zoo_obs$species))
      
      # DIVERSITY for this zoo (Shannon's Index, H)
      zoo_diversity <- zoo_obs %>%
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