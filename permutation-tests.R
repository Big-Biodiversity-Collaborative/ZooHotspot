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
force_perms <- TRUE

if (force_perms | !file.exists(perm_test_file))  {
  #parameters for perm tests
  nreps <-  1000
  min_obs <- 35
  
  perm_tests <- list()
  for (zoo_i in 1:nrow(zoos)) {
    #get file names for garden and cities
    zoo_name <- tolower(x= gsub(pattern = " ",
                                replacement = "_",
                                x = zoos$name[zoo_i]))
    zoo_file <- paste0("Data/GBIF/", zoo_name, "-obs.csv")
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
      zoo_richness <- length(unique(zoo_obs$species))
      
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
      
      city_state <- paste(zoos$city[zoo_i],
                          zoos$state[zoo_i],
                          sep = ", ")
      message(paste0("Starting permutation test for ", zoos$name[zoo_i]))
      city_name <- tolower(x = gsub(pattern = ", ",
                                    replacement = "_",
                                    x = city_state))
      city_name <- gsub(pattern = " ",
                        replacement = "_",
                        x = city_name)
      city_file <- paste0("Data/GBIF/", city_name, "-obs.csv")
      
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
      
      # Drop singletons, if appropriate
      if (drop_single) {
        city_multiples <- city_obs %>%
          group_by(species) %>%
          summarize(num_obs = n()) %>%
          filter(num_obs > 1)
        city_obs <- city_obs %>%
          filter(species %in% city_multiples$species)
      }
      city_obs <- city_obs %>%
        dplyr::filter(!(gbifID %in% zoo_obs$gbifID))
      
      # Also, drop duplicate lat/lon coordinates before selecting points, 
      # otherwise highly sampled areas will be over-represented in our sampling
      city_sample_points <- city_obs %>%
        distinct(decimalLongitude, decimalLatitude) %>%
        slice_sample(n = nreps, replace = TRUE)
      
      # Grab city bounding box, so we can pull out rectangles of similar size to 
      # the garden
      # city_poly <- osmdata::getbb(place_name = city_state,
      #                             format_out = "polygon")
      # Most queries return a list, and we just want the first matrix element; when 
      # a single polygon is returned, it is already a matrix
      # if (class(city_poly) == "list") {
      #   city_poly <- city_poly[[1]]
      # }
      # Convert the polygon to a simple feature for random sampling of points
      # city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")
      # city_sample_points <- sf::st_coordinates(sf::st_sample(x = city_sf,
      #                                                        size = nreps))
      # Don't want to sample sampling deserts, so use a loop to continue sampling
      # until there is at least one point in each rectangle
      
      # Use zoo dimensions to dictate bounding box size for sampling within 
      # the city
      lat_dim <- zoos$lat_max[zoo_i] - zoos$lat_min[zoo_i]
      lon_dim <- zoos$lon_max[zoo_i] - zoos$lon_min[zoo_i]
      
      # The data frame to hold box coordinates and resulting richness and 
      # diversity
      city_samples <- data.frame(min_lon = city_sample_points$decimalLongitude - lon_dim/2,
                                 max_lon = city_sample_points$decimalLongitude + lon_dim/2,
                                 min_lat = city_sample_points$decimalLatitude - lat_dim/2,
                                 max_lat = city_sample_points$decimalLatitude + lat_dim/2,
                                 richness = NA,
                                 diversity = NA)
      
      message(paste0("Running ", nreps, " reps for ", zoos$name[zoo_i]))
      for (rep_i in 1:nreps) {
        # Use the rectangle to select points from the city_obs that fall within 
        # the current rectangle
        one_sample <- city_samples[rep_i, ]
        city_sample <- city_obs %>%
          filter(decimalLongitude <= one_sample$max_lon[1],
                 decimalLongitude >= one_sample$min_lon[1],
                 decimalLatitude <= one_sample$max_lat[1],
                 decimalLatitude >= one_sample$min_lat[1])
        
        # RICHNESS for this sample
        sample_richness <- 0
        if (nrow(city_sample) > 0) {
          sample_richness <- length(unique(city_sample$species))
          # message(paste0("Richness ", sample_richness, " in rep ", rep_i))
        } else {
          message("Richness of zero encountered during replicate")
        }
        
        city_samples$richness[rep_i] <- sample_richness
        
        # DIVERSITY for this sample (Shannon's Index, H)
        sample_diversity <- city_sample %>%
          group_by(species) %>%
          summarize(abun = n()) %>%
          ungroup() %>%
          mutate(total_abun = sum(abun)) %>%
          mutate(p = abun/total_abun) %>%
          mutate(plogp = -1 * (p * log(p))) %>%
          select(plogp) %>%
          sum()
        
        city_samples$diversity[rep_i] <- sample_diversity
      }
      
      # Find the probability of a garden richness value this large or larger by 
      # using the ecdf function (note ecdf itself returns a function, to which 
      # we immediately pass the garden richness)
      richness_quantile <- ecdf(x = city_samples$richness)(zoo_richness)
      diversity_quantile <- ecdf(x = city_samples$diversity)(zoo_diversity)
      
      perm_tests[[zoo_name]] <- list(sample_values = city_samples,
                                        zoo_richness = zoo_richness,
                                        zoo_diversity = zoo_diversity,
                                        # zoo_prob = zoo_prob,
                                        # upper_95 = upper_95,
                                        richness_quantile = richness_quantile,
                                        diversity_quantile = diversity_quantile)
 #Permutation t-tests, we bootstrap zoo and city obs, creating a random sample
#of 20% of zoo obs to test if city richness and diversity are higher than zoo richness and diversity
      #restrict to only zoos with >= 50 obs
      
    if (nrow(zoo_obs) >= 50) {
      message(paste0("Running permutation t-test for ", zoos$name[zoo_i]))
      sample_size <- ceiling(x = 0.2 * nrow(zoo_obs))
      
      richness_mat <- matrix(data = NA, nrow = nreps, ncol = 2)
      diversity_mat <- matrix(data = NA, nrow = nreps, ncol = 2)
      for (rep_i in 1:nreps) {      
        zoo_sample <- zoo_obs %>%
          slice_sample(n = sample_size)
        
        city_sample <- city_obs %>%
          slice_sample(n = sample_size)
        
        # zoo richness in column 1, city richness in column 2
        richness_mat[rep_i, 1] <- length(unique(zoo_sample$species))
        richness_mat[rep_i, 2] <- length(unique(city_sample$species))
        
        # zoo diversity in column 1, city diversity in column 2
        diversity_mat[rep_i, 1] <- zoo_sample %>%
          group_by(species) %>%
          summarize(abun = n()) %>%
          ungroup() %>%
          mutate(total_abun = sum(abun)) %>%
          mutate(p = abun/total_abun) %>%
          mutate(plogp = -1 * (p * log(p))) %>%
          select(plogp) %>%
          sum()
        
        diversity_mat[rep_i, 2] <- city_sample %>%
          group_by(species) %>%
          summarize(abun = n()) %>%
          ungroup() %>%
          mutate(total_abun = sum(abun)) %>%
          mutate(p = abun/total_abun) %>%
          mutate(plogp = -1 * (p * log(p))) %>%
          select(plogp) %>%
          sum()
      }
      # Run t-test on resulting matrix; is zoo richness significantly lower 
      # than city?
      richness_t <- t.test(x = richness_mat[, 1], 
                           y = richness_mat[, 2],
                           alternative = "less")
      diversity_t <- t.test(x = diversity_mat[, 1],
                            y = diversity_mat[, 2],
                            alternative = "less")
      
      perm_tests[[zoo_name]][["richness_t"]] <- richness_t
      perm_tests[[zoo_name]][["diversity_t"]] <- diversity_t
    } else {
      message(paste0("Skipping permutation t-test for ", zoos$name[zoo_i],
                     ", too few observations"))
    }
    
    
    
  } else {
    message("Too few observations in ", zoos$name[zoo_i], 
            " no permutation tests performed.")
  }
}
saveRDS(object = perm_tests,
        file = perm_test_file)
} else {
  message("Permutation test results already on file. Using those.")
}

# Extract values from the list of permutations, putting them in one data frame 
# for ease of plotting. The garden column gets lots of mutates to make plot 
# a little nicer

if (file.exists(perm_test_file)) {
  perm_tests <- readRDS(file = perm_test_file)
} else {
  message("No perm test results file on disk")
}
sample_values_list <- lapply(X = perm_tests, FUN = "[[", "sample_values")
sample_values <- dplyr::bind_rows(sample_values_list, .id = "zoo")
sample_values <- sample_values %>%
  rename(zoo_name = zoo) %>%
  mutate(zoo_print = gsub(pattern = "_",
                             replacement = " ",
                             x = zoo_name)) %>%
  mutate(zoo_print = stringr::str_to_title(zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Abq",
                             replacement = "ABQ",
                             x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "San Antonio", # To fit on plot
                             replacement = "\nSan Antonio",
                             x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Reid Park",
                             replacement = "\nReid Park",
                             x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Phx",
                             replacement = "\nPhoenix",
                             x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "San Diego",
                          replacement = "\nSan Diego",
                          x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Los Angeles",
                        replacement = "\nLos Angeles",
                        x = zoo_print))


# Pull out values for each zoo; similar mutations in zoo name for plots
zoo_richness <- unlist(lapply(X = perm_tests, 
                                 FUN = "[[", "zoo_richness"))
zoo_diversity <- unlist(lapply(X = perm_tests, 
                                  FUN = "[[", "zoo_diversity"))
richness_quantiles <- unlist(lapply(X = perm_tests, 
                                    FUN = "[[", "richness_quantile"))
diversity_quantiles <- unlist(lapply(X = perm_tests, 
                                     FUN = "[[", "diversity_quantile"))
zoo_values <- data.frame(zoo = names(zoo_richness),
                            richness = zoo_richness,
                            richness_quantile = richness_quantiles,
                            diversity = zoo_diversity,
                            diversity_quantile = diversity_quantiles)
rownames(zoo_values) <- NULL


# Updates to zoo names for printing purposes
zoo_values <- zoo_values  %>%
  rename(zoo_name = zoo) %>%
  mutate(zoo_print = gsub(pattern = "_",
                             replacement = " ",
                             x = zoo_name)) %>%
  mutate(zoo_print = stringr::str_to_title(zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Abq",
                             replacement = "ABQ",
                             x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "San Antonio", # To fit on plot
                          replacement = "\nSan Antonio",
                          x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Reid Park",
                          replacement = "\nReid Park",
                          x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Phx",
                          replacement = "\nPhoenix",
                          x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "San Diego",
                        replacement = "\nSan Diego",
                        x = zoo_print)) %>%
  mutate(zoo_print = gsub(pattern = "Los Angeles",
                        replacement = "\nLos Angeles",
                        x = zoo_print))
  

richness_plot <- ggplot(data = sample_values, mapping = aes(x = zoo_print, 
                                                            y = richness,
                                                            fill = zoo_print)) +
  geom_violin() +
  # geom_boxplot() +
  ylab("Species Richness") +
  xlab(element_blank()) +
  scale_fill_discrete(type = rainbow(length(unique(sample_values$zoo_print)))) +
  geom_point(data = zoo_values, 
             mapping = aes(x = zoo_print, y = richness),
             # color = "black", shape = 17, size = 4.5,
             shape = 24, 
             fill = "#FFFFFF",
             color = "#000000",
             size = 3,
             stroke = 0.6) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 6), # Font sizes for print
        axis.title.y = element_text(size = 8),
        text = element_text(family = "ArialMT"))
richness_plot
ggsave(filename = "output/Richness-plot.pdf",
       plot = richness_plot,
       width = 5,
       height = 3,
       units = "in")
ggsave(filename = "output/Richness-plot.png",
       plot = richness_plot,
       width = 5,
       height = 3,
       units = "in")

# Diversity
diversity_plot <- ggplot(data = sample_values, mapping = aes(x = zoo_print, 
                                                             y = diversity,
                                                             fill = zoo_print)) +
  geom_violin() + # Ahem
  # geom_boxplot() +
  ylab("Shannon's Index") +
  xlab(element_blank()) +
  scale_fill_discrete(type = rainbow(length(unique(sample_values$zoo_print)))) +
  geom_point(data = zoo_values, 
             mapping = aes(x = zoo_print, y = diversity),
             # color = "black", shape = 17, size = 4.5,
             shape = 24, 
             fill = "#FFFFFF",
             color = "#000000",
             size = 3,
             stroke = 0.6) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 6), # Font sizes for print
        axis.title.y = element_text(size = 8),
        text = element_text(family = "ArialMT"))
diversity_plot
ggsave(filename = "output/Diversity-plot.pdf",
       plot = diversity_plot,
       width = 5,
       height = 3,
       units = "in")
ggsave(filename = "output/Diversity-plot.png",
       plot = diversity_plot,
       width = 5,
       height = 3,
       units = "in")

# And some final cleaning up before writing values to file
zoo_values %>%
  select(-zoo_print) %>%
  mutate(diversity = round(diversity, digits = 2)) %>%
  rename(zoo = zoo_name) %>%
  mutate(zoo = gsub(pattern = "_",
                       replacement = " ",
                       x = zoo)) %>%
  mutate(zoo = stringr::str_to_title(zoo)) %>%
  mutate(zoo = gsub(pattern = "Abq",
                       replacement = "ABQ",
                       x = zoo)) %>%
  write.csv(file = "output/perm-tests.csv",
            row.names = FALSE)

# Pull out values for those permutation t-tests
richness_t_list <- lapply(X = perm_tests, FUN = "[[", "richness_t")
diversity_t_list <- lapply(X = perm_tests, FUN = "[[", "diversity_t")
