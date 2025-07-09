# Zoo Hotspots

## Summary

This project uses community science observations of butterflies to compare 
biodiversity in zoological parks to that of surrounding urban areas in the 
southwestern United States of America. It uses two measures, species richness 
and Shannon's Index of diversity, along with bootstrapping replicates to 
illustrate how relatively small zoological parks serve as biodiversity 
hotspots for urban areas.

## Dependencies

The data downloads, analyses, and visualization are written in R and use the 
following additional R packages:

+ dplyr: general data wrangling
+ extrafont: use of Arial font in plots
+ ggplot2: data visualization
+ ggpubr: multi-panel plots
+ osmdata: querying [OpenStreetMaps](https://www.openstreetmap.org) for city 
boundaries
+ rgbif: downloading data from [GBIF](https://gbif.org)
+ sf: point filtering of city observations (with osmdata)
+ stringr: text formatting in data visualization
+ tidyr: general data wrangling

## Project organization

R scripts for data downloads, analysis, and visualization are included in the 
top-level folder. Additional folders and contents are:

+ data: information about gardens (coordinate bounding boxes, cities) and 
rainfall for corresponding cities
    + gbif: community science records for gardens and cities downloaded from 
    GBIF; dataset record [10.15468/dd.uqf3vw](https://doi.org/10.15468/dd.uqf3vw)
+ functions: utility function to help with pageination during GBIF downloads
+ output: plots and output tables; most files are not under version control
