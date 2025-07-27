# Download observation data from GBIF
# Logan Estell
# Based on Jeff Oliver codesin BotanicGardenHotspot Repo
# 2025-07-27

# Libraries
require(dplyr)    # data wrangling
require(osmdata)  # city boundaries from OpenStreetProject
require(sf)       # point filtering for cities
source(file = "functions/query_gbif.R")

# TODO: some "cities" should actually be multiple bounding boxes
# Palm Desert = Palm Desert + Indian Wells
# Phoenix = Phoenix + Tempe
# El Paso = El Paso + Ciudad Juárez
#TODO: keep city-stats updated based on this

# Lod zoo data
zoos <- read.csv(file = "data/zoos.csv")


