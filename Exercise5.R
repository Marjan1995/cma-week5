library(readr)
library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tmap)

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") %>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

# Task 1
fields <- read_sf("Feldaufnahmen_Fanel.gpkg")
ggplot(fields, aes(fill = Frucht)) +
geom_sf() +
coord_sf(datum = 2056)
fields


# Task 2