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
wildschwein_filter <- wildschwein_BE %>% 
mutate(month = month(DatetimeUTC)) %>% 
filter(month == 5 | month == 6)
ggplot() +
geom_sf(data = fields, aes(fill = Frucht)) +
geom_point(data = wildschwein_filter, aes(E, N, colour = TierID)) +
coord_sf(datum = 2056)
wildschwein_join <- st_join(x = wildschwein_filter, y = fields)

# Task 3: Explore annotated trajectories
wildschwein_sum <- wildschwein_join %>% 
st_drop_geometry() %>%
mutate(hour = hour(DatetimeUTC)) %>% 
group_by(TierName, Frucht, hour) %>% 
summarise(n = n()) %>%
group_by(TierName, hour) %>% # regroup by animal and hour to calculate relative sums 
mutate(n_tot = sum(n),
sum_rel = n/n_tot)
ggplot(wildschwein_sum, aes(hour, sum_rel, fill = Frucht)) +
geom_col() +
facet_wrap(~ TierName) +
scale_y_continuous(labels = scales::percent) +
labs(x = "Time (rounded to the nearest hour)", y = "Percentage")
ggplot(wildschwein_sum, aes(hour, sum_rel, fill = Frucht)) +
geom_col() +
facet_wrap(~ TierName) +
scale_y_continuous(labels = scales::percent) +
labs(x = "Time (rounded to the nearest hour)", y = "Percentage") +
coord_polar (start = -0.1)

# Task 4: Import and visualize vegetationindex (raster data)
vegetation <- terra::rast("vegetationshoehe_LFI.tif")
vegetation
plot(vegetation)
tm_shape(vegetation) +
tm_raster(palette = "viridis") +
tm_layout(legend.outside = TRUE)

# Task 5: Annotate Trajectories from raster data
wildschwein_coords <- st_coordinates(wildschwein_filter)
vegetation_raster <- extract(vegetation, wildschwein_coords)
wildschwein_raster <- bind_cols(wildschwein_filter, vegetation_raster)
wildschwein_sum2 <- wildschwein_raster %>% 
st_drop_geometry() %>%
mutate(hour = hour(DatetimeUTC)) %>% 
group_by(TierName, vegetationshoehe_LFI, hour) %>% 
summarise(n = n()) %>%
group_by(TierName, hour) %>% # regroup by animal and hour to calculate relative sums 
mutate(n_tot = sum(n),
sum_rel = n/n_tot)
ggplot(wildschwein_sum2, aes(hour, sum_rel, fill = vegetationshoehe_LFI)) +
geom_col() +
facet_wrap(~ TierName) +
scale_y_continuous(labels = scales::percent) +
scale_fill_gradient(low = "blue", high = "yellow") +
labs(x = "Time (rounded to the nearest hour)", y = "Percentage")
ggplot(wildschwein_sum2, aes(hour, sum_rel, fill = vegetationshoehe_LFI)) +
geom_col() +
facet_wrap(~ TierName) +
scale_y_continuous(labels = scales::percent) +
scale_fill_gradient(low = "blue", high = "yellow") +
labs(x = "Time (rounded to the nearest hour)", y = "Percentage") +
coord_polar (start = -0.1)

