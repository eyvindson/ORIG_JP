# INPUTS:
#   * WeatherData - raw weather data in tabular form, contains temperature and precipitation over a number of a years from 
#   a large array of different weather stations
#   * main areas - GIS shapefiles defining the sub-areas from which mean values are calculated from
#
# The script first reads all the shapefiles that define areas into which the weather stations will be divided into.
#
# How this script works:
# 1. 1x1 km grid of weather data from 1960-current inventory year
# 2. read shapefiles, assign each weather observartion point to said shapefiles (normally South/North Finland)
# 3. read in NFI sampling plot coordinates, find the nearest weather grid point for each
# 4. calculate monthly average temperatures, precipitation and temperature ampilitude for each NFI sampling plot
# 5. aggregate mean values per peatland forest type, divided to S/N Finland
# 6. calculate YASSO weather data (full time series)
# 7. calculate peat degradation weather data, 30 year rolling average


#library(sf)

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")

# This function takes all the different shapefiles and checks whether a specific coordinate falls within it.
# It outputs the name of the shapefile.
find_region <- function(point, named_shapefiles) {
  names(named_shapefiles)[which(sapply(named_shapefiles, 
                                       st_intersects, 
                                       point, sparse = FALSE))]
}

NFI12_coordinates <- read.csv("C:/Users/03180980/luke-peatland/Input/Weather/vmi12_pisteet.csv")

NFI12_plots <- read.table(
  'Input/Weather/NFI12PLOTS',
  col.names=c('region','cluster','plot','stand','centre','area','forest','drpeat','peat_type'),
  na.strings='.'
)

# Reading the raw weather data
weather_data <- read.table(PATH_weather_data, header = TRUE)

# Properly format dates, add separate columns for "year" and "month"
weather_data$date_start <- as.Date(weather_data$date_start, "%Y-%m-%d")
weather_data$year <- as.numeric(format(weather_data$date_start,'%Y'))
weather_data$month <- as.numeric(format(weather_data$date_start,'%m'))

full_weatherdata <- 
  weather_data %>% 
  # Create a unique identifier for each grid point
  mutate(gridpoint = paste(x, y, sep = "-")) %>% 
  # We only want gridpoints that have an NFI sampling plot associated to them
  filter(gridpoint %in% unique(NFI12_coordinates$gridpoint)) %>% 
  select(year, month, gridpoint, temp_avg, prec)

NFI_coords <- 
  NFI12_coordinates %>% 
  filter(alue > 0, fra == 1) %>% 
  select(region = alue, lohko, koeala, gridpoint) 

NFI_weather <-
  NFI12_plots %>% 
  filter(forest == 1, drpeat == 1) %>% 
  select(region, lohko = cluster, koeala = plot, area, peat_type, weight = area) %>% 
  left_join(NFI_coords) %>% 
  left_join(full_weatherdata) %>% 
  filter(month %in% c(5:10)) %>% 
  mutate(mainregion = if_else(region %in% c(1,2), "south", "north")) %>% 
  group_by(mainregion, peat_type, year) %>% 
  # Due to different sampling regimes in different NFI areas, weighted mean has to be used
  summarize(avg_T = weighted.mean(temp_avg, weight)) %>% 
  mutate(roll_T = rollmean(avg_T, 30, align="right", fill=NA)) %>% 
  filter(year > 1989) %>% 
  rename(region = mainregion) %>% 
  select(-avg_T)

# YASSO weather here

y_old <-
  yasso_weather_old %>% 
  pivot_longer(cols = sum_P:ampli_T) %>% 
  mutate(age = "old")


YASSO_weather <-
  NFI12_plots %>% 
  filter(forest == 1, drpeat == 1) %>% 
  select(region, lohko = cluster, koeala = plot, area, peat_type, weight = area) %>% 
  left_join(NFI_coords) %>% 
  left_join(full_weatherdata) %>% 
  mutate(mainregion = if_else(region %in% c(1,2), 1, 2)) %>% 
  group_by(lohko, year) %>% 
  mutate(sum_P = sum(prec),
         ampli_T = (max(temp_avg) - min(temp_avg)) /2) %>% 
  ungroup() %>% 
  group_by(mainregion, year) %>% 
  summarize(sum_P = weighted.mean(sum_P, w = weight),
            avg_T = weighted.mean(temp_avg, w = weight),
            ampli_T = weighted.mean(ampli_T, w = weight)) %>% 
  pivot_longer(cols = sum_P:ampli_T) %>% 
  mutate(age = "new") %>% 
  rename(region = mainregion) %>% 
  rbind(y_old)

ggplot(YASSO_weather, aes(x = year, y = value, col = age)) + 
  geom_point() +
  geom_path() +
  facet_grid(name~region, scales = "free_y")

# Save the output


write.table(x = NFI_weather, 
            file = PATH_weather_data_aggregated, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")
