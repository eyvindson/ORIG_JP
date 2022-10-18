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


library(sf)

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
  'Input/Weather//NFI12PLOTS',
  col.names=c('region','cluster','plot','stand','centre','area','forest','drpeat','peat_type'),
  na.strings='.'
)


NFI12 <-
  NFI12_plots %>% 
  filter(drpeat == 1, forest == 1) %>% 
  group_by(region, peat_type) %>% 
  summarize(counts = n())

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
  select(region, cluster, plot, area, peat_type) %>% 
  rename(lohko = cluster,
         koeala = plot) %>% 
  left_join(NFI_coords) %>% 
  left_join(full_weatherdata) %>% 
  filter(month %in% c(5:10)) %>% 
  mutate(mainregion = if_else(region %in% c(1,2), "south", "north")) %>% 
  group_by(mainregion, peat_type, year) %>% 
  summarize(avg_T = mean(temp_avg)) %>% 
  mutate(roll_T = rollmean(avg_T, 30, align="right", fill=NA)) %>% 
  filter(year > 1989) %>% 
  select(-avg_T)


  
# Find out which NFI sample plots have a peat type assigned


# Summarizing the data and calculating averages

# result <- 
#   weather_data %>% 
#   # First we add the region data
# 
#   group_by(region, year, month) %>% 
#   summarise(mean_T = mean(temp_avg), sd_T = sd(temp_avg), cv = sd(temp_avg) / mean(temp_avg) * 100) %>% 
#   arrange(year, month, region)

# Save the output


write.table(x = NFI_weather, 
            file = PATH_weather_data_aggregated, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")
