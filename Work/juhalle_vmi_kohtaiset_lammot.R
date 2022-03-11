# INPUTS:
#   * WeatherData - raw weather data in tabular form, contains temperature and precipitation over a number of a years from 
#   a large array of different weather stations
#   * main areas - GIS shapefiles defining the sub-areas from which mean values are calculated from
#
# The script first reads all the shapefiles that define areas into which the weather stations will be divided into.
# Then the script calculates average annual temperature and precipitation per shapefile area.


library(sf)
library(dplyr)
library(reshape2)
library(ggplot2)
#library(RPostgreSQL)

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


# !TODO! Commented away until this step can be performed reliably
#
# install.packages("RPostgreSQL")
# require("RPostgreSQL")
# 
# drv <- dbDriver("PostgreSQL")
# 
# con <- dbConnect(drv,
#                  host="lukedb1.ns.luke.fi",
#                  dbname = "weather",
#                  port = "5432",
#                  user = "mkf140",
#                  password = "XXXXXXX")
# 
# dbListTables(con)
# dbListFields(con, "grid_month")
# t=dbGetQuery(con, "SELECT x,y,date_start,temp_avg,prec FROM grid_month WHERE y::text LIKE '___0%' AND x::text LIKE '___0%'")
# 
# write.table(t, "V:/hirsi/hsan2/khk/ghg/2019/soil/weather/WeatherData_1960_2019.csv", sep=" ", row.names = FALSE, col.names = TRUE, quote=FALSE)
# 
# dbDisconnect(con)


# This script works automatically with any arbitrary amount of shapefiles. However, since many other steps of the
# calculation use only South / North division, all shapefiles are categorized into these main categories as well as the subregions.

weather_shapefiles_list <- list.files(PATH_weather_mainareas, 
                                      pattern = "*.shp", 
                                      full.names = TRUE)
# Read in the names of the shapefiles without file extension
weather_shapefiles_names <- unlist(strsplit(list.files(PATH_weather_mainareas, 
                                                       pattern = "*.shp", 
                                                       full.names = FALSE), 
                                            ".shp"), use.names = TRUE)

# Then read aforementioned subregion files as shapefiles. NOTE! There is no check for overlap etc., so make sure the shapefiles are correct. 
# Also worth noting, that the since filenames are used in identifying the shapefiles, said filenames should be sensible and human readable.
weather_shapefiles <- lapply(weather_shapefiles_list, read_sf)
weather_shapefiles <- setNames(weather_shapefiles, weather_shapefiles_names)



all_areas <- do.call(rbind, weather_shapefiles)
all_areas$NAME <- weather_shapefiles_names
# Then draw the plot and save it
mapfig <- ggplot(all_areas) +
  geom_sf() +
  geom_sf_label(aes(label = NAME)) +
  labs(title = "Area subdivision of the weather data") 



# Reading the raw weather data
weather_data <- read.table(PATH_weather_data, header = TRUE)

# Create a unique identifier for each weather station, based on their coordinates
weather_data$station <- paste(weather_data$x, weather_data$y, sep ="-")

# Properly format dates, add separate columns for "year" and "month"
weather_data$date_start <- as.Date(weather_data$date_start, "%Y-%m-%d")
weather_data$year <- as.numeric(format(weather_data$date_start,'%Y'))
weather_data$month <- as.numeric(format(weather_data$date_start,'%m'))

# Processing the whole data set for GIS information would be too resource intensive, so we take out the weather stations and put locate them on the map.

# First we filter out everything else except the stations
weather_stations <- 
  weather_data %>%  
  select(station, x, y) %>% 
  group_by(station, x,y) %>% 
  summarise(station = unique(station))

avg_temp_by_station <-
  weather_data %>% 
  filter(year < 2017) %>% 
  group_by(station) %>% 
  summarise(mean_temp = mean(temp_avg)) %>% 
  rename(weather_gridpoint = station)

avg_temp_30y_by_station <-
  weather_data %>% 
  group_by(station, year) %>% 
  summarise(mean_temp = mean(temp_avg)) %>% 
  mutate(roll30y_temp = rollmean(mean_temp, 30, align="right", fill=NA)) %>% 
  filter( year < 2017) %>% 
  rename(weather_gridpoint = station)
  


# Combine

vmi12_pisteet <- read.csv("C:/Users/03180980/luke-peatland/Work/vmi12_pisteet.csv")

vmi12_lampotilat <-
  vmi12_pisteet %>% 
  rename(weather_gridpoint = HubName, dist_to_gridpoint = HubDist) %>% 
  left_join(avg_temp_by_station)


write.table(vmi12_lampotilat, file = "work/VMI12_lampotilat.csv", quote = FALSE, row.names = FALSE, col.names = TRUE)


vmi12_lampotilat_aikasarja <-
  vmi12_pisteet %>% 
  rename(weather_gridpoint = HubName, dist_to_gridpoint = HubDist) %>% 
  left_join(avg_temp_30y_by_station)


write.table(vmi12_lampotilat_aikasarja, file = "work/VMI12_lampotilat_aikasarja.csv", quote = FALSE, row.names = FALSE, col.names = TRUE)

##############


avg_stuff_all_years <-
  weather_data %>%
  filter(year < 2017, month %in% c(5:10)) %>% 
  group_by(year, station) %>%
  summarise(sum_P = sum(prec),
            avg_T = mean(temp_avg),
            ampli_T = (max(temp_avg) - min(temp_avg)) /2) %>% 
  rename(weather_gridpoint = station)



vmi12_lampotilat_kaikki <-
  vmi12_pisteet %>% 
  rename(weather_gridpoint = HubName, dist_to_gridpoint = HubDist) %>% 
  left_join(avg_stuff_all_years) %>% 
  select(lohko, koeala, weather_gridpoint, year, sum_P, avg_T, ampli_T) %>% 
  group_by(lohko, koeala, year) %>% 
  summarize(sum_P = mean(sum_P),
            avg_T = mean(avg_T),
            ampli_T = mean(ampli_T)) 

nfi12plots <- read.table(
  'work/NFI12PLOTS',
  col.names=c('region','cluster','plot','stand','centre','area','forest','drpeat','peat_type'),
  na.strings='.'
)

vmi_alat <-
  nfi12plots %>% 
  filter(forest == 1, drpeat == 1) %>% 
  select(region, cluster, plot, area, peat_type) %>% 
  rename(lohko = cluster,
         koeala = plot) %>% 
  mutate(mainregion = if_else(region %in% c(1:2), 1, 2)) %>% 
  group_by(mainregion, peat_type) %>% 
  mutate(totarea = sum(area)) %>% 
  ungroup %>% 
  left_join(vmi12_lampotilat_kaikki) %>% 
  group_by(mainregion, peat_type, year) %>% 
  summarize(sum_P = weighted.mean(sum_P, w = totarea),
            avg_T = weighted.mean(avg_T, w = totarea),
            ampli_T = weighted.mean(ampli_T, w = totarea)) %>% 
  group_by()

