# ProcessWeatherData.r
#
# This script processes the weather data from getData.r for compatibility with YASSO
#
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

weather_shapefiles_list <- list.files(PATH_weather_subareas, 
                                      pattern = "*.shp", 
                                      full.names = TRUE)
# Read in the names of the shapefiles without file extension
weather_shapefiles_names <- unlist(strsplit(list.files(PATH_weather_subareas, 
                                                pattern = "*.shp", 
                                                full.names = FALSE), 
                                     ".shp"), use.names = TRUE)

# Then read aforementioned subregion files as shapefiles. NOTE! There is no check for overlap etc., so make sure the shapefiles are correct. 
# Also worth noting, that the since filenames are used in identifying the shapefiles, said filenames should be sensible and human readable.
weather_shapefiles <- lapply(weather_shapefiles_list, read_sf)
weather_shapefiles <- setNames(weather_shapefiles, weather_shapefiles_names)



#Draw a plot of the subregions used and save it
  # Combine all shapefiles, name them for labeling
  all_areas <- do.call(rbind, weather_shapefiles)
  all_areas$NAME <- weather_shapefiles_names
  # Then draw the plot and save it
  mapfig <- ggplot(all_areas) +
    geom_sf() +
    coord_sf(datum = NA) +
    geom_sf_label(aes(label = NAME)) +
    xlab("") + ylab("") +
    theme_few()
  ggsave(mapfig, filename = file.path(PATH_figures, "weather_data_area_division.png"), dpi = 120)
  

# This overly complicated bit of code takes the weather subregions, calculates the centroids for the polygons
# and finally assigns the subregions into main North/South division
  
# Here were repeat the same steps for the main North/South area division, as to the subareas previously
  weather_northsouth_list <- list.files(PATH_weather_mainareas, 
                                        pattern = "*.shp", 
                                        full.names = TRUE)
  weather_northsouth_names <- unlist(strsplit(list.files(PATH_weather_mainareas, 
                                                         pattern = "*.shp", 
                                                         full.names = FALSE), 
                                              ".shp"), use.names = TRUE)  
  weather_northsouth <- lapply(weather_northsouth_list, read_sf)
  weather_northsouth <- setNames(weather_northsouth, weather_northsouth_names)
  
  # Calculate centroids, transform them into sf geometry and assign to a dataframe
  weather_shapefile_centroids <- lapply(weather_shapefiles, st_centroid)
  weather_shapefile_centroids <- lapply(weather_shapefile_centroids, st_as_sf)
  weather_shapefile_centroids <- data.frame(
    subregion = names(weather_shapefile_centroids),
    geometry = do.call(rbind, weather_shapefile_centroids))
  # Assign the subregions to main regions. Create a lookup table for later use
  weather_areas_lookup <- within(weather_shapefile_centroids, region <- sapply(geometry, 
                                                                find_region, 
                                                                weather_northsouth))
  weather_areas_lookup$geometry <- NULL # remove unnecessary GIS data
  
  
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

# Then we transform the coordinates in the weather data for GIS compatibility. Using the CRS
# the first available shapefile. It is assumed all shapefiles share the same CRS
weather_stations <- st_as_sf(x = weather_stations,
                             coords = c('x', 'y'),
                             crs = st_crs(weather_shapefiles[[1]]))

# Assign the weather stations to the provided subregions
weather_stations <- within(weather_stations, subregion <- sapply(geometry, 
                                                              find_region, 
                                                              weather_shapefiles))

# This step is required to remove the extra GIS data from the data frame
weather_stations$geometry <- NULL
weather_stations$subregion <- as.character(weather_stations$region)

# Summarizing the data and calculating averages

result <- 
  weather_data %>% 
  # First we add the region data
  right_join(weather_stations) %>% 
  # Some stations invariably fall outside the defined shapefiles, so we filter those out.
  filter(subregion %in% weather_shapefiles_names) %>%
  #select(year, month, station, region, temp_avg) %>% 
  group_by(subregion, year, month) %>% 
  summarise(mean_T = mean(temp_avg), mean_prec = mean(prec)) %>% 
  #arrange(year, month, region, subregion) %>%
  arrange(year, month, region, subregion) %>% 
  # include the north/south division
  right_join(weather_areas_lookup)
  
# Save the output


write.table(x = result, 
            file = PATH_weather_data_aggregated, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

# Finally, we create the appropriate metadata for the file

FUNC_create_metadata(datafile = PATH_weather_data_aggregated, 
                     description = "Aggregated weather data from 1960-2019", 
                     source = "FMI / Luke DB",
                     fields = c("region", 
                                "mean_T", 
                                "mean_prec"), 
                     units_or_desc = c("Region of Finland as defined by a shapefile", 
                                       "Mean monhtly temperature in C", 
                                       "Mean monthly precipitation in mm"))