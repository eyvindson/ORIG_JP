# This script deals with the decomposition of peat
# It requires weather and basal area data 
# Based on 

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")

# Here we input the tree basal area and weather data

basal_area_data <- read.table(PATH_basal_area_data, header = TRUE)
weather_data_aggregated <- read.table(PATH_weather_data_aggregated, header = TRUE)

# Start by calculating a 30 year rolling average for mean temperature in May-Nov 1990->

 weather_data_30rollavg <- read.csv("C:/Users/03180980/luke-peatland/Input/Weather/weather_data_by_peattype.csv", sep="")

weathers <- 
  weather_data_aggregated %>%
  filter(month > 4 & month < 11) %>%
  group_by(region, year) %>%
  summarise(mean_T = mean(mean_T)) %>%
  mutate(roll_T = rollmean(mean_T, 30, align="right", fill=NA))
  


# Here is the actual for peat degradation. Linear equations 

peat_decomposition <-
  basal_area_data %>% 
  # First add in the weather data
  right_join(weather_data_30rollavg) %>% 
  # Then add the constants used in calculating the decomposition of peat
  right_join(CONST_peat_decomposition_by_peatland_type) %>% 
  # Finally calculate degradation using the constants provided separately. 
  # The equation used here is (a * [basal_area] + b * [t]) - c
  mutate(peat_deg = ((CONST_peat_decomposition_a * basal_area + CONST_peat_decomposition_b * roll_T)) - decomposition_constant) %>% 
  # Convert g CO2 / m2 to ton C / ha, note 10^4 * 10^-6 = 0.01
  mutate(peat_deg = peat_deg / (44/12) * 0.01) %>% 
  # Select only the end result for saving
  select(region, year, peat_type, peat_deg)


# Save data and metadata

write.table(x = peat_decomposition, 
            file = PATH_peat_decomposition, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

# Finally, we create the appropriate metadata for the file



FUNC_create_metadata(datafile = PATH_peat_decomposition, 
                     description = "Calculated peat decomposition type divided by region and peatland type", 
                     source = "Calculated based on basal area data and weather data",
                     fields = c("region", 
                                "year",
                                "peat_type", 
                                "peat_name", 
                                "peat_deg"), 
                     units_or_desc = c("Region of Finland", 
                                       "Year",
                                       "Peatland type coded numerically",
                                       "Peatland type code, see documentation for further information",
                                       "Degradation of peat, in tons of C per ha per year"))


# Draw a plot

peat_decomposition <- right_join(peat_decomposition, CONST_peat_lookup)
peat_decomposition <- FUNC_regionify(peat_decomposition, peatnaming = TRUE)

fig <- ggplot(data=peat_decomposition, aes(x = year, y = peat_deg, col = peat_name, shape = peat_name)) +
  geom_point() +
  geom_path() +
  ylab("Tons of C / ha / y") +
  xlab("Year") +
  facet_wrap(~region) +
  ylim(0, NA) +
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  labs(color='Peatland forest type', shape = "Peatland forest type") 
ggsave(fig, filename = file.path(PATH_figures, "peat_decomposition.png"), dpi = 120)

