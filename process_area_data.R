# Process area data #
# 
# This script reads area data from the inventory and calculates the appropriate areas for calculation

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
# Process area data

# Load area data, convert to "long"

all_areas <- read.table(PATH_ghgi_area, header = TRUE)
areas_long <- FUNC_longify(all_areas, value_name = "area")

# Lookup table for regions for converting NFI areas to South/North Finland 1,2 = south, 3,4, = north
regsum <- data.frame(region = c(1,2,3,4),
                      mainreg = rep(c("south", "north"), each = 2))

# Filter and aggregate the data
areas_aggregated <-
  areas_long %>%
  filter(soil == 2, tkang %in% c(1:7), year < 2017) %>%
  right_join(regsum) %>%
  # Combine forest peatland types 2&3 and 4&4
  mutate(tkang = ifelse(tkang == 3, 2, tkang),
         tkang = ifelse(tkang == 5, 4, tkang)) %>%
  group_by(mainreg, tkang, year) %>%
  summarize(area = sum(area)) %>%
  rename(region = mainreg, peat_type = tkang) %>%
  filter(!is.na(peat_type))

# Save the result

write.table(x = areas_aggregated, 
            file = PATH_total_area, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")
