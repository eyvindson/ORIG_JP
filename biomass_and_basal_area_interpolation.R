# This script interpolates the basal area and biomass data to cover annual observations using simple linear interploation.
# Since NFI starting years vary, some backwards extrapolation is needed, in order to cover the early years
# Based on NFI data, provided by Helena Henttonen

rm(list=ls())
source("PATHS.R")
source("CONSTANTS.R")


ba_bm_values <- read.csv2(PATH_ba_bm_raw_data, dec = ".")
# Remove biom8 and biom9, since they are just sums
ba_bm_values$biom8 <- ba_bm_values$biom9 <- NULL

# Let's take a raw data copy, for later comparison
raw_data <- 
  ba_bm_values %>% 
  filter(fra == 1) %>% 
  # We need to combine peat types 2&3 ja 4&5 and use weighted mean since their areas are disproportional
  left_join(CONST_peatland_weights) %>% 
  mutate(tkg = ifelse(tkg == 3, 2, tkg)) %>% 
  mutate(tkg = ifelse(tkg == 5, 4, tkg)) %>% 
  pivot_longer(cols = keskippa:biom7,
               names_to = "component", values_to = "value") %>% 
  group_by(vmi, muoto11, tkg, laji1, component) %>% 
  summarize(value = weighted.mean(value, area_weight),
            ala = weighted.mean(ala, area_weight),
            keskivuosi = weighted.mean(keskivuosi, area_weight)) %>% 
  ungroup() %>% 
  # Some NFI counts are missing values, fill those in using values from the previous and next NFIs
  # Here we make sure all combinations of variables are present
  group_by(vmi, muoto11, laji1, component) %>% 
  complete(tkg = c(1,2,4,6,7)) %>% 
  ungroup() %>% 
  # Here we include the missing values 
  group_by(muoto11, tkg, laji1, component) %>% 
  mutate(value = na.approx(value),
         keskivuosi = na.approx(keskivuosi), 
         ala = na.approx(ala)) %>% 
  ungroup()

# Here we combine NFI areas 1&2 <- south finland, 3&4 <- north
# We use peatland areas as weights for calculating means, in order to 
# decrease the disproportionally large effect small peatland area types may have
# (e.g. 7 million ha of peatland 1 in NFI region 1, 1 ha in region 2. Simple mean of 
# any values from these two would definitely be representative)
NFI_area_combine <- 
  raw_data %>% 
  # Combining regions
  mutate(region = if_else(muoto11 %in% c(1:2), 1, 2)) %>% 
  rename(laji = laji1) %>% 
  group_by(vmi, region, tkg, laji, component) %>% 
  # Here use the peatland areas as weights
  summarize(year = round(mean(keskivuosi)),
            value = weighted.mean(value, ala)) %>% 
  ungroup()

# Starting the interpolation. First we need to complete the time series as it exists
# per each group. Then we have to include the missing early years to Northern Finland

complete_timeseries <-
  NFI_area_combine %>% 
  select(-vmi) %>% 
  # First complete the time series. All interim years are filled at this point.
  group_by(region, tkg, laji, component) %>% 
  complete(year = min(year):max(year)) %>% 
  mutate(value = na.approx(value)) %>%
  # Include the missing early years. Note that we have to reference the previous
  # dataframe because we are handling grouped data here and all groups need to have the same 
  # set of years present
  complete(year = min(NFI_area_combine$year):max(NFI_area_combine$year)) %>% 
  ungroup()

# Here we create a linear model for each group. We use only the 1990s years for the fit.
interpolation_model <-
  complete_timeseries %>% 
  filter(year < 2000) %>% 
  group_by(region, tkg, laji, component) %>% 
  do(model= lm(value ~ year, data = ., na.action = "na.exclude")) %>%
  ungroup()

# Here we extrapolate the values for the early years. Forward extrapolation
# is not done, instead the last known value is repeated.

full_results <-
  complete_timeseries %>% 
  left_join(interpolation_model) %>% 
  # Apply the model for each group
  group_by(region, tkg, laji, component) %>% 
  do(modelr::add_predictions(., first(.$model))) %>% 
  # Replace NAs with predicted values
  mutate(value = if_else(is.na(value), pred, value)) %>% 
  select(-pred, -model) %>% 
  # Filter out the early years, set less than zero valus to zero
  filter(year > 1989) %>% 
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Finally we add the repeating final years
  complete(year = 1990:max(year) + CONST_forward_years) %>% 
  mutate(value = if_else(year > max(year) - CONST_forward_years, 
                         value[which(year == max(year) - CONST_forward_years)], 
                         value)) %>% 
  arrange(year) %>% 
  ungroup()

# Finally, save the results separately for basal areas and biomasses

basal_area_save <-
  full_results %>% 
  filter(component == "keskippa") %>% 
  select(-component) %>% 
  rename(basal_area = value,
         peat_type = tkg) %>% 
  mutate(region = if_else(region == 1, "south", "north")) %>% 
  left_join(CONST_species_lookup) %>% 
  select(region, peat_type, tree_type, year, basal_area) 

  write.table(basal_area_save,
              file = PATH_below_ground_litter_basal_data,
              row.names = FALSE, 
              quote = FALSE, 
              col.names = TRUE, 
              sep =" ")

basal_area_aggregated <-
  basal_area_save %>% 
  group_by(region, peat_type, year) %>% 
  summarize(basal_area = sum(basal_area)) %>% 
  write.table(file = PATH_basal_area_data, 
              row.names = FALSE, 
              quote = FALSE, 
              col.names = TRUE, 
              sep =" ")

# Biomass

biomass_save <-
  basal_area_save <-
  full_results %>% 
  filter(component != "keskippa") %>% 
  rename(species = laji,
         bm = value) %>% 
  mutate(component = as.numeric(sub("biom", "", component))) %>% 
  write.table(file = PATH_interpolated_biomass,
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

  