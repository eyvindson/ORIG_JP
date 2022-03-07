# This script calculates the total below ground litter production, as a function of fine root biomass, 
# and different categories of litter. The final production amount is weighted by the proportional area of different
# peatland types.

# The biomass of fine roots based on the basal area of different tree types, based on linear equations from Ojanen et al. 2014

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")

# Read the input data. Tree basal areas and average dwarf shrub coverages for different peatland types.

basal_area_by_treetype <- read.table(PATH_below_ground_litter_basal_data, header = TRUE)

# The actual calculation that yields us fine root biomass in tons of biomass / ha
fine_root_biomass_regression <-
  basal_area_by_treetype %>% 
  # Add in the regression constants for calculating the biomass
  right_join(CONST_fine_root_biomass_by_treetype) %>% 
  # Then calculate the biomass for each tree type using the regression constants provided
  mutate(biomass = basal_area * regression_constant) %>% 
  # Group and sump up all tree types together 
  group_by(region, peat_type, year) %>% 
  summarize(biomass = sum(biomass)) %>% 
  # Add in dwarf shrub coverage by peatland type, along with regional constants needed in the calculation
  right_join(CONST_dwarfshrub_coverage) %>% 
  right_join(CONST_fine_root_biomass_region) %>% 
  # Calculate the final biomass, by adding the contribution from dwarf shrubs and adjust with the regional modifiers. Convert to tons BM / ha (0.01)
  mutate(fine_root_biomass = (biomass + (dshrub_cover * CONST_dwarfshrub_root_biomass) + regional_modifier) * 0.01) %>% 
  # Drop out unnecessary variables
  select(region, peat_type, year, fine_root_biomass)

# Final step is calculating fine root litter production based on fine root turnover rate, adjusted for deep roots missing from the original data
# and assuming 50% of dry BM is carbon
fine_root_litter_production <-
  fine_root_biomass_regression %>% 
  # Add in the turnover rates for different peatland types
  right_join(CONST_fine_root_turnover) %>% 
  mutate(fine_root_litter_production = fine_root_biomass * CONST_fine_root_deep_fraction * fine_root_turnover * CONST_biomass_to_C) %>% 
  # drop out all the unnecessary variables
  select(region, peat_type, year, fine_root_litter_production)
  
# Moving on. Combining the the fine root litter production to other litter types.

# Read in the woody litter data from GHG inventory
total_tree_litter <- read.table(PATH_total_tree_litter, header = TRUE) 
# Select only underground litter and sum up the total amount
tree_litter_sum  <-
  total_tree_litter %>% 
  filter(ground == "below", mortality == "alive") %>%
  group_by(region, year, peat_type) %>% 
  summarize(litter_biomass = sum(litter_biomass)) %>% 
  ungroup() 

# Calculate the total amount of litter per peatland forest type combining the calculated fine root litter production with woody litter 
total_below_ground_litter <- 
  fine_root_litter_production %>% 
  # add in the the litter sums from NFI
  right_join(tree_litter_sum) %>% 
  # Calculate total litter amount
  mutate(total_below_ground_litter = fine_root_litter_production + litter_biomass) 
  #drop out unneeded columns
  #select(-fine_root_litter_production, -litter_biomass)


# Save the results

write.table(x = total_below_ground_litter, 
            file = PATH_below_ground_litter_total, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

# Draw the figure

total_below_ground_litter <- right_join(total_below_ground_litter, CONST_peat_lookup)

fig <- ggplot(data=total_below_ground_litter, aes(x = year, y = total_below_ground_litter, col = peat_name)) +
  geom_point() +
  geom_path() +
  ylab("tons of C / ha / y") +
  labs(title = "Below ground litter production") + 
  ylim(0, NA) +
  facet_grid(~region) +
  theme_bw()
ggsave(fig, filename = file.path(PATH_figures, "below_ground_litter.png"), dpi = 120)

  