# This script calculates the total above ground litter production. It's based on linear equation by
# Ojanen and tree basal area data.

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")

# Read in the basal area data. in m2/ha
basal_area_data <- read.table(PATH_basal_area_data, header = TRUE)

# Calculate ground vegetation biomass

ground_vegetation_litter <-
  basal_area_data %>% 
  # Add in the regression constants and calculate the biomass
  right_join(CONST_total_ground_vegetation_biomass_by_peatland_type) %>% 
  mutate(biomass = (CONST_total_ground_vegetation_biomass_A * basal_area) + regression_constant) %>% 
  # Transform g/m2 dry mass to ton C/ha/y
  mutate(ground_vegetation_litter = biomass * CONST_biomass_to_C * 0.01) %>% 
  # leave out unnecessary columns
  select(region, peat_type, year, ground_vegetation_litter)

# Read in the tree litter data from ghgi
tree_litter_data <- read.table(PATH_total_tree_litter, header = TRUE) 

# Calculate the total above ground litter production
above_ground_litter <-
  tree_litter_data %>% 
  # Filter in only litter types above ground, exclude dom and under vegetation
  filter(ground == "above", litter_type %ni% c("DOM", "under_vegetation")) %>% 
  group_by(region, year, peat_type) %>% 
  summarize(above_ground_litter_total = sum(litter_biomass))

# Now calculate the total above ground litter production rate as ton C / ha / y
# by summing up the under vegetation litter and soil litter and weighing the sum by proportional areas of different peatland types

total_above_ground_litter <-
  above_ground_litter %>%
  # Include the previously calculated ground vegetation litter
  right_join(ground_vegetation_litter) %>% 
  #group_by(region, year, peat_type) %>%
  # Sum up the total soil litter and ground vegetation
  mutate(total_above_ground_litter = (ground_vegetation_litter + above_ground_litter_total))
  #select(region, year, peat_type, total_above_ground_litter)

# Save the results

write.table(x = total_above_ground_litter, 
            file = PATH_above_ground_litter_total, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

# Draw a figure. Add in peatland name for clarity.

if(PARAM_draw_plots) {

total_above_ground_litter <- right_join(total_above_ground_litter, CONST_peat_lookup)

fig <- ggplot(data=total_above_ground_litter, aes(x = year, y = total_above_ground_litter, col = peat_name)) +
  geom_point() +
  geom_path() +
  ylab("tons of C / ha / y") +
  labs(title = "Above ground litter production") + 
  ylim(0, NA) +
  facet_wrap(~region) +
  theme_bw()
ggsave(fig, filename = file.path(PATH_figures, "above_ground_litter.png"), dpi = 120)

}