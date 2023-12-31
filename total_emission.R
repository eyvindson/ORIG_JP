# Total emission / Soil CO2 balance
#
# This script calculates the final emission factors. Mostly just summing up results of other calculations.
#
# Results are divided into total, regional and balance

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")

# Read in total area of Finland, as well as the emission factors and logging+natural mortality data

total_area <- read.table(PATH_total_area, header = TRUE) # area data
emission_factor <- read.table(PATH_ef_emission_factor, header = TRUE) # below ground litter
lognat_mortality <- read.table(PATH_ef_lognat_mortality, header = TRUE) # decomposition of logging and natural litter, from Yasso07 modelling
lognat_litter <- read.table(PATH_dead_litter, header = TRUE) # litter from logging and natural mortality
tree_litter_data <- read.table(PATH_total_tree_litter, header = TRUE) # litter data from GHGI

# Removing decomposition from natural and logging litter
lognat_litter_total <-
  lognat_litter %>% 
  right_join(lognat_mortality) %>% 
  mutate(lognat_leftover = litter - lognat_mortality) %>% 
  select(region, year, lognat_leftover)


# Calculate total emission by peatland type 

soil_carbon_balance_peattype <- 
  total_area %>% 
  right_join(emission_factor) %>% 
  mutate(total_emission = area * emission_factor) %>% 
  group_by(region, peat_type, year) %>% 
  summarize(total_emission = sum(total_emission) / 1000)

if(PARAM_draw_plots) {

fig_peattype <- ggplot(data=soil_carbon_balance_peattype, aes(x = year, y = total_emission)) +
  geom_point() +
  geom_path() +
  ylab("kt of C / y") +
  labs(title = "Soil carbon balance") +
  facet_grid(peat_type~region) 
ggsave(fig_peattype, filename = file.path(PATH_figures, "total_emission_peattype.png"), dpi = 120)

}

# Save results
write.table(x = soil_carbon_balance_peattype, 
            file = PATH_total_soil_carbon_by_peattype, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

# Calculate total logging and natural litter leftover

total_lognat <- 
  total_area %>% 
  group_by(region, year) %>% 
  summarize(area = sum(area)) %>% 
  right_join(lognat_litter_total) %>% 
  mutate(lognat_absolute = lognat_leftover * area / 1000) %>% 
  select(region, year, lognat_absolute)

# Calculate total carbon balance with South/North division

soil_carbon_balance_southnorth <- 
  total_area %>% 
  right_join(emission_factor) %>% 
  # convert to Kt C
  mutate(total_emission = area * emission_factor / 1000) %>% 
  group_by(region, year) %>% 
  summarize(total_emission = sum(total_emission)) %>% 
  right_join(total_lognat) %>% 
  mutate(final_emission = total_emission + lognat_absolute) %>% 
  # Convert into Mt CO2
  mutate(total_CO2 = total_emission * -CONST_C_to_CO2 / 1000,
         lognat_CO2 = lognat_absolute * -CONST_C_to_CO2 / 1000,
         final_CO2 = final_emission * -CONST_C_to_CO2 / 1000)

if(PARAM_debug) {
  
  total_emission_save <-
    soil_carbon_balance_southnorth %>% 
    select(year, region, total_CO2, lognat_CO2, final_CO2) %>% 
    pivot_longer(cols = total_CO2:final_CO2,
                 names_to = "param",
                 values_to = "value") %>% 
    pivot_wider(names_from = region, 
                values_from = value) %>% 
    mutate(total = north + south) %>% 
    pivot_longer(cols = north:total,
                 names_to = "region", 
                 values_to = "value") %>% 
    mutate(scenario = PARAM_scenario) %>% 
    write.table(file = paste(PARAM_scenario, "total.csv", sep = ""), 
                quote = FALSE, sep = ";", 
                row.names = FALSE)
  
}

# Save the results
write.table(x = soil_carbon_balance_southnorth, 
            file = PATH_total_soil_carbon, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

# Calculate total carbon balance

soil_carbon_balance_total <- 
  soil_carbon_balance_southnorth %>% 
  group_by(year) %>% 
  summarize(final_emission = sum(final_emission)) %>% 
  mutate(final_CO2 = final_emission * -CONST_C_to_CO2 / 1000)

# Save results
write.table(x = soil_carbon_balance_total, 
            file = PATH_total_soil_carbon_total, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

if(PARAM_draw_plots) {
  
fig <- ggplot(data=soil_carbon_balance_southnorth, aes(x = year, y = total_emission)) +
  geom_point(aes(col = "EF only")) +
  geom_path(aes(col = "EF only")) +
  geom_point(aes(x = year, y = lognat_absolute, col = "Lognat tot")) +
  geom_path(aes(x = year, y = lognat_absolute, col = "Lognat tot")) +
  geom_point(aes(x = year, y = final_emission, col = "Final emission")) +
  geom_path(aes(x = year, y = final_emission, col = "Final emission")) +
  ylab("kt of C / y") +
  labs(title = "Soil carbon balance") +
  facet_wrap(~region) +
  theme_bw()
ggsave(fig, filename = file.path(PATH_figures, "total_emission_southnorth.png"), dpi = 120)


fig_tot <- 
  ggplot(soil_carbon_balance_total, aes(x = year, y = final_emission)) + 
  geom_point() +
  geom_path() +
  ylab("kt of C / y") +
  labs(title = "Total Soil carbon balance") +
  theme_bw() 
  ggsave(fig_tot, filename = file.path(PATH_figures, "total_emission_total.png"), dpi = 120)
}