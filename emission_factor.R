# This script calculates the final emission factors. Mostly just summing up results of other calculations.

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")

# Here we load in the various data sources used in calculating the total EF

peat_decomposition <- read.table(PATH_peat_decomposition, header = TRUE) # peat degradation
above_ground_litter <- read.table(PATH_above_ground_litter_total, header = TRUE) # above ground litter
below_ground_litter <- read.table(PATH_below_ground_litter_total, header = TRUE) # below ground litter

# First we sum up peat degradation and natural&logging mortality esimated with Yasso07
# peat_decomposition_total <-
#   peat_decomposition %>% 
#   right_join(lognat_mortality) %>% 
#   #mutate(total_peat_deg = peat_deg + lognat_mortality) %>% 
#   mutate(total_peat_deg = peat_deg) %>% 
#   select(-peat_deg, -lognat_mortality)
  
# Then we can calculate the emission factor per peatland type by summing up total litter production and
# subtracting peat degradation from the total. Note that 

emission_factor <-
  above_ground_litter %>% 
  right_join(below_ground_litter) %>% 
  right_join(peat_decomposition) %>% 
  mutate(emission_factor = total_above_ground_litter + total_below_ground_litter - peat_deg) %>% 
  select(region, peat_type, year, emission_factor)


# Save the result

write.table(x = emission_factor, 
            file = PATH_ef_emission_factor, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")


# We also calculate the emission factor by using summed up biomasses weighted with 
# proportional peatland areas 

# First read in the proportional peatland areas for weighing the data 

# peatland_areas <- read.table(PATH_peatland_proportional_area, header = TRUE)
# 
# # Then calculate the weighted EF
# 
# emission_factor_weighted <-
#   above_ground_litter %>% 
#   right_join(below_ground_litter) %>% 
#   right_join(peatland_areas) %>% 
#   group_by(region, year) %>% 
#   # This might look weird, but the reason is we want to subtract peat decomposition from the weighted litter sums
#   mutate(total_above_ground_litter_weighted = sum(total_above_ground_litter * proportional_area),
#          total_below_ground_litter_weighted = sum(total_below_ground_litter * proportional_area)) %>% 
#   right_join(peat_decomposition_total) %>% 
#   # Subtract the peat decomposition
#   mutate(emission_factor_weighted = total_above_ground_litter_weighted + total_below_ground_litter_weighted - total_peat_deg) %>% 
#   select(region, peat_type, year, emission_factor_weighted)


# Drawing a figure. First calculate mean emission factors for both types and then combine them

# ef <-
#   emission_factor %>% 
#   group_by(region, peat_type) %>% 
#   summarise(emission_factor = mean(emission_factor)) 
# 
# 
# fig_tot_ef <- rbind(ef, ef_weighted)

# Clue in the human readable names


# Finally draw the plot and save it

if(PARAM_draw_plots) {

fig <- ggplot(data=emission_factor, aes(x = peat_type, y = emission_factor)) +
  geom_bar(position="dodge", stat="identity") +
  ylab("tons of C / ha / y") +
  xlab("") +
  labs(title = "Mean emission factors per peatland type") + 
  facet_wrap(~region) 
ggsave(fig, filename = file.path(PATH_figures, "emission_factor.png"), dpi = 120)


emission_factor$region <- factor(emission_factor$region, levels = c("south", "north"))

time_fig <- ggplot(data=emission_factor, aes(x = year, y = emission_factor, col = as.factor(peat_type))) +
  geom_point() +
  geom_path() +
  ylab("tons of C / ha / y") +
  #ylim(0, NA) +
  facet_grid(~region) +
  theme_linedraw()
  
}