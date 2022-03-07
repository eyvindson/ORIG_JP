source("PATHS.R")
source("FUNCTIONS.R")


new_ppa <- read.table(PATH_basal_area_data, header = TRUE)

old_ppa <- read.table(paste(PATH_input, "Old/basal_areas.csv", sep = ""), header = TRUE)


new_ppa$age <- "new"
old_ppa$age <- "old"

ppas <- rbind(old_ppa, new_ppa)

ggplot(data=ppas, aes(x = year, y = basal_area, col = age)) +
  geom_point() +
  geom_path() +
  ylim(0, NA) +
  facet_grid(peat_type~region) 


new_bm <- read.table(PATH_total_tree_litter, header = TRUE)
old_bm <- read.table(paste(PATH_input, "Old/total_tree_litter.csv", sep = ""), header = TRUE)

old_bm$age <- "old"

new_bm <- 
  new_bm %>% 
  group_by(region, mortality, litter_type, ground, year) %>% 
  summarize(litter_biomass = mean(litter_biomass)) %>% 
  select(region, mortality,litter_type, ground, year, litter_biomass) %>% 
  mutate(age = "new")

old_bm_crop <-
  old_bm %>% 
  filter(litter_type %ni% c("dom", "under_vegetation")) %>% 
  select(region, mortality,litter_type, ground, year, litter_biomass, age)


combo_bm <- rbind(new_bm, old_bm_crop)

combo_bm_plot <-
  combo_bm %>% 
  filter(mortality == "natural", ground == "below")
  
ggplot(data=combo_bm_plot, aes(x = year, y = litter_biomass, col = age)) +
  geom_point() +
  geom_path() +
  ylim(0, NA) +
  facet_grid(region~litter_type) 


ggplot(data=filter(new_bm, mortality == "alive", ground == "above"), aes(x = year, y = litter_biomass, col = litter_type)) +
  geom_point() +
  geom_path() +
  ylim(0, NA) +
  facet_grid(~region) 

ggplot(data=filter(old_bm, mortality == "alive", ground == "above", litter_type != "under_vegetation"), aes(x = year, y = litter_biomass, col = litter_type)) +
  geom_point() +
  geom_path() +
  ylim(0, NA) +
  facet_grid(~region) 

ggplot(data=filter(crop_newbm, year < 2017), aes(x = year, y = litter_biomass, col = litter_type)) +
  geom_point() +
  geom_path() +
  ylim(0, NA) +
  facet_grid(~region) 



areas <- FUNC_longify(lulucf_rem_kptyy_tkang_ojlk, value_name = "area")

areas_crop <- 
  areas %>% 
  filter(soil == 2, tkang != "NA")
