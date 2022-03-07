CO2 <- 44/12

peat_decomposition <- read.table(PATH_peat_decomposition, header = TRUE) # peat degradation


total_litter <-
  above_ground_litter %>% 
  right_join(below_ground_litter) %>% 
  right_join(peat_decomposition) %>% 
  filter(year < 2017) %>% 
  select(-total_above_ground_litter, -total_below_ground_litter, -peat_name) %>% 
  rename(tree_litter = above_ground_litter_total, 
         fine_woody_litter = litter_biomass,
         fine_root_litter = fine_root_litter_production) %>% 
  mutate(tree_litter = tree_litter * -CO2,
         ground_vegetation_litter = ground_vegetation_litter * -CO2,
         fine_root_litter = fine_root_litter * -CO2,
         fine_woody_litter = fine_woody_litter * -CO2,
         peat_deg = peat_deg * CO2)
# MUUTA HIILIDIOKSIDI

total_net <-
  total_litter %>% 
  mutate(net = tree_litter + ground_vegetation_litter + fine_root_litter + fine_woody_litter + peat_deg) %>% 
  right_join(CONST_peat_lookup) %>% 
  select(region, year, peat_name, net)




total_net <- FUNC_regionify(total_net, peatnaming = TRUE)

total_litter_cats <- melt(total_litter, 
                          id.vars = colnames(total_litter[1:3]), 
                          variable.name = "category", value.name = "litter")


total_litter_cats <- right_join(total_litter_cats, CONST_peat_lookup)

total_litter_cats <- FUNC_regionify(total_litter_cats, peatnaming = TRUE, revreg = TRUE)

total_litter_cats$category <- factor(total_litter_cats$category, 
                                     levels = c("tree_litter",
                                                "ground_vegetation_litter", 
                                                "fine_root_litter", 
                                                "fine_woody_litter",
                                                "peat_deg"),
                                     labels = c("Aboveground tree litter",
                                                "Ground layer vegetation litter", 
                                                "Arboreal fine root litter", 
                                                "Tree thick root litter",
                                                "Decomposition"))

ggplot(data=total_litter_cats, aes(x = year, y = litter)) +
  geom_area(aes(fill = category)) +
  ylab(bquote("Decomposition and litter production (t "~CO[2]~ ~ha^-1~ ~y^-1~")")) + 
  xlab("Year") +
  geom_path(data = total_net, aes(x=year, y=net), linetype = "longdash") +
  facet_grid(region ~ peat_name) +
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  labs(fill="") 