source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")



peat_decomposition <- read.table(PATH_peat_decomposition, header = TRUE) # peat degradation
basal_area_data <- read.table(PATH_basal_area_data, header = TRUE)
above_ground_litter <- read.table(PATH_above_ground_litter_total, header = TRUE) # above ground litter
below_ground_litter <- read.table(PATH_below_ground_litter_total, header = TRUE) # below ground litter
lognat_mortality <- read.table(PATH_ef_lognat_mortality, header = TRUE) # logging and natural mortality
tree_litter_data <- read.table(PATH_total_tree_litter, header = TRUE) 
lognat_litter <- read.table("work/dead_litter_2.csv", header = TRUE)
peatland_areas <- read.table(PATH_peatland_proportional_area, header = TRUE)
GHGI_litter <- read.delim("work/GHGI_litter.txt")

#First we sum up peat degradation and natural&logging mortality esimated with Yasso07
peat_decomp_new <-
  peat_decomposition %>%
  right_join(lognat_mortality) %>%
  mutate(total_peat_deg = peat_deg + lognat_mortality) %>%
  select(-peat_deg, -lognat_mortality) %>% 
  right_join(peatland_areas) %>%
  mutate(peat_deg = total_peat_deg * proportional_area) %>%
  group_by(region, year) %>%
  summarise(peat_deg = sum(peat_deg * 44/12)) %>% 
  rename(value = peat_deg) %>% 
  mutate(component = "Decomposition", 
         method = "New method")

peat_decomp_old <- data.frame(region = rep(c("south", "north"), each = 27),
                              year = rep(seq(1990,2016,1),  2), 
                              value = rep(c(10.50993219, 10.28455715), each = 27), 
                              component = "Decomposition",
                              method = "GHGI method")
litter_old <-
  GHGI_litter %>% 
  filter(ground == "below") %>% 
  select(-ground) %>% 
  mutate(litter_production = litter_production * -(44/12)) %>% 
  rename(value = litter_production) %>% 
  select(region, year, value) %>% 
  mutate(component = "Belowground litter",
         method = "GHGI method") 

lognat_new_above <-
  lognat_litter %>% 
  # Filter in only litter types above ground, exclude dom and under vegetation
  filter(ground == "above") %>% 
  group_by(region, year) %>% 
  summarize(litter = sum(litter)) %>% 
  rename(lognat = litter)


above_melt <- melt(above_ground_litter, 
                          id.vars=colnames(above_ground_litter[1:3]), 
                          variable.name = "component", value.name = "litter")

litter_new_above <-
  above_melt %>% 
  right_join(peatland_areas) %>%
  mutate(litter = litter * proportional_area) %>%
  group_by(region, year, component) %>%
  summarise(litter = sum(litter)) %>% 
  right_join(lognat_new_above) %>% 
  mutate(value = (litter + lognat) * -(44/12)) %>% 
  mutate(method = "New method") %>% 
  select(region, year, component, value, method)


litter_new_above_tot <-
  litter_new_above %>% 
  filter(component == "total_above_ground_litter")

litter_new_above <-
  litter_new_above %>% 
  filter(component != "total_above_ground_litter") %>% 
  mutate(component = if_else(component == "above_ground_litter_total", "Aboveground tree litter", "Ground layer litter"))


lognat_new_below <-
  lognat_litter %>% 
  # Filter in only litter types above ground, exclude dom and under vegetation
  filter(ground == "below") %>% 
  group_by(region, year) %>% 
  summarize(litter = sum(litter))


litter_new_below <-
  below_ground_litter %>% 
  select(region, year, peat_type, total_below_ground_litter) %>% 
  right_join(peatland_areas) %>%
  mutate(total_below_ground_litter = total_below_ground_litter * proportional_area) %>%
  group_by(region, year) %>%
  summarise(total_below_ground_litter = sum(total_below_ground_litter)) %>% 
  right_join(lognat_new_below) %>% 
  mutate(total_below_ground_litter = (total_below_ground_litter + litter) * -(44/12)) %>% 
  select(-litter) %>% 
  rename(value = total_below_ground_litter) %>% 
  mutate(component = "Belowground litter",
         method = "New method") 

old_net <-
  peat_decomp_old %>% 
  rename(peat_decomp = value) %>% 
  select(-component) %>% 
  right_join(litter_old) %>% 
  rename(litter = value) %>% 
  mutate(net = peat_decomp + litter) %>% 
  select(region, year, method, net)

new_net <-
  peat_decomp_new %>% 
  rename(peat_decomp = value) %>% 
  select(-component) %>% 
  right_join(litter_new_above_tot) %>% 
  select(-component) %>% 
  rename(litter_above = value) %>% 
  right_join(litter_new_below) %>% 
  rename(litter_below = value) %>% 
  mutate(net = peat_decomp + litter_above + litter_below) %>% 
  select(region, year, method, net) %>% 
  filter(year < 2017)

nettot <- rbind(old_net, new_net)

nettot_rig <- FUNC_regionify(nettot)
nettot_rig$component <- NA

megafig <- rbind(peat_decomp_old, peat_decomp_new, litter_old, litter_new_above, litter_new_below)

megafig_reg <- FUNC_regionify(megafig, revreg = TRUE)

ggplot(data=filter(megafig_reg, year < 2017), aes(x = year, y = value, fill = component)) +
  geom_area() +
  geom_path(data=filter(nettot_rig, year < 2017), aes(x = year, y = net), linetype = "longdash") +
  xlab("Year") +
  ylab(bquote("Decomposition and litter production (t "~CO[2]~ ~ha~ ~y^-1~")")) + 
  facet_grid(region~method) +
  theme_bw() +
  labs(fill="") +
  theme(strip.background = element_rect(fill="white")) 



#### Kuvaaja 6

lognat_area_above <- filter(lognat_new_above, year < 2017)
lognat_area_below <- filter(lognat_new_below, year < 2017)
lognat_area_decomp <- filter(lognat_mortality, year < 2017)

lognat_area_above$component <- "Aboveground litter"
lognat_area_below$component <- "Belowground litter"
lognat_area_decomp$component <- "Decomposition"

lognat_area_above <- rename(lognat_area_above, litter = lognat)
lognat_area_decomp <- rename(lognat_area_decomp, litter = lognat_mortality)

lognat_area_above$litter <- lognat_area_above$litter * -(44/12)
lognat_area_below$litter <- lognat_area_below$litter * -(44/12)
lognat_area_decomp$litter <- lognat_area_decomp$litter * (44/12)


lognat_plot <- rbind(lognat_area_above, lognat_area_below, lognat_area_decomp)
lognat_plot <- FUNC_regionify(lognat_plot)

# Calculate net

lognat_net <-
  lognat_area_above %>% 
  select(-component) %>% 
  rename(abv = litter) %>% 
  right_join(lognat_area_below) %>% 
  select(-component) %>% 
  rename(bel = litter) %>%
  right_join(lognat_area_decomp) %>% 
  select(-component) %>% 
  mutate(lognet = abv+bel+litter) %>% 
  select(region, year, lognet) %>% 
  mutate(component = NA)

lognat_net <- FUNC_regionify(lognat_net)

ggplot(data=lognat_plot, aes(x = year, y = litter, fill = component)) +
  geom_area() +
  geom_path(data = lognat_net, aes(x = year, y = lognet),linetype = "longdash") +
  xlab("Year") +
  ylab(bquote("Decomposition and litter input (t "~CO[2]~ ~ha^-1~ ~y^-1~")")) + 
  facet_grid(~region) +
  theme_bw() +
  labs(color="Component", shape = "Component") +
  labs(fill="") +
  theme(strip.background =element_rect(fill="white")) 

