rm(list=ls())

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



theme_Publication <- function(base_size=12) {
 # library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            #panel.border = element_rect(colour = NA),
            #axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            legend.key = element_rect(colour = NA),
            #legend.position = "bottom",
            #legend.direction = "horizontal",
           # legend.key.size= unit(0.01, "cm"),
            #legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            #strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.background=element_blank(),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


#######################################

# FIGURE 2 WEATHER

weather_data <- read.csv("C:/Users/03180980/luke-peatland/Input/Weather/weather_data_by_peattype.csv", sep="")
weather_data <- weather_data %>% right_join(CONST_peat_lookup)
weather_data <- FUNC_regionify(weather_data, peatnaming = TRUE)

figure2 <-  ggplot(data=weather_data, aes(x = year, y = roll_T, col = peat_name, shape = peat_name)) +
  geom_point() +
  geom_path() +
  ylab("Air temperature (°C)") + 
  xlab("") +
  facet_wrap(~region) +
  #ylim(0, NA) +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme_Publication() +
  labs(color='Peatland forest type', shape = "Peatland forest type") 

figure2

ggsave(figure2, 
       filename = file.path(PATH_pubfigures, "figure2.png"), 
       dpi = 300,
       width = 8,
       height = 4)

#######################################

# FIGURE 3 Basal areas


basal_areas <- read.table(PATH_basal_area_data, header = TRUE)

basal_areas <- basal_areas %>% right_join(CONST_peat_lookup)
basal_areas <- FUNC_regionify(basal_areas, peat_percentage = TRUE)


figure3 <- ggplot(data=basal_areas, aes(x = year, y = basal_area, col = peat_name, shape = peat_name)) +
  geom_point() +
  geom_path() +
  ylab(bquote("Basal area ("~m^2~ ~ha^-1~")")) + 
  xlab("") +
  facet_wrap(~region) +
  #ylim(0, NA) +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme_Publication() +
  labs(color='Peatland forest type', shape = "Peatland forest type") 

ggsave(figure3, filename = file.path(PATH_pubfigures, "figure3.png"), 
       dpi = 300,
       width = 8,
       height = 4)

figure3

#######################################

# FIGURE 4 TOTALS

soil_carbon_balance_total <- read.table(PATH_total_soil_carbon_total, header = TRUE)
soil_carbon_balance_southnorth <- read.table(PATH_total_soil_carbon, header = TRUE)
GHGI_co2 <- read.delim(paste(PATH_input, "GHGI_co2.txt", sep = ""))
GHGI_co2$Method = "GHGI method"
GHGI_co2 <- rename(GHGI_co2, final_CO2 = ghgi_co2)

soil_comp <- 
  soil_carbon_balance_total %>% 
  select(year, final_CO2) %>% 
  mutate(Method = "new") %>% 
  rbind(., GHGI_co2)

fig_tot_CO2 <- ggplot(data=soil_comp, aes(x = year, y = final_CO2, col = Method)) +
  geom_point() +
  geom_path() +
  ylab(bquote("Soil" ~CO[2]~ "balance (Mt "~CO[2]~")")) +
  xlab("") +
  labs(title = "Whole country") +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme_Publication() +
  theme(legend.position = "none")

new_co2_reg <-
  soil_carbon_balance_southnorth %>% 
  select(region, year, final_CO2) %>% 
  rename(CO2 = final_CO2) %>% 
  mutate(method = "New method")


new_co2_reg <- FUNC_regionify(new_co2_reg, revreg = TRUE)

GHGI_co2_reg <- read.delim(paste(PATH_input, "GHGI_co2_reg.txt", sep = ""))
GHGI_co2_reg$method <- "GHGI method"
GHGI_co2_reg <- FUNC_regionify(GHGI_co2_reg, revreg = FALSE)
co2_reg <- rbind(new_co2_reg, GHGI_co2_reg)


fig_tot_CO2_reg <- ggplot(data=co2_reg, aes(x = year, y = CO2, col = method)) +
  geom_point() +
  geom_path() +
  ylab("") +
  xlab("") +
  facet_grid(region~.) +
  scale_fill_Publication() +
  scale_colour_Publication() +
  labs(title = "Regions") +
  theme_Publication() +
  labs(color="") 

figure4 <- ggarrange(fig_tot_CO2, fig_tot_CO2_reg)
ggsave(figure4, 
       filename = file.path(PATH_pubfigures, "figure4.png"), 
       dpi = 300, 
       width = 8,
       height = 4)

figure4
 

#######################################

# Figure 5

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

figure5 <- ggplot(data=filter(megafig_reg, year < 2017), aes(x = year, y = value, fill = component)) +
  geom_area() +
  geom_path(data=filter(nettot_rig, year < 2017), aes(x = year, y = net), linetype = "longdash") +
  xlab("Year") +
  ylab(bquote("Decomposition and litter production (t "~CO[2]~ ~ha^-1~ ~y^-1~")")) + 
  facet_grid(region~method) +
  labs(fill="") +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme_Publication() +
  theme(legend.position = "bottom")



ggsave(figure5, 
       filename = file.path(PATH_pubfigures, "figure5.png"), 
       dpi = 300,
       width = 8, 
       height = 6)


figure5

#######################################


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

figure6 <- ggplot(data=lognat_plot, aes(x = year, y = litter, fill = component)) +
  geom_area() +
  geom_path(data = lognat_net, aes(x = year, y = lognet),linetype = "longdash") +
  xlab("") +
  ylab(bquote("Decomposition and litter input (t "~CO[2]~ ~ha^-1~ ~y^-1~")")) + 
  facet_grid(~region) +
  labs(color="Component", shape = "Component") +
  labs(fill="") +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme_Publication() +
  theme(legend.position = "bottom")


ggsave(figure6, 
       filename = file.path(PATH_pubfigures, "figure6.png"), 
       dpi = 300, 
       width = 8,
       height = 6)


figure6

#######################################


### Figure 7


CO2 <- 44/12

peat_decomposition <- read.table(PATH_peat_decomposition, header = TRUE) # peat degradation
#total_above_ground_litter <- read.table(PATH_above_ground_litter_total, header = TRUE) # above ground litter
#total_below_ground_litter <- read.table(PATH_below_ground_litter_total, header = TRUE) # below ground litter
#total_tree_litter <- read.csv("C:/Users/03180980/luke-peatland/Input/total_tree_litter.csv", sep="")

total_litter <-
  above_ground_litter %>% 
  right_join(below_ground_litter) %>% 
  right_join(peat_decomposition) %>% 
  right_join(CONST_peat_lookup) %>% 
  filter(year < 2017) %>% 
  select(-total_below_ground_litter, -peat_name) %>% 
  rename(tree_litter = total_above_ground_litter, 
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


total_litter_cats <- 
  total_litter_cats %>% 
  filter(category != "above_ground_litter_total") %>% 
  right_join(CONST_peat_lookup)

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

figure7 <- ggplot(data=total_litter_cats, aes(x = year, y = litter)) +
  geom_area(aes(fill = category)) +
  ylab(bquote("Decomposition and litter production (t "~CO[2]~ ~ha^-1~ ~y^-1~")")) + 
  xlab("Year") +
  geom_path(data = total_net, aes(x=year, y=net), linetype = "longdash") +
  facet_grid(region ~ peat_name) +
  labs(fill="") +
  scale_fill_Publication() +
  scale_colour_Publication() +
  theme_Publication() +
  theme(legend.position = "bottom")

ggsave(figure7, 
       filename = file.path(PATH_pubfigures, "figure7.png"), 
       dpi = 300,
       width = 8,
       height = 4, 
       scale = 1.5)


figure7





