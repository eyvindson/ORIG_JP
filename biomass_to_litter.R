# This script converts biomass fractions into litter fractions

rm(list=ls())

source("PATHS.R")
source("FUNCTIONS.R")
source("CONSTANTS.R")

#BM_interp_long <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/BM_interp_long.txt", sep
BM_interp_long <- read.csv(PATH_interpolated_biomass, header = TRUE, sep = " ")
LOOKUP_litter_conversion <- read.table(PATH_lookup_litter, header = TRUE)
LOOKUP_awentype <- read.table(PATH_lookup_awentype, header = TRUE)
LOOKUP_awen_params <- read.table(PATH_lookup_awenparams, header = TRUE)

#### TABLE OF BIOMASS #####

bm_conv <- LOOKUP_litter_conversion[1:7, 3:4]

bm_conv <- data.frame(component = 1:7,
                      bmtype = c("Stemwood",
                                 "Bark",
                                 "Live branches",
                                 "Foliage",
                                 "Dead brances",
                                 "Stumps",
                                 "Roots"))

# Select only the values we need

# biomass_crop <- 
#   BM_interp_long %>% 
#   group_by(region, tkg, component, year) %>% 
#   summarize(bm = sum(bm)) %>% 
#   ungroup() %>% 
#   rename(peat_type = tkg) %>% 
#   mutate(region = ifelse(region == 1, "south", "north")) %>% 
#   right_join(CONST_peat_lookup) %>% 
#   right_join(bm_conv) %>% 
#   filter(year %in% c(1990, 2000, 2010, 2016)) %>% 
#   select(-peat_type, -component)


# biomass_table <- FUNC_regionify(biomass_crop, peatnaming = TRUE)

# #write.table(bm_table_wide, "wide_bm.csv", quote = FALSE, dec = ",", sep = ";")
# 
# ggplot(biomass_table, aes(x = year, y = bm, group = region)) +
#   geom_col(aes(fill= region), position = "dodge") +
#   geom_text(aes(
#             label = round(bm,1)), 
#             size = 3,
#             vjust = -0.3,
#             # hjust = 0.2,
#             color="black",
#             position = position_dodge(6.5)) +  
#   facet_grid(bmtype~peat_name) +
#   labs(fill="") +
#   ylim(0,max(biomass_table$bm+5)) +
#   xlab("Year") +
#   ylab(bquote("Biomass (t kg "~ ha^-2~ ")")) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill="white"),
#         legend.position = "bottom") 



################################################################################# 

# Calculate litter production

litter_types <-
  BM_interp_long %>%
  # Leave out total biomasses (categories 8 & 9)
  filter(component < 8) %>% 
  left_join(LOOKUP_litter_conversion) %>%
  # Calculate litter production from biomass, convert to C
  mutate(litter = bm * bm_turnover_constant * CONST_biomass_to_C) %>%
  select(-bm, -bm_turnover_constant, -bmtype) %>%
  mutate(region = ifelse(region == 1, "south", "north"))


alive_litter <-
  litter_types %>%
  # Leaving out dead branches in order to avoid double counting
  filter(component != 5) %>%
  # Leave out spruce bark
  filter(!(component == 2 & species == 2)) %>%
  # divide into above and below ground litter
  mutate(ground = ifelse(component == 7, "below", "above")) %>%
  group_by(region, year, tkg, litter_type, ground) %>%
  #group_by(region, year, litter_type, ground) %>%
  summarize(litter = sum(litter)) %>%
  # designate these as alive
  mutate(mortality = "alive")


# alive_litter_mod <-
#   alive_litter %>% 
#   group_by(region, year, litter_type, ground) %>% 
#   summarize(bm = mean(sum))

#####################################################################################

# Now reading in information from ghgi

ghg_path = paste(PATH_ghgi, "2021/trees/drain/remaining/litter/lulucf/", sep ="")

# List all the GHGI files containing the pertinent data
litter_file_list <- list.files(ghg_path, 
                               pattern = "*csv")

listfill <- data.frame()

# These are just for unifying GHGI style nomenclature and markup with the peat stuff 
mort_lookup <- data.frame(mort = c("log", "nat"),
                          mortality = c("logging", "natural"))

litt_lookup <- data.frame(litt = c("cwl", "fwl", "nwl"),
                          litter_type = c("coarse_woody_litter", "fine_woody_litter", "non-woody_litter"))

region_lookup <- data.frame(reg = c("SF", "NF"),
                             region = c("south", "north"))

region2_lookup <- data.frame(reg = c("sf", "nf"),
                            region = c("south", "north"))

ground_lookup <- data.frame(gnd = c("abv", "bel"),
                            ground = c("above", "below"))

# Use file names for classification then read the contents to a table iteratively

for (i in 1:length(litter_file_list)) {
  
  item_to_read <- paste(ghg_path, litter_file_list[i], sep ="")
  read_item <- read.table(item_to_read, sep = ",", header = TRUE, col.names = c("A", "W", "E", "N"))
  categories <- unlist(x = strsplit(litter_file_list[i], split = ".", fixed = TRUE))
  
  read_item$mort <- categories[1]
  read_item$litt <- categories[2]
  read_item$reg <- categories[3]
  read_item$soil <- categories[4]
  read_item$gnd <- categories[5]
  
  # Because no year data are provided, we have to assume that the last row represents the current inventory year
  read_item <- mutate(read_item, year = 2022 - rev(row_number()))
  
  listfill <- rbind(listfill, read_item)  
}


###########################

summarydata <- 
  listfill %>% 
  group_by(mort, litt, reg, soil, gnd) %>% 
  summarize(minyear = min(year), maxyear = max(year))


#############################


# Calculate the litter production from logging and natural mortality
lognat_litter <- 
  listfill %>% 
  # Filter out prior to 1990 and mineral grounds
  filter(year > 1989, soil == "org", gnd != "csv") %>% 
  right_join(mort_lookup) %>% 
  right_join(litt_lookup) %>% 
  right_join(region_lookup) %>% 
  right_join(ground_lookup) %>% 
  mutate(litter = A + W + E + N) %>% 
  select(region, year, litter_type, ground, litter, mortality)

if(PARAM_scenario %in% c(1,3)) {
  lognat_litter <-
    lognat_litter %>%
    group_by(region, litter_type, ground, mortality) %>% 
    mutate(litter = first(litter))
}

  write.table(x = lognat_litter,
              file = "Work/dead_litter_2.csv",
              row.names = FALSE,
              quote = FALSE,
              col.names = TRUE,
              sep =" ")



# Here we expand the dead litter into peatland types and weight them based on type

dead_litter <-
  lognat_litter %>%
  #filter(litter_type == "coarse_woody_litter") %>% 
  # weight the biomasses based on peatland type
  group_by(region, year) %>% 
  summarize(litter = sum(litter))

total_tree_litter <- 
  alive_litter %>% 
  rename(peat_type = tkg,
         litter_biomass = litter)

# DEBUG!
if(PARAM_scenario %in% c(1,3)) {
total_tree_litter <- 
  total_tree_litter %>% 
  group_by(region, peat_type, litter_type, ground, mortality) %>% 
  mutate(litter_biomass = first(litter_biomass))

}

#

# Save the results
write.table(x = total_tree_litter, 
            file = PATH_total_tree_litter, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")

write.table(x = dead_litter, 
            file = PATH_dead_litter,
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")


# THESE ARE COMMENTED OUT BUT LEFT IN CASE AWEN CALCULATIONS ARE NEEDED AT SOME POINT
# # Calculate the awen fractions
# litter_awen <-
#   litter_types %>% 
#   # First define awen params to be used
#   right_join(LOOKUP_awentype) %>% 
#   # include the awen fraction parameters
#   right_join(LOOKUP_awen_params) %>% 
#   # Calculate the awen partitions (in place operation)
#   mutate(A = A * litter, 
#          W = W * litter, 
#          E = E * litter, 
#          N = N * litter) %>% 
#   select(-litter, -awentype)
# 
# # Summarizing into Fine woody litter and Non-woody litter and categorizing 
# # into above and below ground
# 
# litter_categories <-
#   litter_awen %>% 
#   # Leaving out dead branches in order to avoid double counting
#   filter(component != 5) %>% 
#   mutate(ground = ifelse(component == 7, "below", "above")) %>% 
#   group_by(region, year, tkg, litter_type, ground) %>% 
#   # Sum up the components
#   summarize(A = sum(A), 
#             W = sum(W), 
#             E = sum(E),
#             N = sum(N))
#   # Sum up total awen
#   mutate(awensum = A+W+E+N,
#          mortality = "alive")

  