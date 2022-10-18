#!TODO! Update this file

# This script interpolates the basal area data to cover annual observations. Based on NFI data, processed by Helena Henttonen

rm(list=ls())
source("PATHS.R")
source("CONSTANTS.R")

library(reshape2)

# NFI biomass stocks

# Used for distributing the NFI areas to SF/NF division
region_lookup <- data.frame(region = c(1, 1, 2, 2), 
                            muoto11 = c(1,2,3,4))

# For combining the peatland forest types 2&3 and 4&5
tkg_lookup <- data.frame(tkg = c(2,3,4,5), 
                         gen_tkg = c(2, 2, 4, 4))

species_lookup <- data.frame(laji = c(1,2,3),
                             tree_type = c("pine", "spruce", "deciduous "))
reg_lookup <- data.frame(region = c(1,2,3,4),
                         newreg = c("south", "south", "north", "north"))

dir=paste(PATH_ghgi, "2020/trees/stock/remaining/", sep = "")

# Differences between the reference dates January 1, xxxx (xxxx=1990,1991,...,2013) and
# the mean dates of NFIy measurements (y=8,9,10, 11) by region and soil type
ddiff8=read.table(paste(dir,'MeanDateNFI8.dat',sep=''),col.names=c('region','soil','year','ddiff8'))
ddiff9=read.table(paste(dir,'MeanDateNFI9.dat',sep=''),col.names=c('region','soil','year','ddiff9'))
ddiff10=read.table(paste(dir,'MeanDateNFI10.dat',sep=''),col.names=c('region','soil','year','ddiff10'))
ddiff11=read.table(paste(dir,'MeanDateNFI11.dat',sep=''),col.names=c('region','soil','year','ddiff11'))
ddiff12=read.table(paste(dir,'MeanDateNFI12.dat',sep=''),col.names=c('region','soil','year','ddiff12'))
ddiff13=read.table(paste(dir,'MeanDateNFI13.dat',sep=''),col.names=c('region','soil','year','ddiff13'))


ddiff=merge(ddiff8,ddiff9)
ddiff=merge(ddiff,ddiff10)
ddiff=merge(ddiff,ddiff11)
ddiff=merge(ddiff,ddiff12)
ddiff=merge(ddiff,ddiff13)

# Compute weights for linear interpolation between the mean dates
ddiff$w8=-ddiff$ddiff9/(ddiff$ddiff8-ddiff$ddiff9)
ddiff$w8[ddiff$ddiff9>0]=0

ddiff$w9=ddiff$ddiff8/(ddiff$ddiff8-ddiff$ddiff9)
ddiff$w9[ddiff$ddiff9>0]=(-ddiff$ddiff10/(ddiff$ddiff9-ddiff$ddiff10))[ddiff$ddiff9>0]
#ddiff$w9[is.na(ddiff$w9)]=1 NOT NEEDED
ddiff$w9[ddiff$ddiff10>0]=0 #ADDED

ddiff$w10=ddiff$ddiff9/(ddiff$ddiff9-ddiff$ddiff10)
ddiff$w10[ddiff$ddiff10>0]=(-ddiff$ddiff11/(ddiff$ddiff10-ddiff$ddiff11))[ddiff$ddiff10>0] # ADDED
ddiff$w10[ddiff$ddiff9<0]=0
ddiff$w10[ddiff$ddiff11>0]=0

#Lisataan VMI11:lle sama kuin VMI10:lle
ddiff$w11=ddiff$ddiff10/(ddiff$ddiff10-ddiff$ddiff11)
ddiff$w11[ddiff$ddiff11>0]=(-ddiff$ddiff12/(ddiff$ddiff11-ddiff$ddiff12))[ddiff$ddiff11>0] # ADDED
ddiff$w11[ddiff$ddiff10<0]=0
ddiff$w11[ddiff$ddiff12>0]=0

ddiff$w12=ddiff$ddiff11/(ddiff$ddiff11-ddiff$ddiff12) # ADDED
ddiff$w12[ddiff$ddiff11<0]=0 # ADDED
ddiff$w12[ddiff$ddiff12 > 0]=1 


ddiff_weights <- 
  ddiff %>% 
  filter(soil == 2) %>% 
  select(region, year, w8, w9, w10, w11, w12)

# Muokataan ddiff-tiedostoa kattamaan E/P jaon lisäksi VMI-aluejako
ddiff_mod <- 
  region_lookup %>% 
  right_join(ddiff_weights) %>% 
  select(-region) %>% 
  rename(region = muoto11)
# Luetaan ppat

tkg_ppa <- read.csv2(paste(PATH_input, "basal_areas_and_biomass.csv", sep = ""), dec = ".")

# Otetaan painot suoraan Antin laskemista keskiarvoista
tkg_weights <-read.csv(paste(PATH_input, "peatland_area_weights.csv", sep = ""), sep = " ")

ppat <- tkg_ppa[1:9]

# Used proportional weights for combining peatland forest types
ppa_weights <- 
  ppat %>% 
  filter(tkg %in% c(2:5), fra == 1) %>% 
  right_join(tkg_weights) %>% 
  right_join(tkg_lookup) %>% 
  group_by(vmi, muoto11, laji1, gen_tkg, keskivuosi) %>% 
  summarize(painoppa = weighted.mean(keskippa, weight,
            keskivuosi = weighted.mean(keskivuosi, weight))) %>% 
  rename(tkg = gen_tkg)

# Tässä karsitaan kaikki turha pois PPA  datasta jotta voidaan liittää painokertoimiin (muista halutaan vaan  SOIL = 2)
ppat_sum <- 
  ppat %>% 
  filter(fra == 1) %>% 
  select(vmi, muoto11, tkg, laji1, keskivuosi, keskippa) %>% 
    mutate(tkg = ifelse(tkg == 3, 2, tkg),
           tkg = ifelse(tkg == 5, 4, tkg)) %>% 
  group_by(vmi, muoto11, tkg, laji1) %>% 
  mutate(keskippa = mean(keskippa), keskivuosi = mean(keskivuosi)) %>%
  ungroup() %>% 
  left_join(ppa_weights) %>%
  # Include the weighted means instead of normal 
  mutate(keskippa = ifelse(is.na(painoppa), keskippa, painoppa)) %>%
  select(-painoppa) %>%
  rename(laji = laji1, region = muoto11) 


# Add missing values by rotating them and using linear interpolation, add back to the main table


ppa_wide <- dcast(ppat_sum, region+laji+tkg~vmi, value.var = "keskippa", fun.aggregate = mean)
t_ppa <- t(ppa_wide[4:9])
t_napprox <- na.approx(t_ppa)
nt_ppa <- t(t_napprox)
ppa_wide <- cbind(ppa_wide[1:3], nt_ppa)

colnames(ppa_wide) <- c("region", "laji", "tkg",   "vmi8",      "vmi9" ,     "vmi10",     "vmi11", "vmi12", "vmi13")

# Suoritetaan varsinainen interpolaatio

vmi13_addition <-
  ppat_sum %>% 
  filter(vmi == 13) %>% 
  right_join(species_lookup) %>% 
  right_join(reg_lookup) %>% 
  rename(basal_area = keskippa, peat_type = tkg, year = keskivuosi) %>% 
  group_by(newreg, peat_type, tree_type, year) %>% 
  summarize(basal_area = mean(basal_area)) %>% 
  rename(region = newreg)

basal_area_per_species <- 
  ppa_wide %>% 
  right_join(ddiff_mod) %>% 
  mutate(interp_ppa = vmi8*w8 + vmi9*w9 + vmi10*w10 + vmi11*w11 + vmi12*w12) %>% 
  select(region, laji, tkg, year, interp_ppa) %>% 
  rename(basal_area = interp_ppa, 
         peat_type = tkg) %>% 
  right_join(species_lookup) %>% 
  right_join(reg_lookup) %>% 
  group_by(newreg, peat_type, tree_type, year) %>% 
  summarize(basal_area = mean(basal_area)) %>% 
  rename(region = newreg) %>% 
  filter(year < 2017) %>% 
  # here starts vmi13 addon stuff
  rbind(vmi13_addition) %>% 
  group_by(region, peat_type, tree_type) %>% 
  complete(year = 1990:max(year) + CONST_forward_years) %>% 
  # This bit here repeats the last true value n amount of years to the future (we don't like extrapolation)
  mutate(basal_area = if_else(year > max(year) - CONST_forward_years, 
                              basal_area[which(year == max(year) - CONST_forward_years)], 
                              basal_area)) %>% 
  mutate(basal_area = na.approx(basal_area)) %>% 
  arrange(year)

# For comparisojn

raw_data <-
  ppat_sum %>% 
  left_join(species_lookup) %>% 
  left_join(reg_lookup) %>% 
  group_by(newreg, tkg, laji, keskivuosi, tree_type) %>% 
  summarize(basal_area = mean(keskippa)) %>% 
  rename(peat_type = tkg, year = keskivuosi, region = newreg)



# Piirretään kuvaaja 
ggplot(data=basal_area_per_species, aes(x = year, y = basal_area, col = as.factor(tree_type))) +
  geom_point() +
  geom_path() +
  geom_point(data = raw_data, aes(x = year, y = basal_area, col = as.factor(tree_type)), shape = 15, size = 2) +
  facet_grid(peat_type~region)


ggplot(data=raw_data, aes(x = year, y = basal_area, col = as.factor(tree_type))) +
  geom_point() +
  geom_path() +
  #geom_point(data = raw_data, aes(x = year, y = basal_area, col = as.factor(tree_type)), shape = 15, size = 2) +
  facet_grid(peat_type~region)


##########################
# 
# # Kikkailua
# 
# interpolation_trial <- 
#   ppat_sum %>% 
#   rename(basal_area = keskippa) %>% 
#   mutate(year = floor(keskivuosi)) %>% 
#   group_by(region, tkg, laji) %>% 
#   complete(year = min(year):max(year)) %>% 
#   mutate(basal_area = na.approx(basal_area)) %>% 
#   left_join(species_lookup) %>% 
#   left_join(reg_lookup) %>% 
#   rename(peat_type = tkg) %>% 
#   group_by(newreg, peat_type, tree_type, year) %>% 
#   summarize(basal_area = mean(basal_area)) %>% 
# #   rename(region = newreg) %>% 
# #   filter(year > 1989)
# 
# # Piirretään kuvaaja 
# ggplot(data=interpolation_trial, aes(x = year, y = basal_area, col = as.factor(tree_type))) +
#   geom_point() +
#   geom_path() +
#   geom_point(data = funk, aes(x = year, y = basal_area, col = as.factor(tree_type)), shape = 15, size = 2) +
#   facet_grid(peat_type~region)




#############################


# Tallennetaan lajikohtaisesti erotellut
write.table(basal_area_per_species, 
            file = PATH_below_ground_litter_basal_data,
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")
            

###########################

# Save aggregated

basal_area_aggregated <-
  basal_area_per_species %>% 
  group_by(region, peat_type, year) %>% 
  summarize(basal_area = sum(basal_area))

write.table(basal_area_aggregated, 
            file = PATH_basal_area_data, 
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")
