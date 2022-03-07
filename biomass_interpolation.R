#!TODO! Korjaa tästä tiedostosta edes jotenkin tolkullinen


rm(list=ls())
source("PATHS.R")

# NFI biomass stocks

years=1990:2019

dir=paste(PATH_ghgi, "2019/trees/stock/remaining/", sep = "")

# Differences between the reference dates January 1, xxxx (xxxx=1990,1991,...,2013) and
# the mean dates of NFIy measurements (y=8,9,10, 11) by region and soil type
ddiff8=read.table(paste(dir,'MeanDateNFI8.dat',sep=''),col.names=c('region','soil','year','ddiff8'))
ddiff9=read.table(paste(dir,'MeanDateNFI9.dat',sep=''),col.names=c('region','soil','year','ddiff9'))
ddiff10=read.table(paste(dir,'MeanDateNFI10.dat',sep=''),col.names=c('region','soil','year','ddiff10'))
ddiff11=read.table(paste(dir,'MeanDateNFI11.dat',sep=''),col.names=c('region','soil','year','ddiff11'))
ddiff12=read.table(paste(dir,'MeanDateNFI12.dat',sep=''),col.names=c('region','soil','year','ddiff12'))


ddiff=merge(ddiff8,ddiff9)
ddiff=merge(ddiff,ddiff10)
ddiff=merge(ddiff,ddiff11)
ddiff=merge(ddiff,ddiff12)

# Compute weights for linear interpolation between the mean dates
ddiff$w8=-ddiff$ddiff9/(ddiff$ddiff8-ddiff$ddiff9)
ddiff$w8[ddiff$ddiff9>0]=0

ddiff$w9=ddiff$ddiff8/(ddiff$ddiff8-ddiff$ddiff9)
ddiff$w9[ddiff$ddiff9>0]=(-ddiff$ddiff10/(ddiff$ddiff9-ddiff$ddiff10))[ddiff$ddiff9>0]
#ddiff$w9[is.na(ddiff$w9)]=1 NOT NEEDED
ddiff$w9[ddiff$ddiff10>0]=0 #ADDED

ddiff$w10=ddiff$ddiff9/(ddiff$ddiff9-ddiff$ddiff10)
ddiff$w10[ddiff$ddiff10>0]=(-ddiff$ddiff11/(ddiff$ddiff10-ddiff$ddiff11 ) ) [ddiff$ddiff10>0] # ADDED
ddiff$w10[ddiff$ddiff9<0]=0
ddiff$w10[ddiff$ddiff11>0]=0

#Lisataan VMI11:lle sama kuin VMI10:lle
ddiff$w11=ddiff$ddiff10/(ddiff$ddiff10-ddiff$ddiff11)
ddiff$w11[ddiff$ddiff11>0]=(-ddiff$ddiff12/(ddiff$ddiff11-ddiff$ddiff12 ) ) [ddiff$ddiff11>0] # ADDED
ddiff$w11[ddiff$ddiff10<0]=0
ddiff$w11[ddiff$ddiff12>0]=0

ddiff$w12=ddiff$ddiff11/(ddiff$ddiff11-ddiff$ddiff12) # ADDED
ddiff$w12[ddiff$ddiff11<0]=0 # ADDED
ddiff$w12[ddiff$ddiff12 > 0]=1 

ddiff_mod <- 
  ddiff %>% 
  filter(soil == 2) %>% 
  select(region, year, w8, w9, w10, w11, w12)

# Luetaan biomassat
tkg_ppa <- read.csv(paste(PATH_input, "basal_areas_and_biomass.csv", sep = ""))

region_lookup <- data.frame(region = c(1, 1, 2, 2), 
                            muoto11 = c(1,2,3,4))

tkg_lookup <- data.frame(tkg = c(2,3,4,5), 
                         gen_tkg = c(2, 2, 4, 4))

# Otetaan painot suoraan Antin laskemista keskiarvoista
tkg_weights <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/Work/tkg_weights.txt", sep="")

bm <- tkg_ppa

bm$keskippa <- NULL

bm_melt <- melt(bm,
                id.vars=colnames(bm[1:8]),
                variable.name = "component", value.name ="bm")

bm_melt$component <- as.numeric(sub("biom", "", bm_melt$component))

ggplot(data=filter(bm_melt, component <8, fra == 1), aes(x = keskivuosi, y = bm, col = as.factor(component))) +
  geom_point() +
  geom_path() +
  labs(title ="Raakadata") +
  facet_grid(muoto11~laji1~tkg)


# Lasketaan painot
bm_weights <- 
  bm_melt %>% 
  filter(tkg %in% c(seq(2,5, 1)), fra == 1) %>% 
  right_join(tkg_weights) %>% 
  right_join(tkg_lookup) %>% 
  group_by(vmi, muoto11, laji1, gen_tkg, keskivuosi, component) %>% 
   summarize(painobm = weighted.mean(bm, weight,
                                      keskivuosi = weighted.mean(keskivuosi, weight))) %>% 

  rename(tkg = gen_tkg)


# Tässä karsitaan kaikki turha pois PPA  datasta jotta voidaan liittää painokertoimiin (muista halutaan vaan  SOIL = 2)
bm_sum <- 
  bm_melt %>% 
  filter(fra == 1, component <8) %>% 
  select(vmi, muoto11, tkg, laji1, keskivuosi, component, bm) %>% 
  mutate(tkg = ifelse(tkg == 3, 2, tkg),
         tkg = ifelse(tkg == 5, 4, tkg)) %>% 
  group_by(vmi, muoto11, tkg, laji1, component) %>% 
  mutate(bm = mean(bm), keskivuosi = mean(keskivuosi)) %>%
  ungroup() %>% 
  left_join(bm_weights) %>% 
  mutate(bm = ifelse(is.na(painobm), bm, painobm)) %>% 
  select(-painobm) %>% 
  right_join(region_lookup) %>% 
  rename(species = laji1) 

ggplot(data=bm_sum, aes(x = keskivuosi, y = bm, col = as.factor(component))) +
  geom_point() +
  geom_path() +
  labs(title ="Tkgt yhdistetty") +
  facet_grid(muoto11~species~tkg)


bm_wide <- dcast(bm_sum, region+species+tkg+component~vmi, value.var = "bm", fun.aggregate = mean)

bm_remove_na <- na.approx(t(bm_wide[5:9]))

bm_wide <- cbind(bm_wide[1:4], t(bm_remove_na))

colnames(bm_wide) <- c("region", "species", "tkg", "component",  "vmi8",      "vmi9" ,     "vmi10",     "vmi11", "vmi12")

ddiff <- ddiff_mod

# Suoritetaan varsinainen interpolaatio

bm.interp <- 
  bm_wide %>% 
  right_join(ddiff_mod) %>% 
  mutate(bm  = vmi8*w8 + vmi9*w9 + vmi10*w10 + vmi11*w11 + vmi12*w12) %>% 
  select(region, year, tkg, species, component, bm) 


ggplot(data=bm.interp, aes(x = year, y = bm, col = as.factor(component))) +
  geom_point() +
  geom_path() +
  labs(title ="BM interpoloitu") +
  facet_grid(region~species~tkg)


# Tallennetaan lajikohtaisesti erotellut
write.table(bm.interp,
            file = PATH_interpolated_biomass,
            row.names = FALSE, 
            quote = FALSE, 
            col.names = TRUE, 
            sep =" ")
            
# newint_wide <- dcast(newint, region+species+tkg+component~year, value.var = "bm", fun.aggregate = mean)
# write.table(newint_wide, "BM_interp_wide.txt")
# 
# 
# 
# 
# newint_sum <-
#   newint %>% 
#   group_by(region, tkg, year) %>% 
#   summarize(bm = sum(bm))
# 
# write.table(newint_sum, "PPA_interp_long_sum.txt")
# newint_wide_sum <- dcast(newint_sum, region+tkg~year, value.var = "bm", fun.aggregate = mean)
# write.table(newint_wide_sum, "PPA_interp_wide_sum.txt")
