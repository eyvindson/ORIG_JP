#!TODO! Korjaa tästä tiedostosta edes jotenkin tolkullinen


# NFI biomass stocks

path.calc.09='Z:/d4/projects/khk/ghg/2009/trees/stock/remaining/'
path.calc.11='Z:/d4/projects/khk/ghg/2015/trees/stock/remaining/'
path.calc.12='Z:/d4/projects/khk/ghg/2018/trees/stock/remaining/'
path.calc='Z:/d4/projects/khk/ghg/2019/trees/stock/remaining/'


years=1990:2019

dir='Z:/d4/projects/khk/ghg/2019/trees/stock/remaining/'

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

# Luetaan ppat

tkg_ppa_biomass_8 <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/Work/tkg_ppa_biomass_8.csv")
tkg_ppa_biomass_9 <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/Work/tkg_ppa_biomass_9.csv")
tkg_ppa_biomass_10 <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/Work/tkg_ppa_biomass_10.csv")
tkg_ppa_biomass_11 <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/Work/tkg_ppa_biomass_11.csv")
tkg_ppa_biomass_12 <- read.csv("C:/Users/03180980/OneDrive - Valtion/R/Peatlands/Work/tkg_ppa_biomass_12.csv")


tkg_ppa <- rbind(tkg_ppa_biomass_8, tkg_ppa_biomass_9, tkg_ppa_biomass_10, tkg_ppa_biomass_11, tkg_ppa_biomass_12)

rm(tkg_ppa_biomass_8, tkg_ppa_biomass_9, tkg_ppa_biomass_10, tkg_ppa_biomass_11, tkg_ppa_biomass_12)

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
  mutate(interp_bm  = vmi8*w8 + vmi9*w9 + vmi10*w10 + vmi11*w11 + vmi12*w12) %>% 
  select(region, species, tkg, year, component, interp_bm) 

ggplot(data=bm.interp, aes(x = year, y = interp_bm, col = as.factor(component))) +
  geom_point() +
  geom_path() +
  labs(title ="BM interpoloitu") +
  facet_grid(region~species~tkg)


bm.year <- dcast(bm.interp, region+species+tkg+component~year, value.var = "interp_bm", fun.aggregate = sum)
bm.year.mean <- dcast(bm.interp, region+species+tkg+component~year, value.var = "interp_bm", fun.aggregate = mean)

# Luetaan runkotilavuudet
annVol11=read.table(paste(path.calc.11,'AnnVolumeNFI11.dat',sep=""),col.names=c('year','region','soil','species','volume','stderr'))
annDateDiff11=read.table(paste(path.calc.11,'MeanDateNFI11ann.dat',sep=""),col.names=c('region','soil','year','dd'))

annVol12=read.table(paste(path.calc.12,'AnnVolumeNFI12.dat',sep=""),col.names=c('year','region','soil','species','volume','stderr'))
annVol12$year=annVol12$year-2000
annDateDiff12=read.table(paste(dir,'MeanDateNFI12ann.dat',sep=""),col.names=c('region','soil','year','dd')) 

annVol13=read.table(paste(path.calc,'AnnVolumeNFI13.dat',sep=""),col.names=c('year','region','soil','species','volume','stderr'))
annVol13$year=annVol13$year-2000
annDateDiff13=read.table(paste(dir,'MeanDateNFI13ann.dat',sep=""),col.names=c('region','soil','year','dd')) 

tkg_list=c(1,2,4,6,7)

annVol <- rbind(annVol11,annVol12,annVol13)
annDateDiff <- rbind(annDateDiff11,annDateDiff12,annDateDiff13)
annVol=merge(annVol,annDateDiff,all.x=T)
annVol$year=annVol$year+2000
annVol <- subset(annVol,year>=2014)

annVol_origin <- annVol

annVol <-
  annVol_origin %>% 
  filter(soil == 2) %>% 
  select(-soil)
  
vmi12 <- bm_wide
vmi12[5:8] <- NULL

colnames(vmi12) <- c("region", "species", "tkg", "component", "bm")

ddiff12 <-
  ddiff12 %>% 
  filter(soil == 2) %>% 
  select(-soil)


newint=expand.grid(region=1:2,tkg=tkg_list,species=1:3, component=1:7,year=years)
newint=merge(newint,ddiff12,all=T)
newint$bm=numeric(nrow(newint))
for(region in 1:2) for(tkg in tkg_list) for(species in 1:3){
  ann=annVol[annVol$region==region & annVol$species==species,]
  #ann=annVol[annVol$region==region & annVol$tkg==tkg & annVol$laji==laji,]
  X=cbind(1,ann$dd/365+2004)
  W=diag(1/ann$stderr^2)
  slope=(solve(t(X)%*%W%*%X)%*%t(X)%*%W)[2,]%*%ann$volume
  mvol=weighted.mean(ann$volume,diag(W))
  pslope=slope/mvol
  #for(tkg in tkg_list){
  for(cmpnt in 1:7){
    i=(newint$region==region & newint$tkg==tkg & newint$species==species & newint$component==cmpnt)
    newint$bm[i]=
      as.numeric(unlist(bm.year[bm.year$region==region & bm.year$tkg==tkg & bm.year$species==species & bm.year$component==cmpnt,-(1:4)]))
    id=(i & newint$ddiff12>0)
    newint$bm[id]=
      vmi12$bm[vmi12$region==region & vmi12$tkg == tkg & vmi12$species == species & bm.year$component==cmpnt]*
      (1+c(pslope)*newint$ddiff12[id]/365)
  }  
}


newint$ddiff12 <- NULL

# Piirretään kuvaaja 
ggplot(data=newint, aes(x = year, y = bm, col = as.factor(component))) +
  geom_point() +
  geom_path() +
  labs(title ="lopulliset ekstrapoloidut biomassat") +
  facet_grid(region~species~tkg)

# Tarkastus inventaarioon vertaamiseksi
newint_summary <- 
  newint %>% 
  group_by(region, species, component, year) %>% 
  summarize(bm = mean(bm))





# Tallennetaan lajikohtaisesti erotellut
write.table(newint,
            file = "BM_interp_long.txt",
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
