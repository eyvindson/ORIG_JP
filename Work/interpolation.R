# Aloitetaanpas alusta

# Luetaan päiväerotukset


ddiff8=read.table(paste(dir,'MeanDateNFI8.dat',sep=''),col.names=c('region','soil','year','ddiff'))
ddiff9=read.table(paste(dir,'MeanDateNFI9.dat',sep=''),col.names=c('region','soil','year','ddiff'))
ddiff10=read.table(paste(dir,'MeanDateNFI10.dat',sep=''),col.names=c('region','soil','year','ddiff'))
ddiff11=read.table(paste(dir,'MeanDateNFI11.dat',sep=''),col.names=c('region','soil','year','ddiff'))
ddiff12=read.table(paste(dir,'MeanDateNFI12.dat',sep=''),col.names=c('region','soil','year','ddiff'))
ddiff13=read.table(paste(dir,'MeanDateNFI13.dat',sep=''),col.names=c('region','soil','year','ddiff'))

ddiff8$vmi <- 8
ddiff9$vmi <- 9
ddiff10$vmi <- 10
ddiff11$vmi <- 11
ddiff12$vmi <- 12

diff <- rbind(ddiff8, ddiff9, ddiff10, ddiff11, ddiff12)

# Muunnetaan vuodet päiviksi, vähennetään etäisyys referenssipäivästä, saadaan alkuperäinen referenssipäivä


ddiff_f <-
  diff %>% 
  mutate(dyear = year * 365.25) %>% 
  mutate(meandate = (dyear - ddiff) / 365.25) %>% 
  group_by(region, soil, vmi) %>% 
  summarise(meandate = mean(meandate)) %>% 
  mutate(year = floor(meandate)) 

# Luetaan biomassat

bm8=read.table(bm8f,col.names=c('region','soil','species','component','biomass'))
# Results from NFI9
bm9=read.table(bm9f,col.names=c('region','soil','species','component','biomass'))
# Results from NFI10
bm10=read.table(bm10f,col.names=c('region','soil','species','component','biomass','volume'))
bm10=bm10[,-6]			#volume
# Results from NFI11
bm11=read.table(bm11f,col.names=c('region','soil','species','component','biomass','volume'))
bm11=bm11[,-6]			#volume
bm12=read.table(bm12f,col.names=c('region','soil','species','component','biomass'))

# Yhdistetään biomassat

bm8$vmi <- 8
bm9$vmi <- 9
bm10$vmi <- 10
bm11$vmi <- 11
bm12$vmi <- 12

bms <- rbind(bm8, bm9, bm10, bm11, bm12)

# Yhdistellään, tilkitään välit

combo <- 
  bms %>% 
  right_join(ddiff_f) %>% 
  group_by(region, soil, species, component) %>% 
  complete(year = full_seq(min(year):max(year), 1)) %>% 
  fill(vmi) %>% 
  mutate(biomass_approx = na.approx(biomass)) %>% 
  mutate(type = "data")

combo_reg <- 
  combo %>% 
  filter(year > 2011, year < 2017) %>% 
  group_by(region, soil, species, component) %>% 
  do(model = lm(biomass_approx ~ year, data = .)) %>% 
  ungroup() 

combo_model <-
  combo %>% 
  group_by(region, soil, species, component) %>% 
  complete(year = full_seq(2016:2019, 1)) %>% 
  mutate(type = ifelse(year > 2016, "pred", type))
  
combo_predict <- 
  combo_model %>% 
  right_join(combo_reg) %>% 
  filter(type == "pred") %>% 
  do(modelr::add_predictions(., first(.$model))) %>% 
  mutate(biomass_approx = pred) %>% 
  select(-model, -pred)
  
combo_plot <- rbind(combo, combo_predict)
  
combo_sum <-
  combo_plot %>% 
  group_by(region, soil, year, component) %>% 
  summarize(bm = sum(biomass_approx)) %>% 
  filter(component == 1, year > 1989)

combo_sum$model = "new"
newint.sum$model = "old"

combo_combo <- rbind(combo_sum, newint.sum)
  

ggplot(data=combo_combo, aes(x = year, y = bm, col = model)) +
  geom_point() +
  geom_path() +
  facet_grid(soil~region)

