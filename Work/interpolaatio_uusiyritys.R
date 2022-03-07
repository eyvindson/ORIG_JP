interpol <- ppat_sum

interpol_mod <-
  interpol %>% 
  select(-vmi) %>% 
    rename(year = keskivuosi, 
           basal_area = keskippa) %>% 
  # ensin interpolaatio
  mutate(year = floor(year)) %>% 
  group_by(region, tkg, laji) %>% 
  complete(year = full_seq(min(year):max(year), 1)) %>% 
  mutate(approx_basal = na.spline(basal_area)) %>% 
  mutate(grouping = "interpol") %>% 
  ungroup()

extrapol <-
  interpol_mod %>% 
  filter(year > 1989) %>% 
  

ggplot(data=interpol_mod, aes(x = year, y = approx_basal, col = as.factor(laji))) +
  geom_point() +
  geom_path() +
  facet_grid(tkg~region)