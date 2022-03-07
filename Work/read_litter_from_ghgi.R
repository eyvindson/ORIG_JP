# Now reading in information from ghgi
 
ghg_path = "Z:/d4/projects/khk/ghg/2019/trees/drain/remaining/litter/lulucf/"

log_cwl_SF_org_abv <- read.table(paste(ghg_path, "log.cwl.SF.org.abv.csv", 
                                       sep = "/"), sep = ",", header = TRUE,
                                       col.names = c("A", "W", "E", "N"))


litter_file_list <- list.files(ghg_path, 
                               pattern = "*csv")

listfill <- data.frame()

mort_lookup <- data.frame(mort = c("log", "nat"),
                          mortality = c("logging", "natural"))

litt_lookup <- data.frame(litt = c("cws", "fwl", "nwl"),
                          litter_type = c("coarse_woody_litter", "fine_woody_litter", "non-woody_litter"))

region_lookup <- data.frame(reg = c("SF", "NF"),
                            region = c("south", "north"))

ground_lookup <- data.frame(gnd = c("abv", "bel"),
                            ground = c("above", "below"))

# Use file names for classification

for (i in 1:length(litter_file_list)) {
  
  item_to_read <- paste(ghg_path, litter_file_list[i], sep ="")
  read_item <- read.table(item_to_read, sep = ",", header = TRUE, col.names = c("A", "W", "E", "N"))
  categories <- unlist(x = strsplit(litter_file_list[i], split = ".", fixed = TRUE))
  
  read_item$mort <- categories[1]
  read_item$litt <- categories[2]
  read_item$reg <- categories[3]
  read_item$soil <- categories[4]
  read_item$gnd <- categories[5]
  
  read_item <- mutate(read_item, year = 2020 - rev(row_number()))
    
  listfill <- rbind(listfill, read_item)  
}



soil_litter <- 
  listfill %>% 
  # Filter out prior to 1990 and mineral grounds
  filter(year > 1989, soil == "org", gnd != "csv") %>% 
  right_join(mort_lookup) %>% 
  right_join(litt_lookup) %>% 
  right_join(region_lookup) %>% 
  right_join(ground_lookup) %>% 
  mutate(bm = A + W + E + N)