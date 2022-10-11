# Assisting functions go here.
# Prefix all functions with FUNC_


# This function takes data in the "wide" format commonly used in the LUKE GHG inventory, and transforms it into "long"
# Input parameters: 
# wide_table - the table to be transformed. 
# value_name - name to be used for the value column in the new table

FUNC_longify <- function(wide_table, value_name) {
  converted_to_long <- 
    wide_table %>% 
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year", values_to = value_name) %>%
    # Remove the X prefix from years and convert to number
    mutate(year = as.numeric(sub("X", "", year)))
  
  # Return 
  converted_to_long
}

# Reverse %in%, can be read "not in"
'%ni%' <- Negate('%in%')

# This function creates the necessary metadata for a specific datafile. 
# Note 8/22, will probably be dropped out

FUNC_create_metadata <- function(datafile, description, source, author = "N/A", citation = "N/A", notes = "N/A", fields, units_or_desc) {
  metadata <- data.frame(Param = c("datafile", "description", "source", "author", "citation", "notes"),
                        Description = c(datafile, description, source, author, citation, notes))
  
  fields <- paste("FIELD", fields, sep =":")
  metafields <- data.frame(Param = fields,
                           Description = units_or_desc)
  metadata <- rbind(metadata, metafields)
  
  # Metadata is always saved in the same location as the original data with the .meta file extension
  metadata_save_location <- paste(strsplit(datafile, split = ".csv"), "meta", sep =".")
    write.table(x = metadata, 
              file = metadata_save_location, 
              row.names = FALSE, 
              quote = FALSE, 
              col.names = TRUE, 
              sep =";")
  }
             

# This function renames and rearranges area data, mostly for figures
# Input parameters
# peatnaming - whether peatland types should be represented as text rather than integer
# peat_percentage - used for one figure, just labels with a proportions of peat area included
# revreg - override the alphabet (otherwise in figures North data is typically presented first)

FUNC_regionify <- function(datafile, peatnaming = FALSE, peat_percentage = FALSE, revreg = FALSE) {
  outputfile <- datafile
  
  if (revreg) {
    outputfile$region <- factor(outputfile$region, 
                                levels = c("north", "south"), 
                                labels = c("Northern Finland", "Southern Finland"))
    
  } else {
  outputfile$region <- factor(outputfile$region, 
                              levels = c("south", "north"), 
                              labels = c("Southern Finland", "Northern Finland"))
  }
  
  if (peatnaming) {
  outputfile$peat_name <- factor(outputfile$peat_name, 
                                 levels = c("Rhtkg", "Mtkg",  "Ptkg",  "Vatkg", "Jatkg"),
                                 labels = c("Rhtkg", "Mtkg",  "Ptkg",  "Vatkg", "Jatkg"))
                                 
                                 # labels = c("Herb rich type (Rhtkg)", 
                                 #            "Vaccinium myrtillus type (Mtkg)", 
                                 #            "V. vitis-idaea type (Ptkg)",
                                 #            "Dwarf shrub type (Vatkg)",
                                 #            "Cladonia type (Jatkg)"))
  }


  if (peat_percentage) {
    outputfile$peat_name <- factor(outputfile$peat_name, 
                                   levels = c("Rhtkg", "Mtkg",  "Ptkg",  "Vatkg", "Jatkg"),
                                   labels = c("Herb rich type (Rhtkg) 15.2%", 
                                              "Vaccinium myrtillus type (Mtkg) 26.9%", 
                                              "V. vitis-idaea type (Ptkg) 37.5%",
                                              "Dwarf shrub type (Vatkg) 19.6%",
                                              "Cladonia type (Jatkg) 0.8%"))
  }

  outputfile
}

FUNC_write_log <- function(entry) {
  
  if(file.exists(paste(PATH_logs, "log.txt", sep =""))) {
    # add new line
  } else {
    # create file, then add line
  }
  
    
  
  log <- read.csv(paste(PATH_logs, "log.txt", sep =""))
  
  current_time <- timestamp()
  
  full_entry <- data.frame(date = current_time, entry = entry)
    

  write.table(full_entry, file = paste(PATH_logs, "log.txt", sep =""), append = TRUE, quote = FALSE, row.names = FALSE, col.names = TRUE)
  
}

FUNC_qa_check_completeness <- function(values) {
  if (anyNA(x) == TRUE)
    stop(paste0("NAs in the dataset, should not be any: ", deparse(substitute(x))))
  else print(paste0("No NAs in the dataset ", deparse(substitute(x))))
  
  
}

FUNC_qa_sanity_check <- function(values, groups) {
  
  
}


# 
# # This is just a particular visual style used for plotting
# theme_Publication <- function(base_size=14, base_family="sans") {
#   library(grid)
#   library(ggthemes)
#   (theme_foundation(base_size=base_size, base_family=base_family)
#     + theme(plot.title = element_text(face = "bold",
#                                       size = rel(1.2), hjust = 0.5),
#             text = element_text(),
#             panel.background = element_rect(colour = NA),
#             plot.background = element_rect(colour = NA),
#             panel.border = element_rect(colour = NA),
#             axis.title = element_text(face = "bold",size = rel(1)),
#             axis.title.y = element_text(angle=90,vjust =2),
#             axis.title.x = element_text(vjust = -0.2),
#             axis.text = element_text(), 
#             axis.line.x = element_line(colour="black"),
#             axis.line.y = element_line(colour="black"),
#             axis.ticks = element_line(),
#             panel.grid.major = element_line(colour="#f0f0f0"),
#             panel.grid.minor = element_blank(),
#             legend.key = element_rect(colour = NA),
#             legend.position = "bottom",
#             legend.direction = "horizontal",
#             legend.key.size= unit(0.2, "cm"),
#             legend.margin = unit(0, "cm"),
#             legend.title = element_text(face="italic"),
#             plot.margin=unit(c(10,5,5,5),"mm"),
#             strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
#             strip.text = element_text(face="bold")
#     ))
#   
# }
# 
# # A particular color scale used for plotting
# scale_fill_Publication <- function(...){
#   library(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }
# 
# scale_colour_Publication <- function(...){
#   library(scales)
#   discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }