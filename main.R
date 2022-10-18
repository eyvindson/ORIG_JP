# Please read the README_FIRST.txt

rm(list=ls())

source("PATHS.R")

# Update weather data first. 

# !TODO! käy läpi kaikki input tiedostot ja varmista että vuodet mätsää

# Input data preparation
source(PATH_weather_script) # leave this out if new weather data is not needed
source(PATH_area_script)
# The following two scripts will soon be obsolete
source(PATH_basal_area_interpolation_script)
source(PATH_biomass_interpolation_script)
source(PATH_biomass_to_litter_script)

## Here starts the actual main calculation ## 

# Calculate peat decomposition
source(PATH_peat_decomposition_script)

# Calculate below ground litter
source(PATH_below_ground_litter_script)

# Calculate above ground litter
source(PATH_above_ground_litter_script)

# Calculate the emission factors
source(PATH_ef_script)

# Calculate the final emissions based on emission factors and area data
source(PATH_total_script)

# Finally calculate figures used for publication. Placed in /Figures/Publication

#source("PLOTS.R")
