# Define any and all constants in this file. Prefix the variables with CONST_ for unambiguity.


# How many years into the future after the last known value
CONST_forward_years = 1

# GWP ratios here

CONST_gwp5_ch4 <- 25

# This is a core variable containing the peatland forest types used in the calculations, coded as integers.
# Numbers are not completely sequential because original types 2 & 3 were combined into 2 and 4 & 5 into 4. 

CONST_peatland_types = c(1, 2, 4, 6, 7)

# For combining peatland types 2&3 and 3&4. 
CONST_peatland_weights <- data.frame(tkg = 1:7,
                                     area_weight = c(1.00000, 61.32428, 38.67572, 60.07019, 39.92981,  1.00000,  1.00000))

# These are the constants used in the linear equations for decomposition of peat, based on tree basal area 
# and mean air temperature  The specific equation is decomposition = (a * [basal_area] + b * [t]) - c
# See Ojanen etal. 2014 https://doi.org/10.1016/j.foreco.2014.03.049 for original source

CONST_peat_decomposition_a <- 14.74
CONST_peat_decomposition_b <- 242.8
CONST_peat_decomposition_by_peatland_type  <- data.frame(peat_type  = CONST_peatland_types,
                                                         decomposition_constant  = c(1383, 	 1440,	 1662,	 1771,	 1814))

# The proportion of dwarf shrub projection coverage of dwarf shrubs from Ojanen 2014 Table A.3
# Unit: g / m2
CONST_dwarfshrub_root_biomass <- 4.81

# Coverage in percentages of total area
CONST_dwarfshrub_coverage <- data.frame(
  peat_type =    CONST_peatland_types,
  dshrub_cover = c(7, 15.4515, 32.0105, 45, 40))

# Regression constants for fineroot biomass for different tree groups
CONST_fine_root_biomass_by_treetype <- data.frame(
  tree_type = c("pine", "spruce", "deciduous"),
  regression_constant = c(8.8, 6.61, 17.3))

# From Ojanen 2014. Regional fine root biomass constant, in g / m2)
CONST_fine_root_biomass_region <- data.frame(
  region = c("south", "north"),
  regional_modifier = c(120, -53.2))

# Fine root turnover rate, based on Minkkinen et al. 2020
CONST_fine_root_turnover <- data.frame(
  peat_type = CONST_peatland_types,
  fine_root_turnover = c(0.8, 0.5, 0.7, 0.2, 0.2))
  
# Adjustment factor for taking into account roots deeper than 20 cm, Laiho and Finér (1996):
CONST_fine_root_deep_fraction <- 1.043

# Conversion factor of dry biomass into carbon
CONST_biomass_to_C <- 0.5
CONST_C_to_CO2 <- 44/12

# Total ground vegetation biomass. Linear regression equations based on Ojanen 2014. Includes both above and below ground biomass.
CONST_total_ground_vegetation_biomass_A <- -4.52

CONST_total_ground_vegetation_biomass_by_peatland_type  <- data.frame(
    peat_type = CONST_peatland_types,
    regression_constant  = c(227, 	 227,	 256,	 298,	 187))

# For converting between peatland name and code 

CONST_peat_lookup <- data.frame(
  peat_name = as.factor(c("Rhtkg", "Mtkg", "Ptkg", "Vatkg", "Jatkg")),
  peat_type = CONST_peatland_types
)

# Used for converting numeric tree types to string

CONST_species_lookup <- data.frame(laji = c(1:3),
                                   
                                   tree_type = c("pine", "spruce", "deciduous "))