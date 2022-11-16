# Denote all parameters with the prefix PARAM for easier reading

# Lets daisy chain these here since all files call this file
source("LIBRARIES.R")

# Define all paths here. Make sure to include / in the end of directory paths

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This path should point to the root folder of the whole project, where main.R is located.#
# Rest of the paths are generated automatically.                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

  PATH_main = "C:/Users/03180980/luke-peatland/"
  
  # Point this to the root folder of the inventory calculations in Sorvi
  PATH_ghgi = "Z:/d4/projects/khk/ghg/"
  
  # General parameters go here
  
  PARAM_debug = FALSE # A flag used for extra debugging information
  PARAM_draw_plots = TRUE # Set to true to enable drawing of various plots that will be stored under /Plots  
  PARAM_scenario = 0# 0 = default, 1 = PPA, 2 = TEMP, 3 = BOTH, 4 = Loess fitted temperatures

    
###########################################################################################
  
# There should be no need to touch anything beyond this point
  PATH_input =  paste(PATH_main, "Input/", sep = "")
  PATH_midresults = paste(PATH_main, "Midresults/", sep = "")
  PATH_results = paste(PATH_main, "Results/", sep = "")
  PATH_figures =  paste(PATH_main, "Figures", sep = "")
  PATH_pubfigures =  paste(PATH_main, "Figures/Publication/", sep = "")
  PATH_lookup =  paste(PATH_main, "Lookup/", sep = "")
  PATH_scenario =  paste(PATH_input, "Scenario/", sep = "")
  PATH_logs = paste(PATH_main, "Logs/", sep = "")
  
# Lookups. These are tables that are used in converting and aggregating things.
  PATH_lookup_litter = paste(PATH_lookup, "litter_conversion.csv", sep = "") # for converting biomass fractions into litter fractions
  PATH_lookup_awentype = paste(PATH_lookup, "awentype.csv", sep = "") # currently not used
  PATH_lookup_awenparams = paste(PATH_lookup, "awen_params.csv", sep = "") # currently not used
    
# PARAMETERS FOR INDIVIDUAL SCRIPT FILES GO HERE
  
  PATH_ba_bm_raw_data = paste(PATH_input, "basal_areas_and_biomass.csv", sep = "")
  PATH_interpolated_biomass = paste(PATH_input, "biomass_interpolated.csv", sep = "")
  PATH_ba_bm_interpolation_script = paste(PATH_main, "biomass_and_basal_area_interpolation.R", sep = "") 
  PATH_biomass_to_litter_script = paste(PATH_main, "biomass_to_litter.R", sep = "")
  PATH_basal_area_data = paste(PATH_input, "basal_areas.csv", sep = "") # basal area data file
  PATH_total_tree_litter = paste(PATH_input, "total_tree_litter.csv", sep = "") # soil litter biomasses
  PATH_dead_litter = paste(PATH_input, "dead_litter.csv", sep = "") # soil litter biomasses
  PATH_peatland_proportional_area = paste(PATH_input, "peatland_proportional_area.csv", sep = "") # proportional areas of different peatland types
  
# Weather data parameters (process_weather_data.R)

  PATH_weather_script = paste(PATH_main, "process_weather_data.R", sep = "") # main file
  PATH_weather_data = paste(PATH_input, "Weather/WeatherData_1960_2021.csv",  sep = "") # Data file containing the raw weather data
  PATH_weather_mainareas = paste(PATH_input, "Weather/Areas/Main", sep = "") # Path to main shapefiles, e.g. South/North division
  PATH_weather_subareas = paste(PATH_input, "Weather/Areas/Sub", sep = "") # Path to subregions, from NFI
  PATH_weather_data_aggregated = paste(PATH_midresults, "1960-2021_weather_data.csv", sep = "" ) # Aggregated weather data saved here
  PATH_weather_data_30yr_roll_avg <- paste(PATH_input, "Weather/weather_data_by_peattype.csv", sep = "")

# Area data parameters (process_area_data.R)
  
  PATH_area_script = paste(PATH_main, "process_area_data.R", sep = "") # main file
  #PATH_ghgi_area =   paste(PATH_input, "lulucf_rem_kptyy_tkang_ojlk.csv", sep = "")
  PATH_ghgi_area =   paste(PATH_ghgi, "2021/areas/lulucf/results/lulucf_rem_kptyy_tkang_ojlk.txt", sep = "")
  PATH_total_area = paste(PATH_input, "total_area.csv", sep = "")
  
# Peat degradation (peat_degredation.R)

  PATH_peat_decomposition_script = paste(PATH_main, "peat_decomp.R", sep = "") # main file
  PATH_peat_decomposition = paste(PATH_midresults, "peat_decomposition.csv", sep = "" )
  
# Below ground litter
  
  PATH_below_ground_litter_script = paste(PATH_main, "litter_below_ground.R", sep = "") # main file
  PATH_below_ground_litter_basal_data = paste(PATH_input, "Below_ground_litter/basal_areas_by_treetype.csv", sep = "") # basal area data file
  PATH_below_ground_litter_by_peat_type = paste(PATH_midresults, "below_ground_litter_production_by_peat_type.csv", sep = "" )
  PATH_below_ground_litter_total = paste(PATH_midresults, "below_ground_litter_production_total.csv", sep = "" )

# Above ground litter
  
  PATH_above_ground_litter_script = paste(PATH_main, "litter_above_ground.R", sep = "") # main file
  PATH_above_ground_litter_total = paste(PATH_midresults, "above_ground_litter_production_total.csv", sep = "" )
  
# Emission factor
  
  PATH_ef_script = paste(PATH_main, "emission_factor.R", sep = "") # main file
  PATH_ef_lognat_mortality = paste(PATH_input, "lognat_mortality.csv", sep = "") # Natural and logging mortality estimated with Yasso07
  PATH_ef_emission_factor = paste(PATH_midresults, "emission_factor.csv", sep = "" )
  PATH_ef_emission_factor_weighted = paste(PATH_midresults, "emission_factor_weighted.csv", sep = "")
  
# Total emissions
  
  PATH_total_script = paste(PATH_main, "total_emission.R", sep = "") # main file
  PATH_total_soil_carbon = paste(PATH_results, "soil_carbon_balance.csv", sep = "")
  PATH_total_soil_carbon_by_peattype = paste(PATH_results, "soil_carbon_balance_by_peattype.csv", sep = "")
  PATH_total_soil_carbon_total = paste(PATH_results, "soil_carbon_balance_total.csv", sep = "")
  
  
# This whole section is used only for sensitivity analysis  
    
if (PARAM_debug) {
    
  if (PARAM_scenario == 1) {
    PATH_basal_area_data = paste(PATH_scenario, "basal_areas_scenario.csv", sep = "") # basal area data file
    PATH_below_ground_litter_basal_data = paste(PATH_scenario, "basal_areas_by_treetype_scenario.csv", sep = "") # basal area data file 
    
    PATH_ef_lognat_mortality = paste(PATH_scenario, "lognat_mortality_scenarioPPA.csv", sep = "")
  }

  if (PARAM_scenario == 2) {
    PATH_weather_data_30yr_roll_avg <- paste(PATH_scenario, "weather_data_by_peattype_scenario.csv", sep = "")
    PATH_ef_lognat_mortality = paste(PATH_scenario, "lognat_mortality_scenarioTEMP.csv", sep = "")
  }

  if (PARAM_scenario == 3) {
    PATH_weather_data_30yr_roll_avg <- paste(PATH_scenario, "weather_data_by_peattype_scenario.csv", sep = "")
    
    PATH_basal_area_data = paste(PATH_scenario, "basal_areas_scenario.csv", sep = "") # basal area data file
    PATH_below_ground_litter_basal_data = paste(PATH_scenario, "basal_areas_by_treetype_scenario.csv", sep = "") # basal area data file 
    PATH_ef_lognat_mortality = paste(PATH_scenario, "lognat_mortality_scenarioBOTH.csv", sep = "")
  }
  
  if (PARAM_scenario == 4) {
    PATH_weather_data_aggregated <- paste(PATH_scenario, "loess_weather.csv", sep = "")
    PATH_weather_data_30yr_roll_avg  <- paste(PATH_scenario, "loess_weather.csv", sep = "")
  }
  
}
    