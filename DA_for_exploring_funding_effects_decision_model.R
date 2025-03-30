# Decision Model used to compute the Results in:
#"Agroforestry adoption in Germany: using decision analysis to explore the impact of funding mechanisms on system profitability"

# Packages needed ####

# install.packages("decisionSupport")
library(decisionSupport)

#Defining the probabilistic model
#!!NOTE:variables that are in all lower case are from the input_table.csv and the rest (with any upper case letter) are defined and calculated within the code
AF_benefit_with_Risks <- function(x, varnames)
{
  #System modulators ####
  
  #yield failure due to weather events
  
  Arable_yield_if_extreme_weather <- vv(value_if_extreme_weather, var_CV = var_cv, n = n_years, lower_limit = 0.8, upper_limit = 0.9)
  AF_arable_yield_if_extreme_weather <- vv(Arable_yield_if_extreme_weather, var_CV = var_cv, n = n_years, absolute_trend = trees_yield_buffering_effect_trend, lower_limit = 0.8, upper_limit = 1)
  
  Value_if_not <- rep(1, n_years)
  
  
  Yield_reduction_due_to_weather <-
    chance_event(chance = chance_extreme_weather,
                 value_if = Arable_yield_if_extreme_weather,
                 value_if_not = Value_if_not) # 5-30% chance that the event will occur and result in 10-20% yield reduction
  AF_yield_reduction_due_to_weather <-
    chance_event(chance = chance_extreme_weather,
                 value_if = AF_arable_yield_if_extreme_weather,
                 value_if_not = Value_if_not) # 5-30 chance that the event will occur and result in 10-20% yield reduction at first, gradually reducing to 1-15% yield reduction as system matures
  
  Apple_yield_reduction_due_to_weather <- AF_yield_reduction_due_to_weather
  
  #-----------------------------------------------------------------------------------------------------------
  #Introduction of variables, which are not system specific
  #Indices: represent the placement of each crop within the crop rotation, every fifth year the crop rotation repeats.
  #This way the time frame over which the crop rotation will be simulated can be changed by changing the value of "n_years" in the input table
  Maize_indices <- seq(from = 1, to = n_years, by = 4)
  Wheat_indices <- seq(from = 2, to = n_years, by = 4)
  Barley_indices <- seq(from = 3, to = n_years, by = 4)
  Rapeseed_indices <- seq(from = 4, to = n_years, by = 4)
  
  #Labour hours needed to manage crops (per ha per year)
  Maize_labour <- rep(0, n_years)
  Maize_labour[Maize_indices] <- vv(maize_labour, cv_maize_labour, length(Maize_indices))
  Wheat_labour <- rep(0, n_years)
  Wheat_labour[Wheat_indices] <- vv(wheat_labour, cv_wheat_labour, length(Wheat_indices)) 
  Barley_labour <- rep(0, n_years)
  Barley_labour[Barley_indices] <- vv(barley_labour, cv_barley_labour, length(Barley_indices))
  Rapeseed_labour <- rep(0, n_years)
  Rapeseed_labour[Rapeseed_indices] <- vv(rapeseed_labour, cv_rapeseed_labour, length(Rapeseed_indices))
  #----------------------------------------------------------------------------------------------------------- 
  
  #BASELINE SYSTEM - TREELESS ARABLE AGRICULTURE ####
  
  #Cost of managing arable system
  #No investment cost considered, since the arable system is already established and running.
  #The model depicts the implementation of an AF system into an already existing arable system. 
  
  #Running cost ####
  
  #First creating a vector, with as many zeros as there are years in the simulation, indicated by the value of "n_years" in the input table
  #Then, filling the vector with the cost-specific values at specific positions of the vector (determined by the "crop indices".
  #These positions correspond with the crops position within the crop rotation.
  #Adding up all crop-specific costs [€] to generate a value for the total cost associated with cultivating a specific crop
  
  #maize
  Treeless_maize_sowing_cost <- rep(0, n_years)#create vector with as many zeros as number of years in simulation
  Treeless_maize_sowing_cost[Maize_indices] <- vv(maize_seed_price, cv_maize_seed_price, length(Maize_indices)) * arable_area_treeless #fill vector with values at specific positions corresponding to the years maize is present in the crop rotation #cost of seed [€/ha]*area managed [ha]
  
  Treeless_maize_fertilizer_cost <- rep(0, n_years)
  Treeless_maize_fertilizer_cost[Maize_indices] <- vv(maize_fert_price, cv_maize_fert_price, length(Maize_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_maize_pesticide_cost <- rep(0, n_years)
  Treeless_maize_pesticide_cost[Maize_indices] <- vv(maize_cides_price, cv_maize_cides_price, length(Maize_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_maize_machinery_cost <- rep(0, n_years)
  Treeless_maize_machinery_cost[Maize_indices] <- vv(maize_mach_price, cv_maize_mach_price, length(Maize_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_maize_insurance_cost <- rep(0, n_years)
  Treeless_maize_insurance_cost[Maize_indices] <- vv(maize_insurance, cv_maize_insurance, length(Maize_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_maize_labour_cost <- rep(0, n_years)
  Treeless_maize_labour_cost <- Maize_labour * arable_area_treeless * labour_cost #Labour cost associated with maize cultivation in treeless system
  
  Treeless_total_maize_cost <- Treeless_maize_sowing_cost + Treeless_maize_fertilizer_cost + Treeless_maize_pesticide_cost + Treeless_maize_machinery_cost + Treeless_maize_insurance_cost + Treeless_maize_labour_cost
  
  #wheat
  Treeless_wheat_sowing_cost <- rep(0, n_years)
  Treeless_wheat_sowing_cost[Wheat_indices] <- vv(wheat_seed_price, cv_wheat_seed_price, length(Wheat_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  
  Treeless_wheat_fertilizer_cost <- rep(0, n_years)
  Treeless_wheat_fertilizer_cost[Wheat_indices] <- vv(wheat_fert_price, cv_wheat_fert_price, length(Wheat_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_wheat_pesticide_cost <- rep(0, n_years)
  Treeless_wheat_pesticide_cost[Wheat_indices] <- vv(wheat_cides_price, cv_wheat_cides_price, length(Wheat_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_wheat_machinery_cost <- rep(0, n_years)
  Treeless_wheat_machinery_cost[Wheat_indices] <- vv(wheat_mach_price, cv_wheat_mach_price, length(Wheat_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_wheat_insurance_cost <- rep(0, n_years)
  Treeless_wheat_insurance_cost[Wheat_indices] <- vv(wheat_insurance, cv_wheat_insurance, length(Wheat_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_wheat_labour_cost <- rep(0, n_years)
  Treeless_wheat_labour_cost <- Wheat_labour * arable_area_treeless * labour_cost #Labour cost associated with wheat cultivation in treeless system
  
  Treeless_total_wheat_cost <- Treeless_wheat_sowing_cost + Treeless_wheat_fertilizer_cost + Treeless_wheat_pesticide_cost + Treeless_wheat_machinery_cost + Treeless_wheat_insurance_cost + Treeless_wheat_labour_cost
  
  #barley
  Treeless_barley_sowing_cost <- rep(0, n_years)
  Treeless_barley_sowing_cost[Barley_indices] <- vv(barley_seed_price, cv_barley_seed_price, length(Barley_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  
  Treeless_barley_fertilizer_cost <- rep(0, n_years)
  Treeless_barley_fertilizer_cost[Barley_indices] <- vv(barley_fert_price, cv_barley_fert_price, length(Barley_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_barley_pesticide_cost <- rep(0, n_years)
  Treeless_barley_pesticide_cost[Barley_indices] <- vv(barley_cides_price, cv_barley_cides_price, length(Barley_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_barley_machinery_cost <- rep(0, n_years)
  Treeless_barley_machinery_cost[Barley_indices] <- vv(barley_mach_price, cv_barley_mach_price, length(Barley_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_barley_insurance_cost <- rep(0, n_years)
  Treeless_barley_insurance_cost[Barley_indices] <- vv(barley_insurance, cv_barley_insurance, length(Barley_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_barley_labour_cost <- rep(0, n_years)
  Treeless_barley_labour_cost <- Barley_labour * arable_area_treeless * labour_cost #Labour cost associated with barley cultivation in treeless system
  
  Treeless_total_barley_cost <- Treeless_barley_sowing_cost + Treeless_barley_fertilizer_cost + Treeless_barley_pesticide_cost + Treeless_barley_machinery_cost + Treeless_barley_insurance_cost + Treeless_barley_labour_cost
  
  #rapeseed
  Treeless_rapeseed_sowing_cost <- rep(0, n_years)
  Treeless_rapeseed_sowing_cost[Rapeseed_indices] <- vv(rapeseed_seed_price, cv_rapeseed_seed_price, length(Rapeseed_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  
  Treeless_rapeseed_fertilizer_cost <- rep(0, n_years)
  Treeless_rapeseed_fertilizer_cost[Rapeseed_indices] <- vv(rapeseed_fert_price, cv_rapeseed_fert_price, length(Rapeseed_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_rapeseed_pesticide_cost <- rep(0, n_years)
  Treeless_rapeseed_pesticide_cost[Rapeseed_indices] <- vv(rapeseed_cides_price, cv_rapeseed_cides_price, length(Rapeseed_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_rapeseed_machinery_cost <- rep(0, n_years)
  Treeless_rapeseed_machinery_cost[Rapeseed_indices] <- vv(rapeseed_mach_price, cv_rapeseed_mach_price, length(Rapeseed_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_rapeseed_insurance_cost <- rep(0, n_years)
  Treeless_rapeseed_insurance_cost[Rapeseed_indices] <- vv(rapeseed_insurance, cv_rapeseed_insurance, length(Rapeseed_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_rapeseed_labour_cost <- rep(0, n_years)
  Treeless_rapeseed_labour_cost <- Rapeseed_labour * arable_area_treeless * labour_cost #Labour cost associated with rapeseed cultivation in treeless system
  
  Treeless_total_rapeseed_cost <- Treeless_rapeseed_sowing_cost + Treeless_rapeseed_fertilizer_cost + Treeless_rapeseed_pesticide_cost + Treeless_rapeseed_machinery_cost + Treeless_rapeseed_insurance_cost + Treeless_rapeseed_labour_cost
  
  #total cost of arable component of the AF system
  Treeless_total_arable_management_cost <- Treeless_total_maize_cost + Treeless_total_wheat_cost + Treeless_total_barley_cost + Treeless_total_rapeseed_cost
  
  #Benefits treeless system ####
  
  # Arable system is managed with crop rotation of Maize(CCM)-Wheat-Barley-Rapeseed -> one crop is grown once every 4 years 
  #First creating a vector, with as many zeros as there are years in the simulation, indicated by the value of "n_years" in the input table
  #Then, fill the vector with the yield values at specific positions of the vector. These positions correspond with the crops position within the crop rotation
  #Lastly, multiply the yield of each crop with the value of the respective crop to calculate the benefit/revenue 
  
  Treeless_maize_yield <- rep(0, n_years) #create vector with as many zeros as number of years in simulation
  Treeless_maize_yield[Maize_indices] <- vv(maize_yield, cv_maize_yield, length(Maize_indices)) * arable_area_treeless #fill vector with values at specific positions corresponding to the years maize is present in the crop rotation
  Treeless_maize_benefit <- vv(maize_value, cv_maize_value, n_years) * Treeless_maize_yield * Yield_reduction_due_to_weather#calculate the benefit, i.e. revenue #The possibility of a yield reduction due to extreme weather is inegrated here
  
  Treeless_wheat_yield <- rep(0, n_years)
  Treeless_wheat_yield[Wheat_indices] <- vv(wheat_yield, cv_wheat_yield, length(Wheat_indices)) * arable_area_treeless
  Treeless_wheat_benefit <- vv(wheat_value, cv_wheat_value, n_years) * Treeless_wheat_yield * Yield_reduction_due_to_weather
  
  Treeless_barley_yield <- rep(0, n_years)
  Treeless_barley_yield[Barley_indices] <- vv(barley_yield, cv_barley_yield, length(Barley_indices)) * arable_area_treeless
  Treeless_barley_benefit <- vv(barley_value, cv_barley_value, n_years) * Treeless_barley_yield * Yield_reduction_due_to_weather
  
  Treeless_rapeseed_yield <- rep(0, n_years)
  Treeless_rapeseed_yield[Rapeseed_indices] <- vv(rapeseed_yield, cv_rapeseed_yield, length(Rapeseed_indices)) * arable_area_treeless
  Treeless_rapeseed_benefit <- vv(rapeseed_value, cv_rapeseed_value, n_years) * Treeless_rapeseed_yield * Yield_reduction_due_to_weather
  
  Treeless_total_benefit <- Treeless_maize_benefit + Treeless_wheat_benefit + Treeless_barley_benefit + Treeless_rapeseed_benefit
  
  #Treeless system bottom line ####
  Treeless_bottom_line_benefit <- Treeless_total_benefit - Treeless_total_arable_management_cost 
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the baseline system
  
  NPV_treeless_system <- discount(Treeless_total_benefit, discount_rate = discount_rate,
                                  calculate_NPV = TRUE) #NVP of treeless arable system 
  Treeless_cash_flow <- discount(Treeless_total_benefit, discount_rate = discount_rate,
                                 calculate_NPV = FALSE) #Cash flow of treeless system
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of treeless system
  #-----------------------------------------------------------------------------------------------------------  
  
  # AGROFORESTRY SYSTEM ####
  
  #Calculating AF benefits####
  
  #Apples in AF system ####
  
  #First apple yield estimated to happen in year 4 or 5 (according to farmer)
  Time_to_first_apple <- chance_event(chance = 0.5, value_if = time_to_first_apple1, value_if_not = time_to_first_apple2, n = 1)
  
  #Second yield stage is adapted from data received from experts on intensive apple plantations. Second yield stage is estimated to set in in year 7 or 8. 
  Time_to_second_apple <- chance_event(chance = 0.5, value_if = time_to_second_apple1, value_if_not = time_to_second_apple2, n = 1)
  
  #Yield of one apple tree [kg/tree]
  AF_apple_yield <- rep(0, n_years)
  AF_apple_yield <- gompertz_yield(max_harvest = apple_yield_max,
                                   time_to_first_yield_estimate = Time_to_first_apple,
                                   time_to_second_yield_estimate = Time_to_second_apple,
                                   first_yield_estimate_percent = apple_yield_first,
                                   second_yield_estimate_percent = apple_yield_second,
                                   n_years=n_years,
                                   var_CV = var_cv,
                                   no_yield_before_first_estimate = TRUE)
  #Yield of 473 apple trees [kg]  
  AF_tot_apple_yield <- (AF_apple_yield - AF_apple_yield * apple_postharvest_loss) * num_trees *Apple_yield_reduction_due_to_weather #Post-harvest losses and possibility of yield reduction due to extreme weather integrated here.
  #Calculate how many kg have table apple quality and can therefore be marketed at a highest price in percentage
  Pc_table_apples <- vv(perc_table_apple, var_CV = var_cv, n_years)/100
  
  Table_apple_yield <- AF_tot_apple_yield * Pc_table_apples #amount of highest quality apples [kg]
  
  Lower_qual_apple_yield <- AF_tot_apple_yield * (1-Pc_table_apples) #rest of yield is classified as lower quality
  
  Pc_b_qual_apple <- vv(perc_bqual_apple, var_CV = var_cv, n_years)/100 #B-quality apples can still be sold in direct selling operation, but at significantly lower price
  
  B_qual_table_apple_yield <- Lower_qual_apple_yield * Pc_b_qual_apple #amount of  B-quality apples [kg]
  
  Juice_apple_yield <- Lower_qual_apple_yield * (1-Pc_b_qual_apple) #Rest of the apple yield can be sold as juicing apples at lowest price 
  
  #The benefits from table apples and juice apples are calculated by multiplying their yields by their respective prices  
  Table_apple_benefit <- Table_apple_yield * table_apple_price
  B_qual_apple_benefit <- B_qual_table_apple_yield * bqual_apple_price
  Juice_apple_benefit <-  Juice_apple_yield * juice_apple_price
  
  AF_apple_benefit <- Table_apple_benefit + B_qual_apple_benefit + Juice_apple_benefit
  
  #Arable crop component in AF system ####
  
  AF_maize_yield <- rep(0, n_years)
  AF_wheat_yield <- rep(0, n_years)
  AF_barley_yield <- rep(0, n_years)
  AF_rapeseed_yield <- rep(0, n_years)
  
  # account for yield reduction due to shading and competition from trees 
  perc_yield_reduction <- gompertz_yield(
    max_harvest = yield_reduc_max,
    time_to_first_yield_estimate = time_to_first_reduction,
    time_to_second_yield_estimate = time_to_second_reduction,
    first_yield_estimate_percent = perc_max_first_reduction,
    second_yield_estimate_percent = perc_max_second_reduction,
    n_years = n_years)
  
  #Crop rotation in AF system
  
  #Calculating what percentage of arable field remains in AF system.
  #This way, the calculated values from the treeless system (Treeless_maize_yield, Treeless_maize_fertilizer_cost etc.) can be used to calculate the values for the AF system.
  #This ensures, that exact same values for the variables from the input table are used in the calculations, which ensures max. comparability between scenarios.
  AF_arable_area_perc <- (arable_area_treeless - tree_row_area)/arable_area_treeless 
  
  
  AF_maize_yield <- Treeless_maize_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather *(1 - perc_yield_reduction)
  AF_maize_benefit <- AF_maize_yield * maize_value
  
  AF_wheat_yield <- Treeless_wheat_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather*(1 - perc_yield_reduction)
  AF_wheat_benefit <- AF_wheat_yield * wheat_value
  
  AF_barley_yield <- Treeless_barley_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather*(1 - perc_yield_reduction)
  AF_barley_benefit<- AF_barley_yield * barley_value
  
  AF_rapeseed_yield <- Treeless_rapeseed_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather*(1 - perc_yield_reduction)
  AF_rapeseed_benefit <- AF_rapeseed_yield * rapeseed_value
  
  #Subsidy in AF system
  ES3_subsidy <- rep(0, n_years)
  ES3_subsidy[1:n_years] <- es3_subsidy * tree_row_area
  
  #Calculating costs in AF system ####
  #First creating vector, with as many zeros as there are years in the simulation, indicated by the value of "n_years" in the input table
  #Then, filling the vector with the cost-specific values
  #Adding up all costs to generate a value for the total investment cost
  
  #AF investment costs
  #Planning and consulting
  
  AF_planning_cost <- rep(0, n_years) #Invoice of service provider (planners/consultants), planning the AF system [€] + amount of time spent by the farmer to conceptualize the system
  AF_planning_cost[1] <- planning_consulting + farmer_planning_time * labour_cost
  
  AF_pruning_course <- rep(0, n_years) #Costs of the pruning training of one employee [€]
  AF_pruning_course[1] <- pruning_course
  
  #Field prep
  
  AF_gps_measuring <- rep(0, n_years) #First step of implementation: measuring tree strips using GPS[€]
  AF_gps_measuring[1] <- gps_field_measuring * labour_cost
  
  AF_dig_plant_holes <- rep(0, n_years) #Second step of implementation: digging/drilling holes for the trees [€]
  AF_dig_plant_holes[1] <- dig_planting_holes * labour_cost
  
  AF_tree_cost <- rep(0, n_years) #Cost per tree [€]
  AF_tree_cost[1] <- appletree_price * num_trees
  
  AF_plant_tree_cost <- rep(0, n_years) #Labour cost for planting one tree [€]
  AF_plant_tree_cost[1] <- planting_trees * labour_cost
  
  AF_vole_protect_cost <- rep(0, n_years) #Material cost of vole protection mesh [€]
  AF_vole_protect_cost[1] <- vole_protection * num_trees
  
  AF_deer_protect_cost <- rep(0, n_years) #Material cost of deer protection mesh [€]
  AF_deer_protect_cost[1] <- deer_protection * num_trees
  
  AF_weed_protect_cost <- rep(0, n_years) #Material cost of weed suppressing fleece [€]
  AF_weed_protect_cost[1] <- weed_protection * num_trees
  
  AF_compost_cost <- rep(0, n_years) #Cost of compost used during planting [€]
  AF_compost_cost[1] <- compost_after_planting * compost_price * num_trees
  
  AF_irrigation_system_cost <- rep(0, n_years) #Material and labour cost of installing a drip irrigation system in the tree rows [€]
  AF_irrigation_system_cost[1] <- irrigation_sys_install
  
  Irrigation_repair_indices <- sample(1:n_years, size = round(n_years * chance_irrigation_repair), replace = FALSE)
  
  AF_irrigation_system_repair_cost <- rep(0, n_years)
  AF_irrigation_system_repair_cost[Irrigation_repair_indices] <- AF_irrigation_system_cost[1] * irrigation_repair_cost
  
  AF_irrigation_after_planting_cost <- rep(0, n_years) #Cost for watering in newly planted trees [€]
  AF_irrigation_after_planting_cost[1] <- irrigation_after_planting * water_price * num_trees
  
  AF_total_planting_cost <- AF_gps_measuring + AF_dig_plant_holes + AF_tree_cost + AF_plant_tree_cost + AF_vole_protect_cost + AF_deer_protect_cost + AF_weed_protect_cost + AF_compost_cost + AF_irrigation_system_cost + AF_irrigation_after_planting_cost #All costs associated with planting
  
  AF_total_investment_cost <- AF_planning_cost + AF_pruning_course + AF_total_planting_cost #Investment cost of AF system implementation
  
  #Running costs
  ES3_application <- rep(0, n_years) #Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  ES3_application[1] <- es3_application * labour_cost #application for Eco Scheme subsidy has to be repeated annually, but the first time takes considerably longer, since utilisation concept has to be established
  ES3_application[2:n_years] <- es3_application*0.1 * labour_cost
  
  AF_pruning <- rep(0, n_years)#Labour cost of pruning fruit trees [€]
  
  AF_pruning[1:5] <- pruning_juv1 * labour_cost * num_trees
  AF_pruning[6:10] <- pruning_juv2 * labour_cost * num_trees
  AF_pruning[11:15] <- pruning_adult1 * labour_cost * num_trees
  AF_pruning[16:n_years] <- pruning_adult2 * labour_cost * num_trees
  AF_pruning <- AF_pruning[1:n_years]
  
  AF_root_pruning <- rep(0, n_years) #Labour cost of pruning roots of trees next to tree rows [€]
  AF_root_pruning[1:n_years] <- root_pruning * labour_cost
  
  AF_annual_irrigation <- rep(0, n_years) #Cost of annual irrigation of tree rows [€]
  AF_annual_irrigation[1:3] <- irrigation_123
  AF_annual_irrigation[4:n_years] <- irrigation_annual
  AF_annual_irrigation_cost <- AF_annual_irrigation * water_price
  
  AF_codling_moth_protect <- rep(0, n_years) #Cost of hanging up pheromone dispensers for codling moth control (includes material and labour cost) [€]
  AF_codling_moth_protect[1:4] <- 0
  AF_codling_moth_protect[4:n_years] <- codling_moth_protect * tree_row_area
  
  AF_mowing_treerow <- rep(0, n_years) #Labour cost of mowing the tree rows manually [€]
  AF_mowing_treerow[1:n_years] <- mowing_treerow * tree_row_area * labour_cost
  
  AF_apple_harvest <- rep(0, n_years) #Labour cost of harvesting apples manually [€/kg]
  
  AF_apple_harvest[1:n_years] <- apple_harvest * AF_tot_apple_yield #cost calculated per kg of apple * total amount of apples in kg
  
  AF_total_treerow_management_cost <- ES3_application + AF_pruning + AF_root_pruning + AF_annual_irrigation_cost + AF_codling_moth_protect + AF_mowing_treerow + AF_apple_harvest
  
  #Management cost of arable component in AF system
  
  #Maize
  AF_maize_sowing_cost <- Treeless_maize_sowing_cost * AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_maize_fertilizer_cost <- Treeless_maize_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_maize_pesticide_cost <- Treeless_maize_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_maize_machinery_cost <- Treeless_maize_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_maize_insurance_cost <- Treeless_maize_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  
  #Labour cost associated with maize cultivation in AF.
  #Total labour time is estimated to increase by 5-30% communicated by the farmer (extra_arable_time/100) due to complicated navigation of machinery in AF system.
  AF_maize_labour_cost <- rep(0, n_years)
  AF_maize_labour_cost <- (Maize_labour + Maize_labour * (extra_arable_time/100)) * labour_cost * (arable_area_treeless*AF_arable_area_perc)
  
  AF_total_maize_cost <- AF_maize_sowing_cost + AF_maize_fertilizer_cost + AF_maize_pesticide_cost + AF_maize_machinery_cost + AF_maize_insurance_cost + AF_maize_labour_cost
  
  #Wheat
  AF_wheat_sowing_cost <- Treeless_wheat_sowing_cost*AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_wheat_fertilizer_cost <- Treeless_wheat_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_wheat_pesticide_cost <- Treeless_wheat_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_wheat_machinery_cost <- Treeless_wheat_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_wheat_insurance_cost <- Treeless_wheat_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  AF_wheat_labour_cost <- rep(0, n_years)
  AF_wheat_labour_cost <- (Wheat_labour + Wheat_labour * (vv(extra_arable_time, var_cv, n_years)/100)) * labour_cost * (arable_area_treeless*AF_arable_area_perc) #Labour cost associated with wheat cultivation
  
  AF_total_wheat_cost <- AF_wheat_sowing_cost + AF_wheat_fertilizer_cost + AF_wheat_pesticide_cost + AF_wheat_machinery_cost + AF_wheat_insurance_cost + AF_wheat_labour_cost
  
  #Barley
  AF_barley_sowing_cost <- Treeless_barley_sowing_cost*AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_barley_fertilizer_cost <- Treeless_barley_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_barley_pesticide_cost <- Treeless_barley_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_barley_machinery_cost <- Treeless_barley_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_barley_insurance_cost <- Treeless_barley_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  AF_barley_labour_cost <- rep(0, n_years)
  AF_barley_labour_cost <- (Barley_labour + Barley_labour * (vv(extra_arable_time, var_cv, n_years)/100)) * labour_cost * (arable_area_treeless*AF_arable_area_perc) #Labour cost associated with barley cultivation
  
  AF_total_barley_cost <- AF_barley_sowing_cost + AF_barley_fertilizer_cost + AF_barley_pesticide_cost + AF_barley_machinery_cost + AF_barley_insurance_cost + AF_barley_labour_cost
  
  #Rapeseed
  AF_rapeseed_sowing_cost <- Treeless_rapeseed_sowing_cost*AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_rapeseed_fertilizer_cost <- Treeless_rapeseed_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_rapeseed_pesticide_cost <- Treeless_rapeseed_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_rapeseed_machinery_cost <- Treeless_rapeseed_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_rapeseed_insurance_cost <- Treeless_rapeseed_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  AF_rapeseed_labour_cost <- rep(0, n_years)
  AF_rapeseed_labour_cost <- (Rapeseed_labour + Rapeseed_labour * (vv(extra_arable_time, var_cv, n_years)/100)) * labour_cost * (arable_area_treeless*AF_arable_area_perc) #Labour cost associated with rapeseed cultivation
  
  AF_total_rapeseed_cost <- AF_rapeseed_sowing_cost + AF_rapeseed_fertilizer_cost + AF_rapeseed_pesticide_cost + AF_rapeseed_machinery_cost + AF_rapeseed_insurance_cost + AF_rapeseed_labour_cost
  
  
  AF_total_arable_management_cost <- AF_total_maize_cost + AF_total_wheat_cost + AF_total_barley_cost + AF_total_rapeseed_cost #Total cost of arable component in AF system
  
  AF_total_running_cost <- AF_total_treerow_management_cost + AF_total_arable_management_cost #Total running cost of AF system
  
  AF_total_cost <- AF_total_investment_cost + AF_total_running_cost #Total cost of AF system
  
  
  #Agroforestry bottom line ####
  AF_total_benefit <- AF_apple_benefit + AF_maize_benefit + AF_wheat_benefit + AF_barley_benefit + AF_rapeseed_benefit + ES3_subsidy
  
  AF_bottom_line_benefit <- AF_total_benefit - AF_total_cost
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  #AF System
  AF_NPV <- discount(AF_bottom_line_benefit, discount_rate=discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow <- discount(AF_bottom_line_benefit,discount_rate=discount_rate,
                           calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  Decision_benefit <- AF_bottom_line_benefit - Treeless_bottom_line_benefit
  NPV_decision <- discount(Decision_benefit, discount_rate = discount_rate,
                           calculate_NPV = TRUE ) #NPV of the decision
  CF_decision <- discount(Decision_benefit, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision <- cumsum(CF_decision) #Cumulative cash flow of the decision
  #-----------------------------------------------------------------------------------------------------------  
  
  #CREATING THE FUNDING SCENARIOS#######################################################################
  #The default scenario includes ES 3
  #Scenario 0: No funding at all. ####
  #All other scenarios contain at least the annual support through ES 3. 
  AF_total_benefit_no_fund <- AF_apple_benefit + AF_maize_benefit + AF_wheat_benefit + AF_barley_benefit + AF_rapeseed_benefit
  
  AF_total_cost_no_fund <- AF_total_cost - ES3_application #No time has to be invested into applying for ES 3 funding 
  
  AF_bottom_line_benefit_no_fund <- AF_total_benefit_no_fund - AF_total_cost_no_fund
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system without ES 3 funding
  #AF System
  AF_NPV_no_fund <- discount(AF_bottom_line_benefit_no_fund, discount_rate=discount_rate,
                             calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow_no_fund <- discount(AF_bottom_line_benefit_no_fund,discount_rate=discount_rate,
                                   calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow_no_fund <- cumsum(AF_cash_flow_no_fund) #Cumulative cash flow of AF system
  
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision
  Decision_benefit_no_fund <- AF_bottom_line_benefit_no_fund - Treeless_bottom_line_benefit
  NPV_decision_no_fund <- discount(Decision_benefit_no_fund, discount_rate = discount_rate,
                                   calculate_NPV = TRUE ) #NPV of the decision
  CF_decision_no_fund <- discount(Decision_benefit_no_fund, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision_no_fund <- cumsum(CF_decision_no_fund) #Cumulative cash flow of the decision
  
  
  
  #Federal State Investment Support Schemes:
  # Scenario 1: "NI-Subsidy" ####
  #Investment funding of Lower Saxony (NI) - 40 % of eligible investment cost (planning and consulting is not considered eligible investment cost!), capped at 20,000 €, only for silvoarable systems
  NI_sub_application <- rep(0, n_years)
  NI_sub_application[1] <- annual_sub_application*labour_cost # time spent on applying * per hour labour cost
  NI_invest_sub <- rep(0, n_years)
  NI_invest_sub[1] <- ni_invest_sub_max # variable in input table as it is susceptible to change in future
  AF_total_cost_NI <- AF_total_cost
  
  if (arable ==1){
    AF_total_cost_NI <- ifelse((AF_total_investment_cost-AF_planning_cost)*0.4 > 20000, AF_total_investment_cost-NI_invest_sub + NI_sub_application + AF_total_running_cost, (AF_total_investment_cost-AF_planning_cost)*0.6 + AF_planning_cost + NI_sub_application + AF_total_running_cost)} else {
      AF_total_cost_NI <- AF_total_cost
    }
  
  #Total cost, in Scenario "NI-Subsidy" -> important to note: planning and consulting is not considered an eligible investment cost by Lower Saxony's regulation hence the subtraction in the function
  
  AF_bottom_line_benefit_NI <- AF_total_benefit - AF_total_cost_NI#Bottom line, in Scenario "NI-Subsidy"
  AF_NPV_NI <- discount(AF_bottom_line_benefit_NI, discount_rate=discount_rate,
                        calculate_NPV = TRUE)#NVP of AF system, in Scenario "NI-Subsidy"
  AF_cash_flow_NI <- discount(AF_bottom_line_benefit_NI,discount_rate=discount_rate,
                              calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "NI-Subsidy"
  AF_cum_cash_flow_NI <- cumsum(AF_cash_flow_NI)#Cumulative cash flow of AF system, in Scenario "NI-Subsidy"
  
  #Decision NI (difference between AF system with NI-subsidy and treeless baseline system)
  Decision_benefit_NI <- AF_bottom_line_benefit_NI - Treeless_bottom_line_benefit
  NPV_decision_NI <- discount(Decision_benefit_NI, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "NI-Subsidy"
  CF_decision_NI <- discount(Decision_benefit_NI, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "NI-Subsidy"
  CumCF_decision_NI <- cumsum(CF_decision_NI) #Cumulative cash flow of the decision, in Scenario "NI-Subsidy"
  
  
  # Scenario 2: "BY-Subsidy" ####
  #Investment funding of Bavaria (BY) - max. 65 % of eligible investment cost, capped at 50,000 €, furthermore, payment per ha of tree row dependent on type of tree row
  BY_sub_application <- rep(0, n_years)
  BY_sub_application[1] <- annual_sub_application*labour_cost
  BY_invest_SRC <- rep(0, n_years)
  BY_invest_SRC[1] <- by_invest_src
  BY_invest_shrubs <- rep(0, n_years)
  BY_invest_shrubs[1] <- by_invest_shrubs
  BY_invest_timber_food <- rep(0, n_years)
  BY_invest_timber_food[1] <- by_invest_timb_food
  BY_invest_sub <- rep(0, n_years)
  
  #Funding support is capped at 65 % of eligible funding, while also differentiating between different type of tree rows. Only alley cropping systems are eligible for funding. The following function first asks, if system is alley cropping system, than for the type of tree row and then calculates the respective funding sum.
  if (alley_crop == 1){
    if(treerow_SRC == 1) {
      BY_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > BY_invest_SRC, BY_invest_SRC, (AF_total_investment_cost/tree_row_area)*0.65)
    } else if (treerow_shrubs == 1) {
      BY_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > BY_invest_shrubs,
                              BY_invest_shrubs, (AF_total_investment_cost/tree_row_area)*0.65)
    } else if (treerow_timber_food == 1) {
      BY_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > BY_invest_timber_food,
                              BY_invest_timber_food, (AF_total_investment_cost/tree_row_area)*0.65)
    } else if (treerow_SRC == 0 && treerow_shrubs == 0 && treerow_timber_food == 0) {
      BY_invest_sub <- 0 
    }} else {
      BY_invest_sub <- 0
    }
  
  
  #Requested investment support funding must be min. 2,500 € and max. 50,000 €
  BY_invest_sub <- ifelse(BY_invest_sub < 2500, 0, BY_invest_sub)
  BY_invest_sub <- ifelse(BY_invest_sub > 50000, 50000, BY_invest_sub)
  BY_total_invest_sub <- BY_invest_sub*tree_row_area - BY_sub_application
  
  #Calculating bottom line, NPV and Cash Flow
  AF_bottom_line_benefit_BY <- AF_total_benefit + BY_total_invest_sub - AF_total_cost#Bottom line, in Scenario "BY-Subsidy"
  AF_NPV_BY <- discount(AF_bottom_line_benefit_BY, discount_rate=discount_rate,
                        calculate_NPV = TRUE)#NVP of AF system, in Scenario "BY-Subsidy"
  AF_cash_flow_BY <- discount(AF_bottom_line_benefit_BY,discount_rate=discount_rate,
                              calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "BY-Subsidy"
  AF_cum_cash_flow_BY <- cumsum(AF_cash_flow_BY)#Cumulative cash flow of AF system, in Scenario "BY-Subsidy"
  
  #Decision BY (difference between AF system with BY-subsidy and treeless baseline system)
  Decision_benefit_BY <- AF_bottom_line_benefit_BY - Treeless_bottom_line_benefit
  NPV_decision_BY <- discount(Decision_benefit_BY, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "BY-Subsidy"
  CF_decision_BY <- discount(Decision_benefit_BY, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "BY-Subsidy"
  CumCF_decision_BY <- cumsum(CF_decision_BY) #Cumulative cash flow of the decision, in Scenario "BY-Subsidy"
  
  
  # Scenario 3: "MV-Subsidy" ####
  #Investment funding of Mecklenburg Western-Pomerania (MV) - max. 65 % of eligible investment cost, capped at 300,000 €, furthermore, payment per ha of tree row dependent on type of tree row
  MV_sub_application <- rep(0, n_years)
  MV_sub_application[1] <- annual_sub_application*labour_cost
  MV_invest_SRC <- rep(0, n_years)
  MV_invest_SRC[1] <- mv_invest_src
  MV_invest_shrubs <- rep(0, n_years)
  MV_invest_shrubs[1] <- mv_invest_shrubs
  MV_invest_timber_food <- rep(0, n_years)
  MV_invest_timber_food[1] <- mv_invest_timb_food
  MV_invest_sub <- rep(0, n_years)
  
  
  #Funding support is capped at 65 % of eligible funding, while also differentiating between different types of tree rows. The following function first asks for the type of tree row and then calculates the respective funding sum. 
  if (alley_crop == 1){
    if (treerow_SRC == 1) {
      MV_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > MV_invest_SRC,
                              MV_invest_SRC, (AF_total_investment_cost/tree_row_area)*0.65)
    } else if (treerow_shrubs == 1) {
      MV_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > MV_invest_shrubs,
                              MV_invest_shrubs, (AF_total_investment_cost/tree_row_area)*0.65)
    } else if (treerow_timber_food == 1) {
      MV_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > MV_invest_timber_food,
                              MV_invest_timber_food, (AF_total_investment_cost/tree_row_area)*0.65)
    } else if (treerow_SRC == 0 && treerow_shrubs == 0 && treerow_timber_food == 0) {
      MV_invest_sub <- 0 #1 and 0 in conditional functions act as operators "TRUE" and "FALSE" 
    }} else {
      MV_invest_sub <- 0
    }
  
  #Requested investment support funding must be min. 2,500 € and max. 50,000 €
  MV_invest_sub <- ifelse(MV_invest_sub < 2500, 0, MV_invest_sub)
  MV_invest_sub <- ifelse(MV_invest_sub > 300000, 300000, MV_invest_sub)
  MV_total_invest_sub <- MV_invest_sub*tree_row_area - MV_sub_application
  
  #Calculating bottom line, NPV and Cash Flow
  AF_bottom_line_benefit_MV <- AF_total_benefit + MV_total_invest_sub - AF_total_cost#Bottom line, in Scenario "MV-Subsidy"
  AF_NPV_MV <- discount(AF_bottom_line_benefit_MV, discount_rate=discount_rate,
                        calculate_NPV = TRUE)#NVP of AF system, in Scenario "MV-Subsidy"
  AF_cash_flow_MV <- discount(AF_bottom_line_benefit_MV, discount_rate=discount_rate,
                              calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "MV-Subsidy"
  AF_cum_cash_flow_MV <- cumsum(AF_cash_flow_MV)#Cumulative cash flow of AF system, in Scenario "MV-Subsidy"
  
  #Decision MV (difference between AF system with MV-subsidy and treeless baseline system)
  Decision_benefit_MV <- AF_bottom_line_benefit_MV - Treeless_bottom_line_benefit
  NPV_decision_MV <- discount(Decision_benefit_MV, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "MV-Subsidy"
  CF_decision_MV <- discount(Decision_benefit_MV, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "MV-Subsidy"
  CumCF_decision_MV <- cumsum(CF_decision_MV) #Cumulative cash flow of the decision, in Scenario "MV-Subsidy"
  
  
  # Scenario 4: "BW-Subsidy" ####
  #Investment funding of Baden-Württemberg (BW) - max. 80 % of eligible investment cost, capped at 1,500 €. Only cost considered eligible for funding: planning and consulting
  
  BW_sub_application <- rep(0, n_years)
  BW_sub_application[1] <- annual_sub_application*labour_cost #Vector with 30 elements, containing the value of labour cost spent on applying for subsidy
  AF_consulting <- rep(0, n_years) 
  AF_consulting[1] <- planning_consulting #Vector with 30 elements, containing the value of planning_consulting as first element
  BW_invest_sub <- rep(0, n_years)
  BW_invest_sub[1] <- bw_invest_max
  AF_total_cost_BW <- AF_total_cost
  
  AF_total_cost_BW <- ifelse(AF_consulting*0.8 > 1500,
                             AF_total_investment_cost-BW_invest_sub + BW_sub_application + AF_total_running_cost,
                             AF_total_investment_cost-AF_consulting*0.8 + BW_sub_application + AF_total_running_cost) #Total cost of AF system in scenario "BW Subsidy"
  
  
  AF_bottom_line_benefit_BW <- AF_total_benefit - AF_total_cost_BW #Bottom line, in Scenario "BW-Subsidy"
  AF_NPV_BW <- discount(AF_bottom_line_benefit_BW, discount_rate=discount_rate,
                        calculate_NPV = TRUE) #NVP of AF system, in Scenario "BW-Subsidy"
  AF_cash_flow_BW <- discount(AF_bottom_line_benefit_BW,discount_rate=discount_rate,
                              calculate_NPV = FALSE) #Cash flow of AF system, in Scenario "BW-Subsidy"
  AF_cum_cash_flow_BW <- cumsum(AF_cash_flow_BW) #Cumulative cash flow of AF system, in Scenario "BW-Subsidy"
  
  #Decision BW (difference between AF system with BW-subsidy and treeless baseline system)
  Decision_benefit_BW <- AF_bottom_line_benefit_BW - Treeless_bottom_line_benefit
  NPV_decision_BW <- discount(Decision_benefit_BW, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "BW-Subsidy"
  CF_decision_BW <- discount(Decision_benefit_BW, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "BW-Subsidy"
  CumCF_decision_BW <- cumsum(CF_decision_BW) #Cumulative cash flow of the decision, in Scenario "BW-Subsidy"
  
  
  # Scenario 5 "TH-Subsidy" ####
  #Investment funding of Thuringia (TH) - max. 100 % of eligible investment cost, capped at 3*2,000 €. Only cost considered eligible for funding: planning and consulting
  #Three AF-related consulting-topics are funded (representing 3 stages of AF planning, i.e. rough conception, detailed planning, establishment and management)
  
  TH_sub_application <- rep(0, n_years)
  TH_sub_application[1] <- annual_sub_application*labour_cost
  TH_invest_sub <- rep(0, n_years)
  TH_invest_sub[1] <- th_invest_max
  AF_total_cost_TH <- AF_total_cost

  Max_funding_TH <- rep(0, n_years)
  Max_funding_TH[1] <- max_funding_th
  
  # Total cost of AF system in scenario "TH Subsidy"
  AF_total_cost_TH[1] <- ifelse(
    AF_consulting[1] <= 2000,
    AF_total_investment_cost - AF_consulting + TH_sub_application + AF_total_running_cost,
    ifelse(
      AF_consulting[1] <= 4000,
      AF_total_investment_cost - AF_consulting + (TH_sub_application * 2) + AF_total_running_cost,
      ifelse(
        AF_consulting[1] <= 6000,
        AF_total_investment_cost - AF_consulting + (TH_sub_application * 3) + AF_total_running_cost,
        AF_total_investment_cost - Max_funding_TH + (TH_sub_application * 3) + AF_total_running_cost
      )
    )
  )
  
  AF_total_cost_TH[2:n_years] <- AF_total_cost_TH[2:n_years]  # Leave the rest untouched or assign specific values if required
  
  
  AF_bottom_line_benefit_TH <- AF_total_benefit - AF_total_cost_TH#Bottom line, in Scenario "TH-Subsidy"
  AF_NPV_TH <- discount(AF_bottom_line_benefit_TH, discount_rate=discount_rate,
                        calculate_NPV = TRUE)#NVP of AF system, in Scenario "TH-Subsidy"
  AF_cash_flow_TH <- discount(AF_bottom_line_benefit_TH,discount_rate=discount_rate,
                              calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "TH-Subsidy"
  AF_cum_cash_flow_TH <- cumsum(AF_cash_flow_TH)#Cumulative cash flow of AF system, in Scenario "TH-Subsidy"
  
  #Decision TH (difference between AF system with TH-subsidy and treeless baseline system)
  Decision_benefit_TH <- AF_bottom_line_benefit_TH - Treeless_bottom_line_benefit
  NPV_decision_TH <- discount(Decision_benefit_TH, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "TH-Subsidy"
  CF_decision_TH <- discount(Decision_benefit_TH, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "TH-Subsidy"
  CumCF_decision_TH <- cumsum(CF_decision_TH) #Cumulative cash flow of the decision, in Scenario "TH-Subsidy"
  
  
  # Scenario 6: "BB-Subsidy" ####
  #Investment funding of Brandenburg/Berlin-funding region - Investment support capped at 18h*85€/h = 1,530 €. Only cost considered eligible for funding: planning and consulting
  
  BB_sub_application <- rep(0, n_years)
  BB_sub_application[1] <- annual_sub_application*labour_cost #Vector with 30 elements, containing the value of labour cost spent on applying for subsidy
  AF_consulting <- rep(0, n_years) 
  AF_consulting[1] <- planning_consulting #Vector with 30 elements, containing the value of planning_consulting as first element
  BB_invest_sub <- rep(0, n_years)
  BB_invest_sub[1] <- bb_invest_max
  AF_total_cost_BB <- AF_total_cost
  
  AF_total_cost_BB <- ifelse(AF_consulting > 1530,
                             AF_total_investment_cost-BB_invest_sub + BB_sub_application + AF_total_running_cost,
                             AF_total_investment_cost-AF_consulting + BW_sub_application + AF_total_running_cost) #Total cost of AF system in scenario "BB Subsidy"

  AF_bottom_line_benefit_BB <- AF_total_benefit - AF_total_cost_BB #Bottom line, in Scenario "BB-Subsidy"
  AF_NPV_BB <- discount(AF_bottom_line_benefit_BB, discount_rate=discount_rate,
                        calculate_NPV = TRUE) #NVP of AF system, in Scenario "BB-Subsidy"
  AF_cash_flow_BB <- discount(AF_bottom_line_benefit_BB,discount_rate=discount_rate,
                              calculate_NPV = FALSE) #Cash flow of AF system, in Scenario "BB-Subsidy"
  AF_cum_cash_flow_BB <- cumsum(AF_cash_flow_BB) #Cumulative cash flow of AF system, in Scenario "BB-Subsidy"
  
  #Decision BB (difference between AF system with BB-subsidy and treeless baseline system)
  Decision_benefit_BB <- AF_bottom_line_benefit_BB - Treeless_bottom_line_benefit
  NPV_decision_BB <- discount(Decision_benefit_BB, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "BB-Subsidy"
  CF_decision_BB <- discount(Decision_benefit_BB, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "BB-Subsidy"
  CumCF_decision_BB <- cumsum(CF_decision_BB) #Cumulative cash flow of the decision, in Scenario "BB-Subsidy"
  
  
  # Scenario 7 "SN-Subsidy" ####
  #investment funding of Saxony (SN) - Certain agricultural investments can be funded with 20,000 - 5,000,000 € in 2023 to 2027, this includes silvoarable AF systems (and SRC on arable land). 40 % of eligible cost can be funded. 
  
  SN_sub_application <- rep(0, n_years)
  SN_sub_application[1] <- annual_sub_application*labour_cost
  
  AF_total_cost_SN <- AF_total_cost
  
  if (arable == 1){ 
    if (AF_total_investment_cost[1] - AF_planning_cost[1] >= sn_invest_min && #eligible cost must be min. 20,000 €
        AF_total_investment_cost[1] - AF_planning_cost[1] <= sn_invest_max)#investment support is officially limited to 5,000,000 €
    {
      AF_total_cost_SN <- AF_planning_cost + (AF_total_investment_cost-AF_planning_cost)*0.6  + SN_sub_application + AF_total_running_cost #40% funding
    } else if (AF_total_investment_cost[1] - AF_planning_cost[1] < sn_invest_min) {
      AF_total_cost_SN <- AF_total_cost
    } else if (AF_total_investment_cost[1] - AF_planning_cost[1] > sn_invest_max) {
      AF_total_cost_SN <- AF_total_investment_cost + SN_sub_application + AF_total_running_cost - sn_invest_max #5,000,000 € funding
    }}else{
      AF_total_cost_SN <- AF_total_cost
    }
  
  
  AF_bottom_line_benefit_SN <- AF_total_benefit - AF_total_cost_SN#Bottom line, in Scenario "SN-Subsidy"
  AF_NPV_SN <- discount(AF_bottom_line_benefit_SN, discount_rate=discount_rate,
                        calculate_NPV = TRUE)#NVP of AF system, in Scenario "SN-Subsidy"
  AF_cash_flow_SN <- discount(AF_bottom_line_benefit_SN,discount_rate=discount_rate,
                              calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "SN-Subsidy"
  AF_cum_cash_flow_SN <- cumsum(AF_cash_flow_SN)#Cumulative cash flow of AF system, in Scenario "SN-Subsidy"
  
  #Decision SN (difference between AF system with SN-subsidy and treeless baseline system)
  Decision_benefit_SN <- AF_bottom_line_benefit_SN - Treeless_bottom_line_benefit
  NPV_decision_SN <- discount(Decision_benefit_SN, discount_rate = discount_rate,
                              calculate_NPV = TRUE ) #NPV of the decision, in Scenario "SN-Subsidy"
  CF_decision_SN <- discount(Decision_benefit_SN, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "SN-Subsidy"
  CumCF_decision_SN <- cumsum(CF_decision_SN) #Cumulative cash flow of the decision, in Scenario "SN-Subsidy"
  
  # Scenario 8 "DeFAF-Subsidy" ####
  #Investment funding of 100 % for first 10 ha of wooded area, 80 % of additional 10 ha of wooded area and 50 % of every additional ha after 20 ha of total wooded area. 
  #Additionally: annual subsidy of 600 €/ha of wooded area.
  
  #DeFAF annual subsidy 
  
  DeFAF_ES3 <- es3_subsidy*3 - ES3_application #suggested level of ES3 subsidy
 
  DeFAF_annual_sub <- DeFAF_ES3
  
  #DeFAF Investment support funding scheme (first 10 ha 100 % funded, next 10 ha 80 % funded, every additional ha 50 % funded)
  
  #Calculate the investment cost per hectare
  Invest_cost_per_ha <- AF_total_investment_cost[1]/tree_row_area
  
  #Create a modified vector based on the conditions
  AF_total_invest_cost_DeFAF <- AF_total_investment_cost
  
  #If tree_row_area is equal or smaller than 10 ha, then 100 % of the investment cost is subsidised
  if (tree_row_area > 0 && tree_row_area <= 10) {
    AF_total_invest_cost_DeFAF[1] <- 0
    #If tree row area is over 10 ha, the first 10 ha are subsidised 100 %, but additional ha are subsidised differently
  } else if (tree_row_area > 10) {
    #Check if there's a remainder after deducting 10 hectares, since every additional ha up  to 20 ha are subsidised 80%
    Remainder <- tree_row_area - 10
    if (Remainder > 0 && Remainder <= 10) {
      #If remainder is smaller or equal to 10, deduct 80% of the cost for the remaining hectares
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - (10 * Invest_cost_per_ha) - (Remainder * Invest_cost_per_ha * 0.8)
    } else {
      #If remainder is greater than 10, deduct 10 hectares and the next 10 get 80% off and the rest 50% off
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - (10 * Invest_cost_per_ha) - (10 * Invest_cost_per_ha * 0.8) - ((Remainder - 10)*Invest_cost_per_ha * 0.5)
    }
  }
  
  #Ensure the modified investment cost is not negative (code above should not be able to create negative values but next line is added as insurance that negative investment cost is never included in the calculations)
  AF_total_invest_cost_DeFAF[1] <- max(0, AF_total_invest_cost_DeFAF[1])
  
  #Calculate total benefit of DeFAF-subsidy
  
  AF_total_cost_DeFAF <- AF_total_invest_cost_DeFAF + AF_total_running_cost - DeFAF_annual_sub - AF_mowing_treerow
  
  AF_bottom_line_benefit_DeFAF <- AF_total_benefit - AF_total_cost_DeFAF #Bottom line, in Scenario "DeFAF-Subsidy"
  AF_NPV_DeFAF <- discount(AF_bottom_line_benefit_DeFAF, discount_rate=discount_rate,
                           calculate_NPV = TRUE) #NVP of AF system, in Scenario "DeFAF-Subsidy"
  AF_cash_flow_DeFAF <- discount(AF_bottom_line_benefit_DeFAF,discount_rate=discount_rate,
                                 calculate_NPV = FALSE) #Cash flow of AF system, in Scenario "DeFAF-Subsidy"
  AF_cum_cash_flow_DeFAF <- cumsum(AF_cash_flow_DeFAF) #Cumulative cash flow of AF system, in Scenario "DeFAF-Subsidy"
  
  #Decision DeFAF (difference between AF system with DeFAF-subsidy and treeless baseline system)
  Decision_benefit_DeFAF <- AF_bottom_line_benefit_DeFAF - Treeless_bottom_line_benefit
  NPV_decision_DeFAF <- discount(Decision_benefit_DeFAF, discount_rate = discount_rate,
                                 calculate_NPV = TRUE ) #NPV of the decision, in Scenario "DeFAF-Subsidy"
  CF_decision_DeFAF <- discount(Decision_benefit_DeFAF, discount_rate = discount_rate, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "DeFAF-Subsidy"
  CumCF_decision_DeFAF <- cumsum(CF_decision_DeFAF) #Cumulative cash flow of the decision, in Scenario "DeFAF-Subsidy"
  
  #-----------------------------------------------------------------------------------------------------------  
  
  #Defining Monte Carlo output variables #####
  
  return(list(#NPV of the decision in different scenarios
    NPV_decis_AF_ES3 = NPV_decision,
    NPV_decis_no_fund = NPV_decision_no_fund,
    NPV_decis_NI = NPV_decision_NI,
    NPV_decis_BY = NPV_decision_BY,
    NPV_decis_MV = NPV_decision_MV,
    NPV_decis_SN = NPV_decision_SN,
    NPV_decis_BW = NPV_decision_BW,
    NPV_decis_TH = NPV_decision_TH,
    NPV_decis_BB = NPV_decision_BB,
    NPV_decis_DeFAF = NPV_decision_DeFAF,
    #NPV of AF system in different funding scenarios
    NPV_Agroforestry_System = AF_NPV,
    NPV_Agroforestry_no_fund = AF_NPV_no_fund,
    NPV_Treeless_System = NPV_treeless_system,
    NPV_NI=AF_NPV_NI,
    NPV_BY=AF_NPV_BY,
    NPV_MV=AF_NPV_MV,
    NPV_BW=AF_NPV_BW,
    NPV_TH=AF_NPV_TH,
    NPV_BB=AF_NPV_BB,
    NPV_DeFAF_Suggestion = AF_NPV_DeFAF,
    # #cumulative cash flow of the AF system
    AF_CCF_ES3=AF_cum_cash_flow,
    AF_CCF_no_fund=AF_cum_cash_flow_no_fund,
    AF_CCF_NI=AF_cum_cash_flow_NI,
    AF_CCF_BY=AF_cum_cash_flow_BY,
    AF_CCF_MV=AF_cum_cash_flow_MV,
    AF_CCF_BW=AF_cum_cash_flow_BW,
    AF_CCF_TH=AF_cum_cash_flow_TH,
    AF_CCF_BB=AF_cum_cash_flow_BB,
    AF_CCF_SN=AF_cum_cash_flow_SN,
    AF_CCF_DeFAF = AF_cum_cash_flow_DeFAF,
    AF_CF = AF_cash_flow
    ))
}
# END of the Decision Model ####