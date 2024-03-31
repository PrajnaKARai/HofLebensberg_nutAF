# Holistic decision analysis model of silvoarable nut syntropic agroforestry system of Hof Lebensberg ####
# Scenario of changing silvoarable to silvopastoral 
# Institutional support as registered by the farmer to be introduced
# Packages needed ####

install.packages("decisionSupport")
library(decisionSupport)

# Assign input table to object to easily use following "make_variables"-function
Input_Table_df <- read.csv("Easy_DA_Model_Input_DF_comsep.csv")
Input_Table_df1 <- read.csv("DA_Apple_AF_INPUT_TABLE_(notreadable).csv")

#Use make_variables function to test chunks of the function code (AF_benefit) during the development process
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(Input_Table_df)) #Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part


#Defining the probabilisitc model#####
AF_benefit <- function(x, varnames)
{
  #Define each variable as a vector of "n_years" elements. N_years corresponds to the length of the simulation in years. Here: n_years = 30 years
  #Monoculture ####
  #baseline system against which AF systems will be compared
  #Monoculture system costs ####  
  
  #Establishment cost
  #No establishment cost considered, since the arable system is already established and running. 
  #This model depicts the implementation of an AF system into an already existing arable system. 
  
  #Define variables for Running cost (for explanations and units see "Running cost" under "AF costs")
  Treeless_einkorn_sowing_cost <- rep(0, n_years)
  Treeless_wheat_sowing_cost <- rep(0, n_years)
  Treeless_dinkel_sowing_cost <- rep(0, n_years)
  
  Treeless_einkorn_fertilizer_cost <- rep(0, n_years)
  Treeless_wheat_fertilizer_cost <- rep(0, n_years)
  Treeless_dinkel_fertilizer_cost <- rep(0, n_years)
  
  Treeless_einkorn_pesticide_cost <- rep(0, n_years)
  Treeless_wheat_pesticide_cost <- rep(0, n_years)
  Treeless_dinkel_pesticide_cost <- rep(0, n_years)
  
  Treeless_einkorn_machinery_cost <- rep(0, n_years)
  Treeless_wheat_machinery_cost <- rep(0, n_years)
  Treeless_dinkel_machinery_cost <- rep(0, n_years)
  
  Treeless_einkorn_insurance_cost <- rep(0, n_years)
  Treeless_wheat_insurance_cost <- rep(0, n_years)
  Treeless_dinkel_insurance_cost <- rep(0, n_years)
  
  Treeless_einkorn_labour_cost <- rep(0, n_years)
  Treeless_wheat_labour_cost <- rep(0, n_years)
  Treeless_dinkel_labour_cost <- rep(0, n_years)
  
  #???? not system-specific but necessary to calculate respective labour cost  
  einkorn_labour <- rep(0, n_years)
  Wheat_labour <- rep(0, n_years)
  dinkel_labour <- rep(0, n_years)

  # Running cost of managing arable system ##
  
  #einkorn
  Treeless_einkorn_sowing_cost[einkorn_indices] <- vv(einkorn_seed_price, cv_einkorn_seed_price, length(einkorn_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  Treeless_einkorn_fertilizer_cost[einkorn_indices] <- vv(einkorn_fert_price, cv_einkorn_fert_price, length(einkorn_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  Treeless_einkorn_pesticide_cost[einkorn_indices] <- vv(einkorn_cides_price, cv_einkorn_cides_price, length(einkorn_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  Treeless_einkorn_machinery_cost[einkorn_indices] <- vv(einkorn_mach_price, cv_einkorn_mach_price, length(einkorn_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  Treeless_einkorn_insurance_cost[einkorn_indices] <- vv(einkorn_insurance, cv_einkorn_insurance, length(einkorn_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  Treeless_einkorn_labour_cost <- einkorn_labour * arable_area_treeless * Labour_costs #Labour cost associated with einkorn cultivation in treeless arable system
  
  Treeless_total_einkorn_cost <- Treeless_einkorn_sowing_cost + Treeless_einkorn_fertilizer_cost + Treeless_einkorn_pesticide_cost + Treeless_einkorn_machinery_cost + Treeless_einkorn_insurance_cost + Treeless_einkorn_labour_cost
  
  #wheat
  Treeless_wheat_sowing_cost[wheat_indices] <- vv(wheat_seed_price, cv_wheat_seed_price, length(wheat_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  Treeless_wheat_fertilizer_cost[wheat_indices] <- vv(wheat_fert_price, cv_wheat_fert_price, length(wheat_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  Treeless_wheat_pesticide_cost[wheat_indices] <- vv(wheat_cides_price, cv_wheat_cides_price, length(wheat_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  Treeless_wheat_machinery_cost[wheat_indices] <- vv(wheat_mach_price, cv_wheat_mach_price, length(wheat_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  Treeless_wheat_insurance_cost[wheat_indices] <- vv(wheat_insurance, cv_wheat_insurance, length(wheat_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  Treeless_wheat_labour_cost <- Wheat_labour * arable_area_treeless * Labour_costs #Labour cost associated with wheat cultivation in treeless system
  
  Treeless_total_wheat_cost <- Treeless_wheat_sowing_cost + Treeless_wheat_fertilizer_cost + Treeless_wheat_pesticide_cost + Treeless_wheat_machinery_cost + Treeless_wheat_insurance_cost + Treeless_wheat_labour_cost
  
  #dinkel
  Treeless_dinkel_sowing_cost[dinkel_indices] <- vv(dinkel_seed_price, cv_dinkel_seed_price, length(dinkel_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  Treeless_dinkel_fertilizer_cost[dinkel_indices] <- vv(dinkel_fert_price, cv_dinkel_fert_price, length(dinkel_indices)) * arable_area_treeless #cost of fertilizer [€/ha]*area managed [ha]
  Treeless_dinkel_pesticide_cost[dinkel_indices] <- vv(dinkel_cides_price, cv_dinkel_cides_price, length(dinkel_indices)) * arable_area_treeless #cost of pesticides [€/ha]*area managed [ha]
  Treeless_dinkel_machinery_cost[dinkel_indices] <- vv(dinkel_mach_price, cv_dinkel_mach_price, length(dinkel_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  Treeless_dinkel_insurance_cost[dinkel_indices] <- vv(dinkel_insurance, cv_dinkel_insurance, length(dinkel_indices)) * arable_area_treeless #cost of insurance [€/ha]*area managed [ha]
  Treeless_dinkel_labour_cost <- dinkel_labour * arable_area_treeless * Labour_costs #Labour cost associated with dinkel cultivation in treeless system
  
  Treeless_total_dinkel_cost <- Treeless_dinkel_sowing_cost + Treeless_dinkel_fertilizer_cost + Treeless_dinkel_pesticide_cost + Treeless_dinkel_machinery_cost + Treeless_dinkel_insurance_cost + Treeless_dinkel_labour_cost
  
    
  #Monoculture system benefits ####
  Treeless_einkorn_yield <- rep(0, n_years)
  Treeless_wheat_yield <- rep(0, n_years)
  Treeless_dinkel_yield <- rep(0, n_years)
  
  # Arable system is managed with crop rotation of einkorn(CCM)-Wheat-dinkel-> one crop is grown once every 4 years
  
  Treeless_einkorn_yield[einkorn_indices] <- vv(einkorn_yield, cv_einkorn_yield, length(einkorn_indices)) * arable_area_treeless
  Treeless_einkorn_benefit <- vv(einkorn_price, cv_einkorn_price, n_years) * Treeless_einkorn_yield
  
  Treeless_wheat_yield[wheat_indices] <- vv(wheat_yield, cv_wheat_yield, length(wheat_indices)) * arable_area_treeless
  Treeless_wheat_benefit <- vv(wheat_price, cv_wheat_price, n_years) * Treeless_wheat_yield
  
  Treeless_dinkel_yield[dinkel_indices] <- vv(dinkel_yield, cv_dinkel_yield, length(dinkel_indices)) * arable_area_treeless
  Treeless_dinkel_benefit <- vv(dinkel_price, cv_dinkel_price, n_years) * Treeless_dinkel_yield
  
     
  #Monoculture output: system bottom line####
  #total cost of arable component 
  Treeless_total_arable_total_cost <- Treeless_total_einkorn_cost + Treeless_total_wheat_cost + Treeless_total_dinkel_cost
  #total benefit of arable component 
  Treeless_total_benefit <- Treeless_einkorn_benefit + Treeless_wheat_benefit + Treeless_dinkel_benefit
  
  Treeless_bottom_line_benefit <- Treeless_total_benefit - Treeless_total_arable_total_cost 
  
  
  #Agroforestry (AF) System ####
  #AF costs ####  
  # Source: FE- farmer/practitioner estimate; EE- expert estimate
  
  #Calculating costs - tree component ###
  
  #Implementation cost : define variables
  Labour_costs <- rep(0, n_years) #FE; Gross wages of farm employees (also applies to treeless system) [€]
  AF_planning_cost <- rep(0, n_years) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[€]
  # AF_pruning_course <- rep(0, n_years) #FE;Costs of the pruning training of an employee [€]
  # AF_gps_measuring <- rep(0, n_years) #First step of implementation: measuring tree strips using GPS[€]
  AF_dig_plant_holes <- rep(0, n_years) # FE; Second step of implementation: digging/drilling holes for the trees [€]
  AF_tree_cost <- rep(0, n_years) # FE; Cost per tree [€]
  AF_plant_tree_cost <- rep(0, n_years) # FE; Labour cost for planting one tree [€] - 
  AF_protect_cost <- rep(0, n_years) #FE; Material cost of tree protection mesh [€]
  AF_weed_protect_cost <- rep(0, n_years) #Material cost of weed suppressing fleece [€]
  AF_compost_cost <- rep(0, n_years) # FE; Cost of compost used during planting [€]
  AF_irrigation_system_cost <- rep(0, n_years) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [€]
  AF_irrigation_after_planting_cost <- rep(0, n_years) # FE; Cost for watering in newly planted trees [€]
    AF_total_planting_cost <- rep(0, n_years)
  
  #Running cost : define variables
  AF_pruning <- rep(0, n_years) #FE/EE; Labour cost of pruning fruit trees [€]
  AF_root_pruning <- rep(0, n_years) #FE/EE; Labour cost of pruning roots of trees next to tree rows [€]
  AF_apple_harvest <- rep(0, n_years) #FE; Labour cost of harvesting apples manually [€]
  ES3_application <- rep(0, n_years) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  AF_annual_irrigation <- rep(0, n_years) #FE; Cost of annual irrigation of tree rows [€]
  AF_mowing_treerow <- rep(0, n_years) #FE; Labour hours of mowing the tree rows manually [h/ha]
  
  
  # establishment costs
  Labour_costs <- vv(labour_cost, var_CV = var_cv, n_years) 
  #!!Introduce a trend to capture the change in minimum wage over time and the expected wage for agricultural employees
  
  #Planning and consulting 
  AF_planning_cost[1] <- planning_consulting + farmer_planning_time * Labour_costs[1]
  AF_pruning_course[1] <- pruning_course
  
  #Field prep
  AF_gps_measuring[1] <- gps_field_measuring * Labour_costs[1]
  AF_dig_plant_holes[1] <- dig_planting_holes * Labour_costs[1]
  AF_tree_cost[1] <- appletree_price * num_trees
  AF_plant_tree_cost[1] <- planting_trees * Labour_costs[1]
  AF_vole_protect_cost[1] <- vole_protection * num_trees
  AF_deer_protect_cost[1] <- deer_protection * num_trees
  AF_weed_protect_cost[1] <- weed_protection * num_trees
  AF_compost_cost[1] <- compost_after_planting * compost_price * num_trees
  AF_irrigation_system_cost[1] <- irrigation_sys_install
  AF_irrigation_after_planting_cost[1] <- irrigation_after_planting * water_price * num_trees
  
  AF_total_planting_cost <- AF_gps_measuring + AF_dig_plant_holes + AF_tree_cost + AF_plant_tree_cost + AF_vole_protect_cost + AF_deer_protect_cost + AF_weed_protect_cost + AF_compost_cost + AF_irrigation_system_cost + AF_irrigation_after_planting_cost
  
  AF_total_investment_cost <- AF_planning_cost + AF_pruning_course + AF_total_planting_cost #Investment cost of AF system implementation
  
  #Running costs
  ES3_application <- vv(es3_application, var_cv, n_years) * Labour_costs #application for Eco Scheme subsidy has to be repeated annually 
  
  AF_pruning[1:5] <- vv(pruning_juv1, var_CV = var_cv, 5) * Labour_costs[1:5] * num_trees
  AF_pruning[6:10] <- vv(pruning_juv2, var_CV = var_cv, 5) * Labour_costs[6:10] * num_trees
  AF_pruning[11:15] <- vv(pruning_adult1, var_CV = var_cv, 5) * Labour_costs[11:15] * num_trees
  AF_pruning[16:n_years] <- vv(pruning_adult1, var_CV = var_cv, length(16:n_years)) * Labour_costs[16:n_years] * num_trees
  
  AF_root_pruning <- vv(root_pruning, var_CV = var_cv, n_years) * Labour_costs
  
  AF_annual_irrigation[1:3] <- vv(irrigation_123, var_CV = var_cv, 3)
  AF_annual_irrigation[4:n_years] <- vv(irrigation_annual, var_CV = var_cv, length(4:n_years))
  
  AF_annual_irrigation_cost <- AF_annual_irrigation * water_price
  
  AF_mowing_treerow <- vv(mowing_treerow, var_CV = var_cv, n_years) * tree_row_area * Labour_costs
  
  AF_apple_harvest[time_to_first_apple:n_years] <- vv(apple_harvest, var_CV = var_cv, length(time_to_first_apple:n_years))
  
  
  #Total cost of tree component in AF system
  AF_total_treerow_management_cost <- ES3_application + AF_pruning + AF_root_pruning + AF_annual_irrigation_cost + AF_mowing_treerow + AF_apple_harvest
  
  #Cost of arable component ###
  # Arable crops in AF 
  #Cost of seeds of arable crops (Labour involved in sowing is separately listed) [€]
  AF_einkorn_sowing_cost <- rep(0, n_years)
  AF_wheat_sowing_cost <- rep(0, n_years)
  AF_dinkel_sowing_cost <- rep(0, n_years)
  
  #Cost of fertilizer needed for arable crops [€]
  AF_einkorn_fertilizer_cost <- rep(0, n_years)
  AF_wheat_fertilizer_cost <- rep(0, n_years)
  AF_dinkel_fertilizer_cost <- rep(0, n_years)
  
  #Cost of pesticides used in arable crops [€]
  AF_einkorn_pesticide_cost <- rep(0, n_years)
  AF_wheat_pesticide_cost <- rep(0, n_years)
  AF_dinkel_pesticide_cost <- rep(0, n_years)
  
  #Cost for machinery used in arable crops (includes fixed and variable cost but not the investment) [€]
  AF_einkorn_machinery_cost <- rep(0, n_years)
  AF_wheat_machinery_cost <- rep(0, n_years)
  AF_dinkel_machinery_cost <- rep(0, n_years)
  
  #Cost for hail insurance needed for arable crops [€]
  AF_einkorn_insurance_cost <- rep(0, n_years)
  AF_wheat_insurance_cost <- rep(0, n_years)
  AF_dinkel_insurance_cost <- rep(0, n_years)
  
  #Cost of labour in arable cropping system [€]
  AF_einkorn_labour_cost <- rep(0, n_years)
  AF_wheat_labour_cost <- rep(0, n_years)
  AF_dinkel_labour_cost <- rep(0, n_years)
  
  #the following (X-indices) represents the placement of each crop within the crop rotation, every fifth year the crop rotation repeats. This way the time frame over which the crop rotation will be simulated can be changed by changing the value of "n_years" in the input table
  
  einkorn_indices <- seq(from = 1, to = n_years, by = 4)
  wheat_indices <- seq(from = 2, to = n_years, by = 4)
  dinkel_indices <- seq(from = 3, to = n_years, by = 4)
  
  #einkorn
  AF_einkorn_sowing_cost[einkorn_indices] <- vv(einkorn_seed_price, cv_einkorn_seed_price, length(einkorn_indices)) * Arable_area_AF #cost of seed [€/ha]*area managed [ha]
  AF_einkorn_fertilizer_cost[einkorn_indices] <- vv(einkorn_fert_price, cv_einkorn_fert_price, length(einkorn_indices)) * Arable_area_AF #cost of fertilizer [€/ha]*area managed [ha]
  AF_einkorn_pesticide_cost[einkorn_indices] <- vv(einkorn_cides_price, cv_einkorn_cides_price, length(einkorn_indices)) * Arable_area_AF #cost of pesticides [€/ha]*area managed [ha]
  AF_einkorn_machinery_cost[einkorn_indices] <- vv(einkorn_mach_price, cv_einkorn_mach_price, length(einkorn_indices)) * Arable_area_AF #cost of machinery [€/ha]*area managed [ha]
  AF_einkorn_insurance_cost[einkorn_indices] <- vv(einkorn_insurance, cv_einkorn_insurance, length(einkorn_indices)) * Arable_area_AF #cost of insurance [€/ha]*area managed [ha]
  einkorn_labour[einkorn_indices] <- vv(einkorn_labour, cv_einkorn_labour, length(einkorn_indices)) 
  AF_einkorn_labour_cost <- (einkorn_labour + einkorn_labour * (vv(extra_arable_time, var_cv, n_years)/100)) * Labour_costs * Arable_area_AF
  #Labour cost associated with einkorn cultivation. Total labour time is estimated to increase by 5-30% (extra_arable_time/100) due to more complicated navigation of machinery in AF system
  
  AF_total_einkorn_cost <- AF_einkorn_sowing_cost + AF_einkorn_fertilizer_cost + AF_einkorn_pesticide_cost + AF_einkorn_machinery_cost + AF_einkorn_insurance_cost + AF_einkorn_labour_cost
  
  #Wheat
  AF_wheat_sowing_cost[wheat_indices] <- vv(wheat_seed_price, cv_wheat_seed_price, length(wheat_indices)) * Arable_area_AF #cost of seed [€/ha]*area managed [ha]
  AF_wheat_fertilizer_cost[wheat_indices] <- vv(wheat_fert_price, cv_wheat_fert_price, length(wheat_indices)) * Arable_area_AF #cost of fertilizer [€/ha]*area managed [ha]
  AF_wheat_pesticide_cost[wheat_indices] <- vv(wheat_cides_price, cv_wheat_cides_price, length(wheat_indices)) * Arable_area_AF #cost of pesticides [€/ha]*area managed [ha]
  AF_wheat_machinery_cost[wheat_indices] <- vv(wheat_mach_price, cv_wheat_mach_price, length(wheat_indices)) * Arable_area_AF #cost of machinery [€/ha]*area managed [ha]
  AF_wheat_insurance_cost[wheat_indices] <- vv(wheat_insurance, cv_wheat_insurance, length(wheat_indices)) * Arable_area_AF #cost of insurance [€/ha]*area managed [ha]
  Wheat_labour[wheat_indices] <- vv(wheat_labour, cv_wheat_labour, length(wheat_indices)) 
  AF_wheat_labour_cost <- (Wheat_labour + Wheat_labour * (vv(extra_arable_time, var_cv, n_years)/100)) * Labour_costs * Arable_area_AF #Labour cost associated with wheat cultivation
  
  AF_total_wheat_cost <- AF_wheat_sowing_cost + AF_wheat_fertilizer_cost + AF_wheat_pesticide_cost + AF_wheat_machinery_cost + AF_wheat_insurance_cost + AF_wheat_labour_cost
  
  #dinkel
  AF_dinkel_sowing_cost[dinkel_indices] <- vv(dinkel_seed_price, cv_dinkel_seed_price, length(dinkel_indices)) * Arable_area_AF #cost of seed [€/ha]*area managed [ha]
  AF_dinkel_fertilizer_cost[dinkel_indices] <- vv(dinkel_fert_price, cv_dinkel_fert_price, length(dinkel_indices)) * Arable_area_AF #cost of fertilizer [€/ha]*area managed [ha]
  AF_dinkel_pesticide_cost[dinkel_indices] <- vv(dinkel_cides_price, cv_dinkel_cides_price, length(dinkel_indices)) * Arable_area_AF #cost of pesticides [€/ha]*area managed [ha]
  AF_dinkel_machinery_cost[dinkel_indices] <- vv(dinkel_mach_price, cv_dinkel_mach_price, length(dinkel_indices)) * Arable_area_AF #cost of machinery [€/ha]*area managed [ha]
  AF_dinkel_insurance_cost[dinkel_indices] <- vv(dinkel_insurance, cv_dinkel_insurance, length(dinkel_indices)) * Arable_area_AF #cost of insurance [€/ha]*area managed [ha]
  dinkel_labour[dinkel_indices] <- vv(dinkel_labour, cv_dinkel_labour, length(dinkel_indices)) 
  AF_dinkel_labour_cost <- (dinkel_labour + dinkel_labour * (vv(extra_arable_time, var_cv, n_years)/100)) * Labour_costs * Arable_area_AF #Labour cost associated with dinkel cultivation
  
  AF_total_dinkel_cost <- AF_dinkel_sowing_cost + AF_dinkel_fertilizer_cost + AF_dinkel_pesticide_cost + AF_dinkel_machinery_cost + AF_dinkel_insurance_cost + AF_dinkel_labour_cost
  
  #Total cost of arable component in AF system
  AF_total_arable_management_cost <- AF_total_einkorn_cost + AF_total_wheat_cost + AF_total_dinkel_cost
  
  #Total running cost of AF system
  AF_total_running_cost <- AF_total_treerow_management_cost + AF_total_arable_management_cost 
  #Total cost of AF system
  AF_total_cost <- AF_total_investment_cost + AF_total_running_cost 
  
  
  #AF benefits ####
  
  # herbaceous component - should be activated only if it is silvopastoral or agrisilvicultural system or 
  # if farmer grows forage crop
  #AF_grass_yield <- rep(0, n_years)
  
  #Annual arable crop component
  AF_einkorn_yield <- rep(0, n_years)
  AF_wheat_yield <- rep(0, n_years)
  AF_dinkel_yield <- rep(0, n_years)

  
  #Crop rotation in AF system
  AF_einkorn_yield[einkorn_indices] <- vv(einkorn_yield, cv_einkorn_yield, length(einkorn_indices)) * Arable_area_AF
  AF_einkorn_benefit <- vv(einkorn_price, cv_einkorn_price, n_years) * AF_einkorn_yield
  
  AF_wheat_yield[wheat_indices] <- vv(wheat_yield, cv_wheat_yield, length(wheat_indices)) * Arable_area_AF
  AF_wheat_benefit <- vv(wheat_price, cv_wheat_price, n_years) * AF_wheat_yield
  
  AF_dinkel_yield[dinkel_indices] <- vv(dinkel_yield, cv_dinkel_yield, length(dinkel_indices)) * Arable_area_AF
  AF_dinkel_benefit<- vv(dinkel_price, cv_dinkel_price, n_years) * AF_dinkel_yield

  
  #Calculating model parameters based on input table
  Arable_area_AF <- arable_area_treeless - tree_row_area
  
  
  #Tree component - nuts in AF system
  AF_nuts_yield <- rep(0, n_years)
  ES3_subsidy <- rep(0, n_years)
  #Yield of one nut tree [kg/tree]  
  AF_nuts_yield <- gompertz_yield(max_harvest = nuts_yield_max,
                                   time_to_first_yield_estimate = time_to_first_nuts,
                                   time_to_second_yield_estimate = time_to_second_nuts,
                                   first_yield_estimate_percent = nuts_yield_first,
                                   second_yield_estimate_percent = nuts_yield_second,
                                   n_years=n_years,
                                   var_CV = var_cv,
                                   no_yield_before_first_estimate = TRUE)
  
  #??? Yield of timber from one nut tree [kg/tree]  
  # AF_Timber_yield <- gompertz_yield(max_harvest = nuts_yield_max,
  #                                 time_to_first_yield_estimate = time_to_first_nuts,
  #                                 time_to_second_yield_estimate = time_to_second_nuts,
  #                                 first_yield_estimate_percent = nuts_yield_first,
  #                                 second_yield_estimate_percent = nuts_yield_second,
  #                                 n_years=n_years,
  #                                 var_CV = var_cv,
  #                                 no_yield_before_first_estimate = TRUE)
  
  #Yield of XXX nut trees [kg]  
  AF_tot_nuts_yield <- AF_nuts_yield * num_trees
  #Calculate how many kg have nuts quality and can therefore be marketed at a higher price (Pack_nuts) and the rest are used for making oil 
  Pc_table_nuts <- vv(perc_table_nuts, var_CV = var_cv, n_years)/100
  
  Pack_nuts_yield <- AF_tot_nuts_yield * Pc_table_nuts
  Oil_nuts_yield <- AF_tot_nuts_yield * (1-Pc_table_nuts)
  
  #The benefits from table apples and juice apples are calculated by multiplying their yields by their respective prices  
  Pack_nuts_benefit <- vv(Pack_nuts_price, var_cv, n_years) * Pack_nuts_yield
  Oil_nuts_benefit <- vv(Oil_nuts_price, var_cv, n_years) * Oil_nuts_yield
  
  #The benefits from selling the timber of nut trees - one time at th ened of 30 years
  #Timber_nut_benefit <- AF_Timber_yield * num_trees
  
  
  AF_nuts_benefit <- Pack_nuts_benefit + Oil_nuts_benefit #+ Timber_nut_benefit
  

  #Subsidy in AF system
  ES3_subsidy[1:n_years] <- es3_subsidy * tree_row_area
  
  #Agroforestry output: system bottom line ####
  AF_total_benefit <- AF_nuts_benefit + AF_einkorn_benefit + AF_wheat_benefit + AF_dinkel_benefit + ES3_subsidy
  
  AF_bottom_line_benefit <- AF_total_benefit - AF_total_cost
  

  # Calculating NPVs and Cash Flows####
  #AF System
  AF_NPV <- discount(AF_bottom_line_benefit, discount_rate=discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow <- discount(AF_bottom_line_benefit,discount_rate=discount_rate,
                           calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  #Monoculture system  
  NPV_treeless_system <- discount(Treeless_total_benefit, discount_rate = discount_rate,
                                  calculate_NPV = TRUE) #NVP of monoculture arable system 
  Treeless_cash_flow <- discount(Treeless_total_benefit, discount_rate = discount_rate,
                                 calculate_NPV = FALSE) #Cash flow of monoculture system
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of monoculture system
  
  #Tradeoff (difference between AF system and treeless system)
  Tradeoff_benefit <- AF_bottom_line_benefit - Treeless_bottom_line_benefit
  
  NPV_tradeoff <- discount(Tradeoff_benefit, discount_rate = discount_rate,
                           calculate_NPV = TRUE )
  
  
  #CREATING THE FUNDING SCENARIOS#######################################################################

  #Federal State Investment Support Schemes:
  # Scenario 1: "NI-Subsidy" -> Investment funding of Lower Saxony (NI) - 40 % of eligible investment cost (planning and consulting is not considered eligible investment cost!!), capped at 20,000 €, only for silvoarable systems
  NI_sub_application <- rep(0, n_years)
  NI_sub_application[1] <- ni_sub_application*Labour_costs[1]
  NI_invest_sub <- rep(0, n_years)
  NI_invest_sub[1] <- ni_invest_sub_max
  AF_total_cost_NI <- AF_total_cost
  
  AF_total_cost_NI <- ifelse((AF_total_investment_cost-AF_planning_cost)*0.4 > 20000 & arable == 1,
                              AF_total_investment_cost-NI_invest_sub + NI_sub_application + AF_total_running_cost,
                              (AF_total_investment_cost-AF_planning_cost)*0.6 + AF_planning_cost + NI_sub_application + AF_total_running_cost)#Total cost, in Scenario "NI-Subsidy" -> important to note: planning and consulting is not considered an eligible investment cost by Lower Saxony's regulation hence the deduction in the function
  
  AF_bottom_line_benefit_NI <- AF_total_benefit - AF_total_cost_NI#Bottom line, in Scenario "NI-Subsidy"
  AF_NPV_NI <- discount(AF_bottom_line_benefit_NI, discount_rate=discount_rate,
                         calculate_NPV = TRUE)#NVP of AF system, in Scenario "NI-Subsidy"
   AF_cash_flow_NI <- discount(AF_bottom_line_benefit_NI,discount_rate=discount_rate,
                               calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "NI-Subsidy"
   AF_cum_cash_flow_NI <- cumsum(AF_cash_flow_NI)#Cumulative cash flow of AF system, in Scenario "NI-Subsidy"
  
  # Scenario 2: "BY-Subsidy" -> Investment funding of Bavaria (BY) - max. 65 % of eligible investment cost, capped at 50,000 €, furthermore, payment per ha of tree row dependent on type of tree row
   BY_sub_application <- rep(0, n_years)
   BY_sub_application[1] <- by_sub_application*Labour_costs[1]
   BY_invest_SRC <- rep(0, n_years)
   BY_invest_SRC[1] <- by_invest_src
   BY_invest_shrubs <- rep(0, n_years)
   BY_invest_shrubs[1] <- by_invest_shrubs
   BY_invest_timber_food <- rep(0, n_years)
   BY_invest_timber_food[1] <- by_invest_timb_food
   BY_invest_sub <- rep(0, n_years)
  
   #Funding support is capped at 65 % of eligible funding, while also differentiating between different types of tree rows. The following function first asks for the type of tree row and then calculates the respective funding sum.
   if (treerow_SRC == 1) {
     BY_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > BY_invest_SRC,
                             BY_invest_SRC, (AF_total_investment_cost/tree_row_area)*0.65)
   } else if (treerow_shrubs == 1) {
     BY_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > BY_invest_shrubs,
                             BY_invest_shrubs, (AF_total_investment_cost/tree_row_area)*0.65)
   } else if (treerow_timber_food == 1) {
     BY_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > BY_invest_timber_food,
                             BY_invest_timber_food, (AF_total_investment_cost/tree_row_area)*0.65)
   } else if (treerow_SRC == 0 & treerow_shrubs == 0 & treerow_timber_food == 0) {
     BY_invest_sub <- 0
   } #note that 1 and 0 in conditional functions act as boolean operators "TRUE" and "FALSE"
  
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
  
  
   # Scenario 3: "MV-Subsidy" -> Investment funding of Mecklenburg Western-Pomerania (MV) - max. 65 % of eligible investment cost, capped at 300,000 €, furthermore, payment per ha of tree row dependent on type of tree row
   MV_sub_application <- rep(0, n_years)
   MV_sub_application[1] <- mv_sub_application*Labour_costs[1]
   MV_invest_SRC <- rep(0, n_years)
   MV_invest_SRC[1] <- mv_invest_src
   MV_invest_shrubs <- rep(0, n_years)
   MV_invest_shrubs[1] <- mv_invest_shrubs
   MV_invest_timber_food <- rep(0, n_years)
   MV_invest_timber_food[1] <- mv_invest_timb_food
   MV_invest_sub <- rep(0, n_years)
  
   #Funding support is capped at 65 % of eligible funding, while also differentiating between different types of tree rows. The following function first asks for the type of tree row and then calculates the respective funding sum.
   if (treerow_SRC == 1) {
     MV_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > MV_invest_SRC,
                             MV_invest_SRC, (AF_total_investment_cost/tree_row_area)*0.65)
   } else if (treerow_shrubs == 1) {
     MV_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > MV_invest_shrubs,
                             MV_invest_shrubs, (AF_total_investment_cost/tree_row_area)*0.65)
   } else if (treerow_timber_food == 1) {
     MV_invest_sub <- ifelse((AF_total_investment_cost/tree_row_area)*0.65 > MV_invest_timber_food,
                             MV_invest_timber_food, (AF_total_investment_cost/tree_row_area)*0.65)
   } else if (treerow_SRC == 0 & treerow_shrubs == 0 & treerow_timber_food == 0) {
     MV_invest_sub <- 0
   } #note that 1 and 0 in conditional functions act as boolean operators "TRUE" and "FALSE"
  
   #Requested investment support funding must be min. 2,500 € and max. 50,000 €
   MV_invest_sub <- ifelse(BY_invest_sub < 2500, 0, BY_invest_sub)
   MV_invest_sub <- ifelse(BY_invest_sub > 300000, 300000, BY_invest_sub)
   MV_total_invest_sub <- MV_invest_sub*tree_row_area - MV_sub_application
  
   #Calculating bottom line, NPV and Cash Flow
   AF_bottom_line_benefit_MV <- AF_total_benefit + MV_total_invest_sub - AF_total_cost#Bottom line, in Scenario "MV-Subsidy"
   AF_NPV_MV <- discount(AF_bottom_line_benefit_MV, discount_rate=discount_rate,
                         calculate_NPV = TRUE)#NVP of AF system, in Scenario "MV-Subsidy"
   AF_cash_flow_MV <- discount(AF_bottom_line_benefit_MV, discount_rate=discount_rate,
                               calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "MV-Subsidy"
   AF_cum_cash_flow_MV <- cumsum(AF_cash_flow_MV)#Cumulative cash flow of AF system, in Scenario "MV-Subsidy"
  
  
   # Scenario 4: "BW-Subsidy" -> Investment funding of Baden-Württemberg (BW) - max. 80 % of eligible investment cost, capped at 1,500 €. Only cost considered eligible for funding: planning and consulting
  
   BW_sub_application <- rep(0, n_years)
   BW_sub_application[1] <- bw_sub_application*Labour_costs[1] #Vector with 30 elements, containing the value of labour cost spent on applying for subsidy
   AF_consulting <- rep(0, n_years)
   AF_consulting[1] <- planning_consulting #Vector with 30 elements, containing the value of planning_consulting as first element
   BW_invest_sub <- rep(0, n_years)
   BW_invest_sub[1] <- bw_invest_max
   AF_total_cost_BW <- AF_total_cost
  
   AF_total_cost_BW <- ifelse(AF_consulting*0.8 > 1500,
                              AF_total_investment_cost-BW_invest_sub + BW_sub_application + AF_total_running_cost,
                              AF_total_investment_cost-AF_consulting*0.8 + AF_planning_cost + BW_sub_application + AF_total_running_cost) #Total cost of AF system in scenario "BW Subsidy"
  
   AF_bottom_line_benefit_BW <- AF_total_benefit - AF_total_cost_BW #Bottom line, in Scenario "BW-Subsidy"
   AF_NPV_BW <- discount(AF_bottom_line_benefit_BW, discount_rate=discount_rate,
                         calculate_NPV = TRUE) #NVP of AF system, in Scenario "BW-Subsidy"
   AF_cash_flow_BW <- discount(AF_bottom_line_benefit_BW,discount_rate=discount_rate,
                               calculate_NPV = FALSE) #Cash flow of AF system, in Scenario "BW-Subsidy"
   AF_cum_cash_flow_BW <- cumsum(AF_cash_flow_BW) #Cumulative cash flow of AF system, in Scenario "BW-Subsidy"
  
   # Scenario 5 "TH-Subsidy" -> Investment funding of Thuringia (TH) - max. 100 % of eligible investment cost, capped at 1,500 €. Only cost considered eligible for funding: planning and consulting
  
   TH_sub_application <- rep(0, n_years)
   TH_sub_application[1] <- th_sub_application*Labour_costs[1]
   TH_invest_sub <- rep(0, n_years)
   TH_invest_sub[1] <- th_invest_max
   AF_total_cost_TH <- AF_total_cost
  
   AF_total_cost_TH <- ifelse(AF_consulting > 1500,
                              AF_total_investment_cost-TH_invest_sub + TH_sub_application + AF_total_running_cost,
                              AF_total_investment_cost-AF_consulting + AF_planning_cost + TH_sub_application + AF_total_running_cost) #Total cost of AF system in scenario "TH Subsidy"
  
   AF_bottom_line_benefit_TH <- AF_total_benefit - AF_total_cost_TH#Bottom line, in Scenario "TH-Subsidy"
   AF_NPV_TH <- discount(AF_bottom_line_benefit_TH, discount_rate=discount_rate,
                         calculate_NPV = TRUE)#NVP of AF system, in Scenario "TH-Subsidy"
   AF_cash_flow_TH <- discount(AF_bottom_line_benefit_TH,discount_rate=discount_rate,
                               calculate_NPV = FALSE)#Cash flow of AF system, in Scenario "TH-Subsidy"
   AF_cum_cash_flow_TH <- cumsum(AF_cash_flow_TH)#Cumulative cash flow of AF system, in Scenario "TH-Subsidy"
  
   # Scenario 6 "DeFAF-Subsidy" -> Investment funding of 100 % for first 10 ha of wooded area, 80 % of additional 10 ha of wooded area and 50 % of every additional ha after 20 ha of total wooded area. Additionally: annual subsidy of 600 €/ha of wooded area + possibility of combining ES3 ("Maintenance of Agroforestry System") and ES 1 ("Provision of areas to improve biodiversity and preserve habitats (fallow land/flowering-/old grass strips and areas)") leading to another 200 €/ha of e.g. flowering strip.
  
   #DeFAF annual subsidy
   DeFAF_ES3 <- es3_subsidy*3 - ES3_application
   ES1a_Sub <- rep(0, n_years)
   ES1a_Sub[3:n_years] <- es1a_subsidy*tree_row_area - es1_application*Labour_costs[3:n_years] #Beginning in year three to ensure proper establishment of trees
   ES1b_Sub <- rep(0, n_years)
   ES1b_Sub[3:n_years] <- es1b_subsidy*tree_row_area
   ES1_mowing <- AF_mowing_treerow #Assuming, flowering strip is established in year 3
   ES1_mowing[seq(3, length(ES1_mowing), by = 2)] <- 0 #ES1-areas have to be mown every 2 years (note: this could complicate apple harvest!)
   ES1_Sub <- if (gloz8 == 1) {
     ES1a_Sub + ES1b_Sub
   } else {
     0
   }#German CAP regulations demand 4 % of arable land to be turned into "unproductive land" (GLÖZ8) -> ES1 can only be applied for additional area of agr. land turned "unproductive", once the conditions of "GLÖZ 8" are met (can be adjusted in input table by inserting either 1 or 0)
   #It is assumed that the tree row area is part of the first % of additional arable land turned to "unproductive" (hence, es1a_subsidy = 1300 €/ha)
   DeFAF_annual_sub <- DeFAF_ES3 + ES1_Sub #Total annual subsidy suggested by DeFAF is a combination of an increased ES3 combined with ES1.
  
   #DeFAF Investment support funding scheme (first 10 ha 100 % subsidised, next 10 ha 80 % subsidised, every additional ha 50 % subsidised)
  
   #If AF system is registered as anoher system to get better subsidy
  
  
  
  #Calculate the investment cost per hectare
  invest_cost_per_ha <- AF_total_investment_cost[1]/tree_row_area
  
  #Create a modified vector based on the conditions
  AF_total_invest_cost_DeFAF <- AF_total_investment_cost
  
  #If tree_row_area is equal or smaller than 10 ha, then 100 % of the investment cost is subsidised
  if (tree_row_area > 0 && tree_row_area <= 10) {
    AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - tree_row_area * invest_cost_per_ha
    #If tree row area is over 10 ha, the first 10 ha are subsidised 100 %, but additional ha are subsidised differently
  } else if (tree_row_area > 10) {
    AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - 10 * invest_cost_per_ha
    #Check if there's a remainder after deducting 10 hectares, since every additional 10 ha are subsidised 80%
    Remainder <- tree_row_area - 10
    
    if (Remainder > 0 && Remainder <= 10) {
      #If remainder is smaller or equal to 10, deduct 80% of the cost for the remaining hectares
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - Remainder * invest_cost_per_ha * 0.8
    } else {
      #If remainder is greater than 10, deduct 10 hectares * 80% and the rest * 50%
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - 10 * invest_cost_per_ha * 0.8
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - (((Remainder - 10)*invest_cost_per_ha) * 0.5)
    }
  }
  
  #Ensure the modified investment cost is not negative (code above should not be able to create negative values but next line is added as insurance that negative investment cost in ever included in the calculations)
  AF_total_invest_cost_DeFAF[1] <- max(0, AF_total_invest_cost_DeFAF[1])
  
  #Calculate total benefit of DeFAF-subsidy
  
  AF_total_cost_DeFAF <- AF_total_invest_cost_DeFAF + AF_total_running_cost - DeFAF_annual_sub - AF_mowing_treerow + ES1_mowing #Implementation of ES1 (included in DeFAF_annual_sub) leads to adjusted mowing regime, originally calculated annual cost of mowing must be replaced with the cost of mowing under the ES1 schedule
  
  AF_bottom_line_benefit_DeFAF <- AF_total_benefit - AF_total_cost_DeFAF #Bottom line, in Scenario "DeFAF-Subsidy"
  AF_NPV_DeFAF <- discount(AF_bottom_line_benefit_DeFAF, discount_rate=discount_rate,
                           calculate_NPV = TRUE) #NVP of AF system, in Scenario "DeFAF-Subsidy"
  AF_cash_flow_DeFAF <- discount(AF_bottom_line_benefit_DeFAF,discount_rate=discount_rate,
                                 calculate_NPV = FALSE) #Cash flow of AF system, in Scenario "DeFAF-Subsidy"
  AF_cum_cash_flow_DeFAF <- cumsum(AF_cash_flow_DeFAF) #Cumulative cash flow of AF system, in Scenario "DeFAF-Subsidy"
  ####################################################################################################
  
  #Defining what output variables the following Monte Carlo Simulation should create #####
  return(list(NPV_Agroforestry_System = AF_NPV,
              AFcashflow = AF_cash_flow,
              AFcumcashflow = AF_cum_cash_flow,
              NIsubcashflow = AF_cash_flow_NI,
              NIsubcumcashflow = AF_cum_cash_flow_NI,
              NPV_Treeless_System = NPV_treeless_system,
              Treelesscashflow = Treeless_cash_flow,
              TLcumcashflow = Treeless_cum_cash_flow,
              NPVtrade_off = NPV_tradeoff,
              NPV_NI=AF_NPV_NI,
              NPV_BY=AF_NPV_BY,
              NPV_MV=AF_NPV_MV,
              NPV_BW=AF_NPV_BW,
              NPV_TH=AF_NPV_TH,
              NPV_DeFAF = AF_NPV_DeFAF,
              CF_NI=AF_cash_flow_NI,
              CF_BY=AF_cash_flow_BY,
              CF_MV=AF_cash_flow_MV,
              CF_BW=AF_cash_flow_BW,
              CF_TH=AF_cash_flow_TH,
              CF_DeFAF=AF_cash_flow_DeFAF))
}

#Run again the Monte Carlo analysis of the model
mcSimulation_results <- mcSimulation(
  estimate = estimate_read_csv(fileName = input_file),
  model_function = AF_benefit,
  numberOfModelRuns = 2000,
  functionSyntax = "plainNames")

# Plot the output distributions for visual comparison ####

plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)
#comparing scenarios##################################################
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System", "NPV_NI", "NPV_BY", "NPV_MV", "NPV_BW", "NPV_TH", "NPV_DeFAF"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System", "NPV_NI", "NPV_BY", "NPV_MV", "NPV_BW", "NPV_TH", "NPV_DeFAF"),
                   method = 'boxplot_density',
                   base_size = 7)

plot_cashflow(mcSimulation_object = mcSimulation_results, 
              cashflow_var_name = c("AFcashflow", "Treelesscashflow", "CF_NI", "CF_BY", "CF_MV", "CF_BW", "CF_TH", "CF_DeFAF"))

plot_cashflow(mcSimulation_object = mcSimulation_results, 
              cashflow_var_name = c("AFcashflow", "Treelesscashflow", "CF_DeFAF"))
########################################################################

plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                   method = 'boxplot_density',
                   base_size = 7)

plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = "NPVtrade_off",
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

plot_cashflow(mcSimulation_object = mcSimulation_results, 
              cashflow_var_name = "AFcashflow")

plot_cashflow(mcSimulation_object = mcSimulation_results,
              cashflow_var_name = "Treelesscashflow")

plot_cashflow(mcSimulation_object = mcSimulation_results, 
              cashflow_var_name = "AFcumcashflow")

#Sensitivity Analysis and EVPI ####

#Repeat Monte Carlo Simulation, this time creating output file

decisionSupport(inputFilePath = input_file,
                outputPath = paste0(folder, "/", "MCResults", sep=""),
                write_table = TRUE,
                welfareFunction = AF_benefit,
                numberOfModelRuns = 2000,
                functionSyntax = "plainNames")

#Creating object out of Monte Carlo Simulation output file
MCall <- read.table("MCResults/mcSimulationResults.csv", header = TRUE, sep = ",")
mc <- MCall[,c(2:131, 314)] #subsetting the MCall data frame, to only keep the input variables and the Tradeoff-NPV

EVPI <- multi_EVPI(mc, "NPVtrade_off", write_table = TRUE)

plot_evpi(EVPI, "NPVtrade_off")