# Holistic decision analysis model of silvoarable mixed nut syntropic agroforestry system of Hof Lebensberg ####
# 7.53 ha of 420 Hazelnut (Corylus avellana) trees, 208 Walnuts (Juglans spp.) trees, 183 Chestnut (Castanea sativa/Aesculus hippocastanum) trees, 68 Pecan (Carya illinoinensis) trees, 89 Cashewnuts (Anacardium occidentale) trees, 73 Almonds (Prunus dulcis) trees, and 23 Hickory 
# Scenario of changing silvoarable to silvopastoral is yet to be introduced
# Institutional support as registered by the farmer to be introduced; only ES3 implemented now
# shrubs yet to be included

# Packages needed ####
install.packages("decisionSupport")
library(decisionSupport)
library(ggplot2)
library(dplyr)

# Assign input table to object to easily use following "make_variables"-function
input_file <- read.csv("Input_table_R2.csv")

#Use make_variables function to test chunks of the function code (AF_benefit) during the development process
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}

generated_variables <- make_variables(as.estimate(input_file)) #Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part

# From Marcos:
# Modify the gomperty_yield function so that first_yield_estimate_percent is never equal or higher than second_yield_estimate_percent
ontogenic_growth_gompertz <- function (max_harvest, time_to_first_yield_estimate, time_to_second_yield_estimate, 
                                       first_yield_estimate_percent, second_yield_estimate_percent, 
                                       n_years, var_CV = 0, no_yield_before_first_estimate = TRUE) 
{
  a = max_harvest
  t1 = time_to_first_yield_estimate
  t2 = time_to_second_yield_estimate
  p2 = second_yield_estimate_percent/100
  if (p2 > 0.999) 
    p2 <- 0.999
  if (p2 < 0.001) 
    p2 <- 0.001
  if (first_yield_estimate_percent > 99) 
    first_yield_estimate_percent <- 99
  if (first_yield_estimate_percent < 1) 
    first_yield_estimate_percent <- 1
  p1 = p2*(first_yield_estimate_percent/100)
  if (t1 == t2) 
    t2 <- t1 + 1
  c <- sum(log(log(p2)/log(p1))/(t1 - t2)) # why to use sum()? The function only has one element, the output without function sum() is the same
  b <- (-log(p1)/exp(-c * t1))
  gompertz <- function(x) {
    a * exp(-b * exp(-c * x))
  }
  yield_n_years_ideal <- gompertz(1:n_years)
  yield_n_years_real <- unlist(lapply(yield_n_years_ideal, 
                                      vv, var_CV = var_CV, n = 1))
  if (no_yield_before_first_estimate & t1 > 1) {
    yield_n_years_real[1:min(c(n_years, t1 - 1))] <- 0
  }
  return(yield_n_years_real)
}



#Defining the probabilisitc model#####
AF_benefit <- function(x, varnames)
{
  #System modulators ####
  #yield failure due to pests or diseases
  # chance_perc_crop_fail <-
  #   chance_event(chance = chance_pest_diseases,
  #                value_if = value_if_pest_diseases,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 90% failure
  # AF_chance_perc_crop_fail <-
  #   chance_event(chance = chance_pest_diseases,
  #                value_if = af_value_if_pest_diseases,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 10% failure, due to presence of beneficial insects and a more resistant system with AF
  # #yield failure due to weather events
  # chance_perc_weather_fail <-
  #   chance_event(chance = chance_extreme_weather,
  #                value_if = value_if_extreme_weather,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 70% failure
  # AF_chance_perc_weather_fail <-
  #   chance_event(chance = chance_extreme_weather,
  #                value_if = af_value_if_extreme_weather,
  #                value_if_not = 1) # 20% chance that the event will occur and result in 10% failure
  # #risks of not harvesting timber -> institutional settings
  # AF_chance_timber_harvest_fail <-
  #   chance_event(chance = chance_policy_timber_not_harvest,
  #                value_if = value_if_timber_not_harvest,
  #                value_if_not = 1) # 30% chance that the event will occur and result in 10% failure
  # #risk of market fluctuations - chance event is applied by year
  # market_fluc <- rep(0, n_years)
  # no_market_fluc <- rep(1, n_years)
  # # accounting fluctuations in market price of all products
  # market_fluc <- vv(per_market_price, var_CV = var_cv, n_years)
  # # accounting market falls, recession -> economically hard times
  # chance_market_fluc <-
  #   chance_event(chance = chance_market_crash,
  #                value_if = market_fluc,
  #                value_if_not = no_market_fluc) # 10% chance that the event will occur and 
  #                                               #result in use the value from vv function
  # AF_chance_market_fluc <-
  #   chance_event(chance = chance_market_crash,
  #                value_if = market_fluc,
  #                value_if_not = no_market_fluc) #value_if_not = 1) # 10% chance that the event will occur and 
                                                #result in use the value from vv function
  # Arable system is managed with crop rotation of einkorn(CCM)-Wheat-dinkel
  #one crop is grown once every 4 years
  
  #Define each variable as a vector of "n_years" elements. N_years corresponds to the length of the simulation in years. Here: n_years = 30 years
  # define indices that are part of annual arable production
  einkorn_indices <- seq(from = 1, to = n_years, by = 3)
  wheat_indices <- seq(from = 2, to = n_years, by = 3)
  dinkel_indices <- seq(from = 3, to = n_years, by = 3)
  
  #Labour
  labour_cost <- rep(0, n_years) #Gross wages of farm employees (also applies to treeless system) [€]
  Labour_costs <- vv(labour_cost, var_CV = var_cv, n_years) 
  #Introduce a trend to capture the change in minimum wage over time and the expected wage for agricultural employees
  
  einkorn_labour <- rep(0, n_years)
  Wheat_labour <- rep(0, n_years)
  dinkel_labour <- rep(0, n_years)
  
  einkorn_labour[einkorn_indices] <- vv(einkorn_labour, cv_einkorn_labour, length(einkorn_indices))
  Wheat_labour[wheat_indices] <- vv(wheat_labour, cv_wheat_labour, length(wheat_indices))
  dinkel_labour[dinkel_indices] <- vv(dinkel_labour, cv_dinkel_labour, length(dinkel_indices)) 
  
  #Treeless or Monoculture ####
  #baseline system against which AF systems will be compared
  #Treeless system costs ####  
  
  #Establishment cost - No establishment cost considered, since the arable system 
  #is already established and running. 
  #This model depicts the implementation of an AF system into an already existing 
  #arable system. 
  
  #Define variables for Running cost 
  Treeless_einkorn_sowing_cost <- rep(0, n_years)
  Treeless_wheat_sowing_cost <- rep(0, n_years)
  Treeless_dinkel_sowing_cost <- rep(0, n_years)

  Treeless_einkorn_machinery_cost <- rep(0, n_years)
  Treeless_wheat_machinery_cost <- rep(0, n_years)
  Treeless_dinkel_machinery_cost <- rep(0, n_years)
  
  Treeless_einkorn_labour_cost <- rep(0, n_years)
  Treeless_wheat_labour_cost <- rep(0, n_years)
  Treeless_dinkel_labour_cost <- rep(0, n_years)
  
  # Running cost of managing arable system ##
  
  #einkorn
  Treeless_einkorn_sowing_cost[einkorn_indices] <-vv(einkorn_seed_price, cv_einkorn_seed_price, 
                                                     length(einkorn_indices)) * arable_area_treeless 
                                                #cost of seed [€/ha]*area managed [ha]
  
  Treeless_einkorn_machinery_cost[einkorn_indices] <- vv(einkorn_mach_price, cv_einkorn_mach_price, 
                                                         length(einkorn_indices)) * arable_area_treeless 
                                                        #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_einkorn_labour_cost <- einkorn_labour * arable_area_treeless * Labour_costs 
    #Labour cost associated with einkorn cultivation in treeless arable system
  
  Treeless_total_einkorn_cost <- Treeless_einkorn_sowing_cost + 
    Treeless_einkorn_machinery_cost + Treeless_einkorn_labour_cost 
  
  #wheat
  Treeless_wheat_sowing_cost[wheat_indices] <- vv(wheat_seed_price, cv_wheat_seed_price, 
                                                  length(wheat_indices)) * arable_area_treeless 
                                                #cost of seed [€/ha]*area managed [ha]
  
  Treeless_wheat_machinery_cost[wheat_indices] <- vv(wheat_mach_price, cv_wheat_mach_price, 
                                                     length(wheat_indices)) * arable_area_treeless 
                                              #cost of machinery [€/ha]*area managed [ha]
  Treeless_wheat_labour_cost <- Wheat_labour * arable_area_treeless * Labour_costs 
                                            #Labour cost associated with wheat cultivation in treeless system
  
  Treeless_total_wheat_cost <- Treeless_wheat_sowing_cost + 
    Treeless_wheat_machinery_cost + Treeless_wheat_labour_cost
  
  #dinkel
  Treeless_dinkel_sowing_cost[dinkel_indices] <- vv(dinkel_seed_price, cv_dinkel_seed_price,
                                                    length(dinkel_indices)) * arable_area_treeless #cost of seed [€/ha]*area managed [ha]
  
  Treeless_dinkel_machinery_cost[dinkel_indices] <- vv(dinkel_mach_price, cv_dinkel_mach_price, 
                                                       length(dinkel_indices)) * arable_area_treeless #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_dinkel_labour_cost <- dinkel_labour * arable_area_treeless * Labour_costs #Labour cost associated with dinkel cultivation in treeless system
  
  Treeless_total_dinkel_cost <- Treeless_dinkel_sowing_cost +
    Treeless_dinkel_machinery_cost + Treeless_dinkel_labour_cost
  
  #Monoculture system benefits ####
  Treeless_einkorn_yield <- rep(0, n_years)
  Treeless_wheat_yield <- rep(0, n_years)
  Treeless_dinkel_yield <- rep(0, n_years)
  
  Treeless_einkorn_yield[einkorn_indices] <-
    vv(einkorn_yields, cv_einkorn_yield, length(einkorn_indices)) * arable_area_treeless 
  Treeless_einkorn_benefit <- vv(einkorn_price, cv_einkorn_price, n_years) * Treeless_einkorn_yield #* 
    #chance_perc_crop_fail * chance_perc_weather_fail
  
  Treeless_wheat_yield[wheat_indices] <-
    vv(wheat_yields, cv_wheat_yield, length(wheat_indices)) * arable_area_treeless 
  Treeless_wheat_benefit <- vv(wheat_price, cv_wheat_price, n_years) * Treeless_wheat_yield #* 
    #chance_perc_crop_fail * chance_perc_weather_fail
  
  Treeless_dinkel_yield[dinkel_indices] <-
    vv(dinkel_yields, cv_dinkel_yield, length(dinkel_indices)) * arable_area_treeless 
  Treeless_dinkel_benefit <- vv(dinkel_price, cv_dinkel_price, n_years) * Treeless_dinkel_yield #* 
    #chance_perc_crop_fail * chance_perc_weather_fail
  
  #processing and packaging costs per ton - as yield data is required to calculate 
  #this parameter it is placed here in benefits
  PPcost_einkorn <- rep(0, n_years)
  PPcost_wheat <- rep(0, n_years)
  PPcost_dinkel <- rep(0, n_years)
  PPcost_einkorn <-
    vv(crop_pp_cost, var_CV = var_cv, n_years) * Treeless_einkorn_yield
  PPcost_wheat <-
    vv(crop_pp_cost, var_CV = var_cv, n_years) * Treeless_wheat_yield
  PPcost_dinkel <-
    vv(crop_pp_cost, var_CV = var_cv, n_years) * Treeless_dinkel_yield
  
  #Monoculture output: system bottom line####
  # arable component 
  Treeless_total_benefit <- Treeless_einkorn_benefit + Treeless_wheat_benefit + Treeless_dinkel_benefit
  
  # Hail insurance per 1000€ market value - add insurance cost now as it depends 
  #on the turnover of the farm 
  Treeless_insurance <- (Treeless_total_benefit * hail_insurance)/1000
  
  #total cost of arable component
  Treeless_total_arable_total_cost <- Treeless_total_einkorn_cost + Treeless_total_wheat_cost + 
    Treeless_total_dinkel_cost + PPcost_einkorn + PPcost_wheat + PPcost_dinkel + Treeless_insurance

  Treeless_bottom_line_benefit <- (Treeless_total_benefit - Treeless_total_arable_total_cost) #* 
    #chance_market_fluc
  
  #CHECKPOINT!!
  # Create a data frame with the variables
  # results_treeless_df <- data.frame(
  #   n_years = n_years,
  #   Treeless_total_arable_total_cost= Treeless_total_arable_total_cost,
  #   Treeless_total_einkorn_cost = Treeless_total_einkorn_cost,
  #   Treeless_total_wheat_cost = Treeless_total_wheat_cost,
  #   Treeless_total_dinkel_cost = Treeless_total_dinkel_cost,
  #   PPcost_einkorn = PPcost_einkorn,
  #   PPcost_wheat = PPcost_wheat,
  #   PPcost_dinkel = PPcost_dinkel,
  #   Treeless_insurance = Treeless_insurance,
  #   Treeless_total_benefit = Treeless_total_benefit,
  #   Treeless_einkorn_benefit = Treeless_einkorn_benefit,
  #   Treeless_wheat_benefit = Treeless_wheat_benefit,
  #   Treeless_dinkel_benefit = Treeless_dinkel_benefit,
  #   Treeless_bottom_line_benefit = Treeless_bottom_line_benefit
  # )
  # file_path_treeless <- "calculated_treeless_variables.csv" # Specify the file path to save the CSV
  # write.csv(results_treeless_df, file = file_path_treeless, row.names = FALSE) # Write the data frame to a CSV file
  
  
  #Agroforestry (AF) System ####
  #Calculating model parameters based on input table
  Arable_area_AF <- arable_area_treeless - tree_row_area
  #AF benefits ####
    # herbaceous component - should be activated only if it is silvopastoral or agrisilvicultural system or 
  # if farmer grows forage crop
  #AF_grass_yield <- rep(0, n_years)
  
  #Annual arable crop component
    AF_einkorn_yield <- rep(0, n_years)
    AF_wheat_yield <- rep(0, n_years)
    AF_dinkel_yield <- rep(0, n_years)
  
  # account for yield reduction due to shading and competition from trees 
  perc_yield_reduction <- gompertz_yield(
    max_harvest = yield_reduc_max,
    time_to_first_yield_estimate = time_to_first_reduction,
    time_to_second_yield_estimate = time_to_second_reduction,
    first_yield_estimate_percent = perc_max_first_reduction,
    second_yield_estimate_percent = perc_max_second_reduction,
    n_years = n_years)

  #Crop rotation in AF system
  AF_einkorn_yield[einkorn_indices] <-
    vv(AF_einkorn_yields, cv_einkorn_yield, length(einkorn_indices)) *(1 - perc_yield_reduction[einkorn_indices]) * 
    Arable_area_AF #* AF_chance_perc_crop_fail * AF_chance_perc_weather_fail 
  
  AF_einkorn_benefit <- vv(einkorn_price, cv_einkorn_price, n_years) * AF_einkorn_yield 
  
  AF_wheat_yield[wheat_indices] <-
    vv(AF_wheat_yields, cv_wheat_yield, length(wheat_indices)) * (1 - perc_yield_reduction[einkorn_indices]) *
    Arable_area_AF #* AF_chance_perc_crop_fail * AF_chance_perc_weather_fail
  
  AF_wheat_benefit <- vv(wheat_price, cv_wheat_price, n_years) * AF_wheat_yield
  
  AF_dinkel_yield[dinkel_indices] <-
    vv(AF_dinkel_yields, cv_dinkel_yield, length(dinkel_indices)) * (1 - perc_yield_reduction[einkorn_indices])* 
    Arable_area_AF #* AF_chance_perc_crop_fail * AF_chance_perc_weather_fail
  
  AF_dinkel_benefit <- vv(dinkel_price, cv_dinkel_price, n_years) * AF_dinkel_yield
  
  #Tree component - nuts in AF system
  AF_nuts_yield <- rep(0, n_years)
  ES3_subsidy <- rep(0, n_years)

  #Yield of one nut tree [kg/tree]
  AF_nuts_yield <- gompertz_yield(
    max_harvest = nuts_yield_max,
    time_to_first_yield_estimate = time_to_first_nuts,
    time_to_second_yield_estimate = time_to_second_nuts,
    first_yield_estimate_percent = nuts_yield_first,
    second_yield_estimate_percent = nuts_yield_second,
    n_years = n_years,
    var_CV = var_cv,
    no_yield_before_first_estimate = TRUE
  )
  
  #Yield of all nut trees [kg] considering risks
  AF_tot_nuts_yield <-
    AF_nuts_yield * num_trees #* AF_chance_perc_crop_fail * AF_chance_perc_weather_fail
  
  #Calculate how many kg have nuts quality and can therefore be marketed at a higher price (Pack_nuts) and 
  #the rest are used for making oil
  Pc_pack_nuts <- vv(perc_pack_nuts, var_CV = var_cv, n_years) / 100
  
  Pack_nuts_yield <- AF_tot_nuts_yield * Pc_pack_nuts
  Oil_nuts_yield <- AF_tot_nuts_yield * (1 - Pc_pack_nuts)
  
  #The benefits from nuts are calculated by multiplying their yields by their respective prices
  Pack_nuts_benefit <-
    vv(Pack_nuts_price, var_cv, n_years) * Pack_nuts_yield
  Oil_nuts_benefit <-
    vv(Oil_nuts_price, var_cv, n_years) * Oil_nuts_yield
  

  # Timber yield function from Marcos 
  AF_timber_yield <- ontogenic_growth_gompertz(max_harvest = volume_target_rotation,
                                                     time_to_first_yield_estimate = time_first_volume_est,
                                                     time_to_second_yield_estimate = time_sec_volume_est,
                                                     first_yield_estimate_percent = first_vol_rel_to_sec_est_perc,
                                                     second_yield_estimate_percent = sec_volume_est_per,
                                                     n_years = n_years,
                                                     no_yield_before_first_estimate = FALSE)
  #total timber yield considering risks
  AF_tot_timber_yield <- AF_timber_yield * tree_row_area #* (AF_chance_perc_crop_fail * 
    #AF_chance_perc_weather_fail * AF_chance_timber_harvest_fail)
  # percentage of prime and other quality timber yield
  Pc_prime_timber <- vv(perc_prime_timber, var_CV = var_cv, n_years)
  Prime_timber_yield <- AF_tot_timber_yield * Pc_prime_timber
  Pc_bioenergy_timber <-
    vv(perc_bioenergy_timber, var_CV = var_cv, n_years)
  Bioenergy_timber_yield <- AF_tot_timber_yield * Pc_prime_timber
  # Adjust lengths if necessary
  if (length(Pc_prime_timber) != n_years) {
    Pc_prime_timber <- Pc_prime_timber[1:n_years]
  }
  
  if (length(Pc_bioenergy_timber) != n_years) {
    Pc_bioenergy_timber <- Pc_bioenergy_timber[1:n_years]
  }
  
  Other_timber_yield <-
    AF_tot_timber_yield * (1 - Pc_prime_timber - Pc_bioenergy_timber)
  
  #The benefits from selling the timber of nut trees after 25th year
  #The benefits from timber are calculated by multiplying their yields by their respective prices
  # ????Assuming all trees will not be harvested at the same year rather every year after 25th year continuous income 
  timber_harvest_indices <- seq(from = 25, to = n_years)
  Prime_timber_harvest_indices <- seq(from = 40, to = n_years) # takes more years to produce prime quality timber
  Prime_timber_benefit <- rep(0, n_years)
  Other_timber_benefit <- rep(0, n_years)
  
  Prime_timber_benefit[Prime_timber_harvest_indices] <-
    vv(Prime_timber_price, var_cv, length(Prime_timber_harvest_indices)) * Prime_timber_yield[Prime_timber_harvest_indices]
  Bioenergy_timber_benefit <- 
    vv(bioenergy_timber_price, var_cv, n_years) * Bioenergy_timber_yield
  Other_timber_benefit [timber_harvest_indices]<-
    vv(Other_quality_timber_price, var_cv,length(timber_harvest_indices)) * Other_timber_yield[timber_harvest_indices]
  
  
  # Intangible benefits from ESS like soil erosion control, improved soil quality and biodiversity, change in microclimate, reduced impact of extreme events
  #carbon sequestration in T C/ha/yr
  AF_C_sequestration <- gompertz_yield(
    max_harvest = C_sequeter_max,
    time_to_first_yield_estimate = 10, #time_to_first_C_sequester,
    time_to_second_yield_estimate = 15, #time_to_second_C_sequester,
    first_yield_estimate_percent = C_sequester_first,
    second_yield_estimate_percent = C_sequester_second,
    n_years = n_years,
    var_CV = var_cv,
    no_yield_before_first_estimate = TRUE
  )
  
  C_benefit <- vv(pc_carbon_storage, var_cv, n_years) * 
    AF_C_sequestration * arable_area_treeless
  
  GW_benefit <- rep(0, n_years)
  erosion_control_benefit <- rep(0, n_years)
  # groundwater storage and erosion control not realised in the first few years 
  #- can also make a variable and add vv fn.
  NMES_indices <- seq(from = 5, to = n_years)
  GW_benefit[NMES_indices] <-
    vv(pc_ground_water_recharge, var_cv, length(NMES_indices)) * arable_area_treeless
  
  erosion_control_benefit[NMES_indices] <- vv(soil_loss, var_cv, length(NMES_indices)) * 
    vv(pc_soil_loss, var_cv, length(NMES_indices)) * arable_area_treeless  
  #pollinator_benefit yet to be added
  
  Nonmarket_ES_benefit <- C_benefit + GW_benefit + erosion_control_benefit 
  
  # from Porter et al. 2009
  #Nonmarket_ES_benefit <- vv(Nonmarket_ES_value, var_cv, n_years) * tree_row_area
  
  AF_tree_benefit <- Pack_nuts_benefit + Oil_nuts_benefit + 
    Prime_timber_benefit + Other_timber_benefit + Bioenergy_timber_benefit +
    Nonmarket_ES_benefit
  #Subsidy in AF system
  ES3_subsidy[1:n_years] <- es3_subsidy * tree_row_area
  
  #Agroforestry total benefit
  AF_farm_benefit <- AF_tree_benefit + AF_einkorn_benefit + AF_wheat_benefit + 
    AF_dinkel_benefit + ES3_subsidy 
  AF_total_benefit <- AF_tree_benefit + AF_einkorn_benefit + AF_wheat_benefit + 
    AF_dinkel_benefit + ES3_subsidy + Nonmarket_ES_benefit
  
  #AF costs ####
  # Source: FE- farmer/practitioner estimate; EE- expert estimate
  #Calculating costs - tree component ###
  
  #Implementation cost : define variables
  AF_planning_cost <- rep(0, n_years) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[€]
  AF_dig_plant_holes <- rep(0, n_years) # FE; Second step of implementation: digging/drilling holes for the trees [€]
  AF_tree_cost <- rep(0, n_years) #FE; Cost per tree [€]
  AF_plant_tree_cost <- rep(0, n_years) #FE; Labour cost for planting one tree [€] -
  AF_protect_cost <- rep(0, n_years) #FE; Material cost of tree protection mesh [€]
  AF_weed_protect_cost <- rep(0, n_years) #Material cost of weed suppressing fleece [€]
  AF_compost_cost <- rep(0, n_years) #FE; Cost of compost used during planting [€]
  AF_irrigation_system_cost <- rep(0, n_years) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [€]
  AF_irrigation_planting_cost <- rep(0, n_years) #FE; Cost for watering in newly planted trees [€]
  AF_total_planting_cost <- rep(0, n_years)
  
  #Running cost : define variables
  AF_pruning <- rep(0, n_years) #FE/EE; Labour cost of pruning fruit trees [€]
  AF_root_pruning <- rep(0, n_years) #FE/EE; Labour cost of pruning roots of trees next to tree rows [€]
  AF_thinning <- rep(0, n_years) #FE/EE; Labour cost of thinning fruit trees [€]
  AF_nut_harvest <- rep(0, n_years) #FE; Labour cost of harvesting apples manually [€]
  ES3_application <- rep(0, n_years) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  AF_annual_irrigation <- rep(0, n_years) #FE; Cost of annual irrigation of tree rows [€]
  AF_mowing_treerow <- rep(0, n_years) #FE; Labour hours of mowing the tree rows manually [h/ha]
  AF_nuts_harvest <-  rep(0, n_years) #FE; Labour hours of mowing the tree rows manually [h/ha]
  
  # establishment costs
  
  #Planning and consulting
  AF_planning_cost[1] <-    planning_consulting + farmer_planning_time * Labour_costs[1]
  
  #Field prep
  AF_dig_plant_holes[1] <- dig_planting_holes * Labour_costs[1]
  AF_tree_cost[1] <- tree_price * num_trees
  AF_plant_tree_cost[1] <- planting_trees * Labour_costs[1]
  AF_protect_cost[1] <- plant_protection * num_trees
  AF_weed_protect_cost[1] <- weed_protection * num_trees
  AF_compost_cost[1] <- compost_planting * compost_price * num_trees
  AF_irrigation_system_cost[1] <- irrigation_sys_install
  AF_irrigation_planting_cost[1] <- irrigation_planting * water_price * num_trees
  
  AF_total_planting_cost <- AF_dig_plant_holes + AF_tree_cost + AF_plant_tree_cost + 
    AF_protect_cost + AF_weed_protect_cost + AF_compost_cost + AF_irrigation_system_cost + 
    AF_irrigation_planting_cost
  
  AF_total_investment_cost <- AF_planning_cost + AF_total_planting_cost #Investment cost of AF system implementation
  
  #Running costs
  ES3_application <- vv(es3_application, var_cv, n_years) * Labour_costs #application for Eco Scheme subsidy has to be repeated annually 
  
  # Though pruning is dependent on the tree growth, avg cost over the years is presumed 
  AF_pruning <- vv(pruning_annual, var_CV = var_cv, n_years) * Labour_costs * num_trees
  AF_root_pruning <- vv(root_pruning, var_CV = var_cv, n_years) * Labour_costs
  
  ##AF_thinning <- vv(pruning_annual, var_CV = var_cv, n_years) * Labour_costs * thinned_trees
  
  AF_annual_irrigation[1:3] <- vv(irrigation_123, var_CV = var_cv, 3)
  AF_annual_irrigation[4:n_years] <- vv(irrigation_annual, var_CV = var_cv, length(4:n_years))
  AF_annual_irrigation_cost <- AF_annual_irrigation * water_price
  
  AF_mowing_treerow <-
    vv(mowing_treerow, var_CV = var_cv, n_years) * tree_row_area * Labour_costs
  AF_nuts_harvest[time_to_first_nuts:n_years] <-
    vv(nuts_harvest, var_CV = var_cv, length(time_to_first_nuts:n_years)) # labour cost of harvesting
  # Packaging and processing of nuts
  AF_PPcost_nuts <- rep(0, n_years)
  AF_PPcost_nuts[time_to_first_nuts:n_years] <-
    vv(nuts_pp_cost, var_CV = var_cv, length(time_to_first_nuts:n_years)) * AF_tot_nuts_yield[time_to_first_nuts:n_years]
  AF_timber_harvest <- rep(0, n_years)
  AF_timber_harvest[time_first_volume_est:n_years] <-
    vv(timber_harvest, var_CV = var_cv, length(time_first_volume_est:n_years)) # labour cost of harvesting timber
  
  #Total cost of tree component in AF system
  AF_total_treerow_management_cost <- ES3_application + AF_pruning + AF_root_pruning + 
    AF_annual_irrigation_cost + AF_mowing_treerow + AF_nuts_harvest + AF_PPcost_nuts + AF_timber_harvest
  
  #Cost of arable component ###
  # Arable crops in AF 
  #Cost of seeds of arable crops (Labour involved in sowing is separately listed) [€]
  AF_einkorn_sowing_cost <- rep(0, n_years)
  AF_wheat_sowing_cost <- rep(0, n_years)
  AF_dinkel_sowing_cost <- rep(0, n_years)
  
  #Cost for machinery used in arable crops 
  #(includes fixed and variable cost but not the investment) [€]
  AF_einkorn_machinery_cost <- rep(0, n_years)
  AF_wheat_machinery_cost <- rep(0, n_years)
  AF_dinkel_machinery_cost <- rep(0, n_years)

  #Cost of labour in arable cropping system [€]
  AF_einkorn_labour_cost <- rep(0, n_years)
  AF_wheat_labour_cost <- rep(0, n_years)
  AF_dinkel_labour_cost <- rep(0, n_years)
  
  #Processing and packaging costs [€/t]
  AF_PPcost_einkorn <- rep(0, n_years)
  AF_PPcost_wheat <- rep(0, n_years)
  AF_PPcost_dinkel <- rep(0, n_years)
  
  #einkorn
  AF_einkorn_sowing_cost[einkorn_indices] <-
    vv(einkorn_seed_price,
       cv_einkorn_seed_price,
       length(einkorn_indices)) * Arable_area_AF #cost of seed [€/ha]*area managed [ha]
  
  AF_einkorn_machinery_cost[einkorn_indices] <-
    vv(einkorn_mach_price,
       cv_einkorn_mach_price,
       length(einkorn_indices)) * Arable_area_AF #cost of machinery [€/ha]*area managed [ha]
  
  AF_einkorn_labour_cost <-
    (einkorn_labour * Labour_costs * Arable_area_AF) #einkorn_labour * (vv(extra_arable_time, var_cv, n_years) /  100))
  
  AF_PPcost_einkorn <-
    vv(crop_pp_cost, var_CV = var_cv, n_years) * AF_einkorn_yield #processing and packaging costs per ton
  
  AF_total_einkorn_cost <-
    AF_einkorn_sowing_cost + AF_einkorn_machinery_cost + 
    AF_einkorn_labour_cost + AF_PPcost_einkorn 
  
  #Wheat
  AF_wheat_sowing_cost[wheat_indices] <-
    vv(wheat_seed_price, cv_wheat_seed_price, length(wheat_indices)) * Arable_area_AF #cost of seed [€/ha]*area managed [ha]

  AF_wheat_machinery_cost[wheat_indices] <-
    vv(wheat_mach_price, cv_wheat_mach_price, length(wheat_indices)) * Arable_area_AF #cost of machinery [€/ha]*area managed [ha]

  AF_wheat_labour_cost <-
    (Wheat_labour * Labour_costs * Arable_area_AF) #Wheat_labour * (vv(extra_arable_time, var_cv, n_years) /100)) #Labour cost associated with wheat cultivation
  AF_PPcost_wheat <-
    vv(crop_pp_cost, var_CV = var_cv, n_years) * AF_wheat_yield
  
  AF_total_wheat_cost <-
    AF_wheat_sowing_cost + AF_wheat_machinery_cost + 
    AF_wheat_labour_cost + AF_PPcost_wheat #+ AF_wheat_insurance_cost + AF_wheat_fertilizer_cost + AF_wheat_pesticide_cost +
  
  #dinkel
  AF_dinkel_sowing_cost[dinkel_indices] <-
    vv(dinkel_seed_price,
       cv_dinkel_seed_price,
       length(dinkel_indices)) * Arable_area_AF #cost of seed [€/ha]*area managed [ha]
  
  AF_dinkel_machinery_cost[dinkel_indices] <-
    vv(dinkel_mach_price,
       cv_dinkel_mach_price,
       length(dinkel_indices)) * Arable_area_AF #cost of machinery [€/ha]*area managed [ha]
  
  AF_dinkel_labour_cost <-
    (dinkel_labour * Labour_costs * Arable_area_AF) #dinkel_labour * (vv(extra_arable_time, var_cv, n_years) / 100))#Labour cost associated with dinkel cultivation
  AF_PPcost_dinkel <-
    vv(crop_pp_cost, var_CV = var_cv, n_years) * AF_dinkel_yield
  
  AF_total_dinkel_cost <-
    AF_dinkel_sowing_cost + AF_dinkel_machinery_cost + 
    AF_dinkel_labour_cost + AF_PPcost_dinkel #+ AF_dinkel_insurance_cost + AF_dinkel_fertilizer_cost + AF_dinkel_pesticide_cost
  
  #Total cost of arable component in AF system
  AF_total_arable_management_cost <- AF_total_einkorn_cost + AF_total_wheat_cost + AF_total_dinkel_cost
  #Total running cost of AF system
  AF_total_running_cost <- AF_total_treerow_management_cost + AF_total_arable_management_cost 
  # Hail insurance per 1000€ market value - add insurance cost now as it depends on the turnover of the farm 
  AF_insurance <- (AF_total_benefit * hail_insurance)/1000
  #Total cost of AF system
  AF_total_cost <- AF_total_investment_cost + AF_total_running_cost + AF_insurance
  
  #Agroforestry output: system bottomline####
  AF_bottom_line_benefit <- (AF_total_benefit - AF_total_cost) #* AF_chance_market_fluc
  AF_bottom_line_benefit_farm <- (AF_farm_benefit - AF_total_cost) #* AF_chance_market_fluc
  
  #CHECKPOINT!!
  # Create a data frame with the variables
  # results_AF_df <- data.frame(
  #   n_years = n_years,
  #   AF_total_cost= AF_total_cost,
  #   AF_total_investment_cost = AF_total_investment_cost,
  #   AF_total_running_cost = AF_total_running_cost,
  #   AF_insurance = AF_insurance,
  #   AF_total_running_cost = AF_total_running_cost,
  #   AF_total_einkorn_cost = AF_total_einkorn_cost,
  #   AF_total_wheat_cost = AF_total_wheat_cost,
  #   AF_total_dinkel_cost = AF_total_dinkel_cost,
  #   ES3_application = ES3_application,
  #   AF_pruning = AF_pruning,
  #   AF_root_pruning = AF_root_pruning,
  #   AF_annual_irrigation_cost = AF_annual_irrigation_cost,
  #   AF_mowing_treerow = AF_mowing_treerow,
  #   AF_nuts_harvest = AF_nuts_harvest, 
  #   AF_PPcost_nuts = AF_PPcost_nuts,
  #   AF_total_benefit = AF_total_benefit,
  #   AF_tree_benefit = AF_tree_benefit,
  #   AF_einkorn_benefit = AF_einkorn_benefit,
  #   AF_wheat_benefit = AF_wheat_benefit,
  #   AF_dinkel_benefit = AF_dinkel_benefit,
  #   ES3_subsidy = ES3_subsidy,
  #   AF_chance_market_fluc =AF_chance_market_fluc,
  #   AF_bottom_line_benefit = AF_bottom_line_benefit,
  #   AF_bottom_line_benefit_farm = AF_bottom_line_benefit_farm
  # )
  # file_path_AF <- "calculated_AF_variables.csv"# Specify the file path to save the CSV
  # write.csv(results_AF_df, file = file_path_AF, row.names = FALSE)  # Write the data frame to a CSV file

  #Calculating NPVs and Cash Flows####
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
  
  #AF only farm-level without intangibles
  AF_NPV_farm <- discount(AF_bottom_line_benefit_farm, discount_rate=discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow_farm <- discount(AF_bottom_line_benefit_farm,discount_rate=discount_rate,
                           calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow_farm <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  #Tradeoff (difference between AF system and treeless system)
  Tradeoff_benefit <- AF_bottom_line_benefit - Treeless_bottom_line_benefit
  Tradeoff_benefit_farm <- AF_bottom_line_benefit_farm - Treeless_bottom_line_benefit
  
  NPV_tradeoff <- discount(Tradeoff_benefit, discount_rate = discount_rate,
                           calculate_NPV = TRUE )
  NPV_tradeoff_farm <- discount(Tradeoff_benefit_farm, discount_rate = discount_rate,
                           calculate_NPV = TRUE )
  CF_Tradeoff <- discount(Tradeoff_benefit, discount_rate = discount_rate,
                          calculate_NPV = FALSE )
  CF_Tradeoff_farm <- discount(Tradeoff_benefit_farm, discount_rate = discount_rate,
                          calculate_NPV = FALSE )
  Cum_CF_Tradeoff <- cumsum(CF_Tradeoff)
  Cum_CF_Tradeoff_farm <- cumsum(CF_Tradeoff_farm)
  
  #Defining what output variables the following Monte Carlo Simulation should create #####
  return(list(NPV_Agroforestry_System = AF_NPV,
              NPV_Treeless_System = NPV_treeless_system,
              NPV_AF_Farm_level = AF_NPV_farm,
              NPVtrade_off = NPV_tradeoff,
              NPVtradeoff_farm_level = NPV_tradeoff_farm,
              AFcashflow = AF_cash_flow,
              AFcashflow_farm_level = AF_cash_flow_farm,
              AFcumcashflow = AF_cum_cash_flow,
              AFcumcashflow_farm_level = AF_cum_cash_flow_farm,
              Treelesscashflow = Treeless_cash_flow,
              Treelesscumcashflow = Treeless_cum_cash_flow
              #treeless costs#treeless benefits #AF costs #AF benefits
              ))
}

#Run the Monte Carlo analysis of the model
mcSimulation_results <- mcSimulation(
  estimate = estimate_read_csv(fileName = "Input_table_R2.csv"),
  model_function = AF_benefit,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames")

# Print the output using make_variables function
#print(generated_variables)

write.csv(generated_variables, file = "variables_output.csv", row.names = FALSE)

# PLOTS####
# plot NPV distributions
# AF with intangibles and Treeless
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7,
                   x_axis_name = "Outcome as NPV in € for 7.53 ha mixed nuts system",
                   scale_x_continuous(labels = function(x) x / 100000),
                   ggtitle("Net Present Value of the system over 60 years with intangibles"),
                   legend.position="bottom")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# AF without intangibles and Treeless
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_Treeless_System", "NPV_AF_Farm_level"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7,
                   x_axis_name = "Outcome as NPV in € for 7.53 ha mixed nuts system",
                   scale_x_continuous(labels = function(x) x / 100000),
                   ggtitle("Net Present Value of the system over 60 years without intangibles"),
                   legend.position="right")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_farm.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

# boxplots of outcome distributions
# AF with intangibles and Treeless
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_Treeless_System","NPV_Agroforestry_System"),
                                    method = 'boxplot',
                                    x_axis_name = "Outcome as NPV in € for 7.53 ha mixed nuts system",
                                    ggtitle("Net Present Value of the system over 60 years with intangibles"),
                                    legend.position="right")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_boxplot.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# AF without intangibles and Treeless
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_Treeless_System","NPV_AF_Farm_level"),
                                    method = 'boxplot',
                                    x_axis_name = "Outcome as NPV in € for 7.53 ha mixed nuts system",
                                    ggtitle ("Net Present Value of the system over 60 years without intangibles"),
                                    legend.position="right")

ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_farm_boxplot.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

# value of the decision (difference in NPV between AF and treeless
# tradeoff between treeless and AF with intangibles
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPVtrade_off",
                                    old_names = "NPVtrade_off",
                                    new_names = "NPV (over 60 years) of the decision: do mixed nut agroforestry intervention on 7.53 hectares",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "Net decision outcome (NPV in Euro)")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_Tradeoff.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# tradeoff between treeless and AF without intangibles
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPVtradeoff_farm_level",
                                    old_names = "NPVtradeoff_farm_level",
                                    new_names = "NPV (over 60 years) of the decision: do mixed nut agroforestry intervention on 7.53 hectares without intangible benefits",
                                    method = 'boxplot_density',
                                    y_axis_name = "Probability",
                                    x_axis_name = "Net decision outcome (NPV in Euro)")
ggsave(
  filename = "images/NPV_Treeless_vs_NPV_AF_farm_Tradeoff.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

# Cashflow analysis
# AF with intangiles
decisionSupport::plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "AFcashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Annual Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue"
        )
ggsave(
  filename = "images/AnnualCashflow_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#Treeless
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "Treelesscashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Annual Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/AnnualCashflow_Treeless.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# AF without intangiles
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "AFcashflow_farm_level",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Annual Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/AnnualCashflow_AF_farmlevel.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#Cumulative cashflow of AF with intangiles
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "AFcumcashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Cumulative Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/CumulativeCashflow_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
# Cumulative cashflow of treeless
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "Treelesscumcashflow",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Cumulative Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/AnnualCashflow_Treeless.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#Cumulative cashflow of AF without intangiles
plot_cashflow(
  mcSimulation_object = mcSimulation_results,
  cashflow_var_name = "AFcumcashflow_farm_level",
  x_axis_name = "Timeline of intervention [a]",
  y_axis_name = "Cumulative  Cashflow [€]",
  color_25_75 = "navajowhite",
  color_5_95 = "green4",
  color_median = "darkblue",
)
ggsave(
  filename = "images/CumulativeCashflow_AF_farmlevel.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)


#Projection to Latent Structures (PLS) analysis
# AF with intangibles
pls_result_AF <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[1], ncomp = 1)
plot_pls(pls_result_AF, input_table = input_file, cut_off_line = 1, threshold = 0.5)
ggsave(
  filename = "images/PLS_VIP_AF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#treeless
pls_result_treeless <- plsr.mcSimulation(object = mcSimulation_results,
                                         resultName = names(mcSimulation_results$y)[2], ncomp = 1)
plot_pls(pls_result_treeless, input_table = input_file, cut_off_line = 1, threshold = 0.5)
ggsave(
  filename = "images/PLS_VIP_Treeless.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)
#AF without intangibles
pls_result_AF_farm <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)
plot_pls(pls_result_AF_farm, input_table = input_file, cut_off_line = 1, threshold = 0.5)
ggsave(
  filename = "images/PLS_VIP_AF_farmlevel.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

#Value of Information Analysis using decisionSupport package
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[4:5])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPVtrade_off")
plot_evpi(evpi, decision_vars = "NPVtrade_off")


#END!!!!##