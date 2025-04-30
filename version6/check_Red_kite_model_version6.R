# ----------------------------------------------------------------
# check and visualisation of Red Kite Dynamics without disturbance
# ----------------------------------------------------------------
# This script gives a visual overview of the Redkite population without disturbance 

# ----------------------------------------------------------------
# Authors: Neele Haß 
# ----------------------------------------------------------------
source("./help_functions_version6.R")

# ----------------------------------------------------------------
# Simulation Setup -----------------------------------------
# ----------------------------------------------------------------
## Dim and timesteps -----
timesteps <- 30    # Number of timesteps in the simulation
resolution <- 1 * 1 # set resoltuion [km2] 
x_dim <- 100           # Grid width (x-axis)
y_dim <- 100           # Grid height (y-axis)

area <- x_dim * y_dim * resolution  # Area in [km2], dimensions [grid cells], resolution [km2/grid cell]
resolution_factor <- 1 / resolution  # Conversion factor for 1 km2

# load timesteps, dim and REd-kite parameters from `output_simulation_version6.R`
region <- array(0, dim = c(x_dim, y_dim, timesteps)) # Region
building_buffer <- array(FALSE, dim = c(x_dim, y_dim, timesteps)) # Obstacles
turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for turbines
buffer <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for buffer zones

# ----------------------------------------------------------------
# Red Kite Parameters
# ----------------------------------------------------------------
# Growth and Survival Rates
prod_n <- 1
prod_rate <- 0.79  # Breeding success rate
growth_rate <- prod_rate  # Used in Ricker equation

# Initial Population Estimates
initial_adults <- (3 * 2) * (area / 100)  # Initial number of adult pairs per 100 km2
initial_lonely <- initial_adults * 0.10  # 10% are lonely kites
initial_new_born <- (initial_adults / 2) * growth_rate  # Newborns based on half the adult population

# Carrying capacity for red kite pairs (empty nests) calculated based on area
carrying_capacity <- 4 * (area / 100)  # max 19 [18.5 pairs / 100km2]

# Reproductive Age
rep_age <- 3

# Life Expectancy
liv_exp <- 12  # Expected productive lifespan of a nest

# Dispersal Parameters
dis_num <- 20 * resolution_factor
dispersal <- func_dispersal(dis_num)

new_nest_dist <- 1 # threshold of distance between lonely kites to build a new nest
# ----------------------------------------------------------------
# INITIAL Red Kite
# ----------------------------------------------------------------
source("./Redkite_eco_para_setting_version6.R")
ini_red_kite_para_setting <- ini_red_kite_para_setting(timesteps, resolution, x_dim, y_dim,
                                                       rep_age, growth_rate, liv_exp, initial_adults, initial_lonely, initial_new_born,
                                                       region, building_buffer, turbine, buffer)

kites_metric <- ini_red_kite_para_setting$kites_metric
kites <- ini_red_kite_para_setting$kites
kites_abund <- ini_red_kite_para_setting$kites_abund

# ----------------------------------------------------------------
# TEST Red Kite without disturbance ----
# ----------------------------------------------------------------
# Turbines and Obstacles are ZERO!!
source("./Red_kite_model_version6.R")

test_Red_kite_population <- test_Red_kite_population(x_dim, y_dim, t,
                                                region, building_buffer, turbine, buffer,
                                                rep_age, growth_rate, liv_exp, carrying_capacity,
                                                kites_metric, kites_abund)
  
test_wodis_kites_abund <- test_Red_kite_population$kites_abund
test_kites_metric <- test_Red_kite_population$kites_metric
test_kites <- test_Red_kite_population$kites
test_turbines <- test_Red_kite_population$turbine

pop_func_plot(test_wodis_kites_abund, test_turbines, "TEST_Red_kite_carry_capa_4_new_nest_dist_test_without_disturb", 
              addmain = "without disturbance!", 
              commi = "new_nest_dist = 1 \n carrying_capacity = [4 pairs / 100km2] \nused script: Red_kite..version6.R, test function")

# # save plot
# pop_func_plot(test_wodis_kites_abund, test_turbines, "new_nest_dist_test_without_disturb", 
#               addmain = "without disturbance!", 
#               commi = "immigration or\nnest build rate needed\nfor stable population!\npopulation is dying!")

# !!!!!!!!!
# red_kites population will decline (not survive) even without disturbance
# its worth to integrate immigration factor otherwise 
# or 
# integrate new nest building rate! 
# now peak of 547 lonely kites and only 72 new nest were built !
# !!!!!!

