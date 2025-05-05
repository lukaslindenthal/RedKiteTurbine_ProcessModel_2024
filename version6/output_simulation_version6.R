# ----------------------------------------------------------------
# Turbine and Red Kite Model Simulation and Visualisation ----
# ----------------------------------------------------------------
# This script simulates the interaction between wind turbine placement and red 
# kite dynamics over a series of timesteps, each representing one year.

# Key Features and Assumptions the model is based upon:

# - The script integrates environmental constraints and red kite behavioral models 
#   to simulate realistic ecological interactions.
# - Placement of turbines does not initially take red kite populations into account
#   and is therefore modeled independently. Assuming no initial impact assessment or 
#   mitigation strategies are considered in order to answer the research question.
# - Red kite nests are assumed to produce only one juvenile per year, simplifying 
#   reproductive dynamics.
# - Encounters of more than two moving adults result in only two surviving and 
#   forming a nest. With the approach to model social interactions among red kites
#   and simulating territorial or survival challenges. And also to simplify the process:)

# in Version 6 changes were implemented:

# Trubin inital & Red kit inital settings & model as functions
# extra code to run the simulations "output_simulation_version6"

# ----------------------------------------------------------------
# Authors: Neele Haß & Lukas Lindenthal
# ----------------------------------------------------------------

folder_name <- "timesteps_try2_time30"
source("./help_functions_version6.R")

# ----------------------------------------------------------------
# Simulation Set-up -----------------------------------------
# ----------------------------------------------------------------
## Dim and timesteps -----
timesteps <- 30    # Number of timesteps in the simulation
resolution <- 1 * 1 # set resoltuion [km2] 
x_dim <- 100           # Grid width (x-axis)
y_dim <- 100           # Grid height (y-axis)

area <- x_dim * y_dim * resolution  # Area in [km2], dimensions [grid cells], resolution [km2/grid cell]
resolution_factor <- 1 / resolution  # Conversion factor for 1 km2

## Turbine and Region setup -----
# Add Obstacles to the Region
n_hinder_1 <- 50                          # Number of obstacles (e.g. buildings)

# Initialize buffer zones for turbines
buffer_zone <- c(-2, -1, 0, 1, 2)

# initial Turbine set up
n_turb_1 <- 20                                  # Number of initial turbines
turb_neu_max <- 50    # Max total nr of turbines
turb_neu_perc <- 0.8  # Percentage of existing turbines for new construction

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
# Initialize
# ----------------------------------------------------------------

# INITIAL Region, Obsticals, Turbines 
source("./Time_Dim_Turbo_Region_para_setting_version6.R")
ini_time_dim_turb_region_para_setting <- ini_time_dim_turb_region_para_setting(timesteps, resolution, x_dim, y_dim,
                                                   n_hinder_1, 
                                                   n_turb_1, turb_neu_max, turb_neu_perc)
region <- ini_time_dim_turb_region_para_setting$region
building_buffer <- ini_time_dim_turb_region_para_setting$building_buffer
turbine <- ini_time_dim_turb_region_para_setting$turbine
buffer <- ini_time_dim_turb_region_para_setting$buffer

# INITIAL Red Kite
source("./Redkite_eco_para_setting_version6.R")
ini_red_kite_para_setting <- ini_red_kite_para_setting(timesteps, resolution, x_dim, y_dim,
                                                       rep_age, growth_rate, liv_exp, initial_adults, initial_lonely, initial_new_born,
                                                       region, building_buffer, turbine, buffer)

kites_metric <- ini_red_kite_para_setting$kites_metric
kites <- ini_red_kite_para_setting$kites
kites_abund <- ini_red_kite_para_setting$kites_abund

# ----------------------------------------------------------------
# Simulation
# ----------------------------------------------------------------
source("./3_model_version6.R")
model_simualtion <- model_simualtion(timesteps, resolution, x_dim, y_dim, buffer_zone,
                                     region, building_buffer, turbine, buffer,
                                     turb_neu_max, turb_neu_perc, 
                                     rep_age, growth_rate, liv_exp, carrying_capacity,
                                     kites_metric, kites_abund) 
# RESULTS
## Region, Turbine
results_region <- model_simualtion$region
results_building_buffer <- model_simualtion$building_buffer
results_turbine <- model_simualtion$turbine
results_buffer <- model_simualtion$buffer

## Red Kites
results_kites_metric <- model_simualtion$kites_metric
results_kites <- model_simualtion$kites # type = c("loc", "killed_move", "killed_build")
results_kites_abund <- model_simualtion$kites_abund

# ----------------------------------------------------------------
# PLOT Red Kite with disturbance ----
# ----------------------------------------------------------------
# recent set up: init_turb = 20, turb_neu_max = 50, turb_neu_perc = 0.8

# save plot
pop_func_plot(folder_name, 
              results_kites_abund, results_turbine, "model6_disturb_20_max50_turb", 
              addmain = "init_turb = 20, turb_neu_max = 50, turb_neu_perc = 0.8", 
              commi = "new_nest_dist = 1 \n carrying_capacity = [4 pairs / 100km2]")

# ----------------------------------------------------------------
# SPATIAL VISUALISATION ----
# ----------------------------------------------------------------
# time <- c(1, 5, 10, 15, 20, 25, 30)
time <- 1:timesteps

spat_visual(folder_name, time, x_dim, y_dim,
            results_region, results_buffer, results_building_buffer, results_turbine,
            results_kites) 

# ----------------------------------------------------------------
# COMPARISON Red Kite with and without disturbance ----
# ----------------------------------------------------------------

# ! get Results from Red kite model without disturbance "check_Red_kite_model_version6.R"

# plot in COMPARISON
abund <- results_kites_abund
wo_dis <- test_wodis_kites_abund$total_abund # run "check_Red_kite_model_version6.R
turbine <- results_turbine
name <- "comp_3model6_turb_20_max50"
addmain <- "comparision without and with disturbance"
commi <- "new_nest_dist = 1 \ncarrying_capacity = [4 pairs / 100km2]\n 
\ninit_turb = 20, \nturb_neu_max = 50, \nturb_neu_perc = 0.8\n
\n 3_model_verison6"

comp_plot(folder_name,
          abund, wo_dis, turbine, name, addmain, commi)


