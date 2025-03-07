# ----------------------------------------------------------------
# Red Kite Parameter Settings
# ----------------------------------------------------------------
# This script defines the initial parameters for simulating red kite dynamics.

# ----------------------------------------------------------------
# Authors: Neele Haß & Lukas Lindenthal
# ----------------------------------------------------------------

# Assumptions
# ----------------------------------------------------------------
# Kites are assumed to reach sexual maturity at age 3.
# Initial population estimate is 3 pairs per 100km² with an additional 10% random adults.
# Each nest is assumed to produce one juvenile per year.
# Breeding occurs once per timestep/year.
# Distance between nests is randomly set between 4000 and 1500 meters.
# The age of a nest is averaged between the two kites.

# Dependencies ----
source("./scripts/new_Redkite_eco_constrains/new_Time_Dim_Turbo_Region_para_setting.R") # load timesteps, dim and Turbine

library(ggplot2)
library(abind)

# Seed for Reproducibility
set.seed(42)
#test
# ----------------------------------------------------------------
# Red Kite Parameters
# ----------------------------------------------------------------
# Initialize Red Kite Parameters
area <- x_dim * y_dim * resolution  # Area in [km2], dimensions [grid cells], resolution [km2/grid cell]
resolution_factor <- 1 / resolution  # Conversion factor for 1 km2

# Carrying capacity calculated based on area
carrying_capacity <- (19 * 2) * (area / 100)  # [18.5 pairs / 100km2]

# Growth and Survival Rates
prod_n <- 1
prod_rate <- 0.79  # Breeding success rate
growth_rate <- prod_rate  # Used in Ricker equation

# Reproductive Age
rep_age <- 3

# Life Expectancy
liv_exp <- 12  # Expected productive lifespan of a nest

# Initial Population Estimates
initial_adults <- (3 * 2) * (area / 100)  # Initial number of adult pairs per 100 km2
initial_lonely <- initial_adults * 0.10  # 10% are lonely kites
initial_new_born <- (initial_adults / 2) * growth_rate  # Newborns based on half the adult population

# Dispersal Parameters
dis_num <- 20 * resolution_factor
dispersal <- expand.grid(dx = -dis_num:dis_num, dy = -dis_num:dis_num)
# exclude center 
ex <- which((dispersal[,1] == 0) & (dispersal[,2] == 0), arr.ind = TRUE)
dispersal <- dispersal[-ex,]  # Exclude the center point

# ----------------------------------------------------------------
# Kite Dynamics Arrays
# ----------------------------------------------------------------
# Define a 4D array to manage kite data across different dimensions and states
kites_nest <- array(FALSE, dim = c(x_dim, y_dim, timesteps)) # placment of nest
kites_juv <- array(FALSE, dim = c(x_dim, y_dim, timesteps)) # number of new born kites
kites_abund <- array(0, dim = c(x_dim, y_dim, timesteps))  # total abundance red kites
kites_age <- array(0, dim = c(x_dim, y_dim, timesteps))  # age lonely red kites, juv are countet hier as well, dies if > 12
kites_age_nest <- array(0, dim = c(x_dim, y_dim, timesteps))  # age nest, dies if > 12
kites_killed_move <- array(0, dim = c(x_dim, y_dim, timesteps)) # killed lonely adult kites by turbine (movement/flying) (mortality 2)
kites_killed_build <- array(0, dim = c(x_dim, y_dim, timesteps)) # killed kites/nest by turbine builded in timestep (mortality 1)

kites <- abind(kites_nest, kites_juv, kites_abund, kites_age, kites_age_nest, kites_killed_move, kites_killed_build, along = 4)
# Add dimension names for clarity
dimnames(kites) <- list(
  x_dim = NULL,
  y_dim = NULL,
  timesteps = NULL,
  type = c("nest", "juv", "abundance", "age_lonely", "age_nest", "killed_move", "killed_build")
)mes(kites) <- list(NULL, NULL, NULL, c("nest", "juv", "abundance", "age_lonely", "age_nest", "killed_move", "killed_build"))

# ----------------------------------------------------------------
# Initial Placement of Kites
# ----------------------------------------------------------------
# Check for even distribution of initial adult kites
is_even <- (initial_adults %% 2) == 0 # check ob es durch 2 teilbar ist
if (is_even == TRUE){
  num_nests <- initial_adults/2
  lonely_kite <- initial_lonely
} else {
  num_nests <- (initial_adults/2) - 1
  lonely_kite <- initial_lonely + 1
}

# Determine valid cells for initial placement of kite nests
random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                       & !building_buffer[, , 1] & !buffer[, , 1], arr.ind = TRUE)
random_coords <- matrix(random_coords, ncol = 2)

if (nrow(random_coords) > 0) {
  
  selected_coords <- matrix(0, ncol = 2, nrow = 0) # store later valid coords
  
  # Randomly sample potential nest positions
  pot_coords <- random_coords[sample(1:nrow(random_coords), min(num_nests, nrow(random_coords))), , drop = FALSE]
  
  # distnace check, fällt weg wenn resolution 10km x 10 km
  # also:
  selected_coords <- rbind(selected_coords, pot_coords)
  
  # Place red kites in the selected coordinates
  if (nrow(selected_coords) > 0) {
    for (i in 1:nrow(selected_coords)) {
      kites[selected_coords[i, 1], selected_coords[i, 2], 1, "nest"] <- TRUE
    }
  }
}

coords_nest <- which(kites[,,1,"nest"] == 1, arr.ind = TRUE)
for (i in 1:nrow(coords_nest)){
  kites[coords_nest[i,1], coords_nest[i,2], 1, "abundance"] <- 2
  kites[coords_nest[i,1], coords_nest[i,2], 1, "age_nest"] <- sample(rep_age:liv_exp, 1) # all kites are > 1 year
  
}
# new born to nests ----
num_nests <- nrow(coords_nest) # number of nests could chnage due to distance condtions
inital_new_born <- round(num_nests * growth_rate)

coords_juv <- coords_nest[sample(1:nrow(coords_nest), min(inital_new_born, nrow(coords_nest))), , drop = FALSE]

for (i in 1:nrow(coords_juv)) {
  # Add the corresponding number of new borns to each nest
  kites[coords_juv[i,1], coords_juv[i,2], 1, "juv"] <-  1
  kites[coords_juv[i,1], coords_juv[i,2], 1, "age_lonely"] <-  sample(1:(rep_age-1), 1) # new kites age, between new born and almost adult
  
  #add juv abundance per nest
  kites[coords_juv[i,1], coords_juv[i,2], 1, "abundance"] <- 
    kites[coords_juv[i,1], coords_juv[i,2], 1, "abundance"] + 1
  
}

# Verify the total number of new borns
sum(kites[,,1, "juv"]) == initial_new_born

# placment of lonely kit, if exist ----
# age between 3 and liv_exp, adult
if (lonely_kite > 0){
  random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                         & !building_buffer[, , 1] & !buffer[, , 1] 
                         & !kites[,,1,"nest"] == 1, arr.ind = TRUE)
  random_coords <- matrix(random_coords, ncol = 2)
  
  # Only proceed if valid cells exist
  if (nrow(random_coords) > 0) {
    # Sample new positions for red kites
    selected_coords <- random_coords[sample(1:nrow(random_coords), min(lonely_kite, nrow(random_coords))), ]
    selected_coords <- matrix(selected_coords, ncol = 2)
    
    # Place red kites in the selected coordinates
    if (nrow(selected_coords) > 0) {
      for (i in 1:nrow(selected_coords)) {
        kites[selected_coords[i, 1], selected_coords[i, 2], 1, "abundance"] <- 1
        kites[selected_coords[i,1], selected_coords[i,2], 1, "age_lonely"] <- sample(rep_age:liv_exp, 1) 
      }
    }
  }
}



