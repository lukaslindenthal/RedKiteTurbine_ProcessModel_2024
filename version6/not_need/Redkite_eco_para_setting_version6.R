# ----------------------------------------------------------------
# Red Kite Parameter Settings
# ----------------------------------------------------------------
# This script defines the initial parameters for simulating red kite dynamics.

# ----------------------------------------------------------------
# Authors: Neele Haß & Lukas Lindenthal
# ----------------------------------------------------------------

# Assumptions
# ----------------------------------------------------------------
# Initial population estimate is 3 pairs per 100km² with an additional 10% single adults.
# Each nest is assumed to produce one juvenile per year.
# Breeding occurs once per timestep/year.
# Kites are assumed to reach sexual maturity at age 3, then they leave the nest.
# The dispersal is a random distance within a radius of 20km.
# If kites are mature and lonley they move within the dispersal distance.
# If two mature lonely kites meet they form a new nest, and can already breed in this timestep
# The age of a new nest is randomly set.

# Dependencies ----
source("./help_functions_version6.R")
library(abind)

ini_red_kite_para_setting <- function(timesteps, resolution, x_dim, y_dim,
                                      rep_age, growth_rate, liv_exp, initial_adults, initial_lonely, initial_new_born,
                                      region, building_buffer, turbine, buffer) {
  
  # Seed for Reproducibility
  set.seed(42)
  # ----------------------------------------------------------------
  # Kite Dynamics Arrays
  # ----------------------------------------------------------------
  # Define a 4D array to manage kite data across different dimensions and states
  kites_loc <- array(0, dim = c(x_dim, y_dim, timesteps)) # 1 = lonely kites, 2 = empty nest, 3 = nest + juv
  kites_killed_move <- array(0, dim = c(x_dim, y_dim, timesteps)) # killed lonely adult kites by turbine (movement/flying) (mortality 2)
  kites_killed_build <- array(0, dim = c(x_dim, y_dim, timesteps)) # killed kites/nest by turbine builded in timestep (mortality 1)
  
  kites <- abind(kites_loc, kites_killed_move, kites_killed_build, along = 4)
  # Add dimension names for clarity
  dimnames(kites) <- list(
    x_dim = NULL,
    y_dim = NULL,
    timesteps = NULL,
    type = c("loc", "killed_move", "killed_build")
  )
  dimnames(kites)
  
  kites_metric <- vector("list", timesteps*2) # keep track, df is stored each timestep
  names(kites_metric) <- c(paste(rep("timestep_", timesteps), 1:timesteps, sep =""),
                           paste(rep("killed_", timesteps), 1:timesteps, sep =""))
  
  df_kites <- data.frame(x = numeric(), 
                         y = numeric(),
                         nest_age = numeric(),
                         juv_age = numeric(),
                         lonely_age = numeric())
  
  kites_abund <- data.frame(timestep = rep(NA, timesteps),
                            total_abund = rep(NA, timesteps),
                            abund_nest = rep(NA, timesteps),
                            abund_juv = rep(NA, timesteps),
                            abund_lonely = rep(NA, timesteps),
                            abund_new_juv = rep(NA, timesteps),
                            new_nest = rep(NA, timesteps),
                            nat_dead = rep(NA, timesteps),
                            nest_dead = rep(NA, timesteps),
                            killed_move = rep(NA, timesteps),
                            killed_build = rep(NA, timesteps))
  # ----------------------------------------------------------------
  # Initial Placement of Kites
  # ----------------------------------------------------------------
  # Check for even distribution of initial adult kites
  is_even <- (initial_adults %% 2) == 0 # check ob es durch 2 teilbar ist
  if (is_even == TRUE){
    num_nests <- initial_adults/2
    num_lonely <- initial_lonely
  } else {
    num_nests <- (initial_adults/2) - 1
    num_lonely <- initial_lonely + 1
  }
  
  # nests & juvs ----
  # Determine valid cells for initial placement of kite nests
  random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                         & !building_buffer[, , 1] & !buffer[, , 1], arr.ind = TRUE)
  random_coords <- matrix(random_coords, ncol = 2)
  
  if (nrow(random_coords) > 0) {
    
    selected_coords_nest <- select_coords(random_coords, num_nests)
    num_nests <- nrow(selected_coords_nest) # number of nests could chnage due to distance condtions
    
    # random age adult kites, all kites are >= rep_age 
    rand_nest_age <- sample(rep_age:liv_exp, num_nests, replace = TRUE) 
    
    # new born to nests 
    inital_new_born <- round(num_nests * growth_rate)
    vec <- c(sample(1:(rep_age-1), inital_new_born, replace = TRUE), rep(0, num_nests-inital_new_born))
    rand_juv_age <- sample(vec)
    
    # df_kites
    df_kites <- data.frame(x = selected_coords_nest[,1],
                           y = selected_coords_nest[,2],
                           nest_age = rand_nest_age,
                           juv_age = rand_juv_age,
                           lonely_age = rep(0, num_nests))
    
    
  }
  # Verify the total number nest & juv
  if(sum(df_kites["nest_age"] > 0) != num_nests ||
     sum(df_kites["juv_age"] > 0) != inital_new_born){
    print(paste("nests:", sum(df_kites["nest_age"] > 0), num_nests, 
                "\njuvs:",sum(df_kites["juv_age"] > 0) , inital_new_born))
    stop("error| intial kites \n total number of nests and juv")
  }
  
  # lonely kit, if exist ----
  # age between 3 and liv_exp, adult
  if (num_lonely > 0){
    random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                           & !building_buffer[, , 1] & !buffer[, , 1], arr.ind = TRUE)
    random_coords <- matrix(random_coords, ncol = 2)
    
    # delete the one whoch are the same like nest
    random_coords <- random_coords[!(random_coords[,1] %in% df_kites$x & random_coords[,2] %in% df_kites$y),]
    
    # Only proceed if valid cells exist
    if (nrow(random_coords) > 0) {
      # Sample new positions for red kites
      selected_coords_lonely <- select_coords(random_coords, num_lonely)
      
      # random age between rep_age and liv_exp
      rand_lonely_age <- sample(rep_age:liv_exp, num_lonely, replace = TRUE)
      
      # df lonely kites 
      df_lonely <- data.frame(x = selected_coords_lonely[,1],
                              y = selected_coords_lonely[,2],
                              nest_age = rep(0, num_lonely),
                              juv_age = rep(0, num_lonely),
                              lonely_age = rand_lonely_age)
      # check 
      merge(df_kites, df_lonely, by = c("x", "y"))
      
      # df_kites
      df_kites <- rbind(df_kites, df_lonely)
      
    }
  }
  # Verify the total number lonely
  if(sum(df_lonely["lonely_age"] > 0) != num_lonely){
    print("lonely:", sum(df_lonely["lonely_age"] > 0), num_lonely)
    stop("error| intial kites \n total number of lonely")
  }
  
  # final metric and Spatial distribution of kites ----
  # abundance
  abund_nest <- sum(df_kites$nest_age > 0)
  abund_juv <- sum(df_kites$juv_age > 0)
  abund_lonely <- sum(df_kites$lonely_age > 0)
  
  abund_new_juv <- sum(df_kites$juv_age == 1)
  new_nest <- 0
  
  total_abund <- abund_nest*2 + abund_juv + abund_lonely
  
  nat_dead <- 0
  nest_dead <- 0
  killed_move <- 0
  killed_build <- 0
  
  kites_abund[1, ] <- c(1, total_abund, abund_nest, abund_juv, abund_lonely, abund_new_juv, new_nest,
                        nat_dead, nest_dead, killed_move, killed_build)
  
  # location kites_loc
  kites <- spat_palcement(df_kites, kites, timestep = 1, layer = 1) # layer = 1 - "loc"
  sum(kites[,,1,1] ==1)
  sum(kites[,,1,1] ==2)
  sum(kites[,,1,1] ==3)
  sum((df_kites$nest_age > 0) & (df_kites$juv_age > 0))
  
  # write into kites_metric (df_kites + mortality)
  kites_metric[[1]] <- df_kites
  kites_metric[[timesteps+1]] <- 0
  
  # Verify the total abundance
  
  if(sum(kites[,,1,"loc"] == 1) != abund_lonely || sum(kites[,,1,"loc"] == 2) != (abund_nest - abund_juv) ||
     sum(kites[,,1,"loc"] == 3) != abund_juv || sum(kites[,,1,"loc"]) != total_abund){
    print(paste("abund:", sum(kites[,,1,"loc"]), "| theo", total_abund))
    stop("error| intial kites \n total abundance, after placement")
  }
  
  return(list(kites_metric = kites_metric, kites = kites, kites_abund = kites_abund))
}



