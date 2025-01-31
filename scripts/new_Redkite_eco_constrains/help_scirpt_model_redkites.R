# help script to simulate the red kites
# ! Annahme, placing of trubines is not dependen on redkites!


# Dependecies ----
library(dplyr)
library(ggplot2)
source("./scripts/new_Redkite_eco_constrains/utiles/utiles_func.R") # load help functions
source("./scripts/new_Redkite_eco_constrains/new_Time_Dim_Turbo_Region_para_setting.R") # load timesteps, dim and Turbine
source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R") # load redkites initial set up

# ----------------------------------------------------------------
# Initialize Variables for Tracking Results ------------------------------------
# ----------------------------------------------------------------

# Tracking variables
n_turb <- vector(length = timesteps)        # Track turbine counts
kites_abund <- vector(length = timesteps)   # Track total kite population (abundance)
kites_juv  <- vector(length = timesteps)    # Track kite juveniles
kites_nest <- vector(length = timesteps)    # Track kite nests

n_turb[1] <- sum(turbine[,,1])
kites_abund[1] <- sum(kites[,,1, "abundance"])
kites_juv[1] <- sum(kites[,,1, "juv"])
kites_nest[1] <- sum(kites[,,1, "nest"])
# ----------------------------------------------------------------
# Main Simulation Loop ---------------------------------------------------------
# ----------------------------------------------------------------
for (t in 1:(timesteps - 1)) {
  
  # --------------------------------------------------------------
  # Turbine Construction
  # --------------------------------------------------------------
  # Get existing turbine positions
  existing_turbs <- which(turbine[, , t], arr.ind = TRUE)
  n_existing <- nrow(existing_turbs)
  
  # Calculate number of new turbines to add
  n_new <- ceiling(n_existing * turb_neu_perc)
  if (n_new > turb_neu_max) n_new <- turb_neu_max
  
  ## Place turbines near existing ones 
  if (n_existing > 0) {
    for (i in 1:n_new) {
      chosen_turb <- existing_turbs[sample(1:n_existing, 1), ]
      x <- chosen_turb[1]
      y <- chosen_turb[2]
      
      # Skip placement if next to building
      if (building_buffer[x, y, t]) next
      
      # Find valid neighboring cells
      potential_neighbors <- expand.grid(
        x + c(-1, 0, 1),
        y + c(-1, 0, 1)
      )
      colnames(potential_neighbors) <- c("x", "y")
      
      # # Filter valid neighbors
      valid_neighbors <- potential_neighbors %>%
        filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>%
        filter(!apply(., 1, function(coord) {
          turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t] | building_buffer[coord[1], coord[2], t]
        }))
    }
  }
  
  # If no valid neighbors, skip to the next iteration
  if (nrow(valid_neighbors) == 0) next
  
  # Randomly select a valid neighbor
  new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
  turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
  
  # Random turbine placement if new turbines are less than limit
  if (n_new < turb_neu_max) {
    random_coords <- which(!region[, , t] & !turbine[, , t] & !building_buffer[, , t], arr.ind = TRUE)
    if (nrow(random_coords) > 0) {
      n_random_turbs <- turb_neu_max - n_new
      selected_random_coords <- random_coords[sample(1:nrow(random_coords), min(n_random_turbs, nrow(random_coords))), ]
      for (i in 1:nrow(selected_random_coords)) {
        turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
      }
    }
  }
  
  # Buffer around all turbines ----
  # Get x, y coords of all trubines
  turb_coords <- which(turbine[ , , t] , arr.ind = TRUE)
  colnames(trub_coords) <- c("x", "y")
  
  # Creat buffer Coordinates
  for (i in 1:nrow(turb_coords)) {
    x <- turb_coords[i, 1]  
    y <- turb_coords[i, 2]
    
    # new buffer Coords
    for (dx in buffer_offsets) {
      for (dy in buffer_offsets) {
        new_x <- x + dx
        new_y <- y + dy
        
        # Ensure the new coordinates are within bounds
        if (new_x >= 1 && new_x <= x_dim && new_y >= 1 && new_y <= y_dim) {
          buffer[new_x, new_y, t] <- TRUE
        }
      }
    }
  }
  
  # turbine and Buffer layer ----
  # Copy turbines to the next timestep
  turbine[, , t + 1] <- turbine[, , t] | turbine[, , t + 1]
  
  # Copy buffer to the next timestep
  buffer[, , t + 1] <- buffer[, , t] | buffer[, , t + 1]
  
  # --------------------------------------------------------------
  # Red Kite Dynamics
  # --------------------------------------------------------------
  t <- 1
  source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R") # load redkites initial set up
  
  # 1 age of kites and nest increased ----
  # and
  # check if kite or nest will die, age > liv_exp
  # nest
  coords_nests <- which(kites[, , t, "nest"] > 0, arr.ind = TRUE) # check for nests
  num_nests <- nrow(coords_nests)
  
  if (num_nests > 0){
    for (i in 1:num_nests) {
      
      new_age <- kites[coords_nests[i,1],coords_nests[i,2], t, "age_nest"] + 1
      
      # when age > liv_exp --> nest dead, if juv present it survives
      if (new_age > liv_exp) {
        kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- 0
        juv <- kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "juv"]
        # nests dies but juvenile survives
        if (juv > 0 ) {
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 1
        }
      } else {
        kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- new_age
        kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 2 # nest still exists
      }
    }
  }
  # which(kites[,,t+1, "age_nest"] > liv_exp, arr.ind = TRUE)
  
  # lonely and juveniles
  coords_lonely <- which(kites[, , t, "age_lonely"] > 0, arr.ind = TRUE) # check for lonely kites + juv
  num_lonely <- nrow(coords_lonely)
  
  if (num_lonely > 0){
    for (i in 1:num_lonely) {
      
      new_age <- kites[coords_lonely[i,1],coords_lonely[i,2], t, "age_lonely"] + 1
      
      # when age > liv_exp --> dead
      if (new_age > liv_exp) {
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "age_lonely"] <- 0
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "abundance"] <- 0
      } else {
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "age_lonely"] <- new_age
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "abundance"] <- 1 # kite still exists
      }
    }
    }
  
  # which(kites[,,t+1, "age_lonely"] > liv_exp, arr.ind = TRUE)

 
  
  # 2 movement of all lonely adults (age_lonely >= 3) ----
  coords_adults <- which(kites[ , , t+1, "age_lonely"] >= 3 , arr.ind = TRUE)
  n_adults <- nrow(coords_adults)
  
  # to check "abundance" before and after
  check_abund <- sum(kites[ , , t+1, "abundance"]) # 387

  if (n_adults>0){
    
    for (i in 1:n_adults) {
      # replace, get random direction from dispersal 
      # new coords
      row_dispersal <- sample(1:nrow(dispersal),1 )
      rand_dispersal <- dispersal[row_dispersal,]
      dx <- rand_dispersal[,1]
      dy <- rand_dispersal[,2]
      
      new_x <- coords_adults[i,1] + dx
      new_y <- coords_adults[i,2] + dy
      
      # check if new coords are on nest 
      check_if_nest <- apply(coords_nests, 1, function(row) {
        new_x == row[1] && new_y == row[2]
      })
     
      while (any(check_if_nest)) {
        # Recalculate new direction
        row_dispersal <- sample(1:nrow(dispersal), 1)
        rand_dispersal <- dispersal[row_dispersal, ]
        dx <- rand_dispersal[1]
        dy <- rand_dispersal[2]
        
        # Recalculate new coordinates
        new_x <- coords_adults[i, 1] + dx
        new_y <- coords_adults[i, 2] + dy
        
        # Re-check if new coords are on any nest
        check_if_nest <- apply(coords_nests, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
      }
      
      # check if within boundaries
      if (new_x >= 1 && new_x <= x_dim && new_y >= 1 && new_y <= y_dim) {
        # set (age_lonely & abundance) to new location 
        age <- kites[coords_adults[i,1], coords_adults[i,2] , t+1, "age_lonely"] # current age
        kites[new_x[[1]], new_y[[1]] , t+1, "age_lonely"] <- age[[1]]
        kites[new_x[[1]], new_y[[1]] , t+1, "abundance"] <- 1
        
        # remove old location (age_lonely & abundance = 0)
        kites[coords_adults[i,1], coords_adults[i,2] , t+1, "age_lonely"] <- 0
        kites[coords_adults[i,1], coords_adults[i,2] , t+1, "abundance"] <- 0
        
      } else {
        # appearing on the other side
        if (new_x < 1) {
          new_x <- x_dim  # Wrap around to the last column
        } else if (new_x > x_dim) {
          new_x <- 1  # Wrap around to the first column
        }
        
        if (new_y < 1) {
          new_y <- y_dim  # Wrap around to the last row
        } else if (new_y > y_dim) {
          new_y <- 1  # Wrap around to the first row
        }
        
        # Set (age_lonely & abundance) to new location after wrap-around
        age <- kites[coords_adults[i, 1], coords_adults[i, 2], t + 1, "age_lonely"]
        kites[new_x[[1]], new_y[[1]], t + 1, "age_lonely"] <- age[[1]]
        kites[new_x[[1]], new_y[[1]], t + 1, "abundance"] <- 1
        
        # Remove old location (age_lonely & abundance = 0)
        kites[coords_adults[i, 1], coords_adults[i, 2], t + 1, "age_lonely"] <- 0
        kites[coords_adults[i, 1], coords_adults[i, 2], t + 1, "abundance"] <- 0
      
      }
      if(sum(kites[ , , t+1, "abundance"])!= check_abund) break
      print(paste("index:",i))
      print(paste(sum(kites[ , , t+1, "abundance"]), check_abund))
      print(paste("new", new_x, new_y))
      print(paste("old ",coords_adults[i, 1], coords_adults[i, 2]))
    }
  }
  
  print(paste(coords_adults[i+1, 1], coords_adults[i+1, 2]))
  
  # 3 Reproduction ----
  
  

#   old code
#   # Current kite population
#   kite_abund <- kites[, , t, "abundance"]
#   kites_age <- 
#   N_t <- sum(kite_layer)  # Total which can reporduce red kite population at timestep t
#   
#   # Apply Ricker function for population growth
#   N_next <- round(N_t * exp(growth_rate * (1 - N_t / carrying_capacity)))
#   
#   # Determine valid cells for kite placement, considering turbines + buffer + buildings
#   random_coords <- which(!region[, , t] & !turbine[, , t] 
#                          & !buffer[, , t] & !building_buffer[, , t], arr.ind = TRUE)
#   random_coords <- matrix(random_coords, ncol = 2)
#   
#   # Only proceed if valid cells exist
#   if (nrow(random_coords) > 0) {
#     # Sample new positions for red kites
#     selected_coords <- random_coords[sample(1:nrow(random_coords), min(N_next, nrow(random_coords))), ]
#     
#     # Place red kites in the selected coordinates
#     if (nrow(selected_coords) > 0) {
#       for (i in 1:nrow(selected_coords)) {
#         kites[selected_coords[i, 1], selected_coords[i, 2], t + 1] <- 1
#       }
#     }
#   } else {
#     cat(sprintf("No valid cells for red kite placement at timestep %d\n", t))
#   }
#   
#   # Copy kites to the next timestep
#   kites[, , t + 1] <- kites[, , t + 1] | kites[, , t]
#   
#   # Track population counts ---- 
#   n_turb[t+1] <- sum(turbine[, , t + 1])
#   n_kites[t+1] <- sum(kites[, , t + 1])
#   
# }


