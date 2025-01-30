# ----------------------------------------------------------------
# Turbine and Red Kite Model Simulation
# ----------------------------------------------------------------
# This script simulates wind turbine placement and red kite dynamics 
# over a series of timesteps.
# ----------------------------------------------------------------
# init Author: Neele Haß
# other Authors: Lukas Lindenthal
# date: November 2024
# ----------------------------------------------------------------


# Dependecies ----
library(dplyr)
library(ggplot2)
source("./scripts/Turbo_region_parameter_setting.R") # load from Parameter Initalize Code

# ----------------------------------------------------------------
# Initialize Variables for Tracking Results ------------------------------------
# ----------------------------------------------------------------

# Tracking variables
n_turb <- vector(length = timesteps)    # Track turbine counts
n_kites <- vector(length = timesteps)   # Track kite population
n_turb[1] <- sum(turbine[,,1])
n_kites[1] <- sum(kites[,,1])

# Initialize buffer zones for turbines
buffer <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for buffer zones
buffer_zone <- c(-2, -1, 0, 1, 2)

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
  
  # Place turbines near existing ones
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
  # Current kite population
  kite_layer <- kites[, , t]
  N_t <- sum(kite_layer)  # Total red kite population at timestep t
  
  # Apply Ricker function for population growth
  N_next <- round(N_t * exp(growth_rate * (1 - N_t / carrying_capacity)))
  
  # Determine valid cells for kite placement, considering turbines + buffer + buildings
  random_coords <- which(!region[, , t] & !turbine[, , t] 
                         & !buffer[, , t] & !building_buffer[, , t], arr.ind = TRUE)
  random_coords <- matrix(random_coords, ncol = 2)
  
  # Only proceed if valid cells exist
  if (nrow(random_coords) > 0) {
    # Sample new positions for red kites
    selected_coords <- random_coords[sample(1:nrow(random_coords), min(N_next, nrow(random_coords))), ]
    
    # Place red kites in the selected coordinates
    if (nrow(selected_coords) > 0) {
      for (i in 1:nrow(selected_coords)) {
        kites[selected_coords[i, 1], selected_coords[i, 2], t + 1] <- 1
      }
    }
  } else {
    cat(sprintf("No valid cells for red kite placement at timestep %d\n", t))
  }
  
  # Copy kites to the next timestep
  kites[, , t + 1] <- kites[, , t + 1] | kites[, , t]
  
  # Track population counts ---- 
  n_turb[t+1] <- sum(turbine[, , t + 1])
  n_kites[t+1] <- sum(kites[, , t + 1])
  
}


