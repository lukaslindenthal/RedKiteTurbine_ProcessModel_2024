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


# ----------------------------------------------------------------
# Initialize Variables for Tracking Results ------------------------------------
# ----------------------------------------------------------------
n_turb <- vector(length = timesteps)    # Track turbine counts
n_kites <- vector(length = timesteps)  # Track kite population

# Initialize buffer zones for turbines
buffer <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for buffer zones


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
    
  # Place turbines near existing ones and mark buffers
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
        # valid_neighbors <- potential_neighbors %>%
        #   filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>%
        #   filter(!apply(., 1, function(coord) {
        #     turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t] | buffer[coord[1], coord[2], t]
        #   }))
        
        # ich würde hier den buffer rausnehmen, 
        # da ja auch neue Turbinen in der buffer zonen gesetzt werden können
        valid_neighbors <- potential_neighbors %>%
            filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>%
            filter(!apply(., 1, function(coord) {
              turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t]
            }))
        
    #     # Place the turbine and mark buffers if valid neighbors exist
    #     if (nrow(valid_neighbors) > 0) {
    #       new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
    #       turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
    # 
    #       # Mark buffer zones around the new turbine
    #       neighbors_to_buffer <- expand.grid(
    #         x = new_turb$x + c(-2, -1, 0, 1, 2),
    #         y = new_turb$y + c(-2, -1, 0, 1, 2)
    #         )
    #       neighbors_to_buffer <- neighbors_to_buffer[
    #         neighbors_to_buffer$x >= 1 & neighbors_to_buffer$x <= x_dim &
    #         neighbors_to_buffer$y >= 1 & neighbors_to_buffer$y <= y_dim,]
    #       for (j in 1:nrow(neighbors_to_buffer)) {
    #         buffer[neighbors_to_buffer$x[j], neighbors_to_buffer$y[j], t + 1] <- TRUE
    # 
    #       }
    #     }
    #   }
    # }
        
    # ich würde zu erst alle turbinen setzen und dann für jede turbine einen Buffer anlegen
    
        # If no valid neighbors, skip to the next iteration
        if (nrow(valid_neighbors) == 0) next
    
        # Randomly select a valid neighbor
        new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
        turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
      }
    
        # Random turbine placement if new turbines are less than limit
        if (n_new < turb_neu_max) {
          random_coords <- which(!region[, , t] & !turbine[, , t] & !buffer[, , t], arr.ind = TRUE)
          if (nrow(random_coords) > 0) {
            n_random_turbs <- turb_neu_max - n_new
            selected_random_coords <- random_coords[sample(1:nrow(random_coords), min(n_random_turbs, nrow(random_coords))), ]
            for (i in 1:nrow(selected_random_coords)) {
              turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
            }
          }
        }
        
        # Buffer around all turbines:
        # Mark buffer zones around the new turbine
        neighbors_to_buffer <- expand.grid(
          x = new_turb$x + c(-2, -1, 0, 1, 2),
          y = new_turb$y + c(-2, -1, 0, 1, 2)
          )
        neighbors_to_buffer <- neighbors_to_buffer[
          neighbors_to_buffer$x >= 1 & neighbors_to_buffer$x <= x_dim &
          neighbors_to_buffer$y >= 1 & neighbors_to_buffer$y <= y_dim,]
        for (j in 1:nrow(neighbors_to_buffer)) {
          buffer[neighbors_to_buffer$x[j], neighbors_to_buffer$y[j], t + 1] <- TRUE
        }
 
    
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
    
    # Determine valid cells for kite placement, considering buildings
    random_coords <- which(!region[, , t] & !turbine[, , t] & !buffer[, , t] & !building_buffer[, , t], arr.ind = TRUE)
    
    # Only proceed if valid cells exist
    if (nrow(random_coords) > 0) {
      # Sample new positions for red kites
      selected_coords <- random_coords[sample(1:nrow(random_coords), min(N_next, nrow(random_coords))), ]
      selected_coords <- matrix(selected_coords, ncol = 2)
      
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
    
    # Track population counts
    n_turb[t + 1] <- sum(turbine[, , t + 1])
    n_kites[t + 1] <- sum(kites[, , t + 1])
  
  # Copy kites to the next timestep
  kites[, , t + 1] <- kites[, , t + 1] | kites[, , t]
  
  # Track population counts
  n_turb[t + 1] <- sum(turbine[, , t + 1])
  n_kites[t + 1] <- sum(kites[, , t + 1])
}

# ----------------------------------------------------------------
# Visualization of Results -----------------------------------------------------
# ----------------------------------------------------------------
par(mfrow = c(1, 1))
for (t in 1:timesteps) {
  # Combine data into a data frame for plotting
  df <- data.frame(
    x = rep(1:x_dim, each = y_dim),
    y = rep(1:y_dim, times = x_dim),
    region = as.vector(region[, , t]),
    turbine = as.vector(turbine[, , t]),
    kite = as.vector(kites[, , t]),
    buffer = as.vector(buffer[, , t])
  )
  
  # Create the plot with buffer zone
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = region), show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "blue", name = "Region") +
    geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
    geom_point(data = subset(df, kite == TRUE), aes(x = x, y = y), color = "green", size = 2) +
    geom_point(data = subset(df, buffer == TRUE), aes(x = x, y = y), color = "yellow", size = 1, shape = 15) +
    labs(title = paste("Simulation at Timestep = ", t), x = "X", y = "Y") +
    theme_minimal()
  
  # Print the plot
  print(p)
  
  # Pause between plots
  Sys.sleep(0.1)
}


