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
  if (n_new >= turb_neu_max) n_new <- turb_neu_max
  
  # Place turbines near existing ones
  for (i in 1:n_new) {
    # Randomly select an existing turbine as a neighbor
    chosen_turb <- existing_turbs[sample(1:n_existing, 1), ]
    x <- chosen_turb[1]
    y <- chosen_turb[2]
    
    # Find valid neighboring cells
    potential_neighbors <- expand.grid(
      x + c(-1, 0, 1),
      y + c(-1, 0, 1)
    )
    colnames(potential_neighbors) <- c("x", "y")
    
    # Filter valid neighbors
    valid_neighbors <- potential_neighbors %>% 
      filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>%
      # Joel: ----
      # Koennen x und y nicht >= 0 sein? Sie duerfen ja auch <= x_dim (100) sein
      filter(!apply(., 1, function(coord) {
        turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t]
      }))
    
    # Skip if no valid neighbors are found
    if (nrow(valid_neighbors) == 0) next
    
    # Randomly select a valid neighbor
    new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
    turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
  }
  
  # Random turbine placement
  if (n_new != 1) {
    random_coords <- which(!region[, , t] & !turbine[, , t], arr.ind = TRUE)
    if (nrow(random_coords) > 0) {
      n_random_turbs <- sample(n_new:(turb_neu_max - n_new), 1)
      selected_random_coords <- random_coords[sample(1:nrow(random_coords), min(n_random_turbs, nrow(random_coords))), ]
      selected_random_coords <- matrix(selected_random_coords, ncol = 2)
      for (i in 1:nrow(selected_random_coords)) {
        turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
      }
    }
  }
  # Joel: ----
  # Ich wuerde es etwas klarer machen das ihr Turbinen neben alte Turbinen stellt
  # aber noch zusaetzlich zufaellig welche verteilt
  
  
  # Copy turbines to the next timestep
  turbine[, , t + 1] <- turbine[, , t] | turbine[, , t + 1]
  
  # --------------------------------------------------------------
  # Red Kite Dynamics
  # --------------------------------------------------------------
  # Current kite population
  kite_layer <- kites[, , t]
  N_t <- sum(kite_layer)  # Total red kite population at timestep t
  
  # Apply Ricker function for population growth
  N_next <- round(N_t * exp(growth_rate * (1 - N_t / carrying_capacity)))
  
  # Determine valid cells for kite placement
  random_coords <- which(!region[, , t] & !turbine[, , t], arr.ind = TRUE)
  
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
  n_turb[t + 1] <- nrow(which(turbine[, , t + 1], arr.ind = TRUE))
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
    kite = as.vector(kites[, , t])
  )
  
  # Create the plot
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = region), show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "blue", name = "Region") +
    geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
    geom_point(data = subset(df, kite == TRUE), aes(x = x, y = y), color = "green", size = 2) +
    labs(title = paste("Red Kites in Simulation, Timestep = ", t), x = "X", y = "Y") +
    theme_minimal()
  
  # Joel: ----
  # Eine Legende waere noch hilfreich zum Verstehen der Plots
  # Vlt. noch die Anzahl der Turbinen und Voegel plotten? Ihr habt die Daten ja
  # so oder so schon errechnet
  
  # Print the plot
  print(p)
  
  # Pause between plots
  Sys.sleep(2)
}




# old structure -----------------------------------------------------------
# # Turbinen-Model
# # erster Versuch
# 
# # hindernisse und 
# # Bedinung, dass nur neben existierender Turbine neu gebaut werden kann
#   
# # ---
# # set.seed(42)
# library(dplyr)
# library(ggplot2)
# 
# # Simulation über die Zeit --
# n_turb <- vector(length = timesteps)
# n_kites <- vector(length = timesteps)  # Track kite population
# 
# for (t in 1:(timesteps - 1)) {
#   
#   # Number of existing turbines
#   existing_turbs <- which(turbine[, , t], arr.ind = TRUE)
#   n_existing <- nrow(existing_turbs)
#   
#   # Calculate the number of new turbines to be added
#   n_new <- ceiling(n_existing * turb_neu_perc)
#   if (n_new >= turb_neu_max) n_new <- turb_neu_max
#   
#   # Add new turbines based on neighborhood
#   for (i in 1:n_new) {
#     # Randomly select an existing turbine as the neighbor
#     chosen_turb <- existing_turbs[sample(1:n_existing, 1), ]
#     x <- chosen_turb[1]
#     y <- chosen_turb[2]
#     
#     # Find potential neighbors
#     potential_neighbors <- expand.grid(
#       x + c(-1, 0, 1),
#       y + c(-1, 0, 1)
#     )
#     colnames(potential_neighbors) <- c("x", "y")
#     
#     # Filter valid neighbors 
#     valid_neighbors <- potential_neighbors %>% 
#       filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>% 
#       filter(!apply(., 1, function(coord) {
#         turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t]
#       }))
#     
#     # If no valid neighbors, skip to the next iteration
#     if (nrow(valid_neighbors) == 0) next
#     
#     # Randomly select a valid neighbor
#     new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
#     turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
#   }
#   
#   # Random turbine placement
#   if (n_new != 1) {
#     random_coords <- which(!region[, , t] & !turbine[, , t], arr.ind = TRUE)
#     if (nrow(random_coords) > 0) {
#       n_random_turbs <- sample(n_new:(turb_neu_max - n_new), 1)
#       selected_random_coords <- random_coords[sample(1:nrow(random_coords), min(n_random_turbs, nrow(random_coords))), ]
#       selected_random_coords <- matrix(selected_random_coords, ncol = 2)
#       for (i in 1:nrow(selected_random_coords)) {
#         turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
#       }
#     }
#   }
#   
#   # Copy the existing turbines to the next timestep
#   turbine[, , t + 1] <- turbine[, , t] | turbine[, , t + 1]
#   
#   # Red Kite Dynamics --
#   kite_layer <- kites[, , t]  # Current timestep's kites
#   N_t <- sum(kite_layer)      # Total red kite population at timestep t
#   
#   # Ricker Function for Population Growth
#   N_next <- round(N_t * exp(growth_rate * (1 - N_t / carrying_capacity)))
#   
#   # Stochastic Placement of New Red Kites
#   random_coords <- which(!region[, , t] & !turbine[, , t], arr.ind = TRUE)  # Valid empty cells
#   
#   # Only proceed if there are valid cells available
#   if (nrow(random_coords) > 0) {
#     # Sample new positions for red kites
#     selected_coords <- random_coords[sample(1:nrow(random_coords), min(N_next, nrow(random_coords))), ]
#     
#     # Ensure `selected_coords` is a matrix
#     selected_coords <- matrix(selected_coords, ncol = 2)
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
#   # Track population counts for visualization
#   new_turb <- which(turbine[, , t + 1], arr.ind = TRUE)
#   n_turb[t + 1] <- nrow(new_turb)
#   n_kites[t + 1] <- sum(kites[, , t + 1])
# }
# 
# # Visualisierung der Ergebnisse --
# par(mfrow = c(1, 1))
# for (t in 1:timesteps) {
#   # Combine into a data frame
#   x_dim <- dim(region)[1]
#   y_dim <- dim(region)[2]
#   
#   df <- data.frame(
#     x = rep(1:x_dim, each = y_dim),
#     y = rep(1:y_dim, times = x_dim),
#     region = as.vector(region[,,t]),
#     turbine = as.vector(turbine[,,t]),
#     kite = as.vector(kites[,,t])
#   )
#   
#   # Create the ggplot
#   p <- ggplot(df, aes(x = x, y = y)) +
#     geom_tile(aes(fill = region), show.legend = FALSE) +
#     scale_fill_gradient(low = "white", high = "blue", name = "Region") +
#     geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
#     geom_point(data = subset(df, kite == TRUE), aes(x = x, y = y), color = "green", size = 2) +
#     labs(title = paste("Rotmilan in Bullerbüh, timestep = ", t), x = "X", y = "Y") +
#     theme_minimal()
#   
#   print(p)
#   
#   # Pause for 0.1 seconds between plots
#   Sys.sleep(0.1)
# }
# 
# 
# 
# 
# # notes --
# ## 4dim array --> region + trubinen --
# # combined_array <- abind(region, turbine, along = 4)
# # 
# # # Add dimension names for clarity
# # dimnames(combined_array) <- list(
# #   x = NULL,
# #   y = NULL,
# #   time = NULL,
# #   type = c("region", "turbine")
# # )
# # print(dim(combined_array))
# # 
# # print(combined_array[,,1,]) # first timestep
# # print(combined_array[,,2,])
# 
# ## plot region + trubo --
# # col_turb <- c("white","black")
# # col_reg <- colorRampPalette(c("white", "saddlebrown"))(256)
# 
# # Extract data for timestep 1
# # region_layer <- combined_array[,,1,1]  # Region layer
# # turbine_layer <- combined_array[,,1,2]  # Turbine layer


