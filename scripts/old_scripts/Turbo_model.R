# ---
# Turbinen-Model
# erster Versuch

# hindernisse und 
# Bedinung, dass nur neben existierender Turbine neu gebaut werden kann
  
# ---
set.seed(42)
library(dplyr)

# Simulation über die Zeit ----
n_turb <- vector(length = timesteps)
for (t in 1:(timesteps - 1)) {
  
  # Number of existing turbines
  existing_turbs <- which(turbine[, , t], arr.ind = TRUE)
  n_existing <- nrow(existing_turbs)
  
  # Calculate the number of new turbines to be added
  n_new <- ceiling(n_existing * turb_neu_perc)
  if (n_new >= turb_neu_max) {
    n_new <- turb_neu_max
  }
  # }
  
  # Add new turbines based on neighborhood
  for (i in 1:n_new) {
    # Randomly select an existing turbine as the neighbor
    chosen_turb <- existing_turbs[sample(1:n_existing, 1), ]
    x <- chosen_turb[1]
    y <- chosen_turb[2]
    
    # Find potential neighbors # similar for buffer for red kit
    potential_neighbors <- expand.grid(
      x + c(-1, 0, 1),
      y + c(-1, 0, 1)
    )
    colnames(potential_neighbors) <- c("x", "y")
    
    # Filter valid neighbors (within grid, not occupied by a turbine or region)
    valid_neighbors <- potential_neighbors %>% #pipeline (%>%) -- passing data to function
      filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>% # filter() creating subsets by row --> x,y within x/ydim range
      filter(!apply(., 1, function(coord) { #. = datat from subset
        turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t] #! negates the result --> rows where the turb or region = TRUE will be excluded
      }))
    
    # If no valid neighbors, skip to the next iteration
    if (nrow(valid_neighbors) == 0) next
    
    # Randomly select a valid neighbor
    new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
    
    # Add a new turbine
    turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
    
  }
  
  if (n_new != 1){
  # Random turbine placement (in valid regions where neither a turbine nor a region exists)
  random_coords <- which(!region[, , t] & !turbine[, , t], arr.ind = TRUE)
  
  # Check if there are any valid random coordinates available
  if (nrow(random_coords) > 0) {
    # Random number of turbines to add
    n_random_turbs <- sample(n_new:(turb_neu_max-n_new), 1)  # Random number of turbines
    selected_random_coords <- random_coords[sample(1:nrow(random_coords), min(n_random_turbs, nrow(random_coords))), ]
    selected_random_coords <- matrix(selected_random_coords, ncol =2)
    # Add turbines to random valid coordinates
    for (i in 1:nrow(selected_random_coords)) {
      turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
    }
  }
  }
  # Copy the existing turbines to the next timestep
  turbine[, , t + 1] <- turbine[, , t] | turbine[, , t + 1]
  new_turb <- which(turbine[, , t], arr.ind = TRUE)
  n_turb[t+1] <- nrow(new_turb)

}

# Visualisierung der Ergebnisse ----

par(mfrow = c(1, 1))
for (t in 1:timesteps) {
  # Combine into a data frame
  x_dim <- dim(region)[1]
  y_dim <- dim(region)[2]
  
  df <- data.frame(
    x = rep(1:x_dim, each = y_dim),
    y = rep(1:y_dim, times = x_dim),
    region = as.vector(region[,,t]),
    turbine = as.vector(turbine[,,t])
  )
  
  # Create the ggplot
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = region), show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "blue", name = "Region") +
    geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
    #geom_point(data = subset(df, region == TRUE), aes(x = x, y = y), color = "blue", size = 2) +
    labs(title = paste("Timestep = ", t), x = "X", y = "Y") +
    theme_minimal()

  print(p)
  
  # number of turb
  new_turb <- which(turbine[, , t], arr.ind = TRUE)
  n_turb[t] <- nrow(new_turb)
  #plot(n_turb,1:timesteps)
  
  # Pause for 0.1 seconds between plots
  Sys.sleep(0.1)
  
}


# notes ----
## 4dim array --> region + trubinen ----
# combined_array <- abind(region, turbine, along = 4)
# 
# # Add dimension names for clarity
# dimnames(combined_array) <- list(
#   x = NULL,
#   y = NULL,
#   time = NULL,
#   type = c("region", "turbine")
# )
# print(dim(combined_array))
# 
# print(combined_array[,,1,]) # first timestep
# print(combined_array[,,2,])

## plot region + trubo ----
# col_turb <- c("white","black")
# col_reg <- colorRampPalette(c("white", "saddlebrown"))(256)

# Extract data for timestep 1
# region_layer <- combined_array[,,1,1]  # Region layer
# turbine_layer <- combined_array[,,1,2]  # Turbine layer


