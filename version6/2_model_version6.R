# ----------------------------------------------------------------
# Turbine and Red Kite Model ----
# ----------------------------------------------------------------
# This script models the interaction between wind turbine placement and red 
# kite dynamics over a series of 5 timesteps, each representing one year.

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

# Dependecies ----
source("./Red_kite_model_version6.R")

model_simualtion <- function(timesteps, resolution, x_dim, y_dim, buffer_zone,
                             region, building_buffer, turbine, buffer,
                             rep_age, growth_rate, liv_exp, carrying_capacity,
                             kites_metric, kites_abund) {
  # # ----------------------------------------------------------------
  # # Main Simulation Loop ---------------------------------------------------------
  # # ----------------------------------------------------------------
  for (t in 1:(timesteps - 1)) {
    print(paste("t=", t, "& timestep= ", t+1))
    
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
        selected_random_coords <- matrix(selected_random_coords, ncol = 2)
        if(nrow(selected_random_coords) > 0){
          for (i in 1:nrow(selected_random_coords)) {
            turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
          }
        }
      }
    }
    
    # Buffer around all turbines ----
    # Get x, y coords of all trubines
    turb_coords <- which(turbine[ , , t] , arr.ind = TRUE)
    colnames(turb_coords) <- c("x", "y")
    
    # Creat buffer Coordinates
    for (i in 1:nrow(turb_coords)) {
      x <- turb_coords[i, 1]
      y <- turb_coords[i, 2]
      
      # new buffer Coords
      for (dx in buffer_zone) {
        for (dy in buffer_zone) {
          
          if (dx == 0 && dy == 0) next
          
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
    # Red Kite Dynamics ----
    # --------------------------------------------------------------
    
    Red_kite_population <- Red_kite_population(x_dim, y_dim, t,
                                                    region, building_buffer, turbine, buffer,
                                                    rep_age, growth_rate, liv_exp, carrying_capacity,
                                                    kites_metric, kites_abund)

    kites_abund <- Red_kite_population$kites_abund
    kites_metric <- Red_kite_population$kites_metric
    kites <- Red_kite_population$kites
    

  } # end of for loop timesteps
  
  return(list(kites_metric = kites_metric, kites = kites, kites_abund = kites_abund,
              region = region, building_buffer = building_buffer, turbine = turbine, buffer = buffer))
}

