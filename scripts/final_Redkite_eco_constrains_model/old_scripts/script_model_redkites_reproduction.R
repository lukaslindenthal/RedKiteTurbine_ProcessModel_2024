# help script to simulate the red kites
# ! Assumption, placing of turbines is not dependant on occurence of red kites!

# Dependecies ----
library(dplyr)
library(ggplot2)

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
# for (t in 1:(timesteps - 1)) {
#   
#   # --------------------------------------------------------------
#   # Turbine Construction
#   # --------------------------------------------------------------
#   # Get existing turbine positions
#   existing_turbs <- which(turbine[, , t], arr.ind = TRUE)
#   n_existing <- nrow(existing_turbs)
#   
#   # Calculate number of new turbines to add
#   n_new <- ceiling(n_existing * turb_neu_perc)
#   if (n_new > turb_neu_max) n_new <- turb_neu_max
#   
#   ## Place turbines near existing ones 
#   if (n_existing > 0) {
#     for (i in 1:n_new) {
#       chosen_turb <- existing_turbs[sample(1:n_existing, 1), ]
#       x <- chosen_turb[1]
#       y <- chosen_turb[2]
#       
#       # Skip placement if next to building
#       if (building_buffer[x, y, t]) next
#       
#       # Find valid neighboring cells
#       potential_neighbors <- expand.grid(
#         x + c(-1, 0, 1),
#         y + c(-1, 0, 1)
#       )
#       colnames(potential_neighbors) <- c("x", "y")
#       
#       # # Filter valid neighbors
#       valid_neighbors <- potential_neighbors %>%
#         filter(x >= 1 & x <= x_dim, y >= 1 & y <= y_dim) %>%
#         filter(!apply(., 1, function(coord) {
#           turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t] | building_buffer[coord[1], coord[2], t]
#         }))
#     }
#   }
#   
#   # If no valid neighbors, skip to the next iteration
#   if (nrow(valid_neighbors) == 0) next
#   
#   # Randomly select a valid neighbor
#   new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
#   turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
#   
#   # Random turbine placement if new turbines are less than limit
#   if (n_new < turb_neu_max) {
#     random_coords <- which(!region[, , t] & !turbine[, , t] & !building_buffer[, , t], arr.ind = TRUE)
#     if (nrow(random_coords) > 0) {
#       n_random_turbs <- turb_neu_max - n_new
#       selected_random_coords <- random_coords[sample(1:nrow(random_coords), min(n_random_turbs, nrow(random_coords))), ]
#       for (i in 1:nrow(selected_random_coords)) {
#         turbine[selected_random_coords[i, 1], selected_random_coords[i, 2], t + 1] <- TRUE
#       }
#     }
#   }
#   
#   # Buffer around all turbines ----
#   # Get x, y coords of all trubines
#   turb_coords <- which(turbine[ , , t] , arr.ind = TRUE)
#   colnames(trub_coords) <- c("x", "y")
#   
#   # Creat buffer Coordinates
#   for (i in 1:nrow(turb_coords)) {
#     x <- turb_coords[i, 1]  
#     y <- turb_coords[i, 2]
#     
#     # new buffer Coords
#     for (dx in buffer_offsets) {
#       for (dy in buffer_offsets) {
#         new_x <- x + dx
#         new_y <- y + dy
#         
#         # Ensure the new coordinates are within bounds
#         if (new_x >= 1 && new_x <= x_dim && new_y >= 1 && new_y <= y_dim) {
#           buffer[new_x, new_y, t] <- TRUE
#         }
#       }
#     }
#   }
#   
#   # turbine and Buffer layer ----
#   # Copy turbines to the next timestep
#   turbine[, , t + 1] <- turbine[, , t] | turbine[, , t + 1]
#   
#   # Copy buffer to the next timestep
#   buffer[, , t + 1] <- buffer[, , t] | buffer[, , t + 1]
#   
#   # --------------------------------------------------------------
#   # Red Kite Dynamics
#   # --------------------------------------------------------------
source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R") # load redkites initial set up
t <- 1

# 1 mortality 1 - Kites getötet durhc turbien bau (von buffer/turbie´ne getroffen) [Lukas]

# 2 age of kites and nest increased ----
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


# 3 movement of all lonely adults (age_lonely >= 3) ----
coords_adults <- which(kites[ , , t+1, "age_lonely"] >= 3 , arr.ind = TRUE)
n_adults <- nrow(coords_adults)

# to check "abundance" before and after
check_abund <- sum(kites[ , , t+1, "abundance"]) # 387

# idea to fix bug 
# Ensures kites forming nests are removed only after processing all kites
# it has to be out side of the loop, otherwise its not stored 
# nesting_kites <- c()
# # Store index for removal **outside the loop**
# nesting_kites <- c(nesting_kites, i)
# # **Remove all kites that formed nests from coords_adults**
# if (length(nesting_kites) > 0) {
#   coords_adults <- coords_adults[-nesting_kites, , drop = FALSE]
#   # Update number of adults after removal
#   n_adults <- nrow(coords_adults)

# split into two loops 1 for the nest formation, one for the dispersal

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
    
    # idea: alle bewegen sich, coord wird abgespeichert
    # check ob es doppelter coord gibt (dh ob zwei nach bewegenung aufeinander getroffen sind)
    # wenn ja bilden sie ein nest
    
    # check if new coords is matching with other adult coord
    # get new_coords_adults
    new_coords_adults <- which(kites[ , , t+1, "age_lonely"] >= 3 , arr.ind = TRUE)
    check_if_kite <- apply(new_coords_adults, 1, function(row) {
      new_x == row[1] && new_y == row[2]
    })
    if(any(check_if_kite)){
      # if true creat nest 
      
      # average age
      age_moving <- kites[coords_adults[i,1], coords_adults[i,2] , t+1, "age_lonely"] # current age
      age_partner <- kites[new_x[[1]], new_y[[1]] , t+1, "age_lonely"] # current age partner
      age <- mean(age_partner+age_moving)
      
      # creat new nest (nest, age, abundance)
      kites[new_x[[1]], new_y[[1]] , t+1, "nest"] <- 1
      kites[new_x[[1]], new_y[[1]] , t+1, "age_nest"] <- age
      kites[new_x[[1]], new_y[[1]] , t+1, "abundance"] <- 2
      
      # remove moving from old location
      kites[coords_adults[i,1], coords_adults[i,2] , t+1, "age_lonely"] <- 0 # moving kite
      kites[coords_adults[i,1], coords_adults[i,2] , t+1, "abundance"] <- 0
      
      # remove partner from location
      kites[new_x[[1]], new_y[[1]] , t+1, "age_lonely"] <- 0 # moving kite
      
      # remove coords from coords_adults
      row_rm <- which(check_if_kite, arr.ind = TRUE)
      coords_adults <- coords_adults[-row_rm,]
      n_adults <- nrow(coords_adults)
    }
    
    # check if within boundaries
    # migration = immigration
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
    #if(sum(kites[ , , t+1, "abundance"])!= check_abund) break
    print(paste("index:",i))
    print(paste(sum(kites[ , , t+1, "abundance"]), check_abund))
    print(paste("new", new_x, new_y))
    print(paste("old ",coords_adults[i, 1], coords_adults[i, 2]))
  }
}

# movement lonely kites [Neele]

# 4 mortality 2 - die kites die in schon gebaute trubine wandern sterben [Neele]
# lonely kites sterben wenn sie in turibe/buffer rein fliegen 
# wenn neue turb gebaut wird sterben nester/lonely kites wenn sie dort sind
# siehe /kite_mortality.R, muss modifiziert werden
# zuerst turbinen platzieren 

# 5 Reproduction ---- [Lukas]
# alle nester reporduzieren mit 0.79 (growth rate) (auch die neu gebildeten)
# nester die schon ein juv haben, kein neues 


# --- Reproduction Step for Red Kites at time t ---
# (we need to insert  after mortality/aging and before dispersal at timestep t+1 I think)

# Let's look for eligible=geeignete nests
# active nests (TRUE in the "nest" layer)
# and have not yet reproduced (FALSE in the "juv" layer).
eligible_nests <- which(kites[,,t, "nest"] == TRUE & kites[,,t, "juv"] == FALSE, arr.ind = TRUE)
N_t <- nrow(eligible_nests)

if (N_t > 0) {
  
  # with ricker calculate average expected offspring per nest
  # should be density‐dependent, so considers the current numer of nests at given timestep
  avg_offspring_per_nest <- exp(growth_rate * (1 - N_t / carrying_capacity))
  
  # Loop over all eligible nest and get reproduction outcome
  for (i in 1:N_t) {
    
    # Init  number of juvs produced by this nest
    juveniles_produced <- 0
    
    if (avg_offspring_per_nest < 1) {
      # If  expected number < 1,  nest reproduces with probability = to avg_offspring_per_nest
      if (runif(1) < avg_offspring_per_nest) {
        juveniles_produced <- 1
      }
    } else {
      # when expected number is 1 or greater,assigns one juvenile for sure
      juveniles_produced <- 1
      
      # Determine extra reproduction potential
      # A nest can produce a maximum of 2 extra juvs (max total of 3) 
      extra_mean <- min(avg_offspring_per_nest - 1, 2)
      
      # split this extra potential into two independent chances
      prob_extra <- extra_mean / 2
      
      # first extra offspring chance
      if (runif(1) < prob_extra) {
        juveniles_produced <- juveniles_produced + 1
      }
      # second extra offspring chance
      if (runif(1) < prob_extra) {
        juveniles_produced <- juveniles_produced + 1
      }
    }
    
    # ecological constraint; no more than three juvs (Neele paper?)
    if (juveniles_produced > 3) juveniles_produced <- 3
    
    # Update nest only if reproduction occurs
    if (juveniles_produced > 0) {
      # Extract coordinates of current nest
      # ATTENTION;) in the old model we use these as global variables I think
      x_coord <- eligible_nests[i, 1]
      y_coord <- eligible_nests[i, 2]
      
      # Mark nest as having produced juveniles (cant reproduce again until the juvenile ages out)
      kites[x_coord, y_coord, t+1, "juv"] <- TRUE
      
      # Update the total abundance at this cell
      #  adding new juveniles to abundance carried over from timestep t.
      kites[x_coord, y_coord, t+1, "abundance"] <- kites[x_coord, y_coord, t, "abundance"] + juveniles_produced
      
      # (Optional) maybe we could track the number of juveniles separately, 
      # with new layer (e.g., "juv_count").
      # kites[x_coord, y_coord, t+1, "juv_count"] <- juveniles_produced 
      # but yeah, maybe not also;)
    }
  }
}



# reproduction mit ricker-function
# growth_rate und carrying_capacity sind in new_redkite_eco_para.R definiert
# N_next <- round(N_t * exp(growth_rate * (1 - N_t / carrying_capacity)))

# kites[, , t+1, "nest"]
# kites[, , t+1, "juv"]
# kites[, , t+1, "abundance"]

# 6 visual as extra .R file 
# siehe /visual_model.R muss modifiziert werden
# in new_redkite_eco_para.R unten Versuch zu ploten
