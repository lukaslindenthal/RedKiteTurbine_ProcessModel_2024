# help script to simulate the red kites
# ! Annahme, placing of trubines is not dependen on redkites!


# Dependecies ----
library(dplyr)
library(ggplot2)

source("./scripts/new_Redkite_eco_constrains/new_Time_Dim_Turbo_Region_para_setting.R") # load timesteps, dim and Turbine
source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R") # load redkites initial set up

# # ----------------------------------------------------------------
# # Initialize Variables for Tracking Results ------------------------------------
# # ----------------------------------------------------------------
# 
# # Tracking variables
# n_turb <- vector(length = timesteps)        # Track turbine counts
# kites_abund <- vector(length = timesteps)   # Track total kite population (abundance)
# kites_juv  <- vector(length = timesteps)    # Track kite juveniles
# kites_nest <- vector(length = timesteps)    # Track kite nests
# 
# n_turb[1] <- sum(turbine[,,1])
# kites_abund[1] <- sum(kites[,,1, "abundance"])
# kites_juv[1] <- sum(kites[,,1, "juv"])
# kites_nest[1] <- sum(kites[,,1, "nest"])
# # ----------------------------------------------------------------
# # Main Simulation Loop ---------------------------------------------------------
# # ----------------------------------------------------------------
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
#   colnames(turb_coords) <- c("x", "y")
# 
#   # Creat buffer Coordinates
#   for (i in 1:nrow(turb_coords)) {
#     x <- turb_coords[i, 1]
#     y <- turb_coords[i, 2]
# 
#     # new buffer Coords
#     for (dx in buffer_zone) {
#       for (dy in buffer_zone) {
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

# --------------------------------------------------------------
# Red Kite Dynamics ----
source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R") # load redkites initial set up
t <- 1

#for (t in 1:(timesteps - 1)) {
  # 1 mortality 1 - Kites getötet durch Turbinenbau (von buffer/Turbine getroffen) [Lukas] ----

  # 2 age of kites and nest increased ----
  # and
  # check if kite or nest will die, age > liv_exp
  abund_beginning <- sum(kites[,,t,"abundance"])
  
  # nest
  coords_nests <- which(kites[, , t, "age_nest"] > 0, arr.ind = TRUE) # check for nests
  num_nests <- nrow(coords_nests)
  
  if (num_nests > 0){
    for (i in 1:num_nests) {
      
      new_age_nest <- kites[coords_nests[i,1],coords_nests[i,2], t, "age_nest"] + 1
      juv <- kites[coords_nests[i,1],coords_nests[i,2] , t, "juv"]
      
      if (juv[[1]] == 0 ) { 
        # no juv
        if (new_age_nest[[1]] > liv_exp) {
          # nests without juv dies
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- 0
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 0 
        } else {
          # nest survives
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- new_age_nest[[1]]
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 2 # nest still exists
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "nest"] <- 1
        }
        
      } else {
        # if juv exists
         age_juv <- kites[coords_nests[i,1],coords_nests[i,2] , t, "age_lonely"]
         
        if (new_age_nest[[1]] > liv_exp) {
          # nests with juv, nest dies juv survives
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- 0
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "nest"] <- 0
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 1 
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_lonely"] <- age_juv[[1]] + 1 # juv new age
        } else {
          
          # nest + juv survives
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- new_age_nest[[1]]
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 3 # nest still exists
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_lonely"] <- age_juv[[1]] + 1 # juv new age
          kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "nest"] <- 1
        }
      }
    }
  }
  # which(kites[,,t+1, "age_nest"] > liv_exp, arr.ind = TRUE)
  
  # lonely
  coords_lonely <- which(kites[, , t, "age_lonely"] >= rep_age , arr.ind = TRUE) # check for lonely kites + juv
  num_lonely <- nrow(coords_lonely)
  
  if (num_lonely > 0){
    for (i in 1:num_lonely) {
      
      new_age <- kites[coords_lonely[i,1],coords_lonely[i,2], t, "age_lonely"] + 1
      
      # when age > liv_exp --> dead
      if (new_age[[1]] > liv_exp) {
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "age_lonely"] <- 0
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "abundance"] <- 0
      } else {
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "age_lonely"] <- new_age[[1]]
        kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "abundance"] <- 1 # kite still exists
      }
    }
    }
  
  # juveniles t+1
  coords_juv<- which(kites[, , t+1, "age_lonely"] < rep_age & kites[, , t+1, "age_lonely"] < 0 , arr.ind = TRUE) # check for lonely kites + juv
  num_juv <- nrow(coords_juv)
  if (num_juv > 0){
    for (i in 1:num_juv) {
      kites[coords_juv[i,1],coords_juv[i,2],t+1, "juv"] <- 1 
      
    }
  }
  # sum(kites[,,t,"age_lonely"] < rep_age & kites[,,t,"age_lonely"] > 0)
  
  # check 
  # to check "abundance" before and after aging
  n_died_natural <- nrow(which(kites[,,t, "age_lonely"] >= 12, arr.ind = TRUE)) +  
    nrow(which(kites[,,t, "age_nest"] >= 12, arr.ind = TRUE))*2
  
  ab_t1 <- abund_beginning - n_died_natural

  check_abund_before <- sum(kites[ , , t+1, "abundance"])
  
  # 2 check ageing ----
  if (ab_t1 == check_abund_before){
    print(paste("same nr after aging: t1", ab_t1, "; real t1", check_abund_before, 
                "\n natural dead:", n_died_natural, abund_beginning))
  } else {
    stop(paste("nr not the same after ageing! t1", ab_t1, "; real t1", check_abund_before, abund_beginning))
  }
  
  # 3 movement of all lonely adults (age_lonely >= 3) and new nest building ----
  
  # 3.1 assigne new coordinates and save them 
  # 3.1.1 generate new coords, get random direction from dispersal 
  # 3.1.2 check if new coords are on nest 
  # 3.1.3 check if building buffer / region - if true get new coord
  # 3.1.4 check if within boundaries (and if coords not match with nest)
  
  # 3.2 mortality & new nets & placment of new_coords 
  # 3.2.1 mortality 2, check if new coords are in turbine/buffer, if true - killed
  # 3.2.2 new nest if two kites meet
  # 3.2.3 movement of rest (set age, abundance at new coords)
  
  # positions of lonely kites (age_lonely >= 3)
  coords_adults <- which(kites[ , , t+1, "age_lonely"] >= rep_age , arr.ind = TRUE)
  n_adults <- nrow(coords_adults)
  
  # to save new coords
  new_coords <- data.frame(x = rep(NA, n_adults), y = rep(NA, n_adults))
  
  # get position of recent turbines/buffer
  coords_turb_buffer <- which(turbine[, , t + 1] | buffer[, , t + 1] , arr.ind = TRUE)

  # get postion of region / building_buffer 
  coords_building_buffer <- which(building_buffer[, , t + 1] | region[, , t + 1] , arr.ind = TRUE)
  
  # 3.1 assigne new coordinates and save them ----
  # 3.1.1 generate new coords, get random direction from dispersal 
  # 3.1.2 check if new coords are on nest 
  # 3.1.3 check if building buffer / region - if true get new coord
  # 3.1.4 check if within boundaries (and if coords not match with nest)
  if (n_adults>0){
    
    for (i in 1:n_adults) {
      
      # 3.1.1 generate new coords, get random direction from dispersal 
      row_dispersal <- sample(1:nrow(dispersal),1 )
      rand_dispersal <- dispersal[row_dispersal,]
      dx <- rand_dispersal[,1]
      dy <- rand_dispersal[,2]
      
      new_x <- coords_adults[i,1] + dx
      new_y <- coords_adults[i,2] + dy
      
      # 3.1.2 check if new coords are on nest 
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
        
        check_if_nest <- apply(coords_nests, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
      }
      
      # 3.1.3 check if new coords are on building/ region
      check_if_building <- apply(coords_building_buffer, 1, function(row) {
        new_x == row[1] && new_y == row[2]
      })
      
      while (any(check_if_building)) {
        # Recalculate new direction
        row_dispersal <- sample(1:nrow(dispersal), 1)
        rand_dispersal <- dispersal[row_dispersal, ]
        dx <- rand_dispersal[1]
        dy <- rand_dispersal[2]
        
        # Recalculate new coordinates
        new_x <- coords_adults[i, 1] + dx
        new_y <- coords_adults[i, 2] + dy
        
        # Re-check if new coords are on building/ region
        check_if_building <- apply(coords_building_buffer, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
      }
      
      # 3.1.3 check if within boundaries (and if coords not match with nest)
      # migration = immigration
      # while loop with two conditions (coords within boundaries and not matching a nest)
      while (TRUE) {
        
        if (new_x >= 1 && new_x <= x_dim && new_y >= 1 && new_y <= y_dim) {
          break  # exit if coords are within boundaries

        } else {
          # If outside the boundaries, wrap around to the other side
          if (new_x < 1) {
            new_x <- x_dim  
          } else if (new_x > x_dim) {
            new_x <- 1  
          }
          
          if (new_y < 1) {
            new_y <- y_dim  
          } else if (new_y > y_dim) {
            new_y <- 1  
          }
        }
        
        # Recheck if the new coordinates are on a nest after boundary wrapping
        check_if_nest <- apply(coords_nests, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
        
        # If the new coordinates land on a nest, recalculate
        if (any(check_if_nest)) {
          # Recalculate new direction if landing on a nest
          row_dispersal <- sample(1:nrow(dispersal), 1)
          rand_dispersal <- dispersal[row_dispersal, ]
          dx <- rand_dispersal[1]
          dy <- rand_dispersal[2]
          
          # Recalculate new coordinates
          new_x <- coords_adults[i, 1] + dx
          new_y <- coords_adults[i, 2] + dy
        } else {
          break  # Exit the loop as coordinates are valid
        }
        
        #  Recheck if new coords are on building/ region
        check_if_building <- apply(coords_building_buffer, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
      
        # If the new coordinates land on a nest, recalculate
        if (any(check_if_building)) {
          # Recalculate new direction if landing on a nest
          row_dispersal <- sample(1:nrow(dispersal), 1)
          rand_dispersal <- dispersal[row_dispersal, ]
          dx <- rand_dispersal[1]
          dy <- rand_dispersal[2]
          
          # Recalculate new coordinates
          new_x <- coords_adults[i, 1] + dx
          new_y <- coords_adults[i, 2] + dy
        } else {
          break  # Exit the loop as coordinates are valid
        }
      }
        
      # 3.1.3 save new_coords
      new_coords[i,] <- c(new_x[[1]], new_y[[1]])
      
    } # close first loop (generate new_coords)
    
    # 3.1 check new coords ----
    if (n_adults == nrow(new_coords)){
      print(paste("generate newcoords: same nr. of coords", nrow(new_coords)))
    } else {
      stop(paste("generate newcoords: nr not the same of coords!", n_adults, n(new_coords)))
    }
    
    # check if new coords are nest coords
    coords_nests <- which(kites[,,t+1, "nest"] > 0, arr.ind = TRUE)
    coords_nests_df <- data.frame(x = coords_nests[,1], y = coords_nests[,2])
    # Find rows where (x, y) coordinates match between coords_nests and new_coords
    matching_coords <- which(apply(new_coords, 1, function(row) {
      any(row[1] == coords_nests_df$x & row[2] == coords_nests_df$y)
    }))
    # Check if there are any matching coordinates
    if(length(matching_coords) > 0) {
      print("Matching:")
      print(new_coords[matching_coords, ])
      stop(paste(length(matching_coords), 
                  "generate newcoords: matching coords between coords_nests & new_coords"))
    } else {
      print("generate newcoords: No matching coords between coords_nests & new_coords")
    }
  
    # 3.2 mortality & new nets & placment of new_coords  ----
    # 3.2.1 mortality 2, check if new coords are in turbine/buffer, if true - killed
    # 3.2.2 new nest if two kites meet
    # 3.2.3 movment of rest (set age, abundance at new coords)
    
    # 3.2.1 mortality 2, check if new coords are in turbine/buffer ----
    killed_indices <- which(apply(new_coords, 1, function(row) {
      any(row[1] == coords_turb_buffer[,1] & row[2] == coords_turb_buffer[,2])
    }))
    
    n_killed <- length(killed_indices)
    
    # if killed, set old coord to 0 (age, abundance), set killed to true
    if(n_killed > 0){
      print(paste(n_killed, "killed by turbine"))
      
      
      for(idx in killed_indices){
        
        # set new coord (age <- 0, abundance <- -1)
        new_x <- new_coords[idx,1]
        new_y <- new_coords[idx,2]
        
        kites[new_x,new_y,t+1, "age_lonely"] <- 0
        kites[new_x,new_y,t+1, "abundance"] <- 0
        kites[new_x,new_y,t+1, "killed_turb"] <- 1
        
        # set old coords (age, and abundance <- 0)
        x <- coords_adults[idx,1]
        y <- coords_adults[idx,2]
        kites[x,y,t+1, "age_lonely"] <- 0
        kites[x,y,t+1, "abundance"] <- 0
        
      }
      
    } else {
      print("no kites killed in turbine")
    }
    
    # delete killed coords from coords_adult and new_coords
    new_coords <- new_coords[-killed_indices,]
    coords_adults <- coords_adults[-killed_indices,]
    
    # reset row indices
    rownames(new_coords) <- NULL
    rownames(coords_adults) <- NULL
    
    # 3.2.1 check after killing ----
    if(nrow(new_coords) != nrow(coords_adults)) {
      stop("nrow(new_coords) != nrow(coords_adults), after kite killed by turbine")
    } else {
      print(paste(nrow(new_coords),nrow(coords_adults)))
    }
    
    # check same abundance is right
    if (sum(kites[,,t+1, "abundance"])+ n_killed == check_abund_before) {
      print(paste("(after killing)", sum(kites[,,t+1, "abundance"]),
                  ",killed:", n_killed,
                  " | before", check_abund_before))
    } else {
      stop(paste("(after killing)", sum(kites[,,t+1, "abundance"]),
                  ",killed:", n_killed,
                  " | before", check_abund_before))
    }
    
    
    # 3.2.2 new nest if two kites meet  ----
    # get duplicates
   
    duplicate_idx <- which(duplicated(new_coords) | duplicated(new_coords, fromLast = TRUE))
    dup_coords <- new_coords[duplicate_idx, ]
    
   
    # coords of new nests and random age of nest
    new_coords_nest <- dup_coords[duplicated(dup_coords), ] # new x,y for nest
    n_new_nests <- nrow(new_coords_nest)
    
    if (length(duplicate_idx) > 0) {
      print(paste(n_new_nests, "new nests"))
      
      for (i in 1:n_new_nests) {
        kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "nest"] <- 1
        kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "age_nest"] <- sample(rep_age:liv_exp-1, 1) # could be changed to average
        kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "abundance"] <- 2
        }
      
      # delet old coords
      # problem in coords_adult[idx] same coords
      
      for (idx in duplicate_idx) {
        
        # check if old coord is nest 
        check_nest <- kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "nest"]
      
        if (check_nest[[1]] == 0 ){
          
          # no nest - delete old coords
          kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "age_lonely"] <- 0
          kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "abundance"] <- 0
          
        } else {
          # if nest exists - abundance still 2
          kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "age_lonely"] <- 0
          kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "abundance"] <- 2
          
        }
        
      }
    } 
    
    # delete killed coords from coords_adult and new_coords
    new_coords <- new_coords[-duplicate_idx,]
    coords_adults <- coords_adults[-duplicate_idx,]
    
    # reset row indices
    rownames(new_coords) <- NULL
    rownames(coords_adults) <- NULL
    
    # 3.2.2 check new nests ----
    if(nrow(new_coords) != nrow(coords_adults)) {
      stop("new_coords != coords_adults, after nest placment")
    } 
    
    if (sum(kites[,,t+1, "abundance"]) + n_killed == check_abund_before) {
      
      print(paste(n_new_nests, "new nests"))
      print(paste("(after killing & nests placment)", sum(kites[,,t+1, "abundance"]),
                  ",killed:", n_killed,
                  " | before", check_abund_before))
    } else {
      stop(paste(n_new_nests, "new nests,", 
                 "(after killing & nests placment)", sum(kites[,,t+1, "abundance"]),
                       "killed:", n_killed,
                       " | before", check_abund_before))
    }
    
    # 3.2.3 movement of rest lonely adult kites (set age, abundance at new coords) ----
    if(nrow(new_coords) > 0){
      
      for(i in 1:nrow(new_coords)){
        
        # check if old coord is nest 
        check_nest <- kites[coords_adults[i,1], coords_adults[i,2], t+1, "nest"]
        
        if (check_nest[[1]] == 0 ){
          
          # no nest - delete old coords
          kites[coords_adults[i,1], coords_adults[i,2], t+1, "age_lonely"] <- 0
          kites[coords_adults[i,1], coords_adults[i,2], t+1, "abundance"] <- 0
          
          # new placment of lonely adult kite
          kites[new_coords[i,1], new_coords[i,2], t+1, "age_lonely"] <- 
            kites[coords_adults[i,1], coords_adults[i,2], t, "age_lonely"][[1]]
          
          kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"] <- 1
          
        } else {
          
          # if nest exists - abundance still 2
          kites[coords_adults[i,1], coords_adults[i], t+1, "age_lonely"] <- 0
          kites[coords_adults[i,1], coords_adults[i,2], t+1, "abundance"] <- 2
        
          # new placment of lonely adult kite
          kites[new_coords[i,1], new_coords[i,2], t+1, "age_lonely"] <- 
            kites[coords_adults[i,1], coords_adults[i,2], t, "age_lonely"][[1]]
          
          kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"] <- 1
        
        }
      }
    }
    # 3.2.3 check abundance after movement ----
    abund_after <- sum(kites[,,t+1, "abundance"])
    if (abund_after + n_killed == check_abund_before){
      print(paste("(after killing & nests placment & movment):", abund_after,
                  ",killed:", n_killed,
                  " | before", check_abund_before))
    } else {
      stop(paste("(after killing & nests placment & movment)", abund_after,
                  "killed:", n_killed,
                  " | before", check_abund_before))
    }
  } # inital checking if lonely kites exist
  
  # 4 Reproduction of kites ---- 
  # # alle nester reporduzieren mit 0.79 (growth rate) (auch die neu gebildeten)
  # # nester die schon ein juv haben, kein neues 
  # # (we need to insert  after mortality/aging and before dispersal at timestep t+1 I think)
  # 
  # # Let's look for eligible=geeignete nests
  # # active nests (TRUE in the "nest" layer)
  # # and have not yet reproduced (FALSE in the "juv" layer).
  # eligible_nests <- which(kites[,,t+1, "nest"] == 1 & kites[,,t+1, "juv"] == 0, arr.ind = TRUE)
  # N_t <- nrow(eligible_nests)
  # 
  # if (N_t > 0) {
  #   # growth rate and nest density
  #   reproduction_probability <- exp(growth_rate * (1 - N_t / carrying_capacity))
  #   
  #   # loop each of the eligable nest
  #   for (i in 1:N_t) {
  #     
  #     # every nest has one juv or none (based on reproduction probability)
  #     if (runif(1) < reproduction_probability) {
  #       x_coord <- eligible_nests[i, 1]
  #       y_coord <- eligible_nests[i, 2]
  #       
  #       # mark nest has juv already; then DONT reproduce again directly
  #       kites[x_coord, y_coord, t+1, "juv"] <- 1
  #       
  #       kites[x_coord, y_coord, t+1, "abundance"] <- kites[x_coord, y_coord, t+1, "abundance"] + 1
  #     }
  #   }
  # }
#} # end of for loop timesteps
  
  sum(kites[,,t+1, "abundance"])
  sum(kites[,,t+1, "juv"])
    
  # 6 visual as extra .R file 
  # siehe /visual_model.R muss modifiziert werden
  # in new_redkite_eco_para.R unten Versuch zu ploten
    

