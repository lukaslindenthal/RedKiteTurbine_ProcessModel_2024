# ----------------------------------------------------------------
# Turbine and Red Kite Model Simulation ----
# ----------------------------------------------------------------
# This script simulates the interaction between wind turbine placement and red 
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

# ----------------------------------------------------------------
# Authors: Neele Haß & Lukas Lindenthal
# ----------------------------------------------------------------

# Dependecies ----
library(dplyr)
library(ggplot2)

# ----------------------------------------------------------------
# Sourcing required parameter and model scripts
# ----------------------------------------------------------------
source("./scripts/final_Redkite_eco_constrains_model/Time_Dim_Turbo_Region_para_setting_version3.R") # load timesteps, dim and Turbine
source("./scripts/final_Redkite_eco_constrains_model/Redkite_eco_para_setting_version3.R") # load redkites initial set up

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
  # source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R") # load redkites initial set up
  # library(dplyr)
  # timesteps <- 5
  
  # for (t in 1:(timesteps - 1)) {
  #   print(paste("t=", t, "& timestep= ", t+1))
  
  # 1 mortality 1 - Kites killed by new turbine placements or buffer [Lukas] ----
  # we run it at the beginning of each new timestep (t+1)
  for (i in 1:x_dim) {
    for (j in 1:y_dim) {
      # check if turbine (or buffer) is present at cell
      if (turbine[i, j, t] == TRUE || buffer[i, j, t] == TRUE ||
          turbine[i, j, t+1] == TRUE || buffer[i, j, t+1] == TRUE) {
        # number of kites that are currently in this cell as killed by building
        kites[i, j, t+1, "killed_build"] <- kites[i, j, t, "abundance"]
        # set abundance to zero = dead
        kites[i, j, t+1, "abundance"] <- 0
        # maybe this makes sense?! clear any other related state (e.g. age and nest/juv flags)
        kites[i, j, t+1, "age_lonely"] <- 0
        kites[i, j, t+1, "age_nest"] <- 0
        kites[i, j, t+1, "nest"] <- 0
        kites[i, j, t+1, "juv"] <-0
      }
    }
  }
  # 2 age of kites and nest increased ----
  # and
  # check if kite or nest will die, age > liv_exp
  abund_beginning <- sum(kites[,,t,"abundance"])
  
  # ageing 
  # nest
  coords_nests <- which(kites[, , t, "age_nest"] > 0, arr.ind = TRUE) # check for nests
  num_nests <- nrow(coords_nests)
  
  if (num_nests > 0){
    for (i in 1:num_nests) {
      new_age_nest <- kites[coords_nests[i,1],coords_nests[i,2], t, "age_nest"] + 1
      
      if(new_age_nest[[1]] <= liv_exp) {
        
        # nest survives
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "age_nest"] <- new_age_nest[[1]]
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "nest"] <- 
          kites[coords_nests[i,1],coords_nests[i,2], t, "nest"]
      } # else place still 0 --> nest = dies
      
    }
  }
  
  # lonely adults + juveniles
  coords_lonely <- which(kites[, , t, "age_lonely"] > 0, arr.ind = TRUE) # check for lonely kites (adults & juveniles)
  num_lonely <- nrow(coords_lonely)
  
  if (num_lonely > 0){
    for (i in 1:num_lonely) {
      new_age_lonely <- kites[coords_lonely[i,1],coords_lonely[i,2], t, "age_lonely"] + 1
      
      if(new_age_nest[[1]] <= liv_exp) {
        # nest survives
        kites[coords_lonely[i,1],coords_lonely[i,2], t+1, "age_lonely"] <- new_age_lonely[[1]]
        
      } # else place still 0 --> kite = dies
    }
  }
  
  # Remove kites that exceed life expectancy
  coords_die <- which(kites[, , t+1, "age_lonely"] > liv_exp | 
                        kites[, , t+1, "age_nest"] > liv_exp, arr.ind = TRUE)
  
  if (nrow(coords_die) > 0) { # if nest dead, juvenile dies aswell
    for (i in 1:nrow(coords_die)) {
      kites[coords_die[i,1], coords_die[i,2], t+1, "age_lonely"] <- 0
      kites[coords_die[i,1], coords_die[i,2], t+1, "age_nest"] <- 0
      kites[coords_die[i,1], coords_die[i,2], t+1, "nest"] <- 0
      kites[coords_die[i,1], coords_die[i,2], t+1, "abundance"] <- 0
    }
  }
  
  
  # juvenile and abundance for t+1
  # juv still at place if age <= rep_age, if age > rep_age the will turn adult and will move in next steps
  coords_nests_t1 <- which(kites[, , t+1, "age_nest"] > 0 & 
                             kites[, , t+1, "age_nest"] <= liv_exp, arr.ind = TRUE) # check for nests
  num_nests_t1 <- nrow(coords_nests_t1)
  # num_nests_t1 + sum(kites[,,t, "age_nest"] >= 12) == num_nests
  
  coords_adults_t1 <- which(kites[, , t+1, "age_lonely"] >= rep_age & 
                              kites[, , t+1, "age_lonely"] <= liv_exp, arr.ind = TRUE) # check for lonely kites (adults = age > rep_age)
  num_adults_t1 <- nrow(coords_adults_t1)
  
  coords_juv_t1 <- which(kites[, , t+1, "age_lonely"] > 0 & # check for juveniles 
                           kites[, , t+1, "age_lonely"] < rep_age, arr.ind = TRUE) # (age < rep_age, still in nest)
  num_juv_t1 <- nrow(coords_juv_t1)
  
  # nests 
  if(num_nests_t1 > 0){
    for(i in 1:num_nests_t1){
      kites[coords_nests_t1[i,1],coords_nests_t1[i,2], t+1, "abundance"] <- 2
    }
  }
  
  # adults / >= 3
  if(num_adults_t1 > 0){
    for(i in 1:num_adults_t1){
      kites[coords_adults_t1[i,1],coords_adults_t1[i,2], t+1, "abundance"] <- 
        kites[coords_adults_t1[i,1],coords_adults_t1[i,2], t+1, "abundance"] + 1
    }
  }
  
  # juveniles
  if(num_juv_t1 > 0){
    for(i in 1:num_juv_t1){
      
      kites[coords_juv_t1[i,1],coords_juv_t1[i,2], t+1, "abundance"] <- 
        kites[coords_juv_t1[i,1],coords_juv_t1[i,2], t+1, "abundance"] + 1
      
      kites[coords_juv_t1[i,1],coords_juv_t1[i,2], t+1, "juv"] <- 1
    }
  }
  
  # 2 check ageing ----
  
  # to check "abundance" before and after aging
  # nrow(which(kites[,,t+1, "age_lonely"] > 12, arr.ind = TRUE))
  
  n_died_natural <- nrow(which(kites[,,t, "age_lonely"] >= 12, arr.ind = TRUE)) +  
    nrow(which(kites[,,t, "age_nest"] >= 12, arr.ind = TRUE))*2
  
  abund_theo <- abund_beginning - n_died_natural
  
  check_abund_before <- sum(kites[ , , t+1, "abundance"])
  
  if (abund_theo == check_abund_before){
    print(paste("2. ageing\n","same nr after aging: theo", abund_theo, "; real t1", check_abund_before,
                "beg:", check_abund_before, 
                "\n natural dead:", n_died_natural))
  } else {
    stop(paste("2. ageing\n", "nr not the same after ageing! theo", abund_theo, "; real t1",check_abund_before,
               "\n beginning:", abund_beginning,
               "\n natural dead:", n_died_natural))
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
  coords_adults <- which(kites[ , , t+1, "age_lonely"] >= rep_age &
                           kites[ , , t+1, "age_lonely"] <= liv_exp, arr.ind = TRUE)
  n_adults <- nrow(coords_adults)
  
  # to save new coords
  new_coords <- data.frame(x = rep(NA, n_adults), y = rep(NA, n_adults))
  
  # get position of recent turbines/buffer
  coords_turb_buffer <- which(turbine[, , t+1] | buffer[, , t+1] , arr.ind = TRUE)
  
  # get postion of nest / juvenile
  coords_nests_juv <- which(kites[ , , t+1, "nest"] > 0 | 
                              kites[ , , t+1, "juv"] > 0 , arr.ind = TRUE) # juv not realy nessacary, but just to be sure
  
  # get postion of region / building_buffer 
  coords_building_buffer <- which(building_buffer[, , t + 1] | region[, , t + 1] , arr.ind = TRUE)
  
  
  # 3.1 assigne new coordinates and save them ----
  # 3.1.1 generate new coords, get random direction from dispersal 
  # 3.1.2 check if new coords are on nest 
  # 3.1.3 check if building buffer / region - if true get new coord
  # 3.1.4 check if within boundaries (and if coords not match with nest)
  
  # reset row indices
  rownames(new_coords) <- NULL
  rownames(coords_adults) <- NULL
  if (n_adults>0){
    
    for (i in 1:n_adults) {
      
      # 3.1.1: Get new random direction
      row_dispersal <- sample(1:nrow(dispersal), 1)
      rand_dispersal <- dispersal[row_dispersal, ]
      dx <- rand_dispersal[1]
      dy <- rand_dispersal[2]
      
      new_x <- coords_adults[i, 1] + dx
      new_y <- coords_adults[i, 2] + dy
      
      # Wrap coordinates within the grid
      new_x <- ((new_x - 1) %% x_dim) + 1 # bereich: [0, x_dim-1]
      new_y <- ((new_y - 1) %% y_dim) + 1
      
      # 3.1.2: Check if new position is on a nest or building
      check_if_nest_juv <- apply(coords_nests_juv, 1, function(row) {
        new_x == row[1] && new_y == row[2]
      })
      
      check_if_building <- apply(coords_building_buffer, 1, function(row) {
        new_x == row[1] && new_y == row[2]
      })
      
      # 3.1.3: Try new locations if invalid
      max_attempts <- 500
      attempt <- 0
      
      while ((any(check_if_nest_juv) || any(check_if_building)) && attempt < max_attempts) {
        attempt <- attempt + 1
        if (attempt >= max_attempts) {
          new_x <- coords_adults[i, 1]
          new_y <- coords_adults[i, 2]
          break
        }
        
        row_dispersal <- sample(1:nrow(dispersal), 1)
        rand_dispersal <- dispersal[row_dispersal, ]
        dx <- rand_dispersal[1]
        dy <- rand_dispersal[2]
        
        new_x <- coords_adults[i, 1] + dx
        new_y <- coords_adults[i, 2] + dy
        
        # Wrap around boundaries
        new_x <- ((new_x - 1) %% x_dim) + 1
        new_y <- ((new_y - 1) %% y_dim) + 1
        
        check_if_nest_juv <- apply(coords_nests_juv, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
        
        check_if_building <- apply(coords_building_buffer, 1, function(row) {
          new_x == row[1] && new_y == row[2]
        })
      }
      
      # 3.1.4: Save new coordinates
      new_coords[i, ] <- c(new_x, new_y)
      
    } # close first loop (generate new_coords)
    
    # reset row indices
    rownames(new_coords) <- NULL
    rownames(coords_adults) <- NULL
    
    # 3.1 check new coords ----
    if (n_adults == nrow(new_coords)){
      print(paste("3.1 new coords  generate newcoords: same nr. of coords", nrow(new_coords)))
    } else {
      stop(paste("3.1 new coords generate newcoords: nr not the same of coords!", n_adults, n(new_coords)))
    }
    
    # check if new coords are nest coords
    coords_nests_juv <- which(kites[,,t+1, "nest"] > 0 |
                                kites[,,t+1, "juv"] > 0, arr.ind = TRUE)
    coords_nests_df <- data.frame(x = coords_nests_juv[,1], y = coords_nests_juv[,2])
    # Find rows where (x, y) coordinates match between coords_nests and new_coords
    matching_coords <- which(apply(new_coords, 1, function(row) {
      any(row[1] == coords_nests_df$x & row[2] == coords_nests_df$y)
    }))
    # Check if there are any matching coordinates
    if(length(matching_coords) > 0) {
      print("Matching:")
      print(new_coords[matching_coords, ])
      stop(paste(length(matching_coords), 
                 "3.1 new coords generate newcoords: matching coords between coords_nests_juv & new_coords"))
    } else {
      print("3.1 new coords generate newcoords: No matching coords between coords_nests_juv & new_coords")
    }
    
    # 3.2 mortality & new nets & placment of new_coords  ----
    # 3.2.1 mortality 2, check if new coords are in turbine/buffer, if true - killed
    # 3.2.2 new nest if two kites meet
    # 3.2.3 movment of rest (set age, abundance at new coords)
    
    # 3.2.1 mortality 2, check if new coords are in turbine/buffer ----
    abund_before_killed <- sum(kites[,,t+1,"abundance"])
    
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
        kites[new_x,new_y,t+1, "killed_move"] <- 1
        
        # set old coords (age, and abundance <- 0)
        x <- coords_adults[idx,1]
        y <- coords_adults[idx,2]
        
        #print(paste("old",  kites[x,y,t+1, "abundance"]))
        
        kites[x,y,t+1, "age_lonely"] <- 0
        kites[x,y,t+1, "abundance"] <- 
          kites[x,y,t+1, "abundance"] - 1
        
        #print(paste("new",  kites[x,y,t+1, "abundance"]))
        #print(paste(x,y))
      }
      
      # delete killed coords from coords_adult and new_coords
      new_coords <- new_coords[-killed_indices,]
      coords_adults <- coords_adults[-killed_indices,]
      
      # reset row indices
      rownames(new_coords) <- NULL
      rownames(coords_adults) <- NULL
      
    } else {
      print("no kites killed in turbine")
    }
    
    abund_after_killed <- sum(kites[,,t+1,"abundance"])
    
    # 3.2.1 check after killing ----
    if(nrow(new_coords) != nrow(coords_adults)) {
      stop("3.2.1 after killing; nrow(new_coords) != nrow(coords_adults), after kite killed by turbine")
    } else {
      print(paste("3.2.1 after killing",nrow(new_coords),nrow(coords_adults)))
    }
    
    # check same abundance is right
    if ((abund_after_killed + n_killed) == abund_before_killed) {
      print(paste("3.2.1 after killing; (after killing)", abund_after_killed,
                  ",killed:", n_killed,
                  " | before", abund_before_killed))
    } else {
      stop(paste("3.2.1 after killing; (after killing)", abund_after_killed,
                 ",killed:", n_killed,
                 " | before", abund_before_killed))
    }
    
    
    # 3.2.2 new nest if two kites meet  ----
    # ! if 3 kites are at the same coordinate, it dies 
    # other idea, if 3 kites are at the same coordinate -> it stays at the old place
    
    abund_before_meeting <- sum(kites[,,t+1, "abundance"])
    # get duplicates
    
    duplicate_idx <- which(duplicated(new_coords) | duplicated(new_coords, fromLast = TRUE))
    dup_coords <- new_coords[duplicate_idx, ]
    
    # if(length(duplicate_idx) %% 2 == 1 ){ # 0 = even, 1 = uneven
    
    # counter:
    # Count occurrences of each unique (x, y) pair
    coord_counts <- dup_coords %>%
      count(x, y) %>%
      arrange(desc(n))  # Sort by count
    
    # coords n > 2
    del_row <- which(coord_counts[,3] > 2, arr.ind = TRUE)
    if(length(del_row) > 0){
      cat("3.2.2 new nest; Duplicate count is odd; removing one kite from the duplicate set.\n")
      del_coords <- coord_counts[del_row,1:2]
      
      # get index of this coords
      del_idx_help <- which(dup_coords[, 1] == del_coords[,1] &
                              dup_coords[, 2] == del_coords[,2], arr.ind = TRUE)
      
      # pick random idx to delet
      rand_idx <- sample(del_idx_help, size = 1)
      new_duplicate_idx <- duplicate_idx[-rand_idx]
      
      if(length(new_duplicate_idx) %% 2 == 1 ){
        stop("meeting of kites is still uneven")
      }
      
      # new coords where nests are set
      dup_coords <- new_coords[new_duplicate_idx, ]
      
      n_killed <- n_killed + (rand_idx-2) # wenn sogar mehr als 3 kites auf den selben ort fallen überleben auch nur 2
      # later down delete from old coords, so from t+1
      # kite dies because couldnt find partner
    }
    
    # } # even uneven
    
    # coords of new nests and random age of nest
    new_coords_nest <- dup_coords[duplicated(dup_coords), ] # new x,y for nest
    n_new_nests <- nrow(new_coords_nest)
    rownames(new_coords_nest) <- NULL
    
    check_nest <- nrow(which(kites[,,t+1, "nest"] >0, arr.ind = TRUE))
    
    if (length(duplicate_idx) > 0) {
      
      # print(paste(n_new_nests, "new nests"))
      
      for (i in 1:n_new_nests) {
        # print("set new nests")
        # print(paste("old", kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "abundance"]))
        
        # set new nests
        kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "nest"] <- 1
        kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "age_nest"] <- sample(rep_age:(liv_exp-1), 1) # could be changed to average
        kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "abundance"] <- 2
        
        # print(paste("new", kites[new_coords_nest[i,1], new_coords_nest[i,2], t+1, "abundance"]))
        # print(paste(new_coords_nest[i,1], new_coords_nest[i,2]))
      }
      
      # delet old coords
      for (idx in duplicate_idx) {
        # print("delete old coords")
        # print(paste("old", kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "abundance"]))
        
        # delete old coords / if nest abundance <- 2
        kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "age_lonely"] <- 0
        kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "abundance"] <- 
          kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "abundance"] - 1
        
        # print(paste("new", kites[coords_adults[idx,1], coords_adults[idx,2], t+1, "abundance"]))
      }
    } 
    
    # delete killed coords from coords_adult and new_coords
    new_coords <- new_coords[-duplicate_idx,]
    coords_adults <- coords_adults[-duplicate_idx,]
    
    # reset row indices
    rownames(new_coords) <- NULL
    rownames(coords_adults) <- NULL
    
    # 3.2.2 check new nests ----
    abund_after_meeting <- sum(kites[,,t+1, "abundance"]) 
    
    if(length(duplicate_idx) %% 2 == 1 ){
      print("3.2.1 after nest; 3 on the same place, one killed")
      abund_after_meeting_help <- sum(kites[,,t+1, "abundance"]) + 1
    } else {
      abund_after_meeting_help <- sum(kites[,,t+1, "abundance"]) 
    }
    
    if(nrow(new_coords) != nrow(coords_adults)) {
      stop("3.2.2 after nest; new_coords != coords_adults")
    } 
    
    if(nrow(new_coords) - (n_new_nests*2) != nrow(coords_adults) - (n_new_nests*2)) {
      stop("3.2.2 after nest; nrow(new_coords) - (n_new_nests*2) == nrow(coords_adults) - (n_new_nests*2)")
    } 
    
    if (abund_before_meeting == abund_after_meeting_help) {
      
      print(paste("3.2.2 after nest; ", n_new_nests, "new nests"))
      print(paste("3.2.2 (after killing & nests placment)",abund_after_meeting,
                  ",killed:", n_killed,
                  " | before", check_abund_before))
    } else {
      stop(paste("3.2.2 after nest; ", n_new_nests, "new nests,", 
                 "3.2.2 after nest \n",
                 "(after killing & nests placment)", abund_after_meeting,
                 "killed:", n_killed,
                 " | before", check_abund_before))
    }
    
    
    # 3.2.3 movement of rest lonely adult kites (set age, abundance at new coords) ----
    abund_before_movement <- sum(kites[,,t+1, "abundance"])
    if(nrow(new_coords) > 0){
      
      for(i in 1:nrow(new_coords)){
        
        # new placment of lonely adult kite
        abund <- kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"] + 1
        
        if(abund >= 2 ){ # recheck nest
          print("nest placement!")
          print("set new coords")
          print(paste("old", kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"]))
          
          kites[new_coords[i,1], new_coords[i,2], t+1, "age_lonely"] <- 0
          kites[new_coords[i,1], new_coords[i,2], t+1, "age_nest"] <- sample(rep_age:(liv_exp-1), 1)
          kites[new_coords[i,1], new_coords[i,2], t+1, "nest"] <- 1
          kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"] <- abund
          
          print(paste("new", kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"]))
          print(paste("coords", new_coords[i,1], new_coords[i,2], t+1))
        } else {
          # print("set new coords")
          # print(paste("old", kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"]))
          # 
          kites[new_coords[i,1], new_coords[i,2], t+1, "age_lonely"] <- 
            kites[coords_adults[i,1], coords_adults[i,2], t+1, "age_lonely"][[1]]
          
          kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"] <- abund
          
          # print(paste("new", kites[new_coords[i,1], new_coords[i,2], t+1, "abundance"]))
        }
        
        # delete old coords, nests remain
        # print("delete old coords")
        # print(paste("old", kites[coords_adults[i,1], coords_adults[i,2], t+1, "abundance"]))
        #
        kites[coords_adults[i,1], coords_adults[i,2], t+1, "age_lonely"] <- 0
        kites[coords_adults[i,1], coords_adults[i,2], t+1, "abundance"] <-
          kites[coords_adults[i,1], coords_adults[i,2], t+1, "abundance"] - 1
        # print(paste("new", kites[coords_adults[i,1], coords_adults[i,2], t+1, "abundance"]))
        
      }
    }
    abund_after_movement <- sum(kites[,,t+1, "abundance"])
    
    # 3.2.3 check abundance after movement ----
    
    if (abund_after_movement == abund_before_movement){
      print(paste("3.2.3 after movement; ",
                  "(after killing & nests placment & movement):", abund_after_movement,
                  ",killed:", n_killed, check_abund_before - n_killed,
                  " | before", check_abund_before))
    } else {
      stop(paste("3.2.3 after movement; ",
                 "(after killing & nests placment & movement)", abund_after_movement,
                 "killed:", n_killed, check_abund_before - n_killed,
                 " | before", check_abund_before))
    }
  } # inital checking if lonely kites exist / movement
  
  # 4 Reproduction of kites ----
  # alle nester reporduzieren mit 0.79 (growth rate) (auch die neu gebildeten)
  # nester die schon ein juv haben, kein neues
  # (we need to insert  after mortality/aging and before dispersal at timestep t+1 I think)
  
  # recheck nest
  coords_abund_nests <- which(kites[,,t+1,"abundance"]> 1, arr.ind = TRUE)
  coords_abund_no_nests <- which(kites[,,t+1,"abundance"] < 2 &
                                   kites[,,t+1,"abundance"] > 0 , arr.ind = TRUE)
  if(nrow(coords_abund_nests) > 0){
    for(i in 1:nrow(coords_abund_nests)){
      x <- coords_abund_nests[i,1]
      y <- coords_abund_nests[i,2]
      kites[x,y,t+1, "nest"] <- 1
    }
  }
  if(nrow(coords_abund_no_nests) > 0){
    for(i in 1:nrow(coords_abund_no_nests)){
      x <- coords_abund_no_nests[i,1]
      y <- coords_abund_no_nests[i,2]
      kites[x,y,t+1, "nest"] <- 0
    }
  }
  # Let's look for eligible=geeignete nests
  # active nests (TRUE in the "nest" layer)
  # and have not yet reproduced (FALSE in the "juv" layer).
  eligible_nests <- which(kites[,,t+1, "nest"] == 1 & kites[,,t+1, "juv"] == 0, arr.ind = TRUE)
  N_t <- nrow(eligible_nests)
  
  if (N_t > 0) {
    # growth rate and nest density
    reproduction_probability <- exp(growth_rate * (1 - N_t / N_t)) # carrying_capacity
    
    # loop each of the eligable nest
    for (i in 1:N_t) {
      
      # every nest has one juv or none (based on reproduction probability)
      if (runif(1) < reproduction_probability) {
        x_coord <- eligible_nests[i, 1]
        y_coord <- eligible_nests[i, 2]
        
        # mark nest has juv already; then DONT reproduce again directly
        kites[x_coord, y_coord, t+1, "juv"] <- 1
        kites[x_coord, y_coord, t+1, "age_lonely"] <- 1
        kites[x_coord, y_coord, t+1, "abundance"] <- kites[x_coord, y_coord, t+1, "abundance"] + 1
        
      }
    }
  }
} # end of for loop timesteps

