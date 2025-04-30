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
library(dplyr)

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
    
    # copy kites_metric from t 
    df_kites  <- kites_metric[[t]]
    
    # 1 age of kites and nest increased ----
    # and check if kite or nest will die, age > liv_exp
    
    # ageing nest, juv and lonely 
    if (nrow(df_kites) > 0) {
      df_kites[, 3:5] <- as.data.frame(apply(df_kites[, 3:5], 2, function(x) ifelse(x > 0, x + 1, x))) # 2 = column-wise
    }
    
    # dying if age > liv_exp, if nest dies juv dies as well
    # write in df_killed and delete from df_kites
    df_killed <-  df_kites[df_kites$nest_age > liv_exp | df_kites$lonely_age > liv_exp, ]
    df_killed$killed <- rep("natural", nrow(df_killed))
    
    #delete from df_kites
    df_kites <- df_kites[!(df_kites$nest_age > liv_exp | df_kites$lonely_age > liv_exp), ]
    
    # # check
    # any(df_kites$nest_age > liv_exp)
    
    # 2 mortality 1 - Kites killed by new turbine placements or buffer ----
    # we run it at the beginning of each new timestep (t+1)
    mort_coords <- which(turbine[ , , t+1] | buffer[, , t+1], arr.ind = TRUE)
    mort_coords <- data.frame(x = mort_coords[,1],
                              y = mort_coords[,2],
                              killed = rep("build", nrow(mort_coords)))
    
    killed_build <- merge(df_kites, mort_coords, by = c("x", "y"))
    
    
    if(nrow(killed_build) > 0){
      # write into df_killed
      df_killed <- rbind(df_killed, killed_build)
      
      # delete from df_kites
      df_kites <- anti_join(df_kites, mort_coords, by = c("x", "y"))
    }
    
    
    # rest index
    rownames(df_kites) <- NULL
    
    # 1.2 ageing juv - adult ----
    # juv to adult lonely if juv_age >= rep_age
    juv_adult <- df_kites[df_kites$juv_age >= rep_age, ]
    if(nrow(juv_adult) > 0){
      # juv_age >= rep_age set 0
      df_kites[df_kites$juv_age >= rep_age, "juv_age"] <- 0
      
      # creat new lonely_age 
      juv_adult$nest_age <- rep(0, nrow(juv_adult))
      juv_adult[, "lonely_age" ] <- juv_adult[, "juv_age"]
      juv_adult[, "juv_age"] <- rep(0, nrow(juv_adult))
      df_kites <- rbind(df_kites, juv_adult)
      
    }
    
    # rest index
    rownames(df_kites) <- NULL
    
    
    # check ageing & mortality ----
    
    # theoretical
    nest <- sum(kites_metric[[t]]$nest_age == liv_exp)
    nest_juv <- sum((kites_metric[[t]]$juv_age > 0) & (kites_metric[[t]]$nest_age == liv_exp))
    lonely <- sum(kites_metric[[t]]$lonely_age == liv_exp)
    theo_died_natural <- nest*2 + nest_juv + lonely
    
    # natural dead
    real_nest <- sum(df_killed$nest_age > liv_exp) 
    real_nest_juv <- sum((df_killed$juv_age > 0) & (df_killed$nest_age > liv_exp))
    real_lonely <- sum(df_killed$lonely_age > liv_exp)
    real_died_natural <- real_nest*2 + real_nest_juv + real_lonely
    
    # total number killed by building turbine
    real_nest <- sum((df_killed$killed == "build") & 
                       (df_killed$nest_age > 0))
    real_nest_juv <- sum((df_killed$killed == "build") & 
                           (df_killed$juv_age > 0) & (df_killed$nest_age > 0))
    real_lonely <- sum((df_killed$killed == "build") & 
                         (df_killed$lonely_age > 0))
    real_died_build <- real_nest*2 + real_nest_juv + real_lonely
    
    # abund check 
    real_nest <- sum(df_kites$nest_age > 0) 
    real_nest_juv <- sum((df_kites$juv_age > 0) & (df_kites$nest_age > 0))
    real_lonely <- sum(df_kites$lonely_age > 0)
    real_abund <- real_nest*2 + real_nest_juv + real_lonely
    
    theo_abund <- kites_abund$total_abund[t] - theo_died_natural - real_died_build
    
    if (theo_died_natural != real_died_natural || theo_abund != real_abund){
      stop(paste("2. ageing \nnatural dead: real ", real_died_natural, "| theo ", theo_died_natural, 
                 "| killed_build:", real_died_build,
                 "\ntheo_abund:",theo_abund, "real_abund:",real_abund))
    } else {
      print(paste("2. ageing \nnatural dead:", theo_died_natural, "| killed build total: ", real_died_build))
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
    old_coords_lonely_kites <- lonely_kites <- df_kites[(df_kites$lonely_age > 0), ]
    
    lonely_kites <- df_kites[(df_kites$lonely_age > 0), ]
    df_kites <- df_kites[df_kites$lonely_age <= 0, ] # delete lonely kites
    n_lonely <- nrow(lonely_kites)
    
    # to save new coords
    new_coords <- data.frame(x = rep(NA, n_lonely), y = rep(NA, n_lonely))
    
    # get position of recent turbines/buffer
    coords_turb_buffer <- which(turbine[, , t+1] | buffer[, , t+1] , arr.ind = TRUE)
    
    # get postion of nest / juvenile
    coords_nests_juv <- df_kites[df_kites$nest_age > 0,c("x","y")]
    
    # get postion of region / building_buffer 
    coords_building_buffer <- which(building_buffer[, , t + 1] | region[, , t + 1] , arr.ind = TRUE)
    
    # 3.1 assigne new coordinates and save them ----
    # 3.1.1 generate new coords, get random direction from dispersal 
    # 3.1.2 check if new coords are on nest 
    # 3.1.3 check if building buffer / region - if true get new coord
    # 3.1.4 check if within boundaries (and if coords not match with nest)
    
    # reset row indices
    rownames(new_coords) <- NULL
    rownames(lonely_kites) <- NULL
    
    if (n_lonely>0){
      
      for (i in 1:n_lonely) {
        
        # 3.1.1: Get new random direction
        row_dispersal <- sample(1:nrow(dispersal), 1)
        rand_dispersal <- dispersal[row_dispersal, ]
        dx <- rand_dispersal[1]
        dy <- rand_dispersal[2]
        
        new_x <- lonely_kites[i, 1] + dx
        new_y <- lonely_kites[i, 2] + dy
        
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
            new_x <- lonely_kites[i, 1]
            new_y <- lonely_kites[i, 2]
            break
          }
          
          row_dispersal <- sample(1:nrow(dispersal), 1)
          rand_dispersal <- dispersal[row_dispersal, ]
          dx <- rand_dispersal[1]
          dy <- rand_dispersal[2]
          
          new_x <- lonely_kites[i, 1] + dx
          new_y <- lonely_kites[i, 2] + dy
          
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
      
      #replace old coords
      lonely_kites[, 1:2] <- new_coords[, c("x", "y")]
      
      # reset row indices
      rownames(new_coords) <- NULL
      rownames(lonely_kites) <- NULL
      
      # 3.1 check new coords ----
      if (n_lonely != nrow(new_coords)){
        stop(paste("3.1 new coords: nr not the same of coords!", n_lonely, n(new_coords)))
      }
      
      # check if new coords are nest coords
      matching_coords <- merge(new_coords, coords_nests_juv)
      # Find rows where (x, y) coordinates match between coords_nests and new_coords
      
      # Check if there are any matching coordinates
      if(nrow(matching_coords) > 0) {
        print("Matching:")
        print(new_coords[matching_coords, ])
        stop(paste(length(matching_coords), 
                   "3.1 new coords: matching coords between coords_nests_juv & new_coords"))
      } else {
        print("3.1 new coords: No matching coords between coords_nests_juv & new_coords")
      }
      
      # 3.2 mortality & new nests & movement   ----
      # 3.2.1 mortality 2, check if new coords are in turbine/buffer, if true - killed
      # 3.2.2 new nest if two kites meet
      # 3.2.3 movment of rest (set age, abundance at new coords)
      
      # 3.2.1 mortality 2, check if new coords are in turbine/buffer ----
      killed_indices <- which(apply(lonely_kites[c("x", "y")], 1, function(row) {
        any(row[1] == coords_turb_buffer[,1] & row[2] == coords_turb_buffer[,2])
      }))
      
      n_killed <- length(killed_indices)
      
      # if killed, set old coord to 0 (age, abundance), set killed to true
      if(n_killed > 0){
        print(paste("3.2.1 killed by turbine:", n_killed))
        
        for(idx in killed_indices){
          
          # set new coord (age <- 0, abundance <- -1)
          new_x <- lonely_kites[c("x", "y")][idx,1]
          new_y <- lonely_kites[c("x", "y")][idx,2]
          
          kites[new_x,new_y,t+1, "killed_move"] <- 
            kites[new_x,new_y,t+1, "killed_move"] + 1 # in case two kites have same new_coord
          
        }
        
        # write into killed 
        dead <- lonely_kites[killed_indices,]
        dead$killed <- rep("move", nrow(dead))
        df_killed <- rbind(df_killed, dead)
        
        # delete from lonely_kites and new_coords
        lonely_kites <- lonely_kites[-killed_indices,]
        new_coords <- new_coords[-killed_indices,]  
        
        # reset row indices
        rownames(lonely_kites) <- NULL
        rownames(new_coords) <- NULL
        
      } else {
        print("3.2.1 no kites killed in turbine")
      }
      
      
      # 3.2.2 new nest if two kites meet  ----
      # if 3 kites are at the same coordinate -> it stays at the old loc
      max_attempts <- 50
      attempt <- 0
      
      while(TRUE){
        attempt <- attempt + 1
        if (attempt >= max_attempts) {
          print("Max attempts reached. Remaining duplicates will be removed.\n")
          break
        }
        
        # Get duplicate coordinates and counts
        result <- get_dup_coords(lonely_kites[c("x", "y")])
        
        # no duplicates, exit the loop
        if (!is.null(result)) {
          dup_coords <- result$dup_coords
          coord_counts <- result$coord_counts
        } else {
          break
        }
        
        # coords n > 2
        del_row <- which(coord_counts[,3] > 2, arr.ind = TRUE)
        
        if (length(del_row) == 0) break
        
        print("3.2.2 new nest; Duplicate count is odd; removing one kite from the duplicate set.\n")
        
        # get where count > 2
        del_coords <- coord_counts[del_row,1:2]
        
        # get index of this coords
        del_idx_help <- which(dup_coords[, 1] == del_coords[,1] &
                                dup_coords[, 2] == del_coords[,2], arr.ind = TRUE)
        
        # pick random idx to let them stay at the old place
        rand_idx <- sample(del_idx_help, size = 1)
        
        # reset lonely_kites x,y with old coords 
        lonely_kites[rand_idx,] <- old_coords_lonely_kites[rand_idx,]
        
        # Recheck for duplicates
        result <- get_dup_coords(lonely_kites[c("x", "y")])
        
        # no duplicates, exit the loop
        if (!is.null(result)) {
          dup_coords <- result$dup_coords
          coord_counts <- result$coord_counts
        } else {
          break
        }
        
        dup_coords <- result$dup_coords
        coord_counts <- result$coord_counts
        
        # coords n > 2
        del_row <- which(coord_counts[,3] > 2, arr.ind = TRUE)
        
        if (length(del_row) == 0) break
        
      }
      
      # If max attempts were reached and there are still >2 kites at one location, set lonely_age to 0
      if (attempt >= max_attempts) {
        remaining_result <- get_dup_coords(lonely_kites[c("x", "y")])
        
        if (!is.null(remaining_result)) {
          coord_counts <- remaining_result$coord_counts
          dup_coords <- remaining_result$dup_coords
          del_row <- which(coord_counts[, 3] > 2, arr.ind = TRUE)
          
          if (length(del_row) > 0) {
            print("Kites still overcrowded. They will now 'die'.\n")
            # get where count > 2
            del_coords <- coord_counts[del_row,1:2]
            
            # write to df_killed
            dead <- merge(lonely_kites, del_coords, by =c("x", "y"))
            dead$killed <- rep("nest", nrow(dead))
            
            df_killed <- rbind(df_killed, dead)
            
            # delet from lonely_kites
            lonely_kites <- anti_join(lonely_kites, del_coords, by =c("x", "y"))
            
          }
        }
      }
      
      remaining_result <- get_dup_coords(lonely_kites[c("x", "y")])
      coord_counts <- remaining_result$coord_counts
      del_row <- which(coord_counts[, 3] > 2, arr.ind = TRUE)
      
      if(!is.null(remaining_result)){
        # new nest set up
        dup_coords <- remaining_result$dup_coords
        
        # get idx per pair 
        indices_by_pair <- aggregate(rownames(dup_coords) ~ x + y, data = dup_coords, FUN = function(i) list(i))
        colnames(indices_by_pair) <- c("x", "y", "idx")
        
        # take oldest age of bird for new nest age
        row2s <- vector(length = nrow(indices_by_pair))
        for(i in 1:nrow(indices_by_pair)){
          row1 <- as.numeric(indices_by_pair[i, "idx"][[1]][1])
          row2 <- as.numeric(indices_by_pair[i, "idx"][[1]][2])
          
          help_new_nest <- lonely_kites[c(row1, row2), ]
          
          # creat new nest coords + take oldest age for new nest age
          new_age <- max(help_new_nest$lonely_age)
          
          # creat new nest with new_age + new coords at row1
          lonely_kites[row1, "nest_age"] <- new_age
          lonely_kites[row1, "lonely_age"] <- 0
          
          row2s[i] <- row2
        }
        
        # delete row2 from lonely_kites
        lonely_kites <- lonely_kites[-row2s,]
        
        # rest index
        rownames(lonely_kites) <- NULL
        
      }
      
      # final write new lonely kites into df_kites
      df_kites <- rbind(df_kites, lonely_kites)
      
      # 3.2.2 check new nests ----
      n_new_nests <- nrow(lonely_kites[lonely_kites$nest_age > 0,])
      print(paste("3.2.2 after movement:", n_new_nests, "new nests"))
      
    } # inital checking if lonely kites exist / movement
    
    # 4 Reproduction of kites ----
    # Reproduce all nests with 0.79 (growth rate) (including newly formed ones)
    # Nests that already have a juvenile do not get a new one
    
    # Let's look for eligible nests
    # active nests (TRUE in the "nest" layer)
    # and have not yet reproduced (FALSE in the "juv" layer).
    eligible_nests <- df_kites[(df_kites$nest_age > 0) & (df_kites$juv_age == 0),]
    el_nest_idx <- rownames(eligible_nests)
    N_t <- nrow(eligible_nests)
    
    new_juv_idx <- vector(length = N_t)
    
    if (N_t > 0) {
      # growth rate and nest density (Ricker function)
      reproduction_probability <- exp(growth_rate * (1 - N_t / N_t)) 
      
      # carrying_capacity? 
      
      # loop each of the eligable nest
      for (i in 1:N_t) {
        
        # every nest has one juv or none (based on reproduction probability)
        if (runif(1) < reproduction_probability) {
          
          # save idx of nest where new juv is "born"
          new_juv_idx[i] <- el_nest_idx[i]
          
        }
      }
      
      # clean new_juv_idx
      clean_new_juv_idx <- new_juv_idx[new_juv_idx != FALSE]
      
      # set juvs in df_kites 
      df_kites[clean_new_juv_idx, "juv_age"] <- 1
      
    }
    
    ## final metric and Spatial distribution of kites ----
    # abundance
    abund_nest <- sum(df_kites$nest_age > 0)
    abund_juv <- sum(df_kites$juv_age > 0)
    abund_lonely <- sum(df_kites$lonely_age > 0)
    
    total_abund <- abund_nest*2 + abund_juv + abund_lonely
    
    abund_new_juv <- length(clean_new_juv_idx)
    new_nest <- n_new_nests
    
    nat_dead <- real_died_natural
    nest_dead <- sum(df_killed$killed == "nest")
    killed_move <- sum(df_killed$killed == "move")
    killed_build <- real_died_build
    
    kites_abund[t+1, ] <- c(t+1, total_abund, abund_nest, abund_juv, abund_lonely, abund_new_juv, new_nest,
                            nat_dead, nest_dead, killed_move, killed_build)
    
    # location kites_loc
    kites <- spat_palcement(df_kites, kites, timestep = t+1, layer = 1) # "loc"
    
    # location mortality
    #df_killed move alread happend
    
    df_killed_build <- df_killed[df_killed$killed == "build", ]
    kites <- spat_palcement(df_killed_build, kites, timestep = t+1, layer = 3) # "killed_build"
    
    # write into kites_metric (df_kites + mortality)
    kites_metric[[t+1]] <- df_kites
    kites_metric[[timesteps + t+1]] <- df_killed
    
    # Verify the total abundance
    
    if(sum(kites[,,t+1,"loc"] == 1) != abund_lonely || sum(kites[,,t+1,"loc"] == 2) != (abund_nest - abund_juv) ||
       sum(kites[,,t+1,"loc"] == 3) != abund_juv || sum(kites[,,t+1,"loc"]) != total_abund){
      print(paste("abund:", sum(kites[,,t+1,"loc"]), "| theo", total_abund))
      stop(paste("error| kites total abundance, after placement | t:", t))
    } else {
      print(paste("4. final abund:", sum(kites[,,t+1,"loc"]), "| theo", total_abund))
    }
  } # end of for loop timesteps
  
  return(list(kites_metric = kites_metric, kites = kites, kites_abund = kites_abund,
              region = region, building_buffer = building_buffer, turbine = turbine, buffer = buffer))
}

