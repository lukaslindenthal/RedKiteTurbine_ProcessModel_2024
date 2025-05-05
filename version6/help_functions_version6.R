# ----------------------------------------------------
# help_function script
# ----------------------------------------------------

library(dplyr)
library(ggplot2)

# dispersal ----
func_dispersal <- function(dis_num) {
  dispersal <- expand.grid(dx = -dis_num:dis_num, dy = -dis_num:dis_num)
  # exclude center 
  ex <- which((dispersal[,1] == 0) & (dispersal[,2] == 0), arr.ind = TRUE)
  dispersal <- dispersal[-ex,]  # Exclude the center point
  return(dispersal)
}

# selection of random coords -----
select_coords <- function(random_coords, condition){
  selected_coords <- matrix(0, ncol = 2, nrow = 0) # store later valid coords
  
  # Randomly sample potential positions
  pot_coords <- random_coords[sample(1:nrow(random_coords), min(condition, nrow(random_coords))), , drop = FALSE]
  
  selected_coords_nest <- rbind(selected_coords, pot_coords)
}

# get dublicates for nest building ----
get_dup_coords <- function(coords){
  # get duplicates
  duplicate_idx <- which(duplicated(coords) | duplicated(coords, fromLast = TRUE))
  
  if (length(duplicate_idx) == 0) {
    return(NULL)  # No duplicates found
  }
  
  # get coords
  dup_coords <- coords[duplicate_idx, ]
  
  # counter:
  # Count occurrences of each unique (x, y) pair
  coord_counts <- dup_coords %>%
    count(x, y) %>%
    arrange(desc(n))  # Sort by count
  
  return(list(dup_coords = dup_coords, coord_counts = coord_counts, duplicate_idx = duplicate_idx))
}

# spatial placement of kites ----
spat_palcement <- function(df, ar, timestep, layer){
  
  lonely <- df[df[, "lonely_age"] > 0, ]
  emp_nest <- df[(df[, "nest_age"] > 0) & (df[, "juv_age"] == 0), ]
  nest_juv <- df[(df[, "nest_age"] > 0) & (df[, "juv_age"] > 0), ]
  
  data <- list(lonely = lonely, emp_nest = emp_nest, nest_juv = nest_juv)
  
  for(i in 1:3){

    df <- data[[i]]
    rownames(df) <- NULL
    
    if(nrow(df) != 0){
      for(n in 1:nrow(df)){
        x <- df[n, "x"]
        y <- df[n, "y"]
        
        if (x > dim(ar)[1] || y > dim(ar)[2] || 
            timestep > dim(ar)[3] || layer > dim(ar)[4]) {
          prin()
          stop("Index out of bounds")
        }
        
        if(i == 1){ # lonely
          ar[x, y, timestep, layer] <- 1
        } else if(i == 2){ # emp nest
          ar[x, y, timestep, layer] <- 2
        } else { # nest + juv
          ar[x, y, timestep, layer] <- 3
        }
      }
    }
  
  }
  
  return(ar)
}

# plot population ----
pop_func_plot <- function(folder_name, 
                          abund, turbine, name, addmain, commi){
  folder_path <- paste("plots/", folder_name, sep = "")
  
  if (!dir.exists(folder_path)) { # check if folder isnt exiting
    dir.create(folder_path, recursive = TRUE)} else {
      print(paste("!",folder_path, "already exists"))
    }
  
  abund[is.na(abund)] <- 0
  
  # Create summary vectors for each timestep
  time <- as.numeric(1:timesteps)
  total_abund <- abund$total_abund
  abund_new_juv <- abund$abund_new_juv
  killed <- (abund$nat_dead + abund$nest_dead + 
               abund$killed_move + abund$killed_build)
  turb_killed <- (abund$killed_move + abund$killed_build)
  nat_killed <- (abund$nat_dead + abund$nest_dead)
  
  diff_new_killed <- abund_new_juv - killed
  
  n_turb <- numeric(timesteps)
  for(t in 1:timesteps) {
    n_turb[t] <- sum(turbine[,,t])
  }
  
  png(paste("./plots/", folder_name, "/", name,".png", sep =""), width = 800, height = 600)
  plot(time, total_abund, type = "b", col = "green", pch = 16,
       xlab = "Timestep (years)", ylab = "Count",
       main = paste("Redkites Over Time\n", addmain),
       ylim = c(min(diff_new_killed)-10,max(total_abund)+10),
       xlim = c(1,timesteps))
  abline(time,0, col = "grey")
  lines(time, abund_new_juv, type = "l", col = "blue")
  lines(time, killed, type = "l", col = "red")
  lines(time, nat_killed, type = "l", col = "pink")
  lines(time, turb_killed, type = "l", col = "magenta")
  lines(time, diff_new_killed, type = "l", col = "darkgrey")
  lines(time, n_turb, type = "b", col = "black", pch = 16)
 
  text(time, total_abund, labels = total_abund, pos = 3, col = "green",cex = 1)
  text(time, diff_new_killed, labels = diff_new_killed, pos = 1, col = "darkgrey",cex = 1)
  text(time, n_turb, labels = n_turb, pos = 3, col = "black",cex = 1)
  
  text(max(time)-5, (max(total_abund)+10)/2, labels = commi, 
       pos = 1, col = "black",cex = 1)
  
  legend("topleft", legend = c("Red Kites", "new_juv", "total killed", "killed nat.","killed by turb","diff_new_killed", "Turbines"), 
         col = c("green" , "blue", "red", "pink", "magenta", "darkgrey","black"), pch = 16, pt.cex = 1,cex = 1)
  dev.off()
}

# comparison Plot ----
comp_plot <- function(folder_name, 
                      abund, wo_dis, turbine, name, addmain, commi){
  
  folder_path <- paste("plots/", folder_name, sep = "")
  
  if (!dir.exists(folder_path)) { # check if folder isnt exiting
    dir.create(folder_path, recursive = TRUE)} else {
      print(paste("!",folder_path, "already exists"))
    }
  abund[is.na(abund)] <- 0
  
  # Create summary vectors for each timestep
  time <- as.numeric(1:timesteps)
  total_abund <- abund$total_abund
  abund_new_juv <- abund$abund_new_juv
  killed <- (abund$nat_dead + abund$nest_dead + 
               abund$killed_move + abund$killed_build)
  turb_killed <- (abund$killed_move + abund$killed_build)
  nat_killed <- (abund$nat_dead + abund$nest_dead)
  
  diff_new_killed <- abund_new_juv - killed
  
  n_turb <- numeric(timesteps)
  for(t in 1:timesteps) {
    n_turb[t] <- sum(turbine[,,t])
  }
  
  png(paste("./plots/", folder_name, "/",name,".png" , sep =""), width = 800, height = 600)
  plot(time, wo_dis, type = "b", col = "darkgreen", pch = 16,
       xlab = "Timestep (years)", ylab = "Count",
       main = paste("Redkites Over Time\n", addmain),
       ylim = c(min(diff_new_killed)-100, max(c(wo_dis, total_abund)))+100,
       xlim = c(1,timesteps))
  abline(time,0, col = "grey")
  lines(time, abund_new_juv, type = "l", col = "blue")
  lines(time, killed, type = "l", col = "red")
  lines(time, nat_killed, type = "l", col = "pink")
  lines(time, turb_killed, type = "l", col = "magenta")
  lines(time, diff_new_killed, type = "l", col = "darkgrey")
  lines(time, n_turb, type = "b", col = "black", pch = 16)
  lines(time, total_abund, type = "b", col = "green", pch = 16)
  
  text(time, total_abund, labels = total_abund, pos = 1, col = "green",cex = 1)
  text(time, diff_new_killed, labels = diff_new_killed, pos = 1, col = "darkgrey",cex = 1)
  text(time, n_turb, labels = n_turb, pos = 3, col = "black",cex = 1)
  text(time, wo_dis, labels = wo_dis, pos = 3, col = "darkgreen",cex = 1)
  
  text(max(time)-5, (max(c(wo_dis, total_abund))/2)+200, labels = commi, 
       pos = 1, col = "black",cex = 1)
  
  legend("topleft", legend = c("Red Kites", "Red Kites without dis.", "new_juv", "total killed", "killed nat.","killed by turb","diff_new_killed", "Turbines"), 
         col = c("green" ,"darkgreen", "blue", "red", "pink", "magenta", "darkgrey","black"), pch = 16, pt.cex = 1,cex = 1)
  dev.off()
}

# spatial visualisation ----
spat_visual <- function(folder_name, time, x_dim, y_dim, 
                        results_region, results_buffer, results_building_buffer, results_turbine,
                        results_kites) {
  folder_path <- paste("plots/", folder_name, sep = "")
  
  if (!dir.exists(folder_path)) { # check if folder isnt exiting
    dir.create(folder_path, recursive = TRUE)} else {
      print(paste("!",folder_path, "already exists"))
    }
  
  for(t in time) { 
    # Combining data into a df for plotting
    df <- data.frame(
      x = rep(1:x_dim, each = y_dim),
      y = rep(1:y_dim, times = x_dim),
      
      # Region / landscape
      region = as.vector(results_region[, , t]),
      buffer = as.vector(results_buffer[, , t]), 
      building_buffer = as.vector(results_building_buffer[, , t]),
      
      # Subjects
      turb = as.vector(results_turbine[, , t]),
      # kite = as.vector(results_kites[, , t, "loc"]),
      nest = as.vector(results_kites[, , t, "loc"] == 2),
      juv_kite = as.vector(results_kites[, , t, "loc"] > 2),
      lonely_kite = as.vector(results_kites[, , t, "loc"] == 1),
      
      # killed kites
      killed_move = as.vector(results_kites[, , t, "killed_move"] > 0),
      killed_build = as.vector(results_kites[, , t, "killed_build"] > 0)
      
    )
    
    # Assign categories for different states and objects in the grid
    df$category <- "Background"
    df$category[df$building_buffer == TRUE] <- "Building Buffer"
    df$category[df$region == TRUE] <- "Region / Building"
    df$category[df$buffer == TRUE] <- "T-Buffer"
    df$category[df$turb == TRUE] <- "Turbine"
    
    df$category[df$nest == TRUE] <- "Redkite nest (2 adults)"
    df$category[df$juv_kite == TRUE] <- "Redkite nest (2 adults + 1 juv)"
    df$category[df$lonely_kite == TRUE] <- "Redkite lonely adult"
    df$category[df$killed_move == TRUE] <- "Redkite killed flying in Turb."
    df$category[df$killed_build == TRUE] <- "Redkite killed by Turb. const."
    
    # # Construct the title with dynamic data
    tit <- paste("Simulation at Timestep =", t,
                 "\n T = ", sum(results_turbine[, , t]),
                 ", \n total K_abund = ", sum(results_kites[, , t, "loc"]),
                 
                 "\n\n K_nest = ", sum(results_kites[, , t, "loc"] >= 2),
                 ", K_lonely = ", sum(results_kites[, , t, "loc"] == 1),
                 ", K_juv=", sum(results_kites[, , t, "loc"] > 2),
                 ", \n K_killed_movement =", sum(results_kites[, , t, "killed_move"] > 0),
                 ", K_killed_construction =", sum(results_kites[, , t, "killed_build"] > 0))
    
    # Create the plot
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_tile(aes(fill = category), show.legend = TRUE) +
      scale_fill_manual(values = c("Turbine" = "black",
                                   "T-Buffer" = "orange2",
                                   "Building Buffer" = "orange",
                                   "Region / Building" = "blue",
                                   "Redkite nest (2 adults)" = "green4",
                                   "Redkite nest (2 adults + 1 juv)" = "green",
                                   "Redkite lonely adult" = "yellow",
                                   "Redkite killed flying in Turb." = "red",
                                   "Redkite killed by Turb. const." = "darkred",
                                   "Background" = "grey95"),
                        name = "Legend") +
      labs(title = tit, x = "X", y = "Y") +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
    
    print(p)
    
    # Save the plot
    name <- paste("./plots/", folder_name, "/timestep_", t, ".png" , sep ="")
    ggsave(name, plot = p)
  }
}

