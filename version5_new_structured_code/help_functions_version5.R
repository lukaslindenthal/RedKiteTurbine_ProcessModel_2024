# help_function script
library(dplyr)

# selection of random coords 
select_coords <- function(random_coords, condition){
  selected_coords <- matrix(0, ncol = 2, nrow = 0) # store later valid coords
  
  # Randomly sample potential positions
  pot_coords <- random_coords[sample(1:nrow(random_coords), min(condition, nrow(random_coords))), , drop = FALSE]
  
  selected_coords_nest <- rbind(selected_coords, pot_coords)
}

# get dublicates for nest building
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

# spatial placement of kites
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

# plot population 
pop_func_plot <- function(abund, name, addmain, commi){
  
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
  
  png(paste("./plots/",name,".png"), width = 800, height = 600)
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
  
  legend("topright", legend = c("Red Kites", "new_juv", "total killed", "killed nat.","killed by turb","diff_new_killed", "Turbines"), 
         col = c("green" , "blue", "red", "pink", "magenta", "darkgrey","black"), pch = 16, pt.cex = 1,cex = 1)
  dev.off()
}


