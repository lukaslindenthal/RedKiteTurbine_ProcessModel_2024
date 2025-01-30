# Explore the impact of increased turbine density on red kite mortality rates

source("./scripts/2_Turbo_kite_model_buffer.R") # loads results from mode

n_killed <- vector(length = timesteps)  # Track kite mortality due to turbine building
coords_killed <- array(0, dim = c(x_dim, y_dim, timesteps))
  
for (t in 2:timesteps) {
  
  # get turbine and buffer coords for t
  turb_coords <- which(turbine[ , , t] , arr.ind = TRUE)
  
  buffer_coords <- which(buffer[ , , t] , arr.ind = TRUE)
  
  dead_zone <- rbind(turb_coords, buffer_coords)
  dead_zone <- as.data.frame(dead_zone)
  colnames(dead_zone) <- c("x", "y")
  # length(dead_zone)
  # length(turb_coords) + length(buffer_coords)
  
  # kite coords for t-1
  kite_coords <- which(kites[ , , t-1] > 0 , arr.ind = TRUE) # find all kites
  kite_coords <- as.data.frame(kite_coords)
  colnames(kite_coords) <- c("x", "y")
  
  # matching -> kites get killed because turbine is build in t+1
  matching_coords <- merge(kite_coords, dead_zone, by = c("x", "y"))

  if (nrow(matching_coords) == 0) next
  
  # save killed kites
  coords_killed[matching_coords$x, matching_coords$y, t] <- TRUE
  
  # set coords of killed kit to FALSE in t
  kites[matching_coords$x, matching_coords$y, t] <- FALSE
  
  # track number of killed kites
  n_killed[t] <- sum(kites[matching_coords$x, matching_coords$y, t-1])
  
}



