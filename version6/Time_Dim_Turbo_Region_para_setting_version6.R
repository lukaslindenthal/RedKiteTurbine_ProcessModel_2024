# ----------------------------------------------------------------
# Wind Turbine Parameter settings
# ----------------------------------------------------------------
# This script sets up the parameters (timesetup, wind turbine placement and background region)
# and initial states for the model 

# ----------------------------------------------------------------
# Authors: Neele Haß & Lukas Lindenthal
# ----------------------------------------------------------------

ini_time_dim_turb_region_para_setting <- function(timesteps, resolution, x_dim, y_dim,
                                              n_hinder_1, 
                                              n_turb_1, turb_neu_max, turb_neu_perc) {
  # Seed for Reproducibility ----
  set.seed(42)
  
  # ----------------------------------------------------------------
  # Region and Obstacles ---------------------------------------------------------
  # ----------------------------------------------------------------
  random_x <- sample(1:x_dim, n_hinder_1, replace = TRUE)  # Random x-coordinates
  random_y <- sample(1:y_dim, n_hinder_1, replace = TRUE)  # Random y-coordinates
  
  # Initialize 3D array for the region
  region <- array(0, dim = c(x_dim, y_dim, timesteps))
  
  # Mark obstacles for all timesteps
  for (i in 1:n_hinder_1) {
    region[random_x[i], random_y[i], ] <- 1
  }
  
  # Initialize an additional layer for building buffers
  building_buffer <- array(FALSE, dim = c(x_dim, y_dim, timesteps))
  
  # Mark obstacles and their buffers for all timesteps
  for (i in 1:n_hinder_1) {
    region[random_x[i], random_y[i], ] <- 1
    # Define buffer around each building
    for (dx in -1:1) {
      for (dy in -1:1) {
        bx <- random_x[i] + dx
        by <- random_y[i] + dy
        if (bx >= 1 && bx <= x_dim && by >= 1 && by <= y_dim) {
          building_buffer[bx, by, ] <- TRUE
        }
      }
    }
  }
  
  # ----------------------------------------------------------------
  # Wind Turbine Parameters and Buffer ------------------------------------------------------
  # ----------------------------------------------------------------
  
  # Initialize Wind Turbines
  turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for turbines
  
  # Buffer zone
  buffer <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for buffer zones
  
  # Randomly place initial turbines
  random_x <- sample(1:x_dim, n_turb_1, replace = TRUE)  # Random x-coordinates
  random_y <- sample(1:y_dim, n_turb_1, replace = TRUE)  # Random y-coordinates
  
  if(n_turb_1 > 0){
    for (i in 1:n_turb_1) {
      turbine[random_x[i], random_y[i], ] <- TRUE  # Place turbines at these positions
      
      # Define buffer around each building
      for (dx in buffer_zone) {
        for (dy in buffer_zone) {
          if (dx == 0 && dy == 0) next
          
          bx <- random_x[i] + dx
          by <- random_y[i] + dy
          if (bx >= 1 && bx <= x_dim && by >= 1 && by <= y_dim) {
            buffer[bx, by, ] <- TRUE
          }
        }
      }
      
    }
  }
  
  return(list(region = region, building_buffer = building_buffer, turbine = turbine, buffer = buffer))
}





