# ----------------------------------------------------------------
# Wind Turbine and Red Kite Simulation
# ----------------------------------------------------------------
# This script sets up the parameters and initial states for a 
# simulation of:
#
# timesetup, wind turbine placement and backgroundregion
# ----------------------------------------------------------------
# init Author: Neele Haß
# other Authors: Lukas Lindenthal
# date: November 2024
# ----------------------------------------------------------------

# Dependencies ----
library(ggplot2)

# Seed for Reproducibility ----
set.seed(42)

# ----------------------------------------------------------------
# Simulation Dimensions and Time Steps -----------------------------------------
# ----------------------------------------------------------------
timesteps <- 10       # Number of timesteps in the simulation
x_dim <- 100           # Grid width (x-axis)
y_dim <- 100           # Grid height (y-axis)
 
# ----------------------------------------------------------------
# Region and Obstacles ---------------------------------------------------------
# ----------------------------------------------------------------
# Initialize 3D array for the region
region <- array(0, dim = c(x_dim, y_dim, timesteps))

# Add Obstacles to the Region
n_hinder_1 <- 100                          # Number of obstacles (e.g. buildings)
random_x <- sample(1:x_dim, n_hinder_1, replace = TRUE)  # Random x-coordinates
random_y <- sample(1:y_dim, n_hinder_1, replace = TRUE)  # Random y-coordinates

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
# Wind Turbine Parameters ------------------------------------------------------
# ----------------------------------------------------------------
turb_neu_max <- 10     # Max turbines added per timestep
turb_neu_perc <- 0.5   # Percentage of existing turbines for new construction

# Initialize Wind Turbines
turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # 3D array for turbines

# Randomly place initial turbines
n_turb_1 <- 10                                   # Number of initial turbines
random_x <- sample(1:x_dim, n_turb_1, replace = TRUE)  # Random x-coordinates
random_y <- sample(1:y_dim, n_turb_1, replace = TRUE)  # Random y-coordinates

for (i in 1:n_turb_1) {
  turbine[random_x[i], random_y[i], ] <- TRUE  # Place turbines at these positions
}


