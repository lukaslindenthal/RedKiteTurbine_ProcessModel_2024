# ----------------------------------------------------------------
# Wind Turbine and Red Kite Simulation
# ----------------------------------------------------------------
# This script sets up the parameters and initial states for a 
# simulation of wind turbine placement and also the parameters for 
# red kite dynamics.
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
timesteps <- 10      # Number of timesteps in the simulation
x_dim <- 100         # Grid width (x-axis)
y_dim <- 100         # Grid height (y-axis)

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

# ----------------------------------------------------------------
# Red Kite Parameters-----------------------------------------------------------
# ----------------------------------------------------------------
# Initialize Red Kite Dynamics
kites <- array(0, dim = c(x_dim, y_dim, timesteps))  # 3D array for red kites
growth_rate <- 0.5  # Growth rate (r in Ricker equation)
carrying_capacity <- 20  # Carrying capacity (K)
initial_kites <- 2  # Initial red kite count

# Randomly place initial red kites
random_x <- sample(1:x_dim, initial_kites, replace = TRUE)  # Random x-coordinates
random_y <- sample(1:y_dim, initial_kites, replace = TRUE)  # Random y-coordinates

for (i in 1:initial_kites) {
  kites[random_x[i], random_y[i], 1] <- 1
}

# ----------------------------------------------------------------
# Initial Plot: Region and Turbines (Timestep = 1) -----------------------------
# ----------------------------------------------------------------
# Extract layers for timestep 1
region_layer <- region[, , 1]
turbine_layer <- turbine[, , 1]

# Combine into a data frame for plotting
df <- data.frame(
  x = rep(1:x_dim, each = y_dim),
  y = rep(1:y_dim, times = x_dim),
  region = as.vector(region_layer),
  turbine = as.vector(turbine_layer)
)

# Plot Region and Turbines
p <- ggplot(df, aes(x = x, y = y)) +
  geom_tile(aes(fill = region), show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "blue", name = "Region") +
  geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
  labs(title = paste("Initial State: Timestep = ", 1), x = "X", y = "Y") +
  theme_minimal()

# Print the plot
print(p)




# old structure  ----------------------------------------------------------

# # ---
# # Turbinen-Parameter
# 
# # region + n hindernisse
# # n turbinen
# 
# # ---
# set.seed(42)
# 
# library(ggplot2)
# 
# # Dimension --
# timesteps <- 10
# x_dim <- 100
# y_dim <- 100
# 
# # Region + hindernisse --
# region <- array(0, dim = c(x_dim, y_dim, timesteps))
# dim(region)
# 
# # Hindernisse in Region
# n_hinder_1 <- 20
# random_x <- sample(1:x_dim, n_hinder_1, replace = 1)  # Random x-coordinates
# random_y <- sample(1:y_dim, n_hinder_1, replace = 1)  # Random y-coordinates
# 
# # For each random coordinate, set TRUE across all timesteps
# for (i in 1:n_hinder_1) {
#   region[random_x[i], random_y[i], ] <- 1
# }
# 
# ## Turbinen-Parameter --
# turb_neu_max <- 10   # Max Turbinen, die pro Zeitschritt gebaut werden können
# turb_neu_perc <- 0.5  # Neubau-Prozentsatz abhängig von existierenden Turbinen
# 
# # Initiale State Turbinen --
# turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # Kopiere die Region
# 
# n_turb_1 <- 10
# random_x <- sample(1:x_dim, n_turb_1, replace = TRUE)  # Random x-coordinates
# random_y <- sample(1:y_dim, n_turb_1, replace = TRUE)  # Random y-coordinates
# 
# # For each random coordinate, set TRUE across all timesteps
# for (i in 1:n_turb_1) {
#   turbine[random_x[i], random_y[i], ] <- TRUE 
#   }
# 
# ## Red Kite Parameters
# kites <- array(0, dim = c(x_dim, y_dim, timesteps))  # Red kite presence
# growth_rate <- 0.5  # Growth rate (r in Ricker equation)
# carrying_capacity <- 20  # Carrying capacity (K)
# initial_kites <- 2  # Initial red kite count
# 
# # Random placement of initial kites
# random_x <- sample(1:x_dim, initial_kites, replace = TRUE)
# random_y <- sample(1:y_dim, initial_kites, replace = TRUE)
# for (i in 1:initial_kites) {
#   kites[random_x[i], random_y[i], 1] <- 1
# }
# 
# 
# 
# # plot region + tubr t=1 --
# region_layer <- region[,,1]
# turbine_layer <- turbine[,,1]
# # Combine into a data frame
# x_dim <- dim(region_layer)[1]
# y_dim <- dim(region_layer)[2]
# 
# # Create a data frame with coordinates and values
# df <- data.frame(
#   x = rep(1:x_dim, each = y_dim),
#   y = rep(1:y_dim, times = x_dim),
#   region = as.vector(region_layer),
#   turbine = as.vector(turbine_layer)
# )
# 
# # plot
# p <- ggplot(df, aes(x = x, y = y)) +
#   geom_tile(aes(fill = region), show.legend = FALSE) +
#   scale_fill_gradient(low = "white", high = "blue", name = "Region") +
#   geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
#   #geom_point(data = subset(df, region == TRUE), aes(x = x, y = y), color = "blue", size = 2) +
#   labs(title = paste("Timestep = ", 1), x = "X", y = "Y") +
#   theme_minimal()
# print(p)

