# ----------------------------------------------------------------
# Turbine and Red Kite Model Visualization
# ----------------------------------------------------------------
# This script visualizes the wind turbine placement and red kite dynamics 
# over a series of 5 timesteps.

# ----------------------------------------------------------------
# Authors: Neele Haß & Lukas Lindenthal
# ----------------------------------------------------------------

# Loading required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Source the parameter and model scripts
source("./scripts/final_Redkite_eco_constrains_model/Time_Dim_Turbo_Region_para_setting_version3.R")   # Loads timesteps, dimensions, turbines, etc.
source("./scripts/final_Redkite_eco_constrains_model/Redkite_eco_para_setting_version3.R")              # Loads red kite parameters and initial setup

# ----------------------------------------------------------------
# Sourcing required parameter and model scripts
# ----------------------------------------------------------------
source("./scripts/final_Redkite_eco_constrains_model/Time_Dim_Turbo_Region_para_setting_version3.R")   # Loads timesteps, dimensions, turbines, etc.
source("./scripts/final_Redkite_eco_constrains_model/Redkite_eco_para_setting_version3.R")              # Loads red kite parameters and initial setup


# the following source runs the model
source("./scripts/final_Redkite_eco_constrains_model/model_version3.R")

# ----------------------------------------------------------------
# Data Preparation and Visualization for Each Timestep
# ----------------------------------------------------------------
for(t in 1:timesteps) {
  # Combining data into a df for plotting
  df <- data.frame(
    x = rep(1:x_dim, each = y_dim),
    y = rep(1:y_dim, times = x_dim),
    region = as.vector(region[, , t]),
    building_buffer = as.vector(building_buffer[, , t]),
    turb = as.vector(turbine[, , t]),
    buffer = as.vector(buffer[, , t]),
    nest = as.vector(kites[, , t, "nest"]),
    juv_kite = as.vector(kites[, , t, "juv"] > 0),
    lonely_kite = as.vector(kites[, , t, "abundance"] == 1),
    killed_move = as.vector(kites[, , t, "killed_move"] > 0),
    killed_build = as.vector(kites[, , t, "killed_build"] > 0)
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
  df$category[df$killed_move == TRUE] <- "Redkite killed flying in turb"
  df$category[df$killed_build == TRUE] <- "Redkite killed by turbine const."
  df$category[df$killed_build == TRUE] <- "Redkite killed by turbine construction"

  
  # Construct the title with dynamic data
  tit <- paste("Simulation at Timestep =", t,
               "\n T = ", sum(turbine[, , t]),
               ", \n K_nest = ", sum(kites[, , t, "nest"]),
               ", K_lonely = ", sum(kites[, , t, "age_lonely"] >= 3),
               ", \n K_abund = ", sum(kites[, , t, "abundance"]),
               ", K_juv=", sum(kites[, , t, "juv"]),
               ", \n K_killed_movement =", sum(kites[, , t, "killed_move"]),
               ", K_killed_construction =", sum(kites[, , t, "killed_build"]))
  
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
                                 "Redkite killed flying in turb" = "red",
                                 "Redkite killed by turbine const." = "darkred",
                                 "Background" = "grey95"),
                      name = "Legend") +
    labs(title = tit, x = "X", y = "Y") +
    theme_minimal()
  
  print(p)
  Sys.sleep(1)
}

# ----------------------------------------------------------------
# Create Scatterplots for Summary Statistics
# ----------------------------------------------------------------
# Create summary vectors for each timestep
time <- as.numeric(1:timesteps)
n_turb <- numeric(timesteps)
n_kites <- numeric(timesteps)
n_killed <- numeric(timesteps)

for(t in 1:timesteps) {
  n_turb[t] <- sum(turbine[,,t])
  n_kites[t] <- sum(kites[,,t, "abundance"])
  n_killed[t] <- sum(kites[,,t, "killed_move"]) + sum(kites[,,t, "killed_build"])
}

# Extend the y-axis range for clarity in plots
y_min_kites <- min(n_kites) - 10
y_max_kites <- max(n_kites) + 30

# Plot for Redkites
plot(time, n_kites, type = "b", col = "green", pch = 16,
     ylim = c(y_min_kites, y_max_kites),
     xlab = "Timestep (years)", ylab = "Count",
     main = "Redkites Over Time")
text(time, n_kites, labels = n_kites, pos = 3, col = "green")
legend("topleft", legend = "Red Kites", col = "green", pch = 16, pt.cex = 1)

# Plot for Turbines and Killed Redkites
y_min_turb <- min(n_killed) - 10
y_max_turb <- max(n_turb) + 30

plot(time, n_turb, type = "b", col = "black", pch = 16,
     ylim = c(y_min_turb, y_max_turb),
     xlab = "Timestep (years)", ylab = "Count",
     main = "Turbines and Killed Redkites Over Time")
lines(time, n_turb, type = "b", col = "black", pch = 16)
lines(time, n_killed, type = "b", col = "red", pch = 16)
text(time, n_killed, labels = n_killed, pos = 3, col = "red")
text(time, n_turb, labels = n_turb, pos = 3, col = "black")
legend("topleft", legend = c("Turbines", "Killed Redkites"), col = c("black", "red"), pch = 16, pt.cex = 1)

