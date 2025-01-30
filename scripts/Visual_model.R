# ----------------------------------------------------------------
# Turbine and Red Kite Model Simulation
# ----------------------------------------------------------------
# This script visualises the wind turbine placement and red kite dynamics 
# over a series of timesteps.
# ----------------------------------------------------------------
# init Author: Neele Haß
# other Authors: Lukas Lindenthal
# date: November 2024
# ----------------------------------------------------------------

source("./scripts/2_Turbo_kite_model_buffer.R") # loads results from model

# ----------------------------------------------------------------
# Visualization of Results -----------------------------------------------------
# ----------------------------------------------------------------
par(mfrow = c(1, 1))
rhg_cols <- c("orange", "orange3", "")
for (t in 1:timesteps) {
  # Combine data into a data frame for plotting
  df <- data.frame(
    x = rep(1:x_dim, each = y_dim),
    y = rep(1:y_dim, times = x_dim),
    
    # Region / landscape
    region = as.vector(region[, , t]),
    buffer = as.vector(buffer[, , t]), 
    building_buffer = as.vector(building_buffer[, , t]),
    
    # Subjects
    turbine = as.vector(turbine[, , t]),
    kite = as.vector(kites[, , t])
  )
  
  # categories
  df$category[df$buffer == TRUE] <- "Buffer"
  df$category[df$building_buffer == TRUE] <- "Building Buffer"
  df$category[df$region == TRUE] <- "Region / Building"
  df$category[df$turbine == TRUE] <- "Turbine"
  df$category[df$kite == TRUE] <- "Redkite"
  
  # Create the plot with buffer zone
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = category), show.legend = TRUE) +
    scale_fill_manual(values = c("Buffer" = "orange", 
                                  "Building Buffer" = "orange3", 
                                  "Region / Building" = "blue", 
                                  "Turbine" = "red", 
                                  "Redkite" = "green"),
                       name = "Legend") +
    labs(title = paste("Simulation at Timestep = ", t, "(T= ", n_turb[t], ", ", "K= ", n_kites[t], ")" ), x = "X", y = "Y") +
    theme_minimal()
  
  # Print the plot
  print(p)
  
  Sys.sleep(0.1)
}

# scatterplot
par(mfrow = c(1,1))

time <- 1:max(timesteps)

plot(time, n_kites, col = "green", pch=16,
     xlab = "timesteps", ylab = "number",
     main = "Redkite and Turbines")
points(time, n_turb, col = "red", pch=16)
legend("topleft",               
       legend = c("Redkites", "Turbines"),  
       col = c("green", "red"),   
       pch = 16,                  
       pt.cex = 1.5)   