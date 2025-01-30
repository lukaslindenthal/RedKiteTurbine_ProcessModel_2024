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
source("./scripts/kite_mortality.R")            # load tracking of killed kites by turbine building

# map ----
par(mfrow = c(1, 1))
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
    kite = as.vector(kites[, , t]),
    
    # killed kites
    killed = as.vector(coords_killed[, , t])
  )
  
  # categories
  df$category[df$buffer == TRUE] <- "Buffer"
  df$category[df$building_buffer == TRUE] <- "Building Buffer"
  df$category[df$region == TRUE] <- "Region / Building"
  df$category[df$turbine == TRUE] <- "Turbine"
  df$category[df$kite == TRUE] <- "Redkite"
  df$category[df$killed == TRUE] <- "killed Redkite"
  
  if (t != 1){
    tit <- paste("Simulation at Timestep = ", t, 
                 "\n T= ", n_turb[t], ", ", "K= ", n_kites[t],
                 "\n killed K = ", n_killed[t])
  } else {
    tit <- paste("Inital Set-up ", t, 
                 "\n T= ", n_turb[t], ", ", "K= ", n_kites[t],
                 "\n killed K = ", n_killed[t])
  }
  # Create the plot with buffer zone
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = category), show.legend = TRUE) +
    scale_fill_manual(values = c("Buffer" = "orange", 
                                  "Building Buffer" = "orange3", 
                                  "Region / Building" = "blue", 
                                  "Turbine" = "black", 
                                  "Redkite" = "green",
                                  "killed Redkite" = "red3"),
                       name = "Legend") +
    labs(title = tit,
         x = "X", y = "Y") +
    theme_minimal()
  
  # Print the plot
  print(p)
  
  Sys.sleep(0.05)
}

# scatterplot ----
par(mfrow = c(1,1))

time <- 1:max(timesteps)
killed <- which(n_killed > 0, arr.ind = TRUE)

plot(time, n_kites, col = "green", pch=16,
     xlab = "timesteps", ylab = "number",
     main = "Redkite and Turbines")
points(time, n_turb, col = "black", pch=16)
text(killed, n_killed[killed], , labels = n_killed[killed], col = "red", cex = 1, font = 1)

legend("topleft",               
       legend = c("Redkites", "Turbines", "Redkites \nkilled by Turbine"),  
       col = c("green", "black", "red"),   
       pch = 16,                  
       pt.cex = 1.5)   
