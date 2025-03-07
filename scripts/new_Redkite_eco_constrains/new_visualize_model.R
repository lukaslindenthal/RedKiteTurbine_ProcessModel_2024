# ----------------------------------------------------------------
# Turbine and Red Kite Model Visualization
# ----------------------------------------------------------------
# This script visualises the wind turbine placement and red kite dynamics 
# over a series of timesteps.
# ----------------------------------------------------------------
# Authors Neele Haß & Lukas Lindenthal
# Start_date: November 2024
# ----------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)

# Source the parameter and model scripts
source("./scripts/new_Redkite_eco_constrains/new_Time_Dim_Turbo_Region_para_setting.R")   # Loads timesteps, dimensions, turbines, etc.
source("./scripts/new_Redkite_eco_constrains/new_Redkite_eco_para_setting.R")              # Loads red kite parameters and initial setup
source("./scripts/new_Redkite_eco_constrains/new_ver3_help_redkite_model_timestep_5.R")

for(t in 1:timesteps){
  # Combine data into a data frame for plotting
  
  
  df <- data.frame(
    
    x = rep(1:x_dim, each = y_dim),
    y = rep(1:y_dim, times = x_dim),
    
    # Region / landscape
    region = as.vector(region[, , t]),
    building_buffer = as.vector(building_buffer[, , t]),
    
    # turbine
    turb = as.vector(turbine[, , t]),
    buffer = as.vector(buffer[, , t]),
    
    # redkite
    nest = as.vector(kites[, , t, "nest"]),
    juv_kite = as.vector(kites[, , t, "juv"] > 0),
    lonely_kite = as.vector(kites[, , t, "abundance"] == 1),
    killed_move = as.vector(kites[, , t, "killed_move"] > 0),
    killed_build = as.vector(kites[, , t, "killed_build"] > 0)
    
  )
  
  # categories
  df$category[df$building_buffer == TRUE] <- "Building Buffer"
  
  df$category[df$region == TRUE] <- "Region / Building"
  
  df$category[df$buffer == TRUE] <- "T-Buffer"
  
  df$category[df$turb == TRUE] <- "Turbine"
  
  df$category[df$nest == TRUE] <- "Redkite nest (2 adults)"
  
  df$category[df$juv_kite == TRUE] <- "Redkite nest (2 adults + 1 juv)"
  
  df$category[df$lonely_kite == TRUE] <- "Redkite lonely adult"
  
  df$category[df$killed_move == TRUE] <- "Redkite killed flying in turb"
  
  df$category[df$killed_build == TRUE] <- "Redkite killed by turbine const."
  
  df$category[is.na(df$category)] <- "Background"
  
  # titel
  
  tit <- paste("Simulation at Timestep =", t,
               "\n T= ", sum(turbine[, , t]),
               ", \n K_nest= ", sum(kites[, , t, "nest"]),
               ", K_lonely= ", sum(kites[, , t, "age_lonely"] >=3),
               ", \n K_abund= ", sum(kites[, , t, "abundance"]),
               ", K_juv=", sum(kites[, , t, "juv"]),
               ", \n K_killed_movement=", sum(kites[, , t, "killed_move"]),
               ", K_killed_construction=", sum(kites[, , t, "killed_build"]))
  
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
    
    labs(title = tit,
         x = "X", y = "Y") +
    theme_minimal()
  
  
  print(p)
  
  Sys.sleep(1)
}

# Scdatterplot ----
par(mfrow = c(1,1))

time <- 1:max(timesteps)
for(t in 1:timesteps){
  # interieren über abundance & killed 
  # in vector schreiben
}
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



# --------------------------
# Option: Create an animated plot (optional) using gganimate
# --------------------------
# library(gganimate)
# p_anim <- ggplot(plot_data, aes(x = x, y = y)) +
#   geom_tile(aes(fill = category), show.legend = TRUE) +
#   scale_fill_manual(values = c(
#     "Turbine" = "black",
#     "T-Buffer" = "orange2",
#     "Building Buffer" = "orange",
#     "Region / Building" = "blue",
#     "Redkite Nest (2 adults)" = "green4",
#     "Redkite Nest (with juv)" = "green",
#     "Redkite Lonely Adult" = "yellow",
#     "Background" = "grey95"
#   ), name = "Legend") +
#   labs(title = 'Timestep: {frame_time}', x = "X", y = "Y") +
#   theme_minimal() +
#   transition_time(timestep) +
#   ease_aes('linear')
#
# animate(p_anim, nframes = timesteps, fps = 1)

# --------------------------
# Option: Plot simulation metrics over time (scatterplot)
# --------------------------
# # we have the tracking vectorsn_turb and n_kites dont we?
# time <- 1:timesteps
# 
# # they are somewhere like so I think:
# # n_turb <- sapply(1:timesteps, function(t) sum(turbine[,,t]))
# # n_kites <- sapply(1:timesteps, function(t) sum(kites[,,t,"abundance"] > 0))
# 
# plot(time, n_kites, col = "green", pch = 16,
#      xlab = "Timestep", ylab = "Number",
#      main = "Redkites and Turbines over Time")
# points(time, n_turb, col = "black", pch = 16)
# legend("topleft",               
#        legend = c("Redkites", "Turbines"),  
#        col = c("green", "black"),   
#        pch = 16,                  
#        pt.cex = 1.5)
