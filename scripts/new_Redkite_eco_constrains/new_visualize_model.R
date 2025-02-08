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
source("./scripts/new_Redkite_eco_constrains/ver3_copie_ver2_help_redkite_model_debugging_nest.R")

# list to store data frames for each timestep
plot_data_list <- list()

for (t in 1:timesteps) {
  
  # data frame for the current timestep
  df <- data.frame(
    x = rep(1:x_dim, each = y_dim),
    y = rep(1:y_dim, times = x_dim),
    
    # Landscape features:
    region = as.vector(region[,,t]),
    building_buffer = as.vector(building_buffer[,,t]),
    
    # Turbine and buffer:
    turb = as.vector(turbine[,,t]),
    buffer = as.vector(buffer[,,t]),
    
    # Red kite features (using "nest" as an example for breeding pairs)
    nest = as.vector(kites[,,t, "nest"]),
    # to differentiate between juvs and adults
    juv_kite = as.vector(kites[,,t, "juv"] > 0),
    lonely_kite = as.vector(kites[,,t, "abundance"] == 1),
    killed_move = as.vector(kites[, , t, "killed_move"] > 0)
  )
  
  # Create categorical variable for plotting.
  # ATTENTION: order of assignment matters when have overlapping attributes
  # this was the case for the turbines and their buffers
  df <- df %>% mutate(
    category = case_when(
      building_buffer == TRUE ~ "Building Buffer",
      region == TRUE ~ "Region / Building",
      buffer == TRUE ~ "T-Buffer",
      turb == TRUE ~ "Turbine",
      nest == TRUE ~ "Redkite Nest (2 adults)",
      juv_kite == TRUE ~ "Redkite Nest (with juv)",
      lonely_kite == TRUE ~ "Redkite Lonely Adult",
      killed_move == TRUE ~ "Redkite killed by turbine (movement)",
      TRUE ~ "Background"
    ),
    timestep = t  # add timestep for later use (mayber usefull if we want to use the animation)
  )
  
  plot_data_list[[t]] <- df
}

# Combine list into one data frame
plot_data <- bind_rows(plot_data_list)

# --------------------------
# Create the simulation plot for each timestep
# --------------------------

# here only exemplarary a static plot for a given timestep:
current_timestep <- 5 # her timestep 1 
t <- current_timestep
tit <- paste("Simulation at Timestep =", t,
             "\n T= ", sum(turbine[, , t]),
             ", \n K_nest= ", sum(kites[, , t, "nest"]),
             ", K_lonely= ", sum(kites[, , t, "age_lonely"] >=3),
             ", \n K_abund= ", sum(kites[, , t, "abundance"]),
             ", K_juv=", sum(kites[, , t, "juv"]),
             ", K_killed_movement=", sum(kites[, , t, "killed_move"]))

p_static <- ggplot(filter(plot_data, timestep == current_timestep), aes(x = x, y = y)) +
  geom_tile(aes(fill = category), show.legend = TRUE) +
  scale_fill_manual(values = c(
    "Turbine" = "black",
    "T-Buffer" = "orange2",
    "Building Buffer" = "orange",
    "Region / Building" = "blue",
    "Redkite Nest (2 adults)" = "green4",
    "Redkite Nest (with juv)" = "green",
    "Redkite Lonely Adult" = "yellow",
    "Redkite killed by turbine (movement)" = "red3",
    "Background" = "grey95"
  ), name = "Legend") +
  labs(title = tit,
       x = "X", y = "Y") +
  theme_minimal()

print(p_static)

# ---------------------------
# attampe form ploting
timestep <- 5 # timestep
for(t in 1:timestep){
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
    killed_move = as.vector(kites[, , t, "killed_move"] > 0)
    
  )
  
  # categories
  df$category[df$building_buffer == TRUE] <- "Building Buffer"
  
  df$category[df$region == TRUE] <- "Region / Building"
  
  df$category[df$turb == TRUE] <- "Turbine"
  
  df$category[df$buffer == TRUE] <- "T-Buffer"
  
  df$category[df$nest == TRUE] <- "Redkite nest (2 adults)"
  
  df$category[df$juv_kite == TRUE] <- "Redkite nest (2 adults + 1 juv)"
  
  df$category[df$lonely_kite == TRUE] <- "Redkite lonely adult"
  
  df$category[df$killed_move == TRUE] <- "Redkite killed flying in turb"
  
  df$category[is.na(df$category)] <- "Background"
  
  # titel
  
  tit <- paste("Simulation at Timestep =", t,
               "\n T= ", sum(turbine[, , t]),
               ", \n K_nest= ", sum(kites[, , t, "nest"]),
               ", K_lonely= ", sum(kites[, , t, "age_lonely"] >=3),
               ", \n K_abund= ", sum(kites[, , t, "abundance"]),
               ", K_juv=", sum(kites[, , t, "juv"]),
               ", K_killed_movement=", sum(kites[, , t, "killed_move"]))
  
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
                                 "Background" = "grey95"),
                      
                      name = "Legend") +
    
    labs(title = tit,
         x = "X", y = "Y") +
    theme_minimal()
  
  
  print(p)
}




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
