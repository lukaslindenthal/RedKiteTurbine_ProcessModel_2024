# ----------------------------------------------------------------
# Wind Turbine and Red Kite Simulation
# ----------------------------------------------------------------
# This script sets up the parameters and initial states for a 
# simulation of:
#
# Redkite (see ./scripts/new_Redkite_eco_constrains/notes)
# ----------------------------------------------------------------
# init Author: Neele Haß
# other Authors: Lukas Lindenthal
# date: November 2024
# ----------------------------------------------------------------

# #################################
# # Parameter setting ----
# inital_kites <- 3*2	 	# [3 pairs / 100km2]
# carrying_capacity <- 19*2		# [18.5 pairs / 100km2]
# 
# nest_dist <- c(4000, 1500) 	# median, min [m]
# 
# if kites[x,y,t] 
# == (region | buffer = Ture) 
# --> False 			# Brutplatz nicht in Siedlung oder Nähe
# 
# prod_n <- 1.7 = 2			# Fortpflanzungsziffern (Junge je begonnene Brut)
# prod_rate <- 0.79		# Bruterfolgsraten (Anteil von Nestern mit mind. einem flüggen Jungvogel an der Gesamtzahl der Nester mit Eiablage)
# 
# action_area <- 9.5 		# Aktionsraum [9,4 km²]
# 
# surv_r_1 <- 0.55		# Überlebensrate bis 1Jahr
# surv_r_a <- 0.84		# Überlebensrate > 1Jahr = adult
#
# if distance(kite, trubine) < actions_area 
# r_d_real <- exp((-0.2)-dis)	# risk distance relationship: tod an Trubine, wenn Turbine innerhalb Aktionsraumes
#
# liv_expec <- 12 # fortpflanzfähige Lebenserwartung ca 12 dh nest besteht 12 Jahre
#
# rep_area <- 21km # Radius der Dispersal nach geschlechstreife
#
# ! Annahmegeschlechtsreif age  3
# ! Inital annahme 3 pair /100km + random nr an adult 10%
# ! 2 Junge pro nest
# ! ein mal Brutzeit pro Jahr, also pro timestep
# ! 
# #################################

# Dependencies ----
library(ggplot2)
source("./scripts/new_Redkite_eco_constrains/new_Time_Dim_Turbo_Region_para_setting.R") # load timesteps, dim and Turbine
library(abind)

# Seed for Reproducibility ----
set.seed(42)

# ----------------------------------------------------------------
# Red Kite Parameters-----------------------------------------------------------
# ----------------------------------------------------------------
# Initialize Red Kite Dynamics
kites_nest <- array(0, dim = c(x_dim, y_dim, timesteps)) # placment of nest
kites_juv <- array(0, dim = c(x_dim, y_dim, timesteps)) # new born kites
kites_abund <- array(0, dim = c(x_dim, y_dim, timesteps))  # 3D array for abundance red kites
kites_age <- array(0, dim = c(x_dim, y_dim, timesteps))  # 3D array for age red kites

kites <- abind(kites_nest, kites_juv, kites_abund, kites_age, along = 4)
# Add dimension names for clarity
dimnames(kites) <- list(
  x_dim = NULL,
  y_dim = NULL,
  timesteps = NULL,
  type = c("nest", "juv", "abundance", "age")
)
# print(dim(kites))
# dimnames(kites)$type
# kites[,,,"abundance"]

# Abundance
area <- x_dim*y_dim
initial_adults <- (3*2) * (area/100)		# [3 pairs / 100km2] + random number
initial_lonely <-  initial_adults*0.1
carrying_capacity <- (19*2)* (area/100)		    # [18.5 pairs / 100km2] # Carrying capacity (K)

# growth and survival rate
prod_n <- 2 			# Fortpflanzungsziffern (Junge je begonnene Brut)
prod_rate <- 0.79		# Bruterfolgsraten (Anteil von Nestern mit mind. einem flüggen Jungvogel an der Gesamtzahl der Nester mit Eiablage)
growth_rate <- prod_rate  # Growth rate (r in Ricker equation)

initial_new_born <- (initial_adults/2) * growth_rate  # inital new born, dependent on nr of nests 

liv_exp <- 12    # fortpflanzfähige Lebenserwartung ca 12 dh nest besteht 12 Jahre

# change vor survival 
surv_r_1 <- 0.55		# Überlebensrate bis 1Jahr
surv_r_a <- 0.84		# Überlebensrate > 1Jahr = adult

# action area
action_area <- 9.5 		# Aktionsraum [9,4 km²]

# turbine risk-distance realationship
# r_d_real <- exp((-0.2)-dis)	# risk distance relationship: tod an Trubine, wenn Turbine innerhalb Aktionsraumes

##############
# Inital Placement of kites ----
##############
# Randomly placment of kite nest (pair) ----
# check if inital placement = even
is_even <- (initial_adults %% 2) == 0 # check ob es durch 2 teilbar ist
if (is_even == TRUE){
  n_nest <- initial_adults/2
  lonely_kite <- initial_lonely
} else {
  n_nest <- (initial_adults/2) - 1
  lonely_kite <- initial_lonely + 1
}

# placements of nests
# Determine valid cells for kite nest placement, considering turbines + buildings
random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                        & !building_buffer[, , 1], arr.ind = TRUE)
random_coords <- matrix(random_coords, ncol = 2)

# Only proceed if valid cells exist
if (nrow(random_coords) > 0) {
  # Sample new positions for red kites
  selected_coords <- random_coords[sample(1:nrow(random_coords), min(n_nest, nrow(random_coords))), ]
  selected_coords <- matrix(selected_coords, ncol = 2)
  
  # Place red kites in the selected coordinates
  if (nrow(selected_coords) > 0) {
    for (i in 1:nrow(selected_coords)) {
      kites[selected_coords[i, 1], selected_coords[i, 2], 1, "nest"] <- TRUE
    }
  }
}

# change abundance for nests & new born ----
# and add 1-2 new born to random chosen nests
# and change age - in nests age > 3, new_born set to 1
coords_nest <- which(kites[,,1,"nest"] == 1, arr.ind = TRUE)
for (i in 1:nrow(coords_nest)){
  kites[coords_nest[i,1], coords_nest[i,2], 1, "abundance"] <- 2
}

num_nests <- nrow(coords_nest) # Anzahl nests
new_per_nest <- sample(0:2, num_nests, replace = TRUE) # random verteilung

# setzte new_born to nest - summe der inital new born bleibt erfüllt
while (sum(new_per_nest) != initial_new_born) {
  new_per_nest <- sample(0:2, num_nests, replace = TRUE)
}
# Loop through the nests and assign the new borns
for (i in 1:num_nests) {
  
  # add adult abundance per nest
  kites[coords_nest[i,1], coords_nest[i,2], 1, "abundance"] <- 2 + new_per_nest[i] 
  kites[coords_abund[i,1], coords_abund[i,2], 1, "age"] <- sample(3:liv_exp, 1) # all kites are > 1 year
  
  # Add the corresponding number of new borns to each nest
  kites[coords_nest[i,1], coords_nest[i,2], 1, "juv"] <-  new_per_nest[i]
  
}

coords_new <- which(kites[,,1,"juv"] > 0 , arr.ind = TRUE)
for (i in 1:nrow(coords_new)){
  kites[coords_new[i,1], coords_new[i,2], 1, "age"] <- 1 # new born kites
}

# # Verify the total number of new borns
# check_new <- sum(new_per_nest) == initial_new_born
# check_new

# placment of lonely kit, if exist ----
# age between 3 and liv_exp, adult
if (lonely_kite > 0){
  random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                         & !building_buffer[, , 1] & !kites[,,1,"nest"] == 1, arr.ind = TRUE)
  random_coords <- matrix(random_coords, ncol = 2)
  
  # Only proceed if valid cells exist
  if (nrow(random_coords) > 0) {
    # Sample new positions for red kites
    selected_coords <- random_coords[sample(1:nrow(random_coords), min(lonely_kite, nrow(random_coords))), ]
    selected_coords <- matrix(selected_coords, ncol = 2)
    
    # Place red kites in the selected coordinates
    if (nrow(selected_coords) > 0) {
      for (i in 1:nrow(selected_coords)) {
        kites[selected_coords[i, 1], selected_coords[i, 2], 1, "abundance"] <- 1
        kites[selected_coords[i,1], selected_coords[i,2], 1, "age"] <- sample(3:liv_exp, 1) 
      }
    }
  }
}


# ####
# # plot for check ----
# t <- 1
# # Combine data into a data frame for plotting
# df <- data.frame(
#   x = rep(1:x_dim, each = y_dim),
#   y = rep(1:y_dim, times = x_dim),
# 
#   # Region / landscape
#   region = as.vector(region[, , t]),
#   building_buffer = as.vector(building_buffer[, , t]),
# 
#   # Subjects
#   turbine = as.vector(turbine[, , t]),
#   nest = as.vector(kites[, , t, "nest"]),
#   juv_kite = as.vector(kites[, , t, "juv"] > 1),
#   lonely_kite = as.vector(kites[, , t, "abundance"] == 1)
# )
# 
# # categories
# df$category[df$building_buffer == TRUE] <- "Building Buffer"
# df$category[df$region == TRUE] <- "Region / Building"
# df$category[df$turbine == TRUE] <- "Turbine"
# df$category[df$nest == TRUE] <- "Redkite nest (2 adults)"
# df$category[df$juv_kite == TRUE] <- "Redkite nest (2 adults + 1-2 juv)"
# df$category[df$lonely_kite == TRUE] <- "Redkite lonely adult"
# 
# # titel
# tit <- paste("Inital Set-up ", t,
#              "\n T= ", sum(turbine[, , t]),
#               ", \n K_nest= ", sum(kites[, , t, "nest"]),
#                ", \n K_abund= ", sum(kites[, , t, "abundance"]),
#              ", K_juv=", sum(kites[, , t, "juv"]))
#                                    
# 
# # Create the plot with buffer zone
# p <- ggplot(df, aes(x = x, y = y)) +
#   geom_tile(aes(fill = category), show.legend = TRUE) +
#   scale_fill_manual(values = c(
#                                "Building Buffer" = "orange3",
#                                "Region / Building" = "blue",
#                                "Turbine" = "black",
#                                "Redkite nest (2 adults)" = "green4",
#                                "Redkite nest (2 adults + 1-2 juv)" = "green3",
#                                "Redkite lonely adult" = "yellow" ),
#                     name = "Legend") +
#   labs(title = tit,
#        x = "X", y = "Y") +
#   theme_minimal()
# 
# # Print the plot 
# print(p)
# 
# # plot age ----
# library(viridisLite)
# df <- data.frame(
#   x = rep(1:x_dim, each = y_dim),
#   y = rep(1:y_dim, times = x_dim),
#   age = as.vector(kites[, , t, "age"])
# )
# 
# p_age <- ggplot(df, aes(x = x, y = y)) +
#   geom_tile(aes(fill = age), show.legend = TRUE) +
#   scale_fill_viridis_c(option = "cividis") +
#   labs(title = "inital age",
#        x = "X", y = "Y") +
#   theme_minimal()
# print(p_age)
# 
# # plot abundance ----
# df <- data.frame(
#   x = rep(1:x_dim, each = y_dim),
#   y = rep(1:y_dim, times = x_dim),
#   abundance = as.vector(kites[, , t, "abundance"])
# )
# 
# p_abund <- ggplot(df, aes(x = x, y = y)) +
#   geom_tile(aes(fill = abundance), show.legend = TRUE) +
#   scale_fill_viridis_c(option = "cividis") +
#   labs(title = "inital abundance",
#        x = "X", y = "Y") +
#   theme_minimal()
# print(p_abund)
