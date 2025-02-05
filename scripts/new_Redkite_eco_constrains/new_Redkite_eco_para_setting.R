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
# ! 1 Junge pro nest
# ! ein mal Brutzeit pro Jahr, also pro timestep
# ! Distanz zwischen nesten = random nr zwischen (4000 an 1500)
# ! age of nest will be the medium of the the two kites
# #################################
####
#open questions
# check if resolution set up makes senes?
# max distance of nest?
########
# Dependencies ----
source("./scripts/new_Redkite_eco_constrains/new_Time_Dim_Turbo_Region_para_setting.R") # load timesteps, dim and Turbine

library(ggplot2)
library(abind)

# Seed for Reproducibility ----
set.seed(42)

# ----------------------------------------------------------------
# Red Kite Parameters-----------------------------------------------------------
# ----------------------------------------------------------------
# Initialize Red Kite parameters ----

area <- x_dim*y_dim * resolution # area in [km2], dim [gridcells], resolution [km2]
resolution_factor <- 1 / (resolution) # 1km2 <- x *(res*res)
# resolution = 10km x 10km 
carrying_capacity <- (19*2)* (area/100)    # [18.5 pairs / 100km2] # Carrying capacity (K)

# growth and survival rate
# prod_n <- 2 			# Fortpflanzungsziffern (Junge je begonnene Brut)
prod_n <- 1
prod_rate <- 0.79		# Bruterfolgsraten (Anteil von Nestern mit mind. einem flüggen Jungvogel an der Gesamtzahl der Nester mit Eiablage)
growth_rate <- prod_rate  # Growth rate (r in Ricker equation)

rep_age <- 3 # Reproduction age 

# # chance of survival 
# surv_r_1 <- 0.55		# Überlebensrate bis 1Jahr
# surv_r_a <- 0.84		# Überlebensrate > 1Jahr = adult

# Lebenserwartung
liv_exp <- 12    # Lebenserwartung ca 12 dh nest besteht 12-rep_age Jahre

# Abundance
initial_adults <- (3*2) * (area/100) # [3 pairs / 100km2] 
initial_lonely <-  initial_adults*0.10
initial_new_born <-  (initial_adults/2) * growth_rate # inital new born, dependent on nr of nests (num_nests) * growth_rate

# Dispersal
# within ~20 km Radius der Dispersal nach geschlechstreife 
# all possible moves within a distance of 20 grid cells
dis_num <- 20 * resolution_factor
dispersal <- expand.grid(dx = -dis_num:dis_num, dy = -dis_num:dis_num)
# exclude center 
ex <- which((dispersal[,1] == 0) & (dispersal[,2] == 0), arr.ind = TRUE)
dispersal <- dispersal[-ex,]

# kites dies if it enters buffer zone of turbine

# 4D array to save, for kites: ----
# Coordinates of, nests, juvenils, the abundance and age
kites_nest <- array(FALSE, dim = c(x_dim, y_dim, timesteps)) # placment of nest
kites_juv <- array(FALSE, dim = c(x_dim, y_dim, timesteps)) # number of new born kites
kites_abund <- array(0, dim = c(x_dim, y_dim, timesteps))  # total abundance red kites
kites_age <- array(0, dim = c(x_dim, y_dim, timesteps))  # age lonely red kites, juv are countet hier as well, dies if > 12
kites_age_nest <- array(0, dim = c(x_dim, y_dim, timesteps))  # age nest, dies if > 12
kites_killed_turb <- array(0, dim = c(x_dim, y_dim, timesteps)) # killed kites/nest by turbine 

kites <- abind(kites_nest, kites_juv, kites_abund, kites_age, kites_age_nest, kites_killed_turb, along = 4)
# Add dimension names for clarity
dimnames(kites) <- list(
  x_dim = NULL,
  y_dim = NULL,
  timesteps = NULL,
  type = c("nest", "juv", "abundance", "age_lonely", "age_nest", "killed_turb")
)
# print(dim(kites))
# dimnames(kites)$type
# kites[,,t,"abundance"]


##############
# Inital Placement of kites ----
##############
# Randomly placment of kite nest (pair) ----
# check if inital placement = even
# check distance between the nests
is_even <- (initial_adults %% 2) == 0 # check ob es durch 2 teilbar ist
if (is_even == TRUE){
  num_nests <- initial_adults/2
  lonely_kite <- initial_lonely
} else {
  num_nests <- (initial_adults/2) - 1
  lonely_kite <- initial_lonely + 1
}

# placements of nests
# Determine valid cells for kite nest placement, considering turbines  + buffer + buildings
random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                        & !building_buffer[, , 1] & !buffer[, , 1], arr.ind = TRUE)
random_coords <- matrix(random_coords, ncol = 2)

# Only proceed if valid cells exist
# choose valid coords which fullfille min / max nest distance
if (nrow(random_coords) > 0) {
  
  selected_coords <- matrix(0, ncol = 2, nrow = 0) # store later valid coords
  
  # Randomly sample potential nest positions
  pot_coords <- random_coords[sample(1:nrow(random_coords), min(num_nests, nrow(random_coords))), , drop = FALSE]
  
  # distnace check, fällt weg wenn resolution 10km x 10 km
  # also:
  selected_coords <- rbind(selected_coords, pot_coords)
  # # check for distance
  # for (i in 1:nrow(pot_coords)) {
  #   # Get current candidate coordinate
  #   candidate_coord <- pot_coords[i, ]
  #   
  #   # Check if this candidate is sufficiently far from all selected coordinates
  #   if (nrow(selected_coords) == 0) {
  #     # First coordinate, automatically select it
  #     selected_coords <- rbind(selected_coords, candidate_coord)
  #   } else {
  #     # Calculate distance to all previously selected coordinates
  #     distances <- apply(selected_coords, 1, function(existing_coord) {
  #       calc_distance(existing_coord, candidate_coord)
  #     })
  #     
  #     # If the minimum distance is greater than the max distance, accept this coordinate
  #     if (min(distances) >= nest_dist_min) {
  #       selected_coords <- rbind(selected_coords, candidate_coord)
  #     }
  #   }
  # }

  # Place red kites in the selected coordinates
  if (nrow(selected_coords) > 0) {
    for (i in 1:nrow(selected_coords)) {
      kites[selected_coords[i, 1], selected_coords[i, 2], 1, "nest"] <- TRUE
    }
  }
}

# change abundance for nests ----
# and change age - in nests age > 3, new_born set to 1
coords_nest <- which(kites[,,1,"nest"] == 1, arr.ind = TRUE)
for (i in 1:nrow(coords_nest)){
  kites[coords_nest[i,1], coords_nest[i,2], 1, "abundance"] <- 2
  kites[coords_nest[i,1], coords_nest[i,2], 1, "age_nest"] <- sample(rep_age:liv_exp, 1) # all kites are > 1 year

}

# new born to nests ----
num_nests <- nrow(coords_nest) # number of nests could chnage due to distance condtions
inital_new_born <- round(num_nests * growth_rate)

coords_juv <- coords_nest[sample(1:nrow(coords_nest), min(inital_new_born, nrow(coords_nest))), , drop = FALSE]

for (i in 1:nrow(coords_juv)) {
  # Add the corresponding number of new borns to each nest
  kites[coords_juv[i,1], coords_juv[i,2], 1, "juv"] <-  TRUE
  kites[coords_juv[i,1], coords_juv[i,2], 1, "age_lonely"] <-  1 # new kites age

  #add adult abundance per nest
  kites[coords_juv[i,1], coords_juv[i,2], 1, "abundance"] <- 
    kites[coords_juv[i,1], coords_juv[i,2], 1, "abundance"] + 1

}

# Verify the total number of new borns
sum(kites[,,1, "juv"]) == initial_new_born

# placment of lonely kit, if exist ----
# age between 3 and liv_exp, adult
if (lonely_kite > 0){
  random_coords <- which(!region[, , 1] & !turbine[, , 1] 
                         & !building_buffer[, , 1] & !buffer[, , 1] 
                         & !kites[,,1,"nest"] == 1, arr.ind = TRUE)
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
        kites[selected_coords[i,1], selected_coords[i,2], 1, "age_lonely"] <- sample(rep_age:liv_exp, 1) 
      }
    }
  }
}

########### check
####
241

# plot for check ----

t <- 1

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
  lonely_kite = as.vector(kites[, , t, "abundance"] == 1)

)

# categories
df$category[df$building_buffer == TRUE] <- "Building Buffer"

df$category[df$region == TRUE] <- "Region / Building"

df$category[df$turb == TRUE] <- "Turbine"

df$category[df$buffer == TRUE] <- "T-Buffer"

df$category[df$nest == TRUE] <- "Redkite nest (2 adults)"

df$category[df$juv_kite == TRUE] <- "Redkite nest (2 adults + 1-2 juv)"

df$category[df$lonely_kite == TRUE] <- "Redkite lonely adult"

df$category[is.na(df$category)] <- "Background"

# titel

tit <- paste("Inital Set-up ", t,
             "\n T= ", sum(turbine[, , t]),
             ", \n K_nest= ", sum(kites[, , t, "nest"]),
             ", K_lonely= ", sum(kites[, , t, "age_lonely"] >=3),
             ", \n K_abund= ", sum(kites[, , t, "abundance"]),
             ", K_juv=", sum(kites[, , t, "juv"]))

p <- ggplot(df, aes(x = x, y = y)) +
geom_tile(aes(fill = category), show.legend = TRUE) +
scale_fill_manual(values = c("Turbine" = "black",
                             "T-Buffer" = "orange2",
                             "Building Buffer" = "orange",
                             "Region / Building" = "blue",
                             "Redkite nest (2 adults)" = "green4",
                             "Redkite nest (2 adults + 1-2 juv)" = "green",
                             "Redkite lonely adult" = "yellow",
                             "Background" = "grey95"),

                  name = "Legend") +

labs(title = tit,
     x = "X", y = "Y") +
theme_minimal()


print(p)



# # plot age ----
# library(viridisLite)
# df <- data.frame(
#   x = rep(1:x_dim, each = y_dim),
#   y = rep(1:y_dim, times = x_dim),
#   age_l = as.vector(kites[, , t, "age_lonely"]),
#   age_n = as.vector(kites[, , t, "age_nest"])
# 
# )
# df$category[df$age_l == TRUE] <- "Building Buffer"
# df$category[df$age_n == TRUE] <- "Region / Building"
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
