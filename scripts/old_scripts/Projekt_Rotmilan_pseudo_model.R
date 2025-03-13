# ---
# Projekt: "Rotmilan in Büllerbüh"
# lecture: Porcess Mod WS 24/25
# authors: Neele & Lukas

# pseudo code > model outline 

# Funktionen ----
ricker_poisson 
# Funktion um Rotmilan Population zu Simulieren 

# output = N_new
# ricker_poisson <- function(N, r, k) { #  r: growth rate; k: carrying capacity
#   N_new <- rpois(1, N * exp(r * (1 - N / k)))
#   return(N_new)
# }

# Set parameters ----
## Dimensions/Scales ----
timesteps 
x_dim 
y_dim 

## Species parameters ----
R # Wachstumsrate
R_tot # Sterberate durch Turbinene 
# ?? ist R_tot abhäning von Trubinenzahl?
k # Carrying capacity
Z_ex <- 0 # externer Zuwachs, Z_ex = 0 da Einwanderung = Abwanderung
dispersal_percentage <- 0.2
dispersal_distance_max <- 4

## Turbinen parameters ----
turb_neu_max <- # max Turbinen die pro timestep neu gebaut werden können
turb_neu_perc <- # neubau Prozentual abhänig davon wie viele Bereits existieren
  # ausgehend davon, dass je mehr existieren, je mehr werden gebaut
  # ??
  # anderes Szenario denkbar

# Initialization ----
## Landscape: To store the landscape with dimensions X, Y, and timesteps ----
# Rotmilan Habitat und Turbinen Areal 
### Rotmilan Habitat ----
habitat # beschreibt mögliche Ausbreitungsgebiete des Rotmilans
# ! ab timestep= 1 > habitat = Abhänig von Areal der Trubinen

### Turbinen Areal ----
A_turb_neu # beschreibt die möglichen Flächen für den Turbinenbau
# dh eine neue Turbine kann nur innerhalb A_turb_neu "gebaut" werden 

### Rotmilan Artbestand ----
abundance <- N_start, x_dim, y_dim # Artbestand und Verteilung bei t=0
# x_dim, y_dim können nur innerhalb von habitat liegen

turbine <- True/ Flase value, x_dim, y_dim # Turbinen  und Verteilung bei t = 0
# # x_dim, y_dim können nur innerhalb von A_turb_neu liegen
# beschreibt die bereits existierenden Turbinen
# ! ab timestep = 1, wird innerhalb von A_turb_neu neue Turbinen "gebaut" 
# und damit ergibt sich neues turbine

# Simulation ----
for (i in 1:(timesteps - 1)) { # Loop through each time step except the last one
  
  # _______________________________
  # Turbinen model 
  for (x,y in A_turb_real)
    # berechne Anzahl an neu zubauenden Turbinen 
    neu <- len(turbine = True)*Turb_neu_perc
    # wähle Turbinen aus an die neu angebaut werden soll (anzhal = neu)
    turbine <- # ausbreitung ausgehend von existiernder Turbine in alle Richtungen
    # aber Ausbreitung muss innerhalb von A_turb_neu sein 
    A_trub_real # neu
  # ________________________________
  
  # neues Rotmilan Habitat
  habitat <- habitat - A_turb_neu
  
  # ______________________________
  # Rotmilan model 
  R_tot_neu <- R_tot * len(turbine = True)
  for (x,y in habitat) # bzw x,y zip(x_dim, y_dim) in habitat
    abundance[i+1] <- ricker_poisson(abundance[i], R, R_tot_neu, k)
  ## Rotmilan dispersal
  ### Dispersal
  abundances_dispersing <- abundances[, , i + 1] * dispersal_percentage
  
  ### Update N:
  abundances[, , i + 1] <- abundances[, , i + 1] - abundances_dispersing[, ]
  
  # ______________________________
  ## Rotmilan Dispersal --> neue Koordinaten 
  
  # Prüfe ob neu Koor valide
  valid <- if innerhalb habitat & 1 gridcelle abstand zu Turbine
  # nur innerhalb habitat kann sich der Rotmilan  ausbreiten
  # ! evtl. Bedinung --> 1 celle Abstand von existing Turbine
  
  # update new_xy
  abundances[new_x[valid], new_y[valid]]...
  
  # Gesamt Artbestand Rotmilan
  abundances_tot[i + 1] <- sum(abundances[, , i + 1])
  
  # ! later: exclude threshold if abundance_tot < threshold --> stop --> return(len(turbine = True))
  # ___________________________
  # Visualize
}

