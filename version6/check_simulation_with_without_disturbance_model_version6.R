# ----------------------------------------------------------------
# check and visualisation of final simulation
# ----------------------------------------------------------------
# This script gives a visual overview of the Redkite population with and without disturbance 

# first run the simulation `output_simulation_version6.R`

# ----------------------------------------------------------------
# Authors: Neele Haß 
# ----------------------------------------------------------------
source("./help_functions_version6.R")

# ----------------------------------------------------------------
# COMPARISON Red Kite with and without disturbance ----
# ----------------------------------------------------------------
# get Results from Red kite model without disturbance
source("./Red_kite_model_version6.R")
test_Red_kite_population <- test_Red_kite_population(x_dim, y_dim, t,
                                                     region, building_buffer, turbine, buffer,
                                                     rep_age, growth_rate, liv_exp, carrying_capacity,
                                                     kites_metric, kites_abund)

test_wodis_kites_abund <- test_Red_kite_population$kites_abund


# plot in COMPARISON
abund <- results_kites_abund
wo_dis <- test_wodis_kites_abund$total_abund
turbine <- results_turbine
name <- "comp_2model6_without_disturbance_20_max50_turb"
addmain <- "comparision without and with disturbance"
commi <- "init_turb = 20, \nturb_neu_max = 50, \nturb_neu_perc = 0.8"

comp <- comp_plot(abund, wo_dis, turbine, name, addmain, commi)

# ----------------------------------------------------------------
# CHECK theoretical abundance and real abundance after modeling ----
# ----------------------------------------------------------------
# each time step should be + dif_abund_dead[t] größer
# (dif_abund_dead t = 1 can be exluded)
theo_abund <- vector(length = timesteps)
theo_abund[1] <- abund$total_abund[1]
diff_new_killed <- comp$diff_new_killed

for(t in 1:timesteps){
  theo_abund[t+1] <- theo_abund[t] + diff_new_killed[t+1]
}
theo_abund <- theo_abund[!is.na(theo_abund)]

if(any((total_abund - theo_abund) != 0) == TRUE){
  print("error - total abund odd, diff between newborn and dying kites is odd")
  print(theo_abund)
  print(total_abund)
  print(theo_abund - total_abund)
} else {
  print("total abund is correct")
}

# ----------------------------------------------------------------
# CHECK Red Kite without disturbance - AGE distribution ----
# ----------------------------------------------------------------
# Examine Red Kite population without disturbance 
# final goal: stable Population dynamics!

test_kites_metric <- test_Red_kite_population$test_kites_metric

age_per_t <- list()

for(i in 1:timesteps){
  age <- c(test_kites_metric[[i]][[3]], test_kites_metric[[i]][[4]], test_kites_metric[[i]][[5]])
  age <- age[age > 0]
  age_per_t[[i]] <- age
}

# plot 
png("./plots/age_histograms.png", width = 3000, height = 3000, res = 150)
par(mfrow = c(6, 5), mar = c(1, 1, 1, 1))
for(i in 1:timesteps){
  hist(age_per_t[[i]], main = i , xlab = "", ylab = "", col = "skyblue", border = "white", breaks = 10)
}
dev.off()
