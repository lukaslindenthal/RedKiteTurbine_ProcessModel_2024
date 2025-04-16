# ----------------------------------------------------------------
# final check and visualisation
# ----------------------------------------------------------------
# This script gives a visual overview of the Redkite population with and without disturbance 

# ----------------------------------------------------------------
# Authors: Neele Haß 
# ----------------------------------------------------------------

source("./help_functions_version5.R")

## Red kites population without disturbance ----

# set n_turb_1 = 0! (in Tim_dim...para_setting.R)

source("./test_Red_kite_model_version5.R")
wo_dis_kites_abund <- kites_abund

# saves plot
pop_func_plot(wo_dis_kites_abund, "without_disturb", 
              addmain = "without disturbance!", 
              commi = "immigration or\nnest build rate needed\nfor stable population!\npopulation is dying!")

# !!!!!!!!!
# red_kites populaion will decline (not survive) even without disturbance
# its worth to integrate immigration factor otherwise 
# or 
# integrate new nest building rate! 
# now peak of 547 lonely kites and only 72 new nest were built !
# !!!!!!

## Red kites population with disturbance (turbine construction) ----
# set 
# init_turb = 20, turb_neu_max = 50, turb_neu_perc = 0.8

source("./model_version5.R")
model_kites_abund <- kites_abund

# saves plot
pop_func_plot(model_kites_abund, "disturb_20_max50_turb", addmain = "init_turb = 20, turb_neu_max = 50, turb_neu_perc = 0.8", commi = "Error: timestep 27")

## compare with and without disturbance ----
abund <- model_kites_abund
wo_dis <- wo_dis_kites_abund$total_abund
name <- "comp_without_disturbance_20_max50_turb"
addmain <- "comparision without and with disturbance"
commi <- "init_turb = 20, \nturb_neu_max = 50, \nturb_neu_perc = 0.8, \nError: timestep 27"
  
abund[is.na(abund)] <- 0

# Create summary vectors for each timestep
time <- as.numeric(1:timesteps)
total_abund <- abund$total_abund
abund_new_juv <- abund$abund_new_juv
killed <- (abund$nat_dead + abund$nest_dead + 
             abund$killed_move + abund$killed_build)
turb_killed <- (abund$killed_move + abund$killed_build)
nat_killed <- (abund$nat_dead + abund$nest_dead)

diff_new_killed <- abund_new_juv - killed

n_turb <- numeric(timesteps)
for(t in 1:timesteps) {
  n_turb[t] <- sum(turbine[,,t])
}

png(paste("./plots/",name,".png"), width = 800, height = 600)
plot(time, total_abund, type = "b", col = "green", pch = 16,
     xlab = "Timestep (years)", ylab = "Count",
     main = paste("Redkites Over Time\n", addmain),
     ylim = c(min(diff_new_killed)-10,max(total_abund)+100),
     xlim = c(1,timesteps))
abline(time,0, col = "grey")
lines(time, abund_new_juv, type = "l", col = "blue")
lines(time, killed, type = "l", col = "red")
lines(time, nat_killed, type = "l", col = "pink")
lines(time, turb_killed, type = "l", col = "magenta")
lines(time, diff_new_killed, type = "l", col = "darkgrey")
lines(time, n_turb, type = "b", col = "black", pch = 16)
lines(time, wo_dis, type = "b", col = "darkgreen", pch = 16)

text(time, total_abund, labels = total_abund, pos = 1, col = "green",cex = 1)
text(time, diff_new_killed, labels = diff_new_killed, pos = 1, col = "darkgrey",cex = 1)
text(time, n_turb, labels = n_turb, pos = 3, col = "black",cex = 1)
text(time, wo_dis, labels = wo_dis, pos = 3, col = "darkgreen",cex = 1)

text(max(time)-5, (max(total_abund)+10)/2, labels = commi, 
     pos = 1, col = "black",cex = 1)

legend("topright", legend = c("Red Kites", "Red Kites without dis.", "new_juv", "total killed", "killed nat.","killed by turb","diff_new_killed", "Turbines"), 
       col = c("green" ,"darkgreen", "blue", "red", "pink", "magenta", "darkgrey","black"), pch = 16, pt.cex = 1,cex = 1)
dev.off()


# check ----
# so each time step should be + dif_abund_dead[t] größer
# (dif_abund_dead t = 1 can be exluded)
theo_abund <- vector(length = timesteps)
theo_abund[1] <- total_abund[1]

for(t in 1:timesteps){
  theo_abund[t+1] <- theo_abund[t] + diff_new_killed[t+1]
}

if(any((total_abund-theo_abund[1:timesteps]) != 0) == TRUE){
  print("error - total abund odd, diff between newborn and dying kites is odd")
  print(theo_abund)
  print(total_abund)
} else {
  print("total abund is correct")
}




