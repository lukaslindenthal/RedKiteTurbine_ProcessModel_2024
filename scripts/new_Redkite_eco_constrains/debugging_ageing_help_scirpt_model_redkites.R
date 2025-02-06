# debugging ageing
source("./scripts/new_Redkite_eco_constrains/help_scirpt_model_redkites.R")
sum(kites[,,3,"abundance"])
# IDEAS ###############
# kites[,,t+1, "abundance"] <- kites[,,t, "abundance"]
# nest 
# age + 1
# if age > liv exp
# nest dies abundance[t+1] <- 0 
# else 
# nest survives abundance[t+1] <- 2

# lonely adults + juveniles
# age + 1
# if age > liv exp
# kite dies abundance[t+1] <- 0 
# else 
# kite survives abundance[t+1] <- kite[t+1 abundance]

# juvenile placment 
# where age_lonely[t+1] <= 3 & age_lonely[t+1] < 0
# set abundance[t+1] <- kite[t+1 abundance] + 1
# set juv[t+1] <- 1
################################
sum(kites[,,t+1, "juv"])

# looping from second timestep (timestep t = 1 is in helping...R)
for (t in 1:(timesteps - 1)) {

  # ageing 
  abund_beginning <- sum(kites[,,t,"abundance"])
  kites[,,t+1, "abundance"] <- kites[,,t, "abundance"] # get abundance from t
  
  # nest
  coords_nests <- which(kites[, , t, "age_nest"] > 0, arr.ind = TRUE) # check for nests
  num_nests <- nrow(coords_nests)
  
  
  if (num_nests > 0){
    for (i in 1:num_nests) {
      
      new_age_nest <- kites[coords_nests[i,1],coords_nests[i,2], t, "age_nest"] + 1
      
      if(new_age_nest[[1]] > liv_exp) {
        # nest dies
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "age_nest"] <- 0
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "nest"] <- 0
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "abundance"] <- 0
      } else {
        # nest survives
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "age_nest"] <- new_age_nest[[1]]
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "nest"] <- 1
        kites[coords_nests[i,1],coords_nests[i,2], t+1, "abundance"] <- 2
      }
    }
  }
  
  
  # lonely adults + juveniles
  coords_lonely <- which(kites[, , t, "age_lonely"] > 0, arr.ind = TRUE) # check for nests
  num_lonely <- nrow(coords_lonely)
  
  if (num_lonely > 0){
    for (i in 1:num_lonely) {
      
      new_age_lonely <- kites[coords_lonely[i,1],coords_lonely[i,2], t, "age_lonely"] + 1
      
      if(new_age_lonely[[1]] > liv_exp) {
        # kite dies
        kites[coords_lonely[i,1],coords_lonely[i,2], t+1, "age_lonely"] <- 0
        kites[coords_lonely[i,1],coords_lonely[i,2], t+1, "abundance"] <- 0
      } else {
        # kite survives
        kites[coords_lonely[i,1],coords_lonely[i,2], t+1, "age_lonely"] <- new_age_lonely[[1]]
       
        kites[coords_lonely[i,1],coords_lonely[i,2], t+1, "abundance"] <- 
          kites[coords_lonely[i,1],coords_lonely[i,2], t+1, "abundance"] # if nest exists, abundance <- 3
      }
    }
  }
  
  # juvenile placement (all kites <= 3)
  coords_juv <- which(kites[, , t, "age_lonely"] <= rep_age & kites[, , t, "age_lonely"] > 0, arr.ind = TRUE) # check for nests
  num_juv <- nrow(coords_juv)
  
  if (num_juv > 0){
    for (i in 1:num_juv) {
      
      kites[coords_juv[i,1],coords_juv[i,2], t+1, "age_lonely"] <- 
        kites[coords_juv[i,1],coords_juv[i,2], t+1, "abundance"][[1]] + 1
      
      age_juv <- kites[coords_juv[i,1],coords_juv[i,2], t+1, "age_lonely"]
      if(age_juv[[1]] < rep_age){
        kites[coords_juv[i,1],coords_juv[i,2], t+1, "juv"] <- 1
      } 
    }
  }
  
  # check 
  # to check "abundance" before and after aging
  n_died_natural <- nrow(which(kites[,,t, "age_lonely"] >= 12, arr.ind = TRUE)) +
    nrow(which(kites[,,t, "age_nest"] >= 12, arr.ind = TRUE))*2
  
  ab_t1 <- abund_beginning - n_died_natural
  
  check_abund_before <- sum(kites[ , , t+1, "abundance"])
  
  # 2 check ageing ----
  if (ab_t1 == check_abund_before){
    print(paste("same nr after aging: t1", ab_t1, "; real t1", check_abund_before,
                "\n natural dead:", n_died_natural, abund_beginning))
  } else {
    stop(paste("nr not the same after ageing! t1", ab_t1, "; real t1", check_abund_before, abund_beginning))
  }
} # timestep foor loop

# ################################
# # old code ----
# # 2 age of kites and nest increased ----
# # and
# # check if kite or nest will die, age > liv_exp
# abund_beginning <- sum(kites[,,t,"abundance"])
# 
# # nest
# coords_nests <- which(kites[, , t, "age_nest"] > 0, arr.ind = TRUE) # check for nests
# num_nests <- nrow(coords_nests)
# 
# 
# if (num_nests > 0){
#   for (i in 1:num_nests) {
#     
#     new_age_nest <- kites[coords_nests[i,1],coords_nests[i,2], t, "age_nest"] + 1
#     juv <- kites[coords_nests[i,1],coords_nests[i,2] , t, "juv"]
#     
#     if (juv[[1]] == 0 ) { 
#       # no juv
#       if (new_age_nest[[1]] > liv_exp) {
#         # nests without juv dies
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- 0
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 0 
#       } else {
#         # nest survives
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- new_age_nest[[1]]
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 2 # nest still exists
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "nest"] <- 1
#       }
#       
#     } else {
#       # if juv exists
#       age_juv <- kites[coords_nests[i,1],coords_nests[i,2] , t, "age_lonely"]
#       
#       if (new_age_nest[[1]] > liv_exp) {
#         # nests with juv, nest dies juv survives
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- 0
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "nest"] <- 0
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 1 
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_lonely"] <- age_juv[[1]] + 1 # juv new age
#       } else {
#         
#         # nest + juv survives
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_nest"] <- new_age_nest[[1]]
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "abundance"] <- 3 # nest still exists
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "age_lonely"] <- age_juv[[1]] + 1 # juv new age
#         kites[coords_nests[i,1],coords_nests[i,2] , t + 1, "nest"] <- 1
#       }
#     }
#   }
# }
# # which(kites[,,t+1, "age_nest"] > liv_exp, arr.ind = TRUE)
# 
# # lonely
# coords_lonely <- which(kites[, , t, "age_lonely"] >= rep_age , arr.ind = TRUE) # check for lonely kites + juv
# num_lonely <- nrow(coords_lonely)
# 
# if (num_lonely > 0){
#   for (i in 1:num_lonely) {
#     
#     new_age <- kites[coords_lonely[i,1],coords_lonely[i,2], t, "age_lonely"] + 1
#     
#     # when age > liv_exp --> dead
#     if (new_age[[1]] > liv_exp) {
#       kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "age_lonely"] <- 0
#       kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "abundance"] <- 0
#     } else {
#       kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "age_lonely"] <- new_age[[1]]
#       kites[coords_lonely[i,1],coords_lonely[i,2] , t + 1, "abundance"] <- 1 # kite still exists
#     }
#   }
# }
# 
# # juveniles t+1
# coords_juv<- which(kites[, , t+1, "age_lonely"] < rep_age & kites[, , t+1, "age_lonely"] < 0 , arr.ind = TRUE) # check for lonely kites + juv
# num_juv <- nrow(coords_juv)
# if (num_juv > 0){
#   for (i in 1:num_juv) {
#     kites[coords_juv[i,1],coords_juv[i,2],t+1, "juv"] <- 1 
#     
#   }
# }
# # sum(kites[,,t,"age_lonely"] < rep_age & kites[,,t,"age_lonely"] > 0)
# 
# # check 
# # to check "abundance" before and after aging
# n_died_natural <- nrow(which(kites[,,t, "age_lonely"] >= 12, arr.ind = TRUE)) +  
#   nrow(which(kites[,,t, "age_nest"] >= 12, arr.ind = TRUE))*2
# 
# ab_t1 <- abund_beginning - n_died_natural
# 
# check_abund_before <- sum(kites[ , , t+1, "abundance"])
# 
# # 2 check ageing ----
# if (ab_t1 == check_abund_before){
#   print(paste("same nr after aging: t1", ab_t1, "; real t1", check_abund_before, 
#               "\n natural dead:", n_died_natural, abund_beginning))
# } else {
#   stop(paste("nr not the same after ageing! t1", ab_t1, "; real t1", check_abund_before, abund_beginning))
# }
# #} # timestep foor loop