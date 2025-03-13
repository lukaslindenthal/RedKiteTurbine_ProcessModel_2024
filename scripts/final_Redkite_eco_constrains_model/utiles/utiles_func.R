# script for help functions used in all scripts

# Function to calculate Euclidean distance between two points
# used for nest distance
# distance between redkites and trubines to caltulate mortality risk
calc_distance <- function(coord1, coord2) {
  return(sqrt((coord1[1] - coord2[1])^2 + (coord1[2] - coord2[2])^2))
}