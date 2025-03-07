teeth_full <- load(file = 'data_set_of_full_teeth.RData')
lower_molars_1 <- ptsTrainList$LM1

# This function will go through all 60 rows of the tooth and spit out lowest values.
# It works row by row and continues past row 40
# Anything past row 40 is an NA, but those get pulled in the other function
lowest_distance <- function(distance_matrix) {apply(X = distance_matrix, 1, function(X){
  # Determines what row the function is on by using which()
  # The row is stored as an index number
  index <- which(X == 0)
  # The minimum value is taken from numbers between 20 and 40 entries from the index
  min(X[(index + 20) : min((index + 40), 60)])
})}


# Function takes in the tooth values from the data set
tooth_cutter <- function(tooth) {
  # Uses dist() on the tooth values to create a distance object
  # Converts the distance object ot a matrix
  tooth_distance <- as.matrix(dist(tooth))
  # Uses which and the earlier function (along with na.rm and arr.ind) to give the two closest points
  
  suitable_points <- which(tooth_distance == min(lowest_distance(tooth_distance), na.rm = 1), arr.ind = 1)
  
  cut_points <- suitable_points[which(diff(t(suitable_points)) > 20 & diff(t(suitable_points)) < 40),]
  
  # Creates new data sets for the tooth in question, depending on if they are between the points in question or not
  # For the set which removes points (tooth_left in this case), the numbers are adjusted so that the closest points are included
  #tooth_left <- tooth[-c((cut_points[1] + 1) : (cut_points[2] - 1)), ] #Grady's original code


  
  tooth_left <- tooth[c((cut_points[2]+1):nrow(tooth),1:(cut_points[1]-1)), ]
  tooth_right <- tooth[c(cut_points[1] : cut_points[2]), ]
  # Returns the left and right halves as a list
  list(tooth_left, tooth_right)
  }

cut_teeth <- lapply(lower_molars_1, tooth_cutter)

# The list with removed points maintains the teeth IDs from the original set
# There are two lists of points, associated with each half of the tooth
for (i in 134) {
  print(i)
  for (j in 1:2) {
    plot(cut_teeth[[i]][[j]], type = 'l', main = i)
    Sys.sleep(.5)
  }
}
## Some teeth the old bit was having problems with
# 12
# 26
# 50
# 76
# 87
# 92
# 94 
# 96
# 101
# 120
# 121?
# 128
# 131
# 133
# 134

# Used to quickly visualize where the cut points were at
## Note: Teeth share beginning and ending points
toothsample <- lower_molars_1[[134]]
sampletest <- tooth_cutter(toothsample)
plot(toothsample, type = 'l')
points(sampletest[[1]], col = 'green')
points(sampletest[[2]], col = 'purple')
