library(fdasrvf)
load("/Users/gregorymatthews/Dropbox/StatisticalShapeAnalysis/data_set_of_full_teeth.RData")
######################################
#This function takes in a list of potential donors and one partial shape 
#It then returns M completed shapes
######################################
partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
complete_shape_list <- lapply(ptsTrainList[[1]], t)

start <- Sys.time()
test <- impute_partial_shape(complete_shape_list,partial_shape, M = 25, k = 10)
end <- Sys.time()
end-start

plot(t(test$imputed[[1]]),type="l", col = rgb(0,0,0,0), xlim = c(-0.4, 0.15), ylim = c(-0.2,0.2), lwd = 3)
points(t(red$donor),col = "red", type = "l",lwd=1, lty = 3)
set.seed(1234)
for (i in 1:50){
  u <- runif(3)
  points(t(test$imputed[[i]]),type="l",col =  rgb(u[1],u[2],u[3],0.5), lwd = 3)
}
red <- complete_partial_shape(t(ptsTrainList[[1]][[1]]),t(ptsTrainList[[1]][[1]][11:42,]))

arr <- array(0, dim = c(2,199,25))
for (i in 1:25){
  arr[,,i] <- test$imputed[[i]]
}

mn_shape <- apply(arr,c(1,2),mean)
points(t(mn_shape), type = "l", col = rgb(0.1,0.2,0.7), lwd = 2)




source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/calc_shape_dist_partial.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/complete_partial_shape.R")

impute_partial_shape <- function(complete_shape_list, partial_shape, k = 10, M = 5){
  
  dist_vec <- c(NA, length(complete_shape_list))
  for (i in 1:length(complete_shape_list)){print(i)
    dist_vec[i] <- calc_shape_dist_partial(complete_shape_list[[i]],partial_shape)
  }
  
  
  potential_donor_indices <- c(1:length(dist_vec))[order(dist_vec)][1:k]
  #Randomly pick M of these with replacement
  donor_indices <- sample(potential_donor_indices, M, replace = TRUE)
  
  #I only need to impute the unique indices and then i'll repeat them when the data gets output
  imputed_partial_list <- list()
  for (q in unique(donor_indices)){print(q)
    imp <- complete_partial_shape(complete_shape_list[[q]],partial_shape)
    imputed_partial_list[[q]] <- imp$partial_shape_imputed
  }
  
  imputed_partial_list_out <- list()
  for (j in 1:M){
    imputed_partial_list_out[[j]] <- imputed_partial_list[[donor_indices[j]]]
  }
  
  out <- list(imputed = imputed_partial_list_out, partial_obs = imp$partial_obs)
  return(out)
  
}