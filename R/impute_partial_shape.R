
######################################
#This function takes in a list of potential donors and one partial shape 
#It then returns M completed shapes
######################################


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