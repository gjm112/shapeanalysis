# plot(t(results_list[[1]]$imputed_partial_shape$imputed[[1]]),type="l")
# points(t(results_list[[1]]$imputed_partial_shape$imputed[[2]]),type="l")
# points(t(results_list[[1]]$imputed_partial_shape$imputed[[3]]),type="l")
# points(t(results_list[[1]]$imputed_partial_shape$imputed[[4]]),type="l")
# points(t(results_list[[1]]$imputed_partial_shape$imputed[[5]]),type="l")

#load("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190525_side=1_k=5_M=5_tooth=LM1.RData")
load("results/results20190525_side=2_k=5_M=5_tooth=LM1.RData")
library(fdasrvf)
library(parallel)

source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")

for (d in 1:length(results_list)){print(d)

arr <- array(NA, c(2,199,M))
for (i in 1:M){
  arr[,,i] <- results_list[[d]]$imputed_partial_shape$imputed[[i]]
}

results_list[[d]]$imputed_partial_shape$mean <- apply(arr,c(1,2),mean)

part <- results_list[[d]]$imputed_partial_shape$mean


get_dist <- function(whole, part){
  print(Sys.time())
  whole <- resamplecurve(whole,N = dim(part)[2], mode = "C") 
  out <- calc_shape_dist(whole, part, mode = "C")
  return(out)
}

dist <- mclapply(complete_shape_list, get_dist, part = results_list[[d]]$imputed_partial_shape$mean, mc.cores = 12)

results_list[[d]]$imputed_partial_shape$dist_mean <- data.frame(DSCN = names(complete_shape_list),dist = unlist(dist))

}

 

save.image("results/results20190525_side=2_k=5_M=5_tooth=LM1.RData")






