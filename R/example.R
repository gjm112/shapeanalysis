###  nohup R CMD BATCH --vanilla /home/gmatthews1/FILENAME.R &

source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/calc_shape_dist_partial.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/complete_partial_shape.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/impute_partial_shape.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/tooth_cutting.R")

library(fdasrvf)
load("/Users/gregorymatthews/Dropbox/StatisticalShapeAnalysis/data_set_of_full_teeth.RData")

i <- 1 #Whcih tooth.  DSCN number 
j <- 1 #could be 1 or 2.  
partial_shape <- t(tooth_cutter(ptsTrainList[["LM1"]][[i]])[[j]])
#partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
complete_shape_list <- lapply(ptsTrainList[["LM1"]], t)

#I can't impute the partial shape with itself!
complete_shape_list[[i]]<-NULL

start <- Sys.time()
imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = 5, k = 10)
end <- Sys.time()
end-start

#Now do classification on the completed shapes just using closest 
ref_file <- read.csv("/Users/gregorymatthews/Dropbox/shapeanalysisgit/refFile.csv")

dist_imputed_to_whole <- function(whole,part){
  whole <- resamplecurve(whole,N =dim(part)[2], mode = "C")  
  out <- calc_shape_dist(whole,part,mode="C")
  return(out)
}

#doesitwork <- list(complete_shape_list[[1]],complete_shape_list[[2]])
#greg <- lapply(doesitwork, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[m]])

dist_imputed_to_whole2 <- function(part){
  out <- lapply(doesitwork, dist_imputed_to_whole, part = part)
  return(out)
}

dist_list <- lapply(imputed_partial_shape[[1]],dist_imputed_to_whole2)
dist <- t(do.call(rbind,lapply(dist_list,unlist)))





#take top 10 closest? 
index <- order(dist)[1]
dscn <- names(complete_shape_list)[index]
subset(ref_file,ref %in% dscn)
subset(ref_file,ref == "DSCN2879")







plot(t(test$imputed[[1]]),type="l", col = rgb(0,0,0,0), xlim = c(-0.4, 0.15), ylim = c(-0.2,0.2), lwd = 3)
points(t(red$donor),col = "red", type = "l",lwd=1, lty = 3)
set.seed(1234)
for (i in 1:25){
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


imputed<-test
out<-list(complete_shape_list,imputed)
save(out,file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/example_data_for_sebastian.RData")

write.csv(do.call(rbind,complete_shape_list), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/complete_shape_list_for_sebastian.csv" )
write.csv(do.call(rbind,test$imputed), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/completed_tooth_for_sebastian.csv" )


