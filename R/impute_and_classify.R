###  nohup R CMD BATCH --vanilla /home/gmatthews1/FILENAME.R &
source("./R/utility.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutting.R")


library(parallel)
load("./data/data_set_of_full_teeth.RData")
load("./data/ptsTrainList.RData")
#save(ptsTrainList, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
i <- 1 #Whcih tooth.  DSCN number 
j <- 1 #could be 1 or 2.  Which side of the tooth 

#Set up the data
partial_shape <- t(tooth_cutter(ptsTrainList[["LM1"]][[i]])[[j]])
complete_shape_list <- lapply(ptsTrainList[["LM1"]], t)  
complete_shape_list[[i]]<-NULL #Can't impute the shape with itself!
DSCN_target <- names(ptsTrainList[["LM1"]])[[i]]

start <- Sys.time()
test <- impute_and_classify(partial_shape,complete_shape_list, DSCN = DSCN_target, M = 5, k = 10)
end <- Sys.time()
end-start

#Now the function should take partial_shape and complete_shape_list as arguments
impute_and_classify <- function(partial_shape,complete_shape_list, DSCN = DSCN_target, M = 5, k = 10){
  
  #1.194916 minutes
  start <- Sys.time()
  imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k)
  end <- Sys.time()
  end-start
  
  #I don't need to compute all M of the imputed shapes if some are duplicated
  #names(imputed_partial_shape$imputed) <- letters[1:5]  
  #names(unique(imputed_partial_shape$imputed))
  #names(duplicated(imputed_partial_shape$imputed))
  
  #Now do classification on the completed shapes just using closest 
  ref_file <- read.csv("./data/refFile.csv")
  truth <- subset(ref_file,ref == DSCN)
  
  
  dist_imputed_to_whole <- function(whole,part){
    whole <- resamplecurve(whole,N =dim(part)[2], mode = "C")  
    out <- calc_shape_dist(whole,part,mode="C")
    print("calc_dist")
    return(out)
  }
  
  #3.353418 seconds
  # start <- Sys.time()
  # dist_imputed_to_whole(complete_shape_list[[1]],imputed_partial_shape$imputed[[1]])
  # end <- Sys.time()
  # end-start
  
  dist_imputed_to_whole2 <- function(part){
    out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part, mc.cores = 12) #takes about 3 minutes.  2.11 minutes with mclapply
    return(out)
  }
  
  start <- Sys.time()
  dist_list <- mclapply(imputed_partial_shape$imputed,dist_imputed_to_whole2, mc.cores = 12)
  end <- Sys.time()
  end-start
  
  dist_mat <- t(do.call(rbind,lapply(dist_list,unlist)))
  
  row.names(dist_mat)<-names(complete_shape_list)
  
  dist_mat <- as.data.frame(dist_mat)
  dist_mat$DSCN <- row.names(dist_mat)
  
  dist_mat <- merge(dist_mat, ref_file, by.x = "DSCN", by.y = "ref", all.x = TRUE)
  
  #Smallest to largest
  #knn <- 5
  # table(as.character(dist$tribe[order(dist$V1)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V2)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V3)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V4)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V5)][1:knn]))
  
  #Classify based on closest match between partial and all full teeth.  
  #fret <- mclapply(complete_shape_list,calc_shape_dist_partial,partial_shape = partial_shape)
  dist_partial <- data.frame(DSCN = names(imputed_partial_shape$dist_vec), dist = imputed_partial_shape$dist_vec)
  dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "ref", all.x = TRUE)
  
  results_list[[DSCN_target]] <- list(dist = dist_mat , dist_partial = dist_partial, truth = truth, imputed_partial_shape = imputed_partial_shape)
}

end_all <- Sys.time()
end_all-start_all
#save.image("/home/gmatthews1/shapeAnalysis/results/results20190220.RData")
#table(as.character(dist_partial$tribe[order(dist_partial$dist)][1:10]))


# #take top 10 closest? 
# index <- order(dist)[1]
# dscn <- names(complete_shape_list)[index]
# subset(ref_file,ref %in% dscn)
# subset(ref_file,ref == "DSCN2879")
# 
# 
# 
# 
# 
# 
# 
# plot(t(test$imputed[[1]]),type="l", col = rgb(0,0,0,0), xlim = c(-0.4, 0.15), ylim = c(-0.2,0.2), lwd = 3)
# points(t(red$donor),col = "red", type = "l",lwd=1, lty = 3)
# set.seed(1234)
# for (i in 1:25){
#   u <- runif(3)
#   points(t(test$imputed[[i]]),type="l",col =  rgb(u[1],u[2],u[3],0.5), lwd = 3)
# }
# red <- complete_partial_shape(t(ptsTrainList[[1]][[1]]),t(ptsTrainList[[1]][[1]][11:42,]))
# 
# arr <- array(0, dim = c(2,199,25))
# for (i in 1:25){
#   arr[,,i] <- test$imputed[[i]]
# }
# 
# mn_shape <- apply(arr,c(1,2),mean)
# points(t(mn_shape), type = "l", col = rgb(0.1,0.2,0.7), lwd = 2)
# 
# 
# imputed<-test
# out<-list(complete_shape_list,imputed)
# save(out,file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/example_data_for_sebastian.RData")
# 
# write.csv(do.call(rbind,complete_shape_list), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/complete_shape_list_for_sebastian.csv" )
# write.csv(do.call(rbind,test$imputed), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/completed_tooth_for_sebastian.csv" )
# 
# 
