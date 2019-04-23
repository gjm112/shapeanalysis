###  nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script.Rout
start_all <- Sys.time()
library(fdasrvf)
library(parallel)

M <- 10
k <- 10
side <- 1 #could be 1 or 2.
tooth <- "LM1"
#/home/gmatthews1/shapeAnalysis

setwd("/home/gmatthews1/shapeAnalysis")
source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")

load("./data/data_set_of_full_teeth.RData")
load("./data/ptsTrainList.RData")
#save(ptsTrainList, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
#i <- 1 #Whcih tooth.  DSCN number 
 


#Need a function that takes each partial tooth as an argument to get to parallel.  
results_list <- list()
for (d in 1:length(ptsTrainList[[tooth]])){
  print(d)
  print(Sys.time())
  partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[d]])[[side]])
  #partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
  complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
  #complete_shape_list <- list(complete_shape_list[[1]],complete_shape_list[[2]],complete_shape_list[[3]],complete_shape_list[[4]],complete_shape_list[[5]])
  
  #I can't impute the partial shape with itself!
  complete_shape_list[[d]]<-NULL
  
  start1 <- Sys.time()
  imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k)
  end1 <- Sys.time()
  end1-start1 #1.4 minutes with 4 cores on server.  Using detectCores()-1 it takes 
  
  #Now do classification on the completed shapes just using closest 
  ref_file <- read.csv("./data/refFile.csv")
  DSCN_target <- names(ptsTrainList[["LM1"]])[[d]]
  truth <- subset(ref_file,ref == DSCN_target)
  
  dist_imputed_to_whole <- function(whole,part){
    whole <- resamplecurve(whole,N = dim(part)[2], mode = "C")  
    out <- calc_shape_dist(whole,part,mode="C")
    print("calc_dist")
    return(out)
  }
  
  
  #out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[1]]) #3.183962 minutes with lapply.  #2.110835 with mclapply #With 4 cores:1.751686 minutes
  
  #doesitwork <- list(complete_shape_list[[1]],complete_shape_list[[2]])
  #greg <- lapply(doesitwork, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[m]])
  
  dist_imputed_to_whole2 <- function(part){
    out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part, mc.cores = 10) #takes about 3 minutes.  2.11 minutes with mclapply
    return(out)
  }
  
  start <- Sys.time()
  dist_list <- mclapply(imputed_partial_shape[[1]],dist_imputed_to_whole2, mc.cores = 10)
  end <- Sys.time()
  end-start
  
  dist <- t(do.call(rbind,lapply(dist_list,unlist)))
  
  row.names(dist)<-names(complete_shape_list)
  
  dist <- as.data.frame(dist)
  dist$DSCN <- row.names(dist)
  
  dist <- merge(dist, ref_file, by.x = "DSCN", by.y = "ref", all.x = TRUE)
  
  #Smallest to largest
  #knn <- 5
  # table(as.character(dist$tribe[order(dist$V1)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V2)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V3)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V4)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V5)][1:knn]))
  
  #Classify based on closest match between partial and all full teeth.  
  fret <- mclapply(complete_shape_list,calc_shape_dist_partial,partial_shape = partial_shape)
  dist_partial <- data.frame(DSCN = names(unlist(fret)), dist = unlist(fret))
  dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "ref", all.x = TRUE)
  
  results_list[[DSCN_target]] <- list(dist = dist , dist_partial = dist_partial, truth = truth, imputed_partial_shape = imputed_partial_shape)
}

end_all <- Sys.time()
end_all-start_all
outfile <- paste0("./results/results20190422_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")
save.image(outfile)
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
