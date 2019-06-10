###  nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script2.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script2.Rout

### nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script1.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script1.Rout

### nohup R CMD BATCH --vanilla /home/gmatthew/Work/shapeanalysis/R/simulation_script_for_server.R /home/gmatthew/Work/shapeanalysis/simulation_script_for_server_side1.Rout &
### tail -f /home/gmatthew/Work/shapeanalysis/simulation_script_for_server_side2.Rout

### nohup R CMD BATCH --vanilla R/simulation_script_for_server.R simulation_script_for_server_side2.Rout &
### tail -f simulation_script_for_server_side2.Rout

# chmod +x /home/gmatthew/Work/shapeanalysis/shape_script.sh
# qsub -A SE_HPC -t 720 -n 1 -q pubnet /home/gmatthew/Work/shapeanalysis/shape_script.sh



start_all <- Sys.time()
library(fdasrvf)
library(parallel)

M <- 5
k <- 5
side <- 1 #could be 1 or 2..
tooth <- "UM2"

#file <- paste0("./results/results20190525_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")
#load(file)
#/home/gmatthews1/shapeAnalysis

#setwd("/home/gmatthews1/shapeAnalysis")
source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/calc_shape_dist_complete.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")

ref_file <- read.csv("./data/reference_db.csv")
load("./data/data_set_of_full_teeth.RData")
load("./data/ptsTrainList.RData")
#save(ptsTrainList, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
#i <- 1 #Whcih tooth.  DSCN number 

#Need a function that takes each partial tooth as an argument to get to parallel.  
results_list <- list()
for (d in 1:length(ptsTrainList[[tooth]])){
#for (d in (length(results_list)+1):length(ptsTrainList[[tooth]])){
  #for (d in 1:1){
  print(d)
  print(Sys.time())
  partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[d]])[[side]])
  #partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
  complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
  
  ##complete_shape_list <- list(complete_shape_list[[1]],complete_shape_list[[2]],complete_shape_list[[3]],complete_shape_list[[4]],complete_shape_list[[5]],complete_shape_list[[6]],complete_shape_list[[7]],complete_shape_list[[8]],complete_shape_list[[9]],complete_shape_list[[10]])
  #names(complete_shape_list) <- names(ptsTrainList[[tooth]])[1:10]
  
  #I can't impute the partial shape with itself!
  complete_shape_list[[d]]<-NULL
  
  start1 <- Sys.time()
  imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k)
  end1 <- Sys.time()
  end1-start1 #1.4 minutes with 4 cores on server.  Using detectCores()-1 it takes 
  
#   colMeans(ptsTrainList[[tooth]][[d]])
#   
# plot(t(imputed_partial_shape$imputed[[1]]), xlim = c(-120, 500), ylim = c(-170,210))
# points(t(imputed_partial_shape$imputed[[2]]))
# points(t(imputed_partial_shape$imputed[[3]]))
# points(t(imputed_partial_shape$imputed[[4]]))
# points(t(imputed_partial_shape$imputed[[5]]))
# points(t(imputed_partial_shape$imputed[[5]]),col = "red")
# points(t(imputed_partial_shape$imputed[[4]]), col = "blue")
# points(t(beta1+c(150,0)),col = "gold", type = "l", lwd = 3)
# 
# beta1 <- t(ptsTrainList[[tooth]][[d]])
#   T1 = ncol(beta1)
#   centroid1 = calculatecentroid(beta1)
#   dim(centroid1) = c(length(centroid1),1)
#   beta1 = beta1 - repmat(centroid1, 1, T1)
  
  
  #Now do classification on the completed shapes just using closest 
  ref_file <- read.csv("./data/reference_db.csv")
  DSCN_target <- names(ptsTrainList[[tooth]])[[d]]
  truth <- subset(ref_file,Image.Name == DSCN_target)
  
  # ref_file[ref_file$tooth == "LM1",]
  # whole <- complete_shape_list[["DSCN2879"]]
  # part <- imputed_partial_shape$imputed[[1]]
  
  
  
  dist_imputed_to_whole <- function(whole,part){
    whole <- resamplecurve(whole,N = dim(part)[2], mode = "C") 
    print(Sys.time())
    out <- calc_shape_dist_complete(whole,part)
    return(out)
  }
  
  
  #out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[1]]) #3.183962 minutes with lapply.  #2.110835 with mclapply #With 4 cores:1.751686 minutes
  
  #doesitwork <- list(complete_shape_list[[1]],complete_shape_list[[2]])
  #greg <- lapply(doesitwork, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[m]])
  
  dist_imputed_to_whole2 <- function(part){
    #out <- lapply(complete_shape_list, dist_imputed_to_whole, part = part) #takes about 3 minutes.  2.11 minutes with mclapply
    out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part, mc.cores = 12) #takes about 3 minutes.  2.11 minutes with mclapply
    return(out)
  }
  
  start <- Sys.time()
  dist_list <- lapply(imputed_partial_shape$imputed,dist_imputed_to_whole2)
  end <- Sys.time()
  end-start
  
  print(Sys.time())
  
  dist <- t(do.call(rbind,lapply(dist_list,unlist)))
  
  row.names(dist) <- names(complete_shape_list)
  
  dist <- as.data.frame(dist)
  dist$DSCN <- row.names(dist)
  
  ################################################################################################################################
  # whole <- resamplecurve(whole,N = dim(part)[2], mode = "C")  
  # out <- calc_shape_dist(whole,part,mode="C")
  # 
  # 
  # 
  # calc_shape_dist(whole,imputed_partial_shape$imputed[[3]], mode = "C")
  # 
  # part <- imputed_partial_shape$imputed[[1]]
  # whole <- resamplecurve(t(ptsTrainList[[1]][["DSCN5630"]]),N = dim(imputed_partial_shape$imputed[[1]])[2], mode = "C")  
  # whole <- resamplecurve(t(ptsTrainList[[1]][["DSCN2879"]]),N = dim(imputed_partial_shape$imputed[[1]])[2], mode = "C")  
  # calc_shape_dist(whole,part,mode="C")
  # 
  # plot(t(whole))
  # points(t(part+c(150,-100)))
  ################################################################################################################################
  
  dist <- merge(dist, ref_file, by.x = "DSCN", by.y = "Image.Name", all.x = TRUE)
  
  #Smallest to largest
  #knn <- 5
  # table(as.character(dist$tribe[order(dist$V1)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V2)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V3)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V4)][1:knn]))
  # table(as.character(dist$tribe[order(dist$V5)][1:knn]))
  
  #Classify based on closest match between partial and all full teeth.  
  #fret <- mclapply(complete_shape_list,calc_shape_dist_partial,partial_shape = partial_shape)
  #dist_partial <- data.frame(DSCN = names(unlist(fret)), dist = unlist(fret))
  dist_partial <- data.frame(DSCN = names(unlist(imputed_partial_shape$dist_vec)), dist = unlist(imputed_partial_shape$dist_vec))
  
  dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "Image.Name", all.x = TRUE)
  
  results_list[[DSCN_target]] <- list(dist = dist , dist_partial = dist_partial, truth = truth, imputed_partial_shape = imputed_partial_shape)
  print(dist)
  
  end_all <- Sys.time()
  end_all-start_all
  outfile <- paste0("./results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")
  save.image(outfile)
}

# end_all <- Sys.time()
# end_all-start_all
# outfile <- paste0("./results/results20190424_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")
# save.image(outfile)
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
# imputed<-test
# out<-list(complete_shape_list,imputed)
# save(out,file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/example_data_for_sebastian.RData")
# 
# write.csv(do.call(rbind,complete_shape_list), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/complete_shape_list_for_sebastian.csv" )
# write.csv(do.call(rbind,test$imputed), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/completed_tooth_for_sebastian.csv" )
# 
# 