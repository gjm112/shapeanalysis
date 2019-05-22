###  nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script2.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script2.Rout

### nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script1.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script1.Rout

###Rscript --verbose /Users/gregorymatthews/Dropbox/shapeanalysisgit/R/simulation_script_for_server_parameterized.R 1 > /Users/gregorymatthews/Dropbox/shapeanalysisgit/check.Rout
###R CMD BATCH '--args d=2' /Users/gregorymatthews/Dropbox/shapeanalysisgit/R/simulation_script_for_server_parameterized.R & 

##First read in the arguments listed at the command line
args=(commandArgs(TRUE))

##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  #a = 1
  #b = c(1,1,1)
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
print(d)

start_all <- Sys.time()
library(fdasrvf)
library(parallel)

args <- commandArgs(trailingOnly = TRUE)
M <- 5
k <- 5
side <- 1 #could be 1 or 2.
tooth <- "LM1"
#d <- 1
date <- 20190522 #d is the number of tooth 
#/home/gmatthews1/shapeAnalysis

#setwd("/home/gmatthews1/shapeAnalysis")
setwd("/Users/gregorymatthews/Dropbox/shapeanalysisgit/")
source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")

ref_file <- read.csv("./data/refFile.csv")
load("./data/data_set_of_full_teeth.RData")
load("./data/ptsTrainList.RData")
#save(ptsTrainList, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
#i <- 1 #Whcih tooth.  DSCN number 

#Need a function that takes each partial tooth as an argument to get to parallel.  
results_list <- list()
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
  ref_file <- read.csv("./data/refFile.csv")
  DSCN_target <- names(ptsTrainList[["LM1"]])[[d]]
  truth <- subset(ref_file,ref == DSCN_target)
  
  # ref_file[ref_file$tooth == "LM1",]
  # whole <- complete_shape_list[["DSCN2879"]]
  # part <- imputed_partial_shape$imputed[[1]]
  
  
  
  dist_imputed_to_whole <- function(whole,part){
    whole <- resamplecurve(whole,N = dim(part)[2], mode = "C") 
    print(Sys.time())
    out <- calc_shape_dist(whole,part,mode="C")
    return(out)
  }
  
  
  #out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[1]]) #3.183962 minutes with lapply.  #2.110835 with mclapply #With 4 cores:1.751686 minutes
  
  #doesitwork <- list(complete_shape_list[[1]],complete_shape_list[[2]])
  #greg <- lapply(doesitwork, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[m]])
  
  dist_imputed_to_whole2 <- function(part){
    out <- lapply(complete_shape_list, dist_imputed_to_whole, part = part) #takes about 3 minutes.  2.11 minutes with mclapply
    #out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part) #takes about 3 minutes.  2.11 minutes with mclapply
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
  
  dist <- merge(dist, ref_file, by.x = "DSCN", by.y = "ref", all.x = TRUE)
  
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
  
  dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "ref", all.x = TRUE)
  
  results_list[[DSCN_target]] <- list(dist = dist , dist_partial = dist_partial, truth = truth, imputed_partial_shape = imputed_partial_shape)
  print(dist)
  
  end_all <- Sys.time()
  end_all-start_all
  outfile <- paste0("./results/results",date,"_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"_d=",d,"_of_",length(ptsTrainList[[tooth]]),".RData")
  save.image(outfile)

