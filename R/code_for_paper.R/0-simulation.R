start_all <- Sys.time()
library(fdasrvf)
library(parallel)
########################################################
#For a combination of M, k and scaling, this runs the simulation for all 6 tooth types and splitting on both sides.  This code was run on a server in parallel for three values of M (5, 10, and 20), three values of k (5, 10, and 20), and two scale settings (TRUE and FALSE).
########################################################
M <- 5
k <- 5
scale <- TRUE

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(tooth)
  for (side in 1:2){print(side)
    
    
    source("./R/utility.R")
    source("./R/curve_functions.R")
    source("./R/calc_shape_dist_partial.R")
    source("./R/calc_shape_dist_complete.R")
    source("./R/complete_partial_shape.R")
    source("./R/impute_partial_shape.R")
    source("./R/tooth_cutter.R")
    
    ref_file <- read.csv("./data/reference_db.csv")
    load("./data/ptsTrainList.RData")
    
    
    #Need a function that takes each partial tooth as an argument to get to parallel.  
    results_list <- list()
    for (d in 1:length(ptsTrainList[[tooth]])){
      print(d)
      print(Sys.time())
      partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[d]])[[side]])
      complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
      
     
      #I can't impute the partial shape with itself!
      complete_shape_list[[d]]<-NULL
      
      start1 <- Sys.time()
      imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k, scale = scale)
      end1 <- Sys.time()
      end1-start1 #1.4 minutes with 4 cores on server.  Using detectCores()-1 it takes 
      
     
      
      
      #Now do classification on the completed shapes just using closest 
      ref_file <- read.csv("./data/reference_db.csv")
      DSCN_target <- names(ptsTrainList[[tooth]])[[d]]
      truth <- subset(ref_file,Image.Name == DSCN_target)
      
      
      
      dist_imputed_to_whole <- function(whole,part, scale = scale){
        whole <- resamplecurve(whole,N = dim(part)[2], mode = "C") 
        print(Sys.time())
        out <- calc_shape_dist_complete(whole,part, scale)
        return(out)
      }
      
      
      
      
      dist_imputed_to_whole2 <- function(part){
        out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part, scale = scale, mc.cores = 12) #takes about 3 minutes.  2.11 minutes with mclapply
        return(out)
      }
      
      start <- Sys.time()
      dist_list <- lapply(imputed_partial_shape$imputed,dist_imputed_to_whole2)
      end <- Sys.time()
      end-start
      
      
      dist <- t(do.call(rbind,lapply(dist_list,unlist)))
      
      row.names(dist) <- names(complete_shape_list)
      
      dist <- as.data.frame(dist)
      dist$DSCN <- row.names(dist)
      
     
      dist <- merge(dist, ref_file, by.x = "DSCN", by.y = "Image.Name", all.x = TRUE)
      
      
      #Classify based on closest match between partial and all full teeth.  
      dist_partial <- data.frame(DSCN = names(unlist(imputed_partial_shape$dist_vec)), dist = unlist(imputed_partial_shape$dist_vec))
      dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "Image.Name", all.x = TRUE)
      
      results_list[[DSCN_target]] <- list(dist = dist , dist_partial = dist_partial, truth = truth, imputed_partial_shape = imputed_partial_shape)
      print(dist)
      
      end_all <- Sys.time()
      end_all-start_all
      if (scale){outfile <- paste0("./results/results20200610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled.RData")}
      if (!scale){outfile <- paste0("./results/results20200610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")}
      save.image(outfile)
    }
    
  }
}

