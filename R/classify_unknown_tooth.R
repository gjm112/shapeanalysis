
###  nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script2.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script2.Rout

### nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script1.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script1.Rout

### nohup R CMD BATCH --vanilla /home/gmatthew/Work/shapeanalysis/R/simulation_script_for_server.R /home/gmatthew/Work/shapeanalysis/simulation_script_for_server_side1.Rout &
### tail -f /home/gmatthew/Work/shapeanalysis/simulation_script_for_server_side2.Rout

# nohup R CMD BATCH --vanilla R/simulation_script_for_server.R simulation_script_for_server_side2.Rout &
# tail -f simulation_script_for_server_side2.Rout

# chmod +x /home/gmatthew/Work/shapeanalysis/shape_script.sh
# qsub -A SE_HPC -t 720 -n 1 -q pubnet /home/gmatthew/Work/shapeanalysis/shape_script.sh

#!/usr/bin/bash

#nohup R CMD BATCH --vanilla /home/gmatthews1/Work/shapeanalysis/R/simulation_script_for_server_LM2_side2_20190610_k5_M5_scaled.R /home/gmatthews1/Work/shapeanalysis/simulation_script_for_server_LM2_side2_20190610_k_M5_scaled.Rout

# chmod +x /home/gmatthew/Work/shapeanalysis/shape_script_LM1_1_k10_M10_scaled.sh
# qsub -A SE_HPC -t 720 -n 1 -q pubnet /home/gmatthew/Work/shapeanalysis/shape_script_LM1_1_k10_M10_scaled.sh


# cd /home/gmatthews1/Work/shapeanalysis
# nohup R CMD BATCH --vanilla /home/gmatthews1/Work/shapeanalysis/R/simulation_script_for_server_LM2_side2_20190610_k5_M5scaled.R /home/gmatthews1/Work/shapeanalysis/R/simulation_script_for_server_LM2_side2_20190610_k5_M5scaled.Rout &


# 4825: UM1
# 4980: LM3
# 4983: LM3
# 4990: LM3
# 5139:LM3
# 9973: LM3
# 5514: maxillary molar.. probably UM2 but could me UM1. 
# This is what I asked you about the other day if you could tell the tooth type. It is  just a single lobe and in 2009 I did not look hard enough. I could likely tell you now but I did not look at other features at the time


classifier_tribe <- list()
classifier_species <- list()
classifier_imputations <- list()


#These tooth type classifications are from Juliet
tooth_type_list <- list()
tooth_type_list[["4825"]] <- "UM1"
tooth_type_list[["4980"]] <- "LM3"
tooth_type_list[["4983"]] <- "LM3"
tooth_type_list[["4990"]] <- "LM3"
tooth_type_list[["5139"]] <- "LM3"
tooth_type_list[["9973"]] <- "LM3"
tooth_type_list[["5514"]] <- "UM2"


start_all <- Sys.time()
library(fdasrvf)
library(parallel)

    
    #setwd("/home/gmatthews1/shapeAnalysis")
    source("./R/utility.R")
    source("./R/curve_functions.R")
    source("./R/calc_shape_dist_partial.R")
    source("./R/calc_shape_dist_complete.R")
    source("./R/complete_partial_shape.R")
    source("./R/impute_partial_shape.R")
    source("./R/tooth_cutter.R")
    
    #Loading in the full teeth
    ref_file <- read.csv("./data/reference_db.csv")
    load("./data/data_set_of_full_teeth.RData")
    load("./data/ptsTrainList.RData")
    #save(ptsTrainList, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
        
    
    #partial_shape2 <- t(tooth_cutter(ptsTrainList[[tooth]][[d]])[[side]])
    
    #Note: Traveling from start to stop should always be in a clowckwise direction!
    
    #Which tooth it is
    for (ggg in 1:length(tooth_type_list)){print(ggg)
    tooth <- tooth_type_list[[names(tooth_type_list)[ggg]]]
    #Load the actual partial tooth.
    partial_shape  <- read.csv(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images_data/DSCN",names(tooth_type_list)[ggg],"bw.csv"), header = FALSE)
    partial_shape <- as.matrix(partial_shape)
    
    start_stop <- read.csv(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images_data/DSCN",names(tooth_type_list)[ggg],"bwstart_stop.csv"), header = FALSE)
    #points(start_stop[1,1],start_stop[1,2],pch = 16, col = "green")
    #points(start_stop[2,1],start_stop[2,2],pch = 16, col = "red")
    start_stop <- as.matrix(start_stop)
    
    #Ok now cut off the part i don't need.  
    start <- start_stop[1,]
    stop <- start_stop[2,]
    
    #Measure distance between start and all points
    d_start <- (partial_shape[,1] - start[1])^2 + (partial_shape[,2] - start[2])^2 
    
    d_end <- (partial_shape[,1] - stop[1])^2 + (partial_shape[,2] - stop[2])^2 
    
    
    
    if(which.min(d_start) < which.min(d_end)){
      partial_shape <- partial_shape[which.min(d_start):which.min(d_end),]
    } else {
      partial_shape <- partial_shape[c(which.min(d_start):nrow(partial_shape),1:which.min(d_end)),]
        }
    #check partial shape
    #plot((partial_shape))
    #points(start_stop[1,1],start_stop[1,2], col = "green", pch = 16)
    #points(start_stop[2,1],start_stop[2,2], col = "red", pch = 16)
    
    
    #Now store it wide rather than long.  
    partial_shape <- t(partial_shape)
    #Now resample it to 250 points 
    partial_shape <- resamplecurve(partial_shape, 40, mode = "O")
    
    #Remember N_partial must be less than or equal to N_complete
    
    
    
    
    
    
    
      #partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
      complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
      
      
      #Resampling so that each complete shape has N points
      #complete_shape_list <- lapply(complete_shape_list, resamplecurve, N = 250, mode = "C")
      
      
      ##complete_shape_list <- list(complete_shape_list[[1]],complete_shape_list[[2]],complete_shape_list[[3]],complete_shape_list[[4]],complete_shape_list[[5]],complete_shape_list[[6]],complete_shape_list[[7]],complete_shape_list[[8]],complete_shape_list[[9]],complete_shape_list[[10]])
      #names(complete_shape_list) <- names(ptsTrainList[[tooth]])[1:10]
      
      #I can't impute the partial shape with itself!
      #complete_shape_list[[d]]<-NULL
      # M <- 5
      # k <- 5
      # scale <- TRUE
      for (M in c(20)){print(paste0("M = ",M))
          for (k in c(20)){ print(paste0("k = ",M))
            for (scale in c(TRUE,FALSE)){ print(paste0("scale = ",scale))
      library(parallel)
      start1 <- Sys.time()
      imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k, scale = scale)
      end1 <- Sys.time()
      end1-start1 #1.4 minutes with 4 cores on server.  Using detectCores()-1 it takes 
      
      plot(t(imputed_partial_shape$imputed[[4]]), col = "red")
      points(t(imputed_partial_shape$partial_obs), col = "blue")
      
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
    #DSCN_target <- names(ptsTrainList[[tooth]])[[d]]
     # truth <- subset(ref_file,Image.Name == DSCN_target)
      
      # ref_file[ref_file$tooth == "LM1",]
      # whole <- complete_shape_list[["DSCN2879"]]
      # part <- imputed_partial_shape$imputed[[1]]
      
      
      
      dist_imputed_to_whole <- function(whole,part, scale = scale){
        whole <- resamplecurve(whole,N = dim(part)[2], mode = "C") 
        print(Sys.time())
        out <- calc_shape_dist_complete(whole,part, scale)
        return(out)
      }
      
      
      #out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[1]]) #3.183962 minutes with lapply.  #2.110835 with mclapply #With 4 cores:1.751686 minutes
      
      #doesitwork <- list(complete_shape_list[[1]],complete_shape_list[[2]])
      #greg <- lapply(doesitwork, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[m]])
      
      dist_imputed_to_whole2 <- function(part){
        #out <- lapply(complete_shape_list, dist_imputed_to_whole, part = part) #takes about 3 minutes.  2.11 minutes with mclapply
        out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part, scale = scale, mc.cores = 12) #takes about 3 minutes.  2.11 minutes with mclapply
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
      
      results <- list(dist = dist , dist_partial = dist_partial, imputed_partial_shape = imputed_partial_shape)
      
      
      end_all <- Sys.time()
      end_all-start_all
      
      
      knn_partial_matching <- function(knn){
        temp <- results$dist_partial
        temp$inv_dist <- 1/temp$dist
        
        temp$Tribe <- factor(temp$Tribe, levels = unique(sort(temp$Tribe)))
        
        #Compute probabilities
        #library(dplyr)
        #probs <- arrange(temp,dist) %>% top_n(knn) %>% group_by(Tribe) %>% summarise(tot = sum(inv_dist))
        #probs$prob <- probs$tot / sum(probs$tot)
        
        
        dat <- data.frame(t(data.frame(c(table(temp$Tribe[order(temp$dist)][1:knn])/knn))))
        
        #Weighted KNN
        #wts <- c(table(temp$Tribe[order(temp$dist)][1:knn])/table(temp$Tribe))
        #dat <- data.frame(t(data.frame((wts/sum(wts)))))
        row.names(dat) <- NULL
        return(dat)
      }
      
      
     
      #plot(ptsTrainList[[1]][["DSCN2879"]])
      #plot(t(results_list[[DSCN]]$imputed_partial_shape$imputed[[1]]))
      
      #DSCN <- "DSCN2871"
      #temp <- results_list[[DSCN]]$dist
      #temp[order(temp[[paste0("V",i)]]),]
      
      #full <- resamplecurve(t(ptsTrainList[[1]][["DSCN3753"]]),199)
      #calc_shape_dist(full,(results_list[[DSCN]]$imputed_partial_shape$imputed[[4]]))
      #1199.961
      
      #Now for the imputed teeth
      knn_imputed <- function(knn){
        temp <- results$dist
        
        temp$Tribe <- factor(temp$Tribe, levels = unique(sort(temp$Tribe)))
        
        dat_list <- list()
        for (i in 1:M){
          pro <- data.frame(t(data.frame(c(table(temp$Tribe[order(temp[[paste0("V",i)]])][1:knn])/knn))))
          
          #Weighted KNN
          #wts <- c(table(temp$Tribe[order(temp[[paste0("V",i)]])][1:knn])/table(temp$Tribe))
          #pro <- data.frame(t(data.frame((wts/sum(wts)))))
          row.names(pro) <- NULL
          dat_list[[i]] <- pro
        }
        df <- do.call(rbind,dat_list)
        dat <- data.frame(t(data.frame(unlist(apply(df,2,mean)))))
        row.names(dat) <- NULL
        return(dat)
      }
      
      
      
      
      
    #Now classify Species
            knn_partial_matching_species <- function(knn){
              temp <- results$dist_partial
              
              temp$Species <- factor(temp$Species, levels = unique(sort(temp$Species)))
              
              dat <- data.frame(t(data.frame(c(table(temp$Species[order(temp$dist)][1:knn])/knn))))
              
              #Weighted KNN
              #wts <- c(table(temp$Tribe[order(temp$dist)][1:knn])/table(temp$Tribe))
              #dat <- data.frame(t(data.frame((wts/sum(wts)))))
               row.names(dat) <- NULL
              # dat$true <- results_list[[DSCN]]$truth$Species[1]
              # dat$DSCN <- DSCN
              return(dat)
            }
            
           
            
            #plot(ptsTrainList[[1]][["DSCN2879"]])
            #plot(t(results_list[[DSCN]]$imputed_partial_shape$imputed[[1]]))
            
            #DSCN <- "DSCN2871"
            #temp <- results_list[[DSCN]]$dist
            #temp[order(temp[[paste0("V",i)]]),]
            
            #full <- resamplecurve(t(ptsTrainList[[1]][["DSCN3753"]]),199)
            #calc_shape_dist(full,(results_list[[DSCN]]$imputed_partial_shape$imputed[[4]]))
            #1199.961
            
            #Now for the imputed teeth
            knn_imputed_species <- function(knn){
              temp <- results$dist
              
              temp$Species <- factor(temp$Species, levels = unique(sort(temp$Species)))
              
              dat_list <- list()
              for (i in 1:M){
                pro <- data.frame(t(data.frame(c(table(temp$Species[order(temp[[paste0("V",i)]])][1:knn])/knn))))
                
                #Weighted KNN
                #wts <- c(table(temp$Tribe[order(temp[[paste0("V",i)]])][1:knn])/table(temp$Tribe))
                #pro <- data.frame(t(data.frame((wts/sum(wts)))))
                row.names(pro) <- NULL
                dat_list[[i]] <- pro
              }
              df <- do.call(rbind,dat_list)
              dat <- data.frame(t(data.frame(unlist(apply(df,2,mean)))))
              row.names(dat) <- NULL
              # dat$true <- results_list[[DSCN]]$truth$Species[1]
              # dat$DSCN <- DSCN
              return(dat)
            }
            
            
            
            nam <- paste0(names(tooth_type_list)[ggg],"_M_",M,"_k_",k,"_scale_",scale)
            
classifier_tribe[[nam]] <- list()
classifier_species[[nam]] <- list()


#10 rows are because I'm uasing different choices of knn.  
classifier_tribe[[nam]]$partial_matching <- matrix(NA, nrow = 10, ncol = 7)
classifier_tribe[[nam]]$imputed <- matrix(NA, nrow = 10, ncol = 7)

classifier_species[[nam]]$partial_matching <- matrix(NA, nrow = 10, ncol = 20)
classifier_species[[nam]]$imputed <- matrix(NA, nrow = 10, ncol = 20)


     
              for (i_knn in 1:10){
            #Tribe classification 
            classifier_tribe[[nam]]$partial_matching[i_knn,] <- unlist(knn_partial_matching(i_knn))
            classifier_tribe[[nam]]$imputed[i_knn,] <- unlist(knn_imputed(i_knn))
            #Species classification
            classifier_species[[nam]]$partial_matching[i_knn,] <- unlist(knn_partial_matching_species(i_knn))
            classifier_species[[nam]]$imputed[i_knn,] <- unlist(knn_imputed_species(i_knn))
              }
            


classifier_tribe[[nam]]$partial_matching <- as.data.frame(classifier_tribe[[nam]]$partial_matching)
classifier_tribe[[nam]]$imputed <- as.data.frame(classifier_tribe[[nam]]$imputed)

names(classifier_tribe[[nam]]$partial_matching) <- names(classifier_tribe[[nam]]$imputed) <- c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")


classifier_species[[nam]]$partial_matching <- as.data.frame(classifier_species[[nam]]$partial_matching)
classifier_species[[nam]]$imputed <- as.data.frame(classifier_species[[nam]]$imputed)

names(classifier_species[[nam]]$partial_matching) <- names(classifier_species[[nam]]$imputed) <- names(unlist(knn_imputed_species(i_knn)))
            

#Store the actual imputations
classifier_imputations[[nam]] <- results

print(classifier_tribe)
print(classifier_species)
save(classifier_tribe, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/classifier_tribe.RData")
save(classifier_species, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/classifier_species.RData")
save(classifier_imputations, file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/classifier_imputations.RData")

            }}}
      

    }
    
    
    
  
#Results tables. 
tab <- classifier_tribe[[1]]$partial_matching[5,]
for (i in 2:length(classifier_tribe)){
  tab <- rbind(tab,classifier_tribe[[i]]$partial_matching[5,])
}  

library(xtable)
xtable(tab, caption = "here")

#Results tables. 
tab_imp <- classifier_tribe[[1]]$imputed[5,]
for (i in 2:length(classifier_tribe)){
  tab_imp <- rbind(tab_imp,classifier_tribe[[i]]$imputed[5,])
}  

row.names(tab_imp) <- paste0("IMG",names(tooth_type_list))
xtable(tab_imp, caption = "here")

#Species classifier
#Results tables. 
tab_species <- classifier_species[[1]]$partial_matching[5,]
for (i in 2:length(classifier_species)){
  tab_species <- rbind(tab_species,classifier_species[[i]]$partial_matching[5,])
}  

row.names(tab_species) <- names(tooth_type_list)
keep <- apply(tab_species,2,function(x){sum(x)>0})
tab_species[,keep]

library(xtable)
xtable(tab_species, caption = "here")

#Results tables. 
tab_species_imp <- classifier_species[[1]]$imputed[5,]
for (i in 2:length(classifier_species)){
  tab_species_imp <- rbind(tab_species_imp,classifier_species[[i]]$imputed[5,])
}  

row.names(tab_species_imp) <- paste0("IMG",names(tooth_type_list))
keep <- apply(tab_species_imp,2,function(x){sum(x)>0})
tab_species_imp[,keep]

xtable(tab_species_imp, caption = "here")

#Plots of completed shapes
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/IMG4825_imputed.png", h = 5, w = 8, res = 300, units = "in")
plot(t(classifier_imputations[[1]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-450,250), ylim = c(-250, 250), xlab = "", ylab = "", main = "IMG4825 - LM1")  
for (i in 1:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
points(t(classifier_imputations[[1]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
}
points(t(classifier_imputations[[1]]$imputed_partial_shape$partial_obs), col = "black", type = "l") 
dev.off()

#IMG2
plot(t(classifier_imputations[[2]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-450,660), ylim = c(-250, 250), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[2]," - LM3"))
for (i in 1:length(classifier_imputations[[2]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[2]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
}
points(t(classifier_imputations[[2]]$imputed_partial_shape$partial_obs), col = "black", type = "l") 



#IMG3
plot(t(classifier_imputations[[3]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-450,500), ylim = c(-250, 250), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[3]," - LM3"))
for (i in 1:length(classifier_imputations[[3]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[3]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
}
points(t(classifier_imputations[[3]]$imputed_partial_shape$partial_obs), col = "black", type = "l")

#IMG4
plot(t(classifier_imputations[[4]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-1050,500), ylim = c(-250, 450), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[4]," - LM3"))
for (i in 1:length(classifier_imputations[[4]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[4]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
}
points(t(classifier_imputations[[4]]$imputed_partial_shape$partial_obs), col = "black", type = "l")

#IMG5
plot(t(classifier_imputations[[5]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-250,600), ylim = c(-250, 250), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[5]," - LM3"))
for (i in 1:length(classifier_imputations[[5]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[5]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
}
points(t(classifier_imputations[[5]]$imputed_partial_shape$partial_obs), col = "black", type = "l") 



