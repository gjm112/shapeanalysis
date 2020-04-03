
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
    
    #Which tooth it is
    ggg <- 1
    tooth <- tooth_type_list[[names(tooth_type_list)[ggg]]]
    #Load the actual partial tooth.
    partial_shape  <- read.csv(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images_data/DSCN",names(tooth_type_list)[ggg],"bw.csv"), header = FALSE)
    partial_shape <- as.matrix(partial_shape)
    
    start_stop <- read.csv(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images_data/DSCN",names(tooth_type_list)[ggg],"bwstart_stop.csv"), header = FALSE)
    #points(start_stop[,1],start_stop[,2],pch = 16, col = "red")
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
      partial_shape <- partial_shape[c(which.min(d_end):nrow(partial_shape),1:which.min(d_start)),]
        }
    #check partial shape
    #plot(t(partial_shape))
    
    #Now store it wide rather than long.  
    partial_shape <- t(partial_shape)
    partial_shape <- resamplecurve(partial_shape, 30, mode = "O")
    
    
    
    
    
    
      #partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
      complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
      
      ##complete_shape_list <- list(complete_shape_list[[1]],complete_shape_list[[2]],complete_shape_list[[3]],complete_shape_list[[4]],complete_shape_list[[5]],complete_shape_list[[6]],complete_shape_list[[7]],complete_shape_list[[8]],complete_shape_list[[9]],complete_shape_list[[10]])
      #names(complete_shape_list) <- names(ptsTrainList[[tooth]])[1:10]
      
      #I can't impute the partial shape with itself!
      #complete_shape_list[[d]]<-NULL
      M <- 5
      k <- 5
      scale <- TRUE
      library(parallel)
      start1 <- Sys.time()
      imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k, scale = scale)
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
      print(dist)
      
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
            
            
            
            nam <- paste0(names(tooth_type_list)[ggg],"_M_",M,"_k_",k)
classifier_tribe[[nam]] <- list()
classifier_species[[nam]] <- list()

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
            
            
            
            
###############################################################
#Code from simulations
###############################################################
            
            
            imputed_match_df$true_pred_prob <- NA
            for (i in 1:nrow(imputed_match_df)){
              imputed_match_df$true_pred_prob[i] <- imputed_match_df[i,as.character(imputed_match_df$true[i])] 
            }
            
            #Now for mean imputed shape
            # knn_imputed_mean <- function(DSCN){
            #   temp <- results_list[[DSCN]]$imputed_partial_shape$dist_mean
            #   
            #   temp$Tribe <- factor(temp$Tribe, levels = unique(sort(temp$Tribe)))
            #   
            #   
            #   dat <- data.frame(t(data.frame(c(table(temp$Tribe[order(temp$dist)][1:knn])/knn))))
            #   row.names(dat) <- NULL
            #   dat$true <- results_list[[DSCN]]$truth$tribe[1]
            #   return(dat)
            #   
            # }
            # 
            # imputed_mean_match <- lapply(ids, knn_imputed_mean)
            # imputed_mean_match_df <- do.call(rbind,imputed_mean_match)
            # 
            # imputed_mean_match_df$true_pred_prob <- NA
            # for (i in 1:nrow(imputed_mean_match_df)){
            #   imputed_mean_match_df$true_pred_prob[i] <- imputed_mean_match_df[i,as.character(imputed_mean_match_df$true[i])] 
            # }
            
            #table(imputed_match_df$true_pred_prob, part_match_df$true_pred_prob)
            
            #logloss_imputed_mean[knn] <- mean(-log(imputed_mean_match_df$true_pred_prob+(10^-12)))
            logloss_imputed[knn] <- mean(-log(imputed_match_df$true_pred_prob+(10^-12)))
            logloss_part[knn] <- mean(-log(part_match_df$true_pred_prob+(10^-12)))
            
            #Predict the class for imputed 
            imputed_match_df$pred <- names(imputed_match_df[,1:20])[apply(imputed_match_df[,1:7],1,which.max)]
            reference <- factor(imputed_match_df$true,levels = c('arundinum','buselaphus','caffer','campestris','capreolus','dorcas','ellipsiprymnus','equinus','fulvorufulva','gazella','gnou','leche','marsupialis','niger','oreotragus','oryx','ourebi','scriptus','strepsiceros','taurinus'))
            pred <- factor(imputed_match_df$pred, levels = c('arundinum','buselaphus','caffer','campestris','capreolus','dorcas','ellipsiprymnus','equinus','fulvorufulva','gazella','gnou','leche','marsupialis','niger','oreotragus','oryx','ourebi','scriptus','strepsiceros','taurinus'))
            
            library(caret)
            acc_imputed[knn] <- confusionMatrix(pred,reference)$overall["Accuracy"]
            
            
            #For partial matching 
            part_match_df$pred <- names(part_match_df[,1:20])[apply(part_match_df[,1:7],1,which.max)]
            
            reference <- factor(part_match_df$true,levels = c('arundinum','buselaphus','caffer','campestris','capreolus','dorcas','ellipsiprymnus','equinus','fulvorufulva','gazella','gnou','leche','marsupialis','niger','oreotragus','oryx','ourebi','scriptus','strepsiceros','taurinus'))
            
            pred <- factor(part_match_df$pred, levels = c('arundinum','buselaphus','caffer','campestris','capreolus','dorcas','ellipsiprymnus','equinus','fulvorufulva','gazella','gnou','leche','marsupialis','niger','oreotragus','oryx','ourebi','scriptus','strepsiceros','taurinus'))
            
            library(caret)
            acc_part[knn] <- confusionMatrix(pred,reference)$overall["Accuracy"]
            
            print(acc_imputed)
            print(acc_part)
          }
          
          
          if (!scaled){
            save(list = c("logloss_imputed","logloss_part","imputed_match_df","part_match_df","acc_part","acc_imputed"), 
                 file = paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"_summaries_species.RData"))
          }
          
          if (scaled){
            save(list = c("logloss_imputed","logloss_part","imputed_match_df","part_match_df","acc_part","acc_imputed"), 
                 file = paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled_summaries_species.RData"))
          }
          
          
        }}
      
      # apply(imputed_match_df[,1:20],2,sum)
      # apply(part_match_df[,1:20],2,sum)
      # table(imputed_match_df$true)
      # 
      # sum((apply(part_match_df[,1:20],2,sum) - table(imputed_match_df$true))^2)
      # sum((apply(imputed_match_df[,1:20],2,sum) - table(imputed_match_df$true))^2)
      # 
      # fret <- data.frame(logloss = c(logloss_imputed,logloss_part), k = c(1:20,1:20), method = c(rep("knn w Imputation",20),rep("knn no imputation",20)))
      # library(ggplot2)
      # ggplot(aes(x = k, y = logloss, col = method), data = fret) +  geom_point() + geom_line()
      # 
      # 
      # #Predict the class
      # imputed_match_df$pred <- names(imputed_match_df[,1:20])[apply(imputed_match_df[,1:20],1,which.max)]
      # table(factor(imputed_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )),factor(imputed_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )))
      # 
      # part_match_df$pred <- names(part_match_df[,1:7])[apply(part_match_df[,1:7],1,which.max)]
      # table(factor(part_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )),factor(part_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )))
      # 
      # plot(imputed_match_df$true_pred_prob,part_match_df$true_pred_prob, xlim = c(0,1), ylim = c(0,1))
      # 
      # #imputed_mean_match_df$pred <- names(imputed_mean_match_df[,1:7])[apply(imputed_mean_match_df[,1:7],1,which.max)]
      # #table(factor(imputed_mean_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )),factor(imputed_mean_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )))
      # 
      # #ROC analysis 
      # 
      # for (q in c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")){
      #   temp <- imputed_match_df
      #   tribe <- q
      #   temp$true <- as.character(temp$true)
      #   temp$true[(temp$true) != tribe] <- 0
      #   temp$true[(temp$true) == tribe] <- 1
      #   
      #   library(ROCR)
      #   pred <- prediction(temp[[tribe]], as.numeric(temp$true))
      #   perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
      #   plot(perf, col="red", main = tribe)
      #   
      #   temp <- part_match_df
      #   temp$true <- as.character(temp$true)
      #   temp$true[(temp$true) != tribe] <- 0
      #   temp$true[(temp$true) == tribe] <- 1
      #   
      #   library(ROCR)
      #   pred <- prediction(temp[[tribe]], as.numeric(temp$true))
      #   perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
      #   plot(perf, col="blue", add = TRUE, lty = 3)
      #   
      # }
      # 
      # 
      # library(geometry)
      # geometry::polyarea(ptsTrainList[[1]][[2]][,1],ptsTrainList[[1]][[2]][,2])
      
      
      
      
#       if (scale){outfile <- paste0("./results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled.RData")}
#       if (!scale){outfile <- paste0("./results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")}
#       save.image(outfile)
#     }
#     
#   }
# }
# 
# # end_all <- Sys.time()
# # end_all-start_all
# # outfile <- paste0("./results/results20190424_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData")
# # save.image(outfile)
# #table(as.character(dist_partial$tribe[order(dist_partial$dist)][1:10]))
# 
# 
# # #take top 10 closest? 
# # index <- order(dist)[1]
# # dscn <- names(complete_shape_list)[index]
# # subset(ref_file,ref %in% dscn)
# # subset(ref_file,ref == "DSCN2879")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # plot(t(test$imputed[[1]]),type="l", col = rgb(0,0,0,0), xlim = c(-0.4, 0.15), ylim = c(-0.2,0.2), lwd = 3)
# # points(t(red$donor),col = "red", type = "l",lwd=1, lty = 3)
# # set.seed(1234)
# # for (i in 1:25){
# #   u <- runif(3)
# #   points(t(test$imputed[[i]]),type="l",col =  rgb(u[1],u[2],u[3],0.5), lwd = 3)
# # }
# # red <- complete_partial_shape(t(ptsTrainList[[1]][[1]]),t(ptsTrainList[[1]][[1]][11:42,]))
# # 
# # arr <- array(0, dim = c(2,199,25))
# # for (i in 1:25){
# #   arr[,,i] <- test$imputed[[i]]
# # }
# # 
# # mn_shape <- apply(arr,c(1,2),mean)
# # points(t(mn_shape), type = "l", col = rgb(0.1,0.2,0.7), lwd = 2)
# # 
# # imputed<-test
# # out<-list(complete_shape_list,imputed)
# # save(out,file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/example_data_for_sebastian.RData")
# # 
# # write.csv(do.call(rbind,complete_shape_list), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/complete_shape_list_for_sebastian.csv" )
# # write.csv(do.call(rbind,test$imputed), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/completed_tooth_for_sebastian.csv" )
# # 
# # 