#This code summarizes the results

# #Read in the reference file.
# library(googlesheets)
library(dplyr)
# greg <- gs_ls()
# bovids <- gs_url("https://docs.google.com/spreadsheets/d/1KGkTVz5IVuBdtQie0QBdeHwyHVH41MjFdbpluFsDX6k/edit#gid=963640939")
# bovids.df <- bovids %>% gs_read(ws = 1)
# subset(bovids.df, `Tooth Type` == "LM1")
# 

########################################################
#For a combination of M, k and scaling, this summarizes the results of the simulation for classifying tribe for all 6 tooth types and both sides.  The summary files created here are then used to create the plots and figures in the manuscript.  
########################################################
M <- 20
k <- 20
scaled <- TRUE

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){print(c(tooth,side))
    
    
    if (!scaled){load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData"))}
    if (scaled){load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled.RData"))}
    
    
    logloss_imputed <- c() 
    logloss_part <- c()
    acc_imputed <- acc_part <- acc_strong_imputed <- acc_strong_part <- c()
    for (knn in c(1:4,6:20,30,40,50,60,5)){print(knn)
      
      ids <- names(results_list)
      
      knn_partial_matching <- function(DSCN){
        temp <- results_list[[DSCN]]$dist_partial
        temp$inv_dist <- 1/temp$dist
        
        temp$Tribe <- factor(temp$Tribe, levels = unique(sort(temp$Tribe)))
        
       
        
        
        dat <- data.frame(t(data.frame(c(table(temp$Tribe[order(temp$dist)][1:knn])/knn))))
        
       
        row.names(dat) <- NULL
        dat$true <- results_list[[DSCN]]$truth$Tribe[1]
        dat$DSCN <- DSCN
        return(dat)
      }
      
      part_match <- lapply(ids, knn_partial_matching)
      part_match_df <- do.call(rbind,part_match)
      
      part_match_df$true_pred_prob <- NA
      for (i in 1:nrow(part_match_df)){
        part_match_df$true_pred_prob[i] <- part_match_df[i,as.character(part_match_df$true[i])] 
      }
      
    
      
      #Now for the imputed teeth
      knn_imputed <- function(DSCN){
        temp <- results_list[[DSCN]]$dist
        
        temp$Tribe <- factor(temp$Tribe, levels = unique(sort(temp$Tribe)))
        
        dat_list <- list()
        for (i in 1:M){
          pro <- data.frame(t(data.frame(c(table(temp$Tribe[order(temp[[paste0("V",i)]])][1:knn])/knn))))
          
          
          row.names(pro) <- NULL
          dat_list[[i]] <- pro
        }
        df <- do.call(rbind,dat_list)
        dat <- data.frame(t(data.frame(unlist(apply(df,2,mean)))))
        row.names(dat) <- NULL
        dat$true <- results_list[[DSCN]]$truth$Tribe[1]
        dat$DSCN <- DSCN
        return(dat)
      }
      
      
      imputed_match <- lapply(ids, knn_imputed)
      imputed_match_df <- do.call(rbind,imputed_match)
      
      imputed_match_df$true_pred_prob <- NA
      for (i in 1:nrow(imputed_match_df)){
        imputed_match_df$true_pred_prob[i] <- imputed_match_df[i,as.character(imputed_match_df$true[i])] 
      }
      
      #Note: In order to prevent infinite loss a small positive number was added
      logloss_imputed[knn] <- mean(-log(imputed_match_df$true_pred_prob+(10^-12)))
      logloss_part[knn] <- mean(-log(part_match_df$true_pred_prob+(10^-12)))
      
      
      #Predict the class for imputed 
      imputed_match_df$pred <- names(imputed_match_df[,1:7])[apply(imputed_match_df[,1:7],1,which.max)]
      reference <- factor(imputed_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      pred <- factor(imputed_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      
      library(caret)
      acc_imputed[knn] <- confusionMatrix(pred,reference)$overall["Accuracy"]
      
      
      #For partial matching 
      part_match_df$pred <- names(part_match_df[,1:7])[apply(part_match_df[,1:7],1,which.max)]
      
      reference <- factor(part_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      
      pred <- factor(part_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      
      library(caret)
      acc_part[knn] <- confusionMatrix(pred,reference)$overall["Accuracy"]
      
      
      #Accuracy of Only "strong" predictions
      #Predict the class for imputed 
      ids <- which(apply(imputed_match_df[,1:7],1,max)>.4)
      imputed_match_df$pred <- names(imputed_match_df[,1:7])[apply(imputed_match_df[,1:7],1,which.max)]
      reference <- factor(imputed_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      pred <- factor(imputed_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      
      pred <- pred[ids]
      reference <- reference[ids]
      
      library(caret)
      acc_strong_imputed[knn] <- confusionMatrix(pred,reference)$overall["Accuracy"]
      
      
      #For partial matching 
      ids <- which(apply(part_match_df[,1:7],1,max)>.4)
      part_match_df$pred <- names(part_match_df[,1:7])[apply(part_match_df[,1:7],1,which.max)]
      reference <- factor(part_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      pred <- factor(part_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" ))
      
      pred <- pred[ids]
      reference <- reference[ids]
      
      library(caret)
      acc_strong_part[knn] <- confusionMatrix(pred,reference)$overall["Accuracy"]
      
    }
    
    if (!scaled){
      save(list = c("logloss_imputed","logloss_part","imputed_match_df","part_match_df","acc_imputed","acc_part"), 
           file = paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"_summaries.RData"))
    }
    
    if (scaled){
      save(list = c("logloss_imputed","logloss_part","imputed_match_df","part_match_df","acc_imputed","acc_part"), 
           file = paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled_summaries.RData"))
    }
    
    
  }}

