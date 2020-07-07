# 
# #Read in the reference file.
 library(googlesheets)
 library(dplyr)
# greg <- gs_ls()
# bovids <- gs_url("https://docs.google.com/spreadsheets/d/1KGkTVz5IVuBdtQie0QBdeHwyHVH41MjFdbpluFsDX6k/edit#gid=963640939")
# bovids.df <- bovids %>% gs_read(ws = 1)
# subset(bovids.df, `Tooth Type` == "LM1")

tooth <- "UM2"
side <- 2

M <- 20
k <- 5
scaled <- FALSE

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  #for (tooth in c("LM2")){
  for (side in 1:2){print(c(tooth,side))
    
    
    if (!scaled){load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,".RData"))}
    if (scaled){load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled.RData"))}
    
    
logloss_imputed <- c() 
logloss_part <- c()
acc_part <- c()
acc_imputed <- c()
#logloss_imputed_mean <- c()
for (knn in c(1:4,6:20,30,40,50,60,5)){print(knn)
  
  ids <- names(results_list)
  
  knn_partial_matching <- function(DSCN){
    temp <- results_list[[DSCN]]$dist_partial
    
    temp$Species <- factor(temp$Species, levels = unique(sort(temp$Species)))
    
    dat <- data.frame(t(data.frame(c(table(temp$Species[order(temp$dist)][1:knn])/knn))))
    
    #Weighted KNN
    #wts <- c(table(temp$Tribe[order(temp$dist)][1:knn])/table(temp$Tribe))
    #dat <- data.frame(t(data.frame((wts/sum(wts)))))
    row.names(dat) <- NULL
    dat$true <- results_list[[DSCN]]$truth$Species[1]
    dat$DSCN <- DSCN
    return(dat)
  }
  
  part_match <- lapply(ids, knn_partial_matching)
  part_match_df <- do.call(rbind,part_match)
  
  part_match_df$true_pred_prob <- NA
  for (i in 1:nrow(part_match_df)){
    part_match_df$true_pred_prob[i] <- part_match_df[i,as.character(part_match_df$true[i])] 
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
  knn_imputed <- function(DSCN){
    temp <- results_list[[DSCN]]$dist
    
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
    dat$true <- results_list[[DSCN]]$truth$Species[1]
    dat$DSCN <- DSCN
    return(dat)
  }
  
  
  imputed_match <- lapply(ids, knn_imputed)
  imputed_match_df <- do.call(rbind,imputed_match)
  
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
