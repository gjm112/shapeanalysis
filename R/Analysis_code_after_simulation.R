
#Read in the reference file.
library(googlesheets)
library(dplyr)
greg <- gs_ls()
bovids <- gs_url("https://docs.google.com/spreadsheets/d/1KGkTVz5IVuBdtQie0QBdeHwyHVH41MjFdbpluFsDX6k/edit#gid=963640939")
bovids.df <- bovids %>% gs_read(ws = 1)
subset(bovids.df, `Tooth Type` == "LM1")


#load("/home/gmatthews1/shapeAnalysis/results/results20190220.RData")
#load("./results20190220.RData")
load("./results20190503_side=1_k=5_M=5_tooth=LM1.RData")
load("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/results20190503_side=1_k=5_M=5_tooth=LM1.RData")

part <- results_list[[1]]$imputed_partial_shape$imputed[[1]]
whole <- resamplecurve(t(ptsTrainList[[tooth]][[d]]),199)
calc_shape_dist(part,whole)
results_list[[1]]$dist[1,]

results_list[[1]]$dist[order(results_list[[1]]$dist$V4),][1:10,]

# for (i in 1:length(results_list)){
# results_list[[i]]$dist <- results_list[[i]]$dist[-grep("Error",results_list[[i]]$dist$V1),]
# results_list[[i]]$dist$V1 <- as.numeric(as.character(results_list[[i]]$dist$V1))
# results_list[[i]]$dist$V2 <- as.numeric(as.character(results_list[[i]]$dist$V2))
# results_list[[i]]$dist$V3 <- as.numeric(as.character(results_list[[i]]$dist$V3))
# results_list[[i]]$dist$V4 <- as.numeric(as.character(results_list[[i]]$dist$V4))
# results_list[[i]]$dist$V5 <- as.numeric(as.character(results_list[[i]]$dist$V5))
# }


DSCN3732 
DSCN2616 
plot(scale(ptsTrainList[["LM1"]][["DSCN3732"]], center = TRUE, scale = FALSE), xlim = c(-500,500), asp = 1, col = "red", type = "l")
points(scale(ptsTrainList[["LM1"]][["DSCN2616"]], center = TRUE, scale = FALSE), col = "blue", type = "l")
points(t(results_list[["DSCN2871"]]$imputed_partial_shape[[1]][[1]]), col = "orange", type = "l")

part <- results_list[["DSCN2871"]]$imputed_partial_shape[[1]][[1]]
whole <- t(ptsTrainList[["LM1"]][["DSCN3732"]])
whole <- resamplecurve(whole, N = 199, mode = "C")
calc_shape_dist(part,whole,mode = "C")

points((scale(t(whole),center = TRUE, scale = FALSE)), type = "l", col = "red")

part <- results_list[["DSCN2871"]]$imputed_partial_shape[[1]][[1]]
whole <- t(ptsTrainList[["LM1"]][["DSCN2616"]])
whole <- resamplecurve(whole, N = 199, mode = "C")
calc_shape_dist(part,whole,mode = "C")

points((scale(t(whole),center = TRUE, scale = FALSE)), type = "l", col = "blue")


logloss_imputed <- c() 
logloss_part <- c()
for (knn in 1:20){print(knn)

ids <- names(results_list)
DSCN <- ids[1]

knn_partial_matching <- function(DSCN){
temp <- results_list[[DSCN]]$dist_partial

dat <- data.frame(t(data.frame(c(table(temp$tribe[order(temp$dist)][1:knn])/knn))))
row.names(dat) <- NULL
dat$true <- results_list[[DSCN]]$truth$tribe[1]
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
  dat_list <- list()
  for (i in 1:M){
  pro <- data.frame(t(data.frame(c(table(temp$tribe[order(temp[[paste0("V",i)]])][1:knn])/knn))))
  row.names(pro) <- NULL
  dat_list[[i]] <- pro
  }
  df <- do.call(rbind,dat_list)
  dat <- data.frame(t(data.frame(unlist(apply(df,2,mean)))))
  row.names(dat) <- NULL
  dat$true <- results_list[[DSCN]]$truth$tribe[1]
  return(dat)
}

imputed_match <- lapply(ids, knn_imputed)
imputed_match_df <- do.call(rbind,imputed_match)

imputed_match_df$true_pred_prob <- NA
for (i in 1:nrow(imputed_match_df)){
  imputed_match_df$true_pred_prob[i] <- imputed_match_df[i,as.character(imputed_match_df$true[i])] 
}


#table(imputed_match_df$true_pred_prob, part_match_df$true_pred_prob)


logloss_imputed[knn] <- mean(-log(imputed_match_df$true_pred_prob+0.0001))
logloss_part[knn] <- mean(-log(part_match_df$true_pred_prob+0.0001))

}

fret <- data.frame(logloss = c(logloss_imputed,logloss_part), k = c(1:20,1:20), method = c(rep("knn w Imputation",20),rep("knn no imputation",20)))
library(ggplot2)
ggplot(aes(x = k, y = logloss, col = method), data = fret) +  geom_point() + geom_line()

plot(1:20, logloss_imputed, type = "l", col = "red", ylim = c(0,7))
points(1:20, logloss_part, type = "l", col = "blue")

points(1:20, logloss_imputed, pch = 16, col = "red")
points(1:20, logloss_part, pch = 16, col = "blue")

#Predict the class
imputed_match_df$pred <- names(imputed_match_df[,1:7])[apply(imputed_match_df[,1:7],1,which.max)]
table(factor(imputed_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )),factor(imputed_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )))

part_match_df$pred <- names(part_match_df[,1:7])[apply(part_match_df[,1:7],1,which.max)]
table(factor(part_match_df$true,levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )),factor(part_match_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )))

