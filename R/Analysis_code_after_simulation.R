#load("/home/gmatthews1/shapeAnalysis/results/results20190220.RData")
load("./results20190220.RData")

logloss_imputed <- c() 
logloss_part <- c()
for (k in 1:20){print(k)

ids <- names(results_list)
DSCN <- ids[1]

knn_partial_matching <- function(DSCN){
temp <- results_list[[DSCN]]$dist_partial

dat <- data.frame(t(data.frame(c(table(temp$tribe[order(temp$dist)][1:k])/k))))
row.names(dat) <- NULL
dat$true <- results_list[[DSCN]]$truth$tribe[1]
return(dat)
}

#Check DSCN3361
part_match <- lapply(ids, knn_partial_matching)
part_match_df <- do.call(rbind,part_match)

part_match_df$true_pred_prob <- NA
for (i in 1:nrow(part_match_df)){
part_match_df$true_pred_prob[i] <- part_match_df[i,as.character(part_match_df$true[i])] 
}


#Now for the imputed teeth
knn_imputed <- function(DSCN){
  temp <- results_list[[DSCN]]$dist
  dat_list <- list()
  for (i in 1:5){
  pro <- data.frame(t(data.frame(c(table(temp$tribe[order(temp[[paste0("V",i)]])][1:k])/k))))
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


logloss_imputed[k] <- mean(-log(imputed_match_df$true_pred_prob+0.0001))
logloss_part[k] <- mean(-log(part_match_df$true_pred_prob+0.0001))

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

