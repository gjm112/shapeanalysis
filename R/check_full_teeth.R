load("/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
ref_file <- read.csv("./refFile.csv")

dist_vecs <- check <- list()
LM1 <- ptsTrainList[["LM1"]]

set.seed(1234)
ind <- sample(1:length(LM1))
test <- train <- list()
for (i in 1:184){
  train[[i]] <- LM1[[ind[i]]]
}


for (i in 185:368){
  test[[i-184]] <- LM1[[ind[i]]]
}


names(train) <- names(LM1)[ind[1:184]]
names(test) <- names(LM1)[ind[185:368]]
k <- 20
library(fdasrvf)
for (j in 1:184){print(j)
dist_vec <- c()
for (i in 1:184){print(i)
  dist_vec[i] <- calc_shape_dist(t(test[[j]]),t(train[[i]]))  
}
dist_vecs[[j]] <- dist_vec
true <- ref_file$tribe[ref_file$ref == names(test)[j]]
check[[j]] <- data.frame(t(data.frame(c(table(ref_file$tribe[ref_file$ref %in% names(train)[order(dist_vec)[1:k]]])/k))))
names(check) <- NULL
check$true <- true

}

check_df <- do.call(rbind,check)
rownames(check_df) <- NULL

true <- rep(NA, 184)
for (j in 1:184){
     true[j] <- as.character(ref_file$tribe[ref_file$ref == names(test)[j]])
}


check_df$true <- c(true,"Alcelaphini")

check_df$pred <- sort(unique(check_df$true))[apply(check_df[,1:7],1,which.max)]

table(factor(check_df$true, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )),factor(check_df$pred, levels = c("Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" )))

      "Alcelaphini", "Antilopini", "Tragelaphini", "Neotragini","Bovini", "Reduncini", "Hippotragini" 







