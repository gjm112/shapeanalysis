###  nohup R CMD BATCH --vanilla /home/gmatthews1/FILENAME.R &
start <- Sys.time()
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/calc_shape_dist_partial.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/complete_partial_shape.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/impute_partial_shape.R")
source("/Users/gregorymatthews/Dropbox/shapeanalysisgit/R/tooth_cutting.R")

library(fdasrvf)
load("/Users/gregorymatthews/Dropbox/StatisticalShapeAnalysis/data_set_of_full_teeth.RData")

i <- 1 #Whcih tooth.  DSCN number 
j <- 1 #could be 1 or 2.  

partial_shape <- t(tooth_cutter(ptsTrainList[["LM1"]][[i]])[[j]])
#partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
complete_shape_list <- lapply(ptsTrainList[["LM1"]], t)
complete_shape_list <- list(complete_shape_list[[1]],complete_shape_list[[2]],complete_shape_list[[3]],complete_shape_list[[4]],complete_shape_list[[5]])

#I can't impute the partial shape with itself!
complete_shape_list[[i]]<-NULL

start1 <- Sys.time()
imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = 5, k = 3)
end1 <- Sys.time()
end1-start1

#Now do classification on the completed shapes just using closest 
ref_file <- read.csv("/Users/gregorymatthews/Dropbox/shapeanalysisgit/refFile.csv")
DSCN_target <- names(ptsTrainList[["LM1"]])[[i]]
truth <- subset(ref_file,ref == DSCN_target)

dist_imputed_to_whole <- function(whole,part){
  whole <- resamplecurve(whole,N =dim(part)[2], mode = "C")  
  out <- calc_shape_dist(whole,part,mode="C")
  print("working")
  return(out)
}
start <-Sys.time()
out <- lapply(complete_shape_list, dist_imputed_to_whole, part = partial_shape)
end <-Sys.time()
end-start
#doesitwork <- list(complete_shape_list[[1]],complete_shape_list[[2]])
#greg <- lapply(doesitwork, dist_imputed_to_whole, part = imputed_partial_shape[[1]][[m]])

dist_imputed_to_whole2 <- function(part){
  out <- lapply(complete_shape_list, dist_imputed_to_whole, part = part)
  print("working")
  return(out)
}

dist_list <- lapply(imputed_partial_shape[[1]],dist_imputed_to_whole2)
dist <- t(do.call(rbind,lapply(dist_list,unlist)))

row.names(dist)<-names(complete_shape_list)

dist <- as.data.frame(dist)
dist$DSCN <- row.names(dist)

dist <- merge(dist, ref_file, by.x = "DSCN", by.y = "ref", all.x = TRUE)
end <- Sys.time()


#Smallest to largest
knn <- 10
table(as.character(dist$tribe[order(dist$V1)][1:knn]))
table(as.character(dist$tribe[order(dist$V2)][1:knn]))
table(as.character(dist$tribe[order(dist$V3)][1:knn]))
table(as.character(dist$tribe[order(dist$V4)][1:knn]))
table(as.character(dist$tribe[order(dist$V5)][1:knn]))

save.image("/Users/gregorymatthews/Dropbox/shapeanalysisgit/test_one_tooth.RData")

fret <- lapply(complete_shape_list,calc_shape_dist_partial,partial_shape = partial_shape)
dist_partial <- data.frame(DSCN = names(unlist(fret)), dist = unlist(fret))
dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "ref", all.x = TRUE)

table(as.character(dist_partial$tribe[order(dist_partial$dist)][1:20]))


#take top 10 closest? 
index <- order(dist)[1]
dscn <- names(complete_shape_list)[index]
subset(ref_file,ref %in% dscn)
subset(ref_file,ref == "DSCN2879")







plot(t(test$imputed[[1]]),type="l", col = rgb(0,0,0,0), xlim = c(-0.4, 0.15), ylim = c(-0.2,0.2), lwd = 3)
points(t(red$donor),col = "red", type = "l",lwd=1, lty = 3)
set.seed(1234)
for (i in 1:25){
  u <- runif(3)
  points(t(test$imputed[[i]]),type="l",col =  rgb(u[1],u[2],u[3],0.5), lwd = 3)
}
red <- complete_partial_shape(t(ptsTrainList[[1]][[1]]),t(ptsTrainList[[1]][[1]][11:42,]))

arr <- array(0, dim = c(2,199,25))
for (i in 1:25){
  arr[,,i] <- test$imputed[[i]]
}

mn_shape <- apply(arr,c(1,2),mean)
points(t(mn_shape), type = "l", col = rgb(0.1,0.2,0.7), lwd = 2)


imputed<-test
out<-list(complete_shape_list,imputed)
save(out,file = "/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/example_data_for_sebastian.RData")

write.csv(do.call(rbind,complete_shape_list), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/complete_shape_list_for_sebastian.csv" )
write.csv(do.call(rbind,test$imputed), file ="/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/completed_tooth_for_sebastian.csv" )


