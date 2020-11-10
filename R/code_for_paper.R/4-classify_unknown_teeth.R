################################################################################
## This code classifies the actual partial teeth by using imputations and also just partial matching.  This code relies on the output of the previous matlab code for generating the open curve data that is used in this code.  
################################################################################

# Florisbad FLO 78 Alcelaphini Connochaetes gnou RM1 max right l 4825
# Florisbad FLO 401 Alcelaphini Connochaetes gnou RM3 mand right l 4980
# Florisbad FLO 1054 AlcelaphiniConnochaetes gnou RM3 mand right l 4983
# Florisbad FLO 1063 Alcelaphini Connochaetes gnou RM3 mand right l 4990
# Florisbad FLO 1021b Alcelaphini Connochaetes gnou RM3 mand right l 5139
# ????????Florisbad FLO 1290 Tragelaphini Taurotragus oryx molar max 5510-12??????
# Florisbad FLO 1289 Tragelaphini Taurotragus oryx molar max l 5514
# Plovers Lake 17783 Antilopini Antidorcas marsupialis M3 mand l 9973


# 4825: UM1
# 4980: LM3
# 4983: LM3
# 4990: LM3
# 5139:LM3
# 9973: LM3
# 5514: maxillary molar.. probably UM2 but could me UM1. 



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

tooth_n_list <- list()
tooth_n_list[["4825"]] <- 48 #80%
tooth_n_list[["4980"]] <- 48 #80%
tooth_n_list[["4983"]] <- 54 #90%
tooth_n_list[["4990"]] <- 20 #33.3333333%
tooth_n_list[["5139"]] <- 30 #50%
tooth_n_list[["9973"]] <- 54 #90%
tooth_n_list[["5514"]] <- 30 #50%


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
load("./data/ptsTrainList_clean.RData")


#Note: Traveling from start to stop should always be in a clowckwise direction!

#Which tooth it is
for (ggg in 1:length(tooth_type_list)){print(ggg)
  tooth <- tooth_type_list[[names(tooth_type_list)[ggg]]]
  
  #Load the actual partial tooth.
  partial_shape  <- read.csv(paste0("./partial_teeth/bw_images_data/DSCN",names(tooth_type_list)[ggg],"bw.csv"), header = FALSE)
  partial_shape <- as.matrix(partial_shape)
  
  start_stop <- read.csv(paste0("./partial_teeth/bw_images_data/DSCN",names(tooth_type_list)[ggg],"bwstart_stop.csv"), header = FALSE)
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
  plot((partial_shape))
  points(start_stop[1,1],start_stop[1,2], col = "green", pch = 16)
  points(start_stop[2,1],start_stop[2,2], col = "red", pch = 16)
  
  
  #Now store it wide rather than long.  
  partial_shape <- t(partial_shape)
  
  #Now resample it to n points 
  partial_shape <- resamplecurve(partial_shape, tooth_n_list[[names(tooth_n_list)[ggg]]], mode = "O")
  
  #Remember N_partial must be less than or equal to N_complete
  
  
  #check partial shape
  # plot(t(partial_shape))
  # points(start_stop[1,1],start_stop[1,2], col = "green", pch = 16)
  # points(start_stop[2,1],start_stop[2,2], col = "red", pch = 16)
  
  #partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])
  complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
  
 for (M in c(10)){print(paste0("M = ",M))
    for (k in c(10)){ print(paste0("k = ",M))
      for (scale in c(TRUE,FALSE)){ print(paste0("scale = ",scale))
        library(parallel)
        start1 <- Sys.time()
        imputed_partial_shape <- impute_partial_shape(complete_shape_list,partial_shape, M = M, k = k, scale = scale)
        end1 <- Sys.time()
        end1-start1 
        
        
        #Now do classification on the completed shapes just using closest 
        ref_file <- read.csv("./data/reference_db.csv")
        
        
        
        dist_imputed_to_whole <- function(whole,part, scale = scale){
          whole <- resamplecurve(whole,N = dim(part)[2], mode = "C") 
          print(Sys.time())
          out <- calc_shape_dist_complete(whole,part, scale)
          return(out)
        }
        
        
       
        
        dist_imputed_to_whole2 <- function(part){
          out <- mclapply(complete_shape_list, dist_imputed_to_whole, part = part, scale = scale, mc.cores = 12)
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
        
        
        
        dist <- merge(dist, ref_file, by.x = "DSCN", by.y = "Image.Name", all.x = TRUE)
        
       
        
        #Classify based on closest match between partial and all full teeth.  
        dist_partial <- data.frame(DSCN = names(unlist(imputed_partial_shape$dist_vec)), dist = unlist(imputed_partial_shape$dist_vec))
        
        dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "Image.Name", all.x = TRUE)
        
        results <- list(dist = dist , dist_partial = dist_partial, imputed_partial_shape = imputed_partial_shape)
        
        
        end_all <- Sys.time()
        end_all-start_all
        
        
        knn_partial_matching <- function(knn){
          temp <- results$dist_partial
          temp$inv_dist <- 1/temp$dist
          
          temp$Tribe <- factor(temp$Tribe, levels = unique(sort(temp$Tribe)))
          
          
          dat <- data.frame(t(data.frame(c(table(temp$Tribe[order(temp$dist)][1:knn])/knn))))
          
          row.names(dat) <- NULL
          return(dat)
        }
        
        
        
        
        #Now for the imputed teeth
        knn_imputed <- function(knn){
          temp <- results$dist
          
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
          return(dat)
        }
        
        
        
        
        
        #Now classify Species
        knn_partial_matching_species <- function(knn){
          temp <- results$dist_partial
          
          temp$Species <- factor(temp$Species, levels = unique(sort(temp$Species)))
          
          dat <- data.frame(t(data.frame(c(table(temp$Species[order(temp$dist)][1:knn])/knn))))
          
          
          row.names(dat) <- NULL
          
          return(dat)
        }
        
        
        
       
        #Now for the imputed teeth
        knn_imputed_species <- function(knn){
          temp <- results$dist
          
          temp$Species <- factor(temp$Species, levels = unique(sort(temp$Species)))
          
          dat_list <- list()
          for (i in 1:M){
            pro <- data.frame(t(data.frame(c(table(temp$Species[order(temp[[paste0("V",i)]])][1:knn])/knn))))
            
            
            row.names(pro) <- NULL
            dat_list[[i]] <- pro
          }
          df <- do.call(rbind,dat_list)
          dat <- data.frame(t(data.frame(unlist(apply(df,2,mean)))))
          row.names(dat) <- NULL
         
          return(dat)
        }
        
        
        
        nam <- paste0(names(tooth_type_list)[ggg],"_M_",M,"_k_",k,"_scale_",scale)
        
        classifier_tribe[[nam]] <- list()
        classifier_species[[nam]] <- list()
        
        
        #10 rows are because I'm using different choices of knn.  
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
        save(classifier_tribe, file = "./results/classifier_tribe.RData")
        save(classifier_species, file = "./results/classifier_species.RData")
        save(classifier_imputations, file = "./results/classifier_imputations.RData")
        
      }}}
  
  
}



#Plot for paper 
#The first tooth is completed well.  
names(tooth_type_list)
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4825_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[1]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), ylim = c(-.15, .15), xlim = c(-.3,.1), xlab = "", ylab = "")
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[1]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[1]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()


names(tooth_type_list)
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4825_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[2]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-450,300), ylim = c(-300,250))
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[2]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[2]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()



names(tooth_type_list)
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4980_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[3]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), ylim = c(-.12, .1), xlab = "", ylab = "")
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[3]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[3]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4980_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[4]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-350,650), ylim = c(-200, 200))
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[4]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[4]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()


png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4983_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[5]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), ylim = c(-.12, .1), xlab = "", ylab = "")
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[5]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[5]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4983_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[6]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-400,550))
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[6]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[6]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()


png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4990_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[7]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", ylim = c(-0.2,0.15))
for (i in 2:length(classifier_imputations[[7]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[7]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[7]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN4990_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[8]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-800,300), ylim = c(-225,225))
for (i in 2:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[8]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[8]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()


names(tooth_type_list)
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN5139_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[9]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", ylim = c(-.15, .15))
for (i in 2:length(classifier_imputations[[9]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[9]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[9]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN5139_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[10]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-200, 850))
for (i in 2:length(classifier_imputations[[10]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[10]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[10]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()



names(tooth_type_list)
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN9973_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[11]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-.25, .25))
for (i in 2:length(classifier_imputations[[11]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[11]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[11]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN9973_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[12]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "" )
for (i in 2:length(classifier_imputations[[12]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[12]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[12]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

names(tooth_type_list)
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN5514_scale_TRUE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[13]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-.1, .35), ylim = c(-.2, .2))
for (i in 2:length(classifier_imputations[[13]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[13]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[13]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()

png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/DSCN5514_scale_FALSE.png", res = 300, h = 6, w = 6, units = "in")
plot(t(classifier_imputations[[14]]$imputed_partial_shape$imputed[[1]]), type = "l", col = rgb(1,0,0,0.25), xlab = "", ylab = "", xlim = c(-200, 750), ylim = c(-300,300))
for (i in 2:length(classifier_imputations[[14]]$imputed_partial_shape$imputed)){
  points(t(classifier_imputations[[14]]$imputed_partial_shape$imputed[[i]]), type = "l", col = rgb(1,0,0,0.25))
}
points(t(classifier_imputations[[14]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
dev.off()




#First unscaled
#Results tables. 
tab <- classifier_tribe[[2]]$partial_matching[10,]
for (i in seq(4,14,2)){
  tab <- rbind(tab,classifier_tribe[[i]]$partial_matching[10,])
}  

row.names(tab) <- paste0("DSCN",names(tooth_type_list))
library(xtable)
xtable(tab, caption = "here")

#Results tables. 
tab_imp <- classifier_tribe[[2]]$imputed[10,]
for (i in seq(4,14,2)){
  tab_imp <- rbind(tab_imp,classifier_tribe[[i]]$imputed[10,])
}  

row.names(tab_imp) <- paste0("DSCN",names(tooth_type_list))
xtable(tab_imp, caption = "here")


########################################
#Scaled results 
########################################
#Results tables. 
tab <- classifier_tribe[[1]]$partial_matching[10,]
for (i in seq(3,13,2)){
  tab <- rbind(tab,classifier_tribe[[i]]$partial_matching[10,])
}  

row.names(tab) <- paste0("DSCN",names(tooth_type_list))
library(xtable)
xtable(tab, caption = "here")

#Results tables. 
tab_imp <- classifier_tribe[[1]]$imputed[10,]
for (i in seq(3,13,2)){
  tab_imp <- rbind(tab_imp,classifier_tribe[[i]]$imputed[10,])
}  

row.names(tab_imp) <- paste0("DSCN",names(tooth_type_list))
xtable(tab_imp, caption = "here")



#Species classifier
#Results tables. 
tab_species <- classifier_species[[2]]$partial_matching[10,]
for (i in seq(4,14,2)){
  tab_species <- rbind(tab_species,classifier_species[[i]]$partial_matching[10,])
}  

row.names(tab_species) <- names(tooth_type_list)
#keep <- apply(tab_species,2,function(x){sum(x)>0})
#tab_species[,keep]

library(xtable)
xtable(tab_species, caption = "here")

#Results tables. 
tab_species_imp <- classifier_species[[2]]$imputed[10,]
for (i in seq(4,14,2)){
  tab_species_imp <- rbind(tab_species_imp,classifier_species[[i]]$imputed[10,])
}  

row.names(tab_species_imp) <- paste0("IMG",names(tooth_type_list))
#keep <- apply(tab_species_imp,2,function(x){sum(x)>0})
#tab_species_imp[,keep]

xtable(tab_species_imp, caption = "here")




################################################
#Scaled results
################################################
tab_species <- classifier_species[[1]]$partial_matching[10,]
for (i in seq(3,13,2)){
  tab_species <- rbind(tab_species,classifier_species[[i]]$partial_matching[10,])
}  

row.names(tab_species) <- paste0("DSCN",names(tooth_type_list))
#keep <- apply(tab_species,2,function(x){sum(x)>0})
#tab_species[,keep]

library(xtable)
xtable(tab_species, caption = "here")

#Results tables. 
tab_species_imp <- classifier_species[[1]]$imputed[10,]
for (i in seq(3,13,2)){
  tab_species_imp <- rbind(tab_species_imp,classifier_species[[i]]$imputed[10,])
}  

row.names(tab_species_imp) <- paste0("IMG",names(tooth_type_list))
#keep <- apply(tab_species_imp,2,function(x){sum(x)>0})
#tab_species_imp[,keep]

xtable(tab_species_imp, caption = "here")
# 
# #Plots of completed shapes
# png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/IMG4825_imputed.png", h = 5, w = 8, res = 300, units = "in")
# plot(t(classifier_imputations[[1]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-450,250), ylim = c(-250, 250), xlab = "", ylab = "", main = "IMG4825 - LM1")  
# for (i in 1:length(classifier_imputations[[1]]$imputed_partial_shape$imputed)){
# points(t(classifier_imputations[[1]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
# }
# points(t(classifier_imputations[[1]]$imputed_partial_shape$partial_obs), col = "black", type = "l") 
# dev.off()
# 
# #IMG2
# plot(t(classifier_imputations[[2]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-450,660), ylim = c(-250, 250), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[2]," - LM3"))
# for (i in 1:length(classifier_imputations[[2]]$imputed_partial_shape$imputed)){
#   points(t(classifier_imputations[[2]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
# }
# points(t(classifier_imputations[[2]]$imputed_partial_shape$partial_obs), col = "black", type = "l") 
# 
# 
# 
# #IMG3
# plot(t(classifier_imputations[[3]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-450,500), ylim = c(-250, 250), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[3]," - LM3"))
# for (i in 1:length(classifier_imputations[[3]]$imputed_partial_shape$imputed)){
#   points(t(classifier_imputations[[3]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
# }
# points(t(classifier_imputations[[3]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
# 
# #IMG4
# plot(t(classifier_imputations[[4]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-1050,500), ylim = c(-250, 450), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[4]," - LM3"))
# for (i in 1:length(classifier_imputations[[4]]$imputed_partial_shape$imputed)){
#   points(t(classifier_imputations[[4]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
# }
# points(t(classifier_imputations[[4]]$imputed_partial_shape$partial_obs), col = "black", type = "l")
# 
# #IMG5
# plot(t(classifier_imputations[[5]]$imputed_partial_shape$imputed[[1]]), col = "white", type = "l", xlim = c(-250,600), ylim = c(-250, 250), xlab = "", ylab = "", main = paste0("IMG",names(tooth_type_list)[5]," - LM3"))
# for (i in 1:length(classifier_imputations[[5]]$imputed_partial_shape$imputed)){
#   points(t(classifier_imputations[[5]]$imputed_partial_shape$imputed[[i]]), col = "red", type = "l")  
# }
# points(t(classifier_imputations[[5]]$imputed_partial_shape$partial_obs), col = "black", type = "l") 
# 
# 
# 
