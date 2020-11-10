################################################
##Summaries of the data
################################################
load("./data/ptsTrainList_clean.RData")

#How many of each tooth type
unlist(lapply(ptsTrainList_clean, length))

#How many in each tribe
ref_file <- read.csv("./data/reference_db.csv")

##########################################
## Table 1
##########################################
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
temp <- subset(ref_file, Image.Name %in% names(ptsTrainList_clean[[tooth]])) 
print(table(temp$Tribe))
}



##########################################
## Table 2
##########################################
res <- data.frame()

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    for (k in c(5,10,20)){
      for (M in c(5,10,20)){
        try(load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"_summaries.RData")))
        
        res <- rbind(res,data.frame(tooth = tooth,side = side,k = k,M = M,accuracy_imputed = acc_imputed[1], accuracy_part = acc_part[1]))
        
      }}}}

test <- rbind(
  c("LM1",1,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 1],3)),
  c("LM1",2,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 2],3)),
  c("LM2",1,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 1],3)),
  c("LM2",2,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 2],3)),
  c("LM3",1,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 1],3)),
  c("LM3",2,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 2],3)),
  c("UM1",1,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 1],3)),
  c("UM1",2,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 2],3)),
  c("UM2",1,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 1],3)),
  c("UM2",2,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 2],3)),
  c("UM3",1,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 1],3)),
  c("UM3",2,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 2],3))
)

test <- cbind(test,round(res$accuracy_part[seq(1,nrow(res),9)],3)[1:12])

library(xtable)
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& & \\multicolumn{3}{c}{k=5} & \\multicolumn{3}{c}{k=10} & \\multicolumn{3}{c}{k=20} & \\\\\n",
                      "Tooth & & Side & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & Partial \\\\\n")
print(xtable(test), add.to.row = addtorow, include.colnames = FALSE, include.rownames = FALSE)


##########################################
## Table 3
##########################################
res <- data.frame()

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    for (k in c(5,10,20)){
      for (M in c(5,10,20)){
try(load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled_summaries.RData")))

res <- rbind(res,data.frame(tooth = tooth,side = side,k = k,M = M,accuracy_imputed = acc_imputed[1], accuracy_part = acc_part[1]))

      }}}}

  test <- rbind(
        c("LM1",1,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 1],3)),
        c("LM1",2,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 2],3)),
        c("LM2",1,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 1],3)),
         c("LM2",2,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 2],3)),
        c("LM3",1,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 1],3)),
        c("LM3",2,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 2],3)),
        c("UM1",1,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 1],3)),
        c("UM1",2,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 2],3)),
        c("UM2",1,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 1],3)),
        c("UM2",2,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 2],3)),
        c("UM3",1,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 1],3)),
        c("UM3",2,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 2],3))
        )

  test <- cbind(test,round(res$accuracy_part[seq(1,nrow(res),9)],3))

  library(xtable)
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& & \\multicolumn{3}{c}{k=5} & \\multicolumn{3}{c}{k=10} & \\multicolumn{3}{c}{k=20} &  \\\\\n",
"Tooth & Side & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & Partial  \\\\\n")
print(xtable(test), add.to.row = addtorow, include.colnames = FALSE, include.rownames = FALSE)


##########################################
#Figure 1 - Example cut
##########################################
png("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/figure/example_cut.png",res = 300, units = "in", h = 5, w = 5)
setwd("/Users/gregorymatthews/Dropbox/shapeanalysisgit/")
source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")

load("./data/data_set_of_full_teeth.RData")
load("./data/ptsTrainList.RData")

tooth <- "LM1"

complete_shape <- t(ptsTrainList[[1]][[1]])
#plot(t(complete_shape), type = "l")
#points(t(complete_shape), pch = 16)

side <- 1
partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[1]])[[side]])
plot(t(complete_shape), type = "l")
points(t(complete_shape), pch = 16)
points(t(partial_shape), pch = 16, col = "red")
points(t(partial_shape), type = "l", col = "red")

side <- 2
partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[1]])[[side]])
points(t(partial_shape), pch = 16, col = "blue")
points(t(partial_shape), type = "l", col = "blue")
dev.off()


#################################################
##Figure 2
#################################################
source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")


load("./data/ptsTrainList_clean.RData")

tooth <- "LM1"
side <- 1
partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[1]])[[side]])
complete_shape <- t(ptsTrainList[[1]][[2]])

scale <- FALSE
plot <- TRUE

#Dimension
d <- dim(complete_shape)[1]
#Number of points for complete_shape and partial_shape
N_complete <- dim(complete_shape)[2]
N_partial <- dim(partial_shape)[2]

t <- seq(0,1,length = 100)
x0 <- matrix(NA,ncol = 100,nrow = 2)
for (j in 1:d){
  x0[j,] <- (1-t)*partial_shape[j,N_partial] + t*partial_shape[j,1]
}

partial_shape_closed <- cbind(partial_shape,x0[,2:100])

N_complete_new <- 500
t <- seq(0,1,length = N_complete_new)

olddel <- get_cumdel(partial_shape_closed)

N <- 100


partial_shape_closed_obs <- resamplecurve(partial_shape_closed[,1:(dim(partial_shape_closed)[2] - (dim(x0)[2] - 1))],N)
partial_shape_closed_mis <- resamplecurve(partial_shape_closed[,(dim(partial_shape_closed)[2] - (dim(x0)[2] - 1)):dim(partial_shape_closed)[2]],N)

#Find the centroid of the observed part
cent1 <- apply(partial_shape_closed_obs,1, mean)

#Centering
partial_shape_closed_obs <- partial_shape_closed_obs - cent1
partial_shape_closed_mis <- partial_shape_closed_mis - cent1


if (scale == TRUE){
  #scale factor
  sc1 <- norm(partial_shape_closed_obs, type = "F")
  
  #Scaling the shape
  partial_shape_closed_obs <- partial_shape_closed_obs/sc1
  partial_shape_closed_mis <- partial_shape_closed_mis/sc1
}

minE <- Inf
jbest <- NA

for (j in 0:(N_complete-1)){
  mu <- ShiftF(complete_shape[,1:(N_complete-1)],j) 
  mu <- cbind(mu,mu[,1])
  
  olddel1 <- get_cumdel(mu)
  
  N <- 100
  library(fdasrvf)
  mu <- resamplecurve(mu,N_complete_new)
  
  newpt1 <- which(t < olddel1[N_partial])
  newpt1 <- newpt1[length(newpt1)]
  
  mu1 <- resamplecurve(mu[,1:newpt1],N) 
  mu2 <- resamplecurve(mu[,newpt1:dim(mu)[2]],N) 
  
  cent2 <- apply(mu1,1,mean)
  mu1 <- mu1 - cent2
  mu2 <- mu2 - cent2
  
  if (scale == TRUE){
    sc2=norm(mu1, type = "F")
    mu1=mu1/sc2
    mu2=mu2/sc2
  }
  
  #Finding the best rotation
  out <- find_best_rotation(partial_shape_closed_obs,mu1)
  R <- out$R
  q2new <- out$q2new
  
  mu1n <- R%*%mu1
  
  Ec <- InnerProd_Q(partial_shape_closed_obs-mu1n,partial_shape_closed_obs-mu1n)
  if (Ec < minE){
    jbest <- j
    Rbest <- R
    complete_shape_obs <- Rbest%*%mu1
    complete_shape_mis <- Rbest%*%mu2
    minE <- Ec
  }
  
}


donor <- cbind(complete_shape_obs,complete_shape_mis[,2:dim(complete_shape_mis)[2]])

partial_shape_closed_new <- cbind(partial_shape_closed_obs,partial_shape_closed_mis[,2:dim(partial_shape_closed_mis)[2]])

n <- 40
tn <- seq(0,1, length = N)

#Defining basic functions
b <- matrix(NA, nrow = 2*n, ncol = length(tn))
for (j in 1:n){
  b[j,] <- sin(2*pi*j*tn)/(sqrt(2)*pi*j)
  b[j+n,] <- (cos(2*pi*j*tn)-1)/(sqrt(2)*pi*j)
}

#Plot
par(mfcol = c(5,2))
par(mar = c(0,0,0,0))
iter <- 0
plot(t(donor), type = "l", xlim = c(-100,550), xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', lwd = 3)
text(375,-150,paste0("Interation: ", iter))
points(t(partial_shape_closed_new), type = "l" , col= "red", lwd = 3)
points(t(partial_shape_closed_mis), type = "l" , col= "blue", lwd = 3)


n <- dim(b)[1]
iter <- 1
eps <- 15

for (iter in 0:500){
  
  v <- partial_shape_closed_mis - complete_shape_mis
  
  #Computing each basis component and then adding them.  
  gradE <- matrix(0,nrow = 2,ncol = N)
  for (j in 1:n){
    for (k in 1:d){
      val <- trapz(tn,v[k,]*b[j,])*b[j,]
      gradE[k,] <- gradE[k,]+val
    }}
  
  ngE <- trapz(tn,apply(gradE*gradE,2,sum))
  
  partial_shape_closed_mis <- partial_shape_closed_mis-eps*gradE
  
  partial_shape_closed_new <- cbind(partial_shape_closed_obs,partial_shape_closed_mis[,2:dim(partial_shape_closed_mis)[2]])
  
  ngE
  
  
  iter=iter+1
  
  
  if (plot == TRUE & iter%in% c(1,2,3,4,100,200,300,400,500)){
    plot(t(donor), type = "l", xlim = c(-100,550), xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', lwd = 3)
    text(375,-150,paste0("Interation: ",iter))
    points(t(partial_shape_closed_new), type = "l" , col= "red", lwd = 3)
    points(t(partial_shape_closed_mis), type = "l" , col= "blue", lwd = 3)
  }
  
  
}

##################################################
#Figure 3: Accuracy of tribe classification unscaled with knn = 1
##################################################

#tooth <- "LM1"
#side <- 1
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){#print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries.RData"))
    acc_part_k20_M20 <- acc_part
    acc_imputed_k20_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    # dat <- data.frame(knn = rep(1:60,4), acc = c(acc_part_5,acc_imputed_5,acc_imputed_10,acc_imputed_20), type = rep(c("no imp","k5 M5","k10 M10", "k20 M20"),each = 60))
    # library(ggplot2)
    # g <- ggplot(aes(x = knn, y = acc, colour = type), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))
    # print(g)
    
    #print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    
    # acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
    #   accuracy  = c(mean(acc_imputed_k20_M20[1:20]),mean(acc_imputed_k20_M10[1:20]),mean(acc_imputed_k20_M5[1:20]),mean(acc_imputed_k10_M20[1:20]),mean(acc_imputed_k10_M10[1:20]),mean(acc_imputed_k10_M5[1:20]),mean(acc_imputed_k5_M20[1:20]),mean(acc_imputed_k5_M10[1:20]),mean(acc_imputed_k5_M5[1:20]),mean(acc_part_k20_M20[1:20])),
    #   tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    #knn = 1
    acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
      accuracy  = c(mean(acc_imputed_k20_M20[1]),mean(acc_imputed_k20_M10[1]),mean(acc_imputed_k20_M5[1]),mean(acc_imputed_k10_M20[1]),mean(acc_imputed_k10_M10[1]),mean(acc_imputed_k10_M5[1]),mean(acc_imputed_k5_M20[1]),mean(acc_imputed_k5_M10[1]),mean(acc_imputed_k5_M5[1]),mean(acc_part_k20_M20[1])),
      tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    
  }}

dat <- do.call(rbind, acc_res_list)
dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))

#png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_knn1_tribe_unscaled.png"), res = 300, units = "in", w = 8, h =4)
ggplot(aes(x = tooth_type, y = accuracy, col = k, shape = M), data = dat) + geom_point()
#dev.off()

############################################################
## Figure 4: Accuracy of tribe classification scaled
############################################################

library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){#print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M20 <- acc_part
    acc_imputed_k20_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    # dat <- data.frame(knn = rep(1:60,4), acc = c(acc_part_5,acc_imputed_5,acc_imputed_10,acc_imputed_20), type = rep(c("no imp","k5 M5","k10 M10", "k20 M20"),each = 60))
    # library(ggplot2)
    # g <- ggplot(aes(x = knn, y = acc, colour = type), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))
    # print(g)
    
    #print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    
    # acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
    #   accuracy  = c(mean(acc_imputed_k20_M20[1:20]),mean(acc_imputed_k20_M10[1:20]),mean(acc_imputed_k20_M5[1:20]),mean(acc_imputed_k10_M20[1:20]),mean(acc_imputed_k10_M10[1:20]),mean(acc_imputed_k10_M5[1:20]),mean(acc_imputed_k5_M20[1:20]),mean(acc_imputed_k5_M10[1:20]),mean(acc_imputed_k5_M5[1:20]),mean(acc_part_k20_M20[1:20])),
    #   tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    #knn = 1
    acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
      accuracy  = c(mean(acc_imputed_k20_M20[1]),mean(acc_imputed_k20_M10[1]),mean(acc_imputed_k20_M5[1]),mean(acc_imputed_k10_M20[1]),mean(acc_imputed_k10_M10[1]),mean(acc_imputed_k10_M5[1]),mean(acc_imputed_k5_M20[1]),mean(acc_imputed_k5_M10[1]),mean(acc_imputed_k5_M5[1]),mean(acc_part_k20_M20[1])),
      tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    
  }}

dat <- do.call(rbind, acc_res_list)
dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))

#png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_knn1_tribe_scaled.png"), res = 300, units = "in", w = 8, h =4)
ggplot(aes(x = tooth_type, y = accuracy, col = k, shape = M), data = dat) + geom_point()
#dev.off()



###################################################
## Figure 5 - Tribe Unscaled Accuracy Results 
###################################################
library(cowplot)
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){print(c(tooth, side))
load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M5 <- acc_part
acc_imputed_k5_M5 <- acc_imputed[1:20]
 rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M10 <- acc_part
acc_imputed_k5_M10 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k5_M20 <- acc_part
acc_imputed_k5_M20 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M5 <- acc_part
acc_imputed_k10_M5 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M10 <- acc_part
acc_imputed_k10_M10 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries.RData"))
acc_part_k10_M20 <- acc_part
acc_imputed_k10_M20 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M5 <- acc_part
acc_imputed_k20_M5 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries.RData"))
acc_part_k20_M10 <- acc_part
acc_imputed_k20_M10 <- acc_imputed[1:20]
rm(acc_part)
rm(acc_imputed)

load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries.RData"))
acc_part <- acc_part[1:20]
acc_imputed_k20_M20 <- acc_imputed[1:20]

dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), k = c(rep("No Imp", 20), rep(c(5,10,20),each = 60)), M = c(rep("No Imp",20),rep(rep(c(5,10,20), each = 20),3)))


dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))


if(side == 1){
g1 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side)) + ylim(.6,.95) + theme(legend.position = "none")
}

if (side == 2){
g2 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))  + ylim(.6,.95)
}

#acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(accuracy  = c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])), tooth_type = rep(paste0(tooth,"_",side),4),  M_k = c("M20 l20", "M10 l10", "M5 l5", "No Imp"))

  }

  #png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_by_knn_tribe_unscaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  library(gridExtra)
legend <- get_legend(g2)
g2 <- g2 + theme(legend.position="none")
grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
#dev.off()

}


###################################################
## Figure 6 - Tribe Scaled Accuracy Results 
###################################################
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries.RData"))
    acc_part <- acc_part[1:20]
    acc_imputed_k20_M20 <- acc_imputed[1:20]
    
    dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), k = c(rep("No Imp", 20), rep(c(5,10,20),each = 60)), M = c(rep("No Imp",20),rep(rep(c(5,10,20), each = 20),3)))
    
    #dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), type = rep(c("no imp","k5 M5","k5 M10","k5 M20","k10 M5","k10 M10", "k10 M20","k20 M5","k20 M10", "k20 M20"),each = 20))
    
    dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
    dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))
    
    library(ggplot2)
    if (side == 1){
      g1 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side)) + ylim(.4,.86) + theme(legend.position = "none")
    }
    
    if (side == 2){
      g2 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))  + ylim(.4,.86)
    }
    
    
    

    
  }
  library(cowplot)
  #png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_by_knn_tribe_scaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  legend <- get_legend(g2)
  g2 <- g2 + theme(legend.position="none")
  grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
  #dev.off()
  
  
}

#######################################################
##Figure 7 - Tribe Scaled Log Loss Results
#######################################################
library(dplyr)
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k5_M5 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k10_M10 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k20_M20 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k20_M5 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k20_M10 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k10_M5 <- logloss_imputed
    
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k5_M10 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k5_M20 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries.RData"))
    
    logloss_imputed_k10_M20 <- logloss_imputed
    
    
    n5_5 <- length(logloss_imputed_k5_M5)
    n10_10 <- length(logloss_imputed_k10_M10)
    n20_10 <- length(logloss_imputed_k20_M20)
    n20_5 <- length(logloss_imputed_k20_M5)
    n20_10 <- length(logloss_imputed_k20_M10)
    n10_5 <- length(logloss_imputed_k10_M5)
    n5_10 <- length(logloss_imputed_k5_M10)
    n5_20 <- length(logloss_imputed_k5_M20)
    n10_20 <- length(logloss_imputed_k10_M20)
    n_part <- length(logloss_part)
    
    fret <- data.frame(logloss = c(logloss_imputed_k5_M5[1:20],
                                   logloss_imputed_k5_M10[1:20],
                                   logloss_imputed_k5_M20[1:20],
                                   logloss_imputed_k10_M5[1:20],
                                   logloss_imputed_k10_M10[1:20],
                                   logloss_imputed_k10_M20[1:20],
                                   logloss_imputed_k20_M5[1:20],
                                   logloss_imputed_k20_M10[1:20],
                                   logloss_imputed_k20_M20[1:20],logloss_part[1:20]),
                       knn =  c(rep(1:20,10)), k = c(rep(c(5,10,20), each = 60),rep("No Imp",20)), M = c(rep(rep(c(5,10,20), each = 20),3),rep("No Imp",20)))
    
    fret$k <- factor(fret$k, levels = c("20","10","5","No Imp"))
    fret$M <- factor(fret$M, levels = c("20","10","5","No Imp"))
    
    library(ggplot2 )
    if (side == 1){
      g1 <- ggplot(aes(x = knn, y = logloss, col = k, shape = M), data = fret) + geom_point() + geom_line() + ggtitle(paste0("Tooth = ",tooth,", Side = 1")) + theme(legend.position = "none")
    }
    
    if (side == 2){
      g2 <- ggplot(aes(x = knn, y = logloss, col = k, shape = M), data = fret) + geom_point() + geom_line() + ggtitle(paste0("Tooth = ",tooth,", Side = 2"))
    }
    
    
    
  }
  
  # png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/logloss_by_tooth_by_knn_tribe_unscaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  library(gridExtra)
  legend <- get_legend(g2)
  g2 <- g2 + theme(legend.position="none")
  grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
  #dev.off()
}



#######################################################
##Figure 8 - Tribe Scaled Log Loss Results
#######################################################
library(dplyr)
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k5_M5 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k10_M10 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k20_M20 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k20_M5 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k20_M10 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k10_M5 <- logloss_imputed
    
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k5_M10 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k5_M20 <- logloss_imputed
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries.RData"))
    
    logloss_imputed_k10_M20 <- logloss_imputed
    
    
    n5_5 <- length(logloss_imputed_k5_M5)
    n10_10 <- length(logloss_imputed_k10_M10)
    n20_10 <- length(logloss_imputed_k20_M20)
    n20_5 <- length(logloss_imputed_k20_M5)
    n20_10 <- length(logloss_imputed_k20_M10)
    n10_5 <- length(logloss_imputed_k10_M5)
    n5_10 <- length(logloss_imputed_k5_M10)
    n5_20 <- length(logloss_imputed_k5_M20)
    n10_20 <- length(logloss_imputed_k10_M20)
    n_part <- length(logloss_part)
    
    fret <- data.frame(logloss = c(logloss_imputed_k5_M5[1:20],
                                   logloss_imputed_k5_M10[1:20],
                                   logloss_imputed_k5_M20[1:20],
                                   logloss_imputed_k10_M5[1:20],
                                   logloss_imputed_k10_M10[1:20],
                                   logloss_imputed_k10_M20[1:20],
                                   logloss_imputed_k20_M5[1:20],
                                   logloss_imputed_k20_M10[1:20],
                                   logloss_imputed_k20_M20[1:20],logloss_part[1:20]),
                       knn =  c(rep(1:20,10)), k = c(rep(c(5,10,20), each = 60),rep("No Imp",20)), M = c(rep(rep(c(5,10,20), each = 20),3),rep("No Imp",20)))
    
    fret$k <- factor(fret$k, levels = c("20","10","5","No Imp"))
    fret$M <- factor(fret$M, levels = c("20","10","5","No Imp"))
    
    library(ggplot2 )
    if (side == 1){
      g1 <- ggplot(aes(x = knn, y = logloss, col = k, shape = M), data = fret) + geom_point() + geom_line() + ggtitle(paste0("Tooth = ",tooth,", side = 1")) +  theme(legend.position="none")
    }
    
    if (side == 2){
      g2 <- ggplot(aes(x = knn, y = logloss, col = k, shape = M), data = fret) + geom_point() + geom_line() + ggtitle(paste0("Tooth = ",tooth,", side = 2"))
    }
    
    
    
  }
  
  #png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/logloss_by_tooth_by_knn_tribe_scaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  library(gridExtra)
  legend <- get_legend(g2)
  g2 <- g2 + theme(legend.position="none")
  grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
  #dev.off()
}


#######################################################
##Table 4 - Species Classification using size and shape and nearest neighborclassification (i.e.  k = 1)
#######################################################
res <- data.frame()

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    for (k in c(5,10,20)){
      for (M in c(5,10,20)){
try(load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"_summaries_species.RData")))

res <- rbind(res,data.frame(tooth = tooth,side = side,k = k,M = M,accuracy_imputed = acc_imputed[1], accuracy_part = acc_part[1]))

      }}}}

  test <- rbind(
        c("LM1",1,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 1],3)),
        c("LM1",2,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 2],3)),
        c("LM2",1,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 1],3)),
         c("LM2",2,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 2],3)),
        c("LM3",1,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 1],3)),
        c("LM3",2,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 2],3)),
        c("UM1",1,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 1],3)),
        c("UM1",2,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 2],3)),
        c("UM2",1,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 1],3)),
        c("UM2",2,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 2],3)),
        c("UM3",1,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 1],3)),
        c("UM3",2,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 2],3))
        )

  test <- cbind(test,round(res$accuracy_part[seq(1,nrow(res),9)],3))

  library(xtable)
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& & \\multicolumn{3}{c}{k=5} & \\multicolumn{3}{c}{k=10} & \\multicolumn{3}{c}{k=20} &  \\\\\n",
"Tooth & Side & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & Partial  \\\\\n")
print(xtable(test), add.to.row = addtorow, include.colnames = FALSE, include.rownames = FALSE)


#######################################################
##Table 5 - Species Classification using shape only and nearest neighborclassification (i.e.  k = 1)
#######################################################

res <- data.frame()

for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){
    for (k in c(5,10,20)){
      for (M in c(5,10,20)){
        try(load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=",k,"_M=",M,"_tooth=",tooth,"scaled_summaries_species.RData")))
        
        res <- rbind(res,data.frame(tooth = tooth,side = side,k = k,M = M,accuracy_imputed = acc_imputed[1], accuracy_part = acc_part[1]))
        
      }}}}

test <- rbind(
  c("LM1",1,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 1],3)),
  c("LM1",2,round(res$accuracy_imputed[res$tooth=="LM1" & res$side == 2],3)),
  c("LM2",1,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 1],3)),
  c("LM2",2,round(res$accuracy_imputed[res$tooth=="LM2" & res$side == 2],3)),
  c("LM3",1,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 1],3)),
  c("LM3",2,round(res$accuracy_imputed[res$tooth=="LM3" & res$side == 2],3)),
  c("UM1",1,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 1],3)),
  c("UM1",2,round(res$accuracy_imputed[res$tooth=="UM1" & res$side == 2],3)),
  c("UM2",1,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 1],3)),
  c("UM2",2,round(res$accuracy_imputed[res$tooth=="UM2" & res$side == 2],3)),
  c("UM3",1,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 1],3)),
  c("UM3",2,round(res$accuracy_imputed[res$tooth=="UM3" & res$side == 2],3))
)

test <- cbind(test,round(res$accuracy_part[seq(1,nrow(res),9)],3))

library(xtable)
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& & \\multicolumn{3}{c}{k=5} & \\multicolumn{3}{c}{k=10} & \\multicolumn{3}{c}{k=20} &  \\\\\n",
                      "Tooth & Side & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & M=5 & M=10 & M=20 & Partial  \\\\\n")
print(xtable(test), add.to.row = addtorow, include.colnames = FALSE, include.rownames = FALSE)

#######################################################
##Figure 9 - Accuracy of species classification unscaled
#######################################################

library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){#print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k20_M20 <- acc_part
    acc_imputed_k20_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    # dat <- data.frame(knn = rep(1:60,4), acc = c(acc_part_5,acc_imputed_5,acc_imputed_10,acc_imputed_20), type = rep(c("no imp","k5 M5","k10 M10", "k20 M20"),each = 60))
    # library(ggplot2)
    # g <- ggplot(aes(x = knn, y = acc, colour = type), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))
    # print(g)
    
    #print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    
    # acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
    #   accuracy  = c(mean(acc_imputed_k20_M20[1:20]),mean(acc_imputed_k20_M10[1:20]),mean(acc_imputed_k20_M5[1:20]),mean(acc_imputed_k10_M20[1:20]),mean(acc_imputed_k10_M10[1:20]),mean(acc_imputed_k10_M5[1:20]),mean(acc_imputed_k5_M20[1:20]),mean(acc_imputed_k5_M10[1:20]),mean(acc_imputed_k5_M5[1:20]),mean(acc_part_k20_M20[1:20])),
    #   tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    #knn = 1
    acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
      accuracy  = c(mean(acc_imputed_k20_M20[1]),mean(acc_imputed_k20_M10[1]),mean(acc_imputed_k20_M5[1]),mean(acc_imputed_k10_M20[1]),mean(acc_imputed_k10_M10[1]),mean(acc_imputed_k10_M5[1]),mean(acc_imputed_k5_M20[1]),mean(acc_imputed_k5_M10[1]),mean(acc_imputed_k5_M5[1]),mean(acc_part_k20_M20[1])),
      tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    
  }}

dat <- do.call(rbind, acc_res_list)
dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))
#png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_knn1_species_unscaled.png"), res = 300, units = "in", w = 8, h =4)
ggplot(aes(x = tooth_type, y = accuracy, col = k, shape = M), data = dat) + geom_point() + theme_grey()
#dev.off()



#######################################################
##Figure 10 - Accuracy of species classification scaled
#######################################################

library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  for (side in 1:2){#print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k20_M20 <- acc_part
    acc_imputed_k20_M20 <- acc_imputed
    rm(acc_part)
    rm(acc_imputed)
    
    # dat <- data.frame(knn = rep(1:60,4), acc = c(acc_part_5,acc_imputed_5,acc_imputed_10,acc_imputed_20), type = rep(c("no imp","k5 M5","k10 M10", "k20 M20"),each = 60))
    # library(ggplot2)
    # g <- ggplot(aes(x = knn, y = acc, colour = type), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))
    # print(g)
    
    #print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    
    # acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
    #   accuracy  = c(mean(acc_imputed_k20_M20[1:20]),mean(acc_imputed_k20_M10[1:20]),mean(acc_imputed_k20_M5[1:20]),mean(acc_imputed_k10_M20[1:20]),mean(acc_imputed_k10_M10[1:20]),mean(acc_imputed_k10_M5[1:20]),mean(acc_imputed_k5_M20[1:20]),mean(acc_imputed_k5_M10[1:20]),mean(acc_imputed_k5_M5[1:20]),mean(acc_part_k20_M20[1:20])),
    #   tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    #knn = 1
    acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(
      accuracy  = c(mean(acc_imputed_k20_M20[1]),mean(acc_imputed_k20_M10[1]),mean(acc_imputed_k20_M5[1]),mean(acc_imputed_k10_M20[1]),mean(acc_imputed_k10_M10[1]),mean(acc_imputed_k10_M5[1]),mean(acc_imputed_k5_M20[1]),mean(acc_imputed_k5_M10[1]),mean(acc_imputed_k5_M5[1]),mean(acc_part_k20_M20[1])),
      tooth_type = rep(paste0(tooth,"_",side),10),  k = c(rep(20,3),rep(10,3),rep(5,3),rep("No Imp",1)), M = c(rep(c(20,10,5),3),rep("No Imp",1)))
    
    
  }}

dat <- do.call(rbind, acc_res_list)
dat$k <- factor(dat$k, levels = c("20", "10", "5", "No Imp"))
dat$M <- factor(dat$M, levels = c("20", "10", "5", "No Imp"))
#png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_knn1_species_scaled.png"), res = 300, units = "in", w = 8, h =4)
ggplot(aes(x = tooth_type, y = accuracy, col = k, shape = M), data = dat) + geom_point() + theme_grey()
#dev.off()

########################################################
##Figure 11 - Species Unscaled accuracy results
########################################################
tooth <- "UM2"
side <- 1
library(ggplot2)
library(cowplot)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  #for (tooth in c("UM2")){
  for (side in 1:2){print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"_summaries_species.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"_summaries_species.RData"))
    acc_part <- acc_part[1:20]
    acc_imputed_k20_M20 <- acc_imputed[1:20]
    
    dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), k = c(rep("no imp", 20), rep(c(5,10,20),each = 60)), M = c(rep("no imp",20),rep(rep(c(5,10,20), each = 20),3)))
    
    dat$k <- factor(dat$k, levels = c("20","10","5","no imp"))
    dat$M <- factor(dat$M, levels = c("20","10","5","no imp"))
    
    # dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), type = rep(c("no imp","k5 M5","k5 M10","k5 M20","k10 M5","k10 M10", "k10 M20","k20 M5","k20 M10", "k20 M20"),each = 20))
    
    library(ggplot2)
    if (side == 1){
      g1 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side)) + ylim(0.1,.5) + theme(legend.position = "none")
    }
    
    if (side == 2){
      g2 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))  + ylim(0.1,.5)
    }
    
    
    
    
    # print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    #
    # acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(accuracy  = c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])), tooth_type = rep(paste0(tooth,"_",side),4),  M_k = c("M20 l20", "M10 l10", "M5 l5", "No Imp"))
    
  }
  
  # png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_by_knn_species_unscaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  library(gridExtra)
  legend <- get_legend(g2)
  g2 <- g2 + theme(legend.position="none")
  grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
  #dev.off()
  
  
}


########################################################
##Figure 12 - Species Scaled accuracy results
########################################################
tooth <- "LM1"
side <- 1
library(ggplot2)
acc_res_list <- list()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  #for (tooth in c("UM2")){
  for (side in 1:2){print(c(tooth, side))
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=5_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k5_M5 <- acc_part
    acc_imputed_k5_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=10_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k5_M10 <- acc_part
    acc_imputed_k5_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=5_M=20_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k5_M20 <- acc_part
    acc_imputed_k5_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=5_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k10_M5 <- acc_part
    acc_imputed_k10_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=10_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k10_M10 <- acc_part
    acc_imputed_k10_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=10_M=20_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k10_M20 <- acc_part
    acc_imputed_k10_M20 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=5_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k20_M5 <- acc_part
    acc_imputed_k20_M5 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=10_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part_k20_M10 <- acc_part
    acc_imputed_k20_M10 <- acc_imputed[1:20]
    rm(acc_part)
    rm(acc_imputed)
    
    load(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/results/summaries/results20190610_side=",side,"_k=20_M=20_tooth=",tooth,"scaled_summaries_species.RData"))
    acc_part <- acc_part[1:20]
    acc_imputed_k20_M20 <- acc_imputed[1:20]
    
    dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), k = c(rep("no imp", 20), rep(c(5,10,20),each = 60)), M = c(rep("no imp",20),rep(rep(c(5,10,20), each = 20),3)))
    
    dat$k <- factor(dat$k, levels = c("20","10","5","no imp"))
    dat$M <- factor(dat$M, levels = c("20","10","5","no imp"))
    
    #dat <- data.frame(knn = rep(1:20,10), acc = c(acc_part,acc_imputed_k5_M5,acc_imputed_k5_M10,acc_imputed_k5_M20,acc_imputed_k10_M5,acc_imputed_k10_M10,acc_imputed_k10_M20,acc_imputed_k20_M5,acc_imputed_k20_M10,acc_imputed_k20_M20), type = rep(c("no imp","k5 M5","k5 M10","k5 M20","k10 M5","k10 M10", "k10 M20","k20 M5","k20 M10", "k20 M20"),each = 20))
    
    library(ggplot2)
    if (side == 1){
      g1 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side)) + ylim(0.05,.4) + theme(legend.position = "none")
    }
    
    if (side == 2){
      g2 <- ggplot(aes(x = knn, y = acc, colour = k, shape = M), data = dat) + geom_point() + geom_line() + ggtitle(paste0("tooth=",tooth,", side=",side))  + ylim(0.05,.4)
    }
    
    
    
    
    #print(c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])))
    
    #acc_res_list[[paste0(tooth,"_",side)]] <- data.frame(accuracy  = c(mean(acc_imputed_20[1:20]),mean(acc_imputed_10[1:20]),mean(acc_imputed_5[1:20]),mean(acc_part_5[1:20])), tooth_type = rep(paste0(tooth,"_",side),4),  M_k = c("M20 l20", "M10 l10", "M5 l5", "No Imp"))
    
  }
  
 # png(paste0("/Users/gregorymatthews/Dropbox/shapeanalysisgit/manuscript/fig/acc_by_tooth_by_knn_species_scaled_",tooth,".png"), res = 300, units = "in", w = 8, h =4)
  library(gridExtra)
  legend <- get_legend(g2)
  g2 <- g2 + theme(legend.position="none")
  grid.arrange(g1, g2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
  #dev.off()
  
  
}
