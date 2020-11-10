load("./results/classifier_tribe.RData")
load("./results/classifier_species.RData")
load("./results/classifier_imputations.RData")

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
