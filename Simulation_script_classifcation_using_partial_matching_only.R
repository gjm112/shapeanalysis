###  nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script2.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script2.Rout

### nohup R CMD BATCH --vanilla /home/gmatthews1/shapeAnalysis/R/simulation_script1.R &
### tail -f /home/gmatthews1/shapeAnalysis/R/simulation_script1.Rout

start_all <- Sys.time()
library(fdasrvf)
library(parallel)

M <- 5
k <- 5
side <- 1 #could be 1 or 2.
tooth <- "LM1"

#setwd("/home/gmatthews1/shapeAnalysis")
source("./R/utility.R")
source("./R/curve_functions.R")
source("./R/calc_shape_dist_partial.R")
source("./R/complete_partial_shape.R")
source("./R/impute_partial_shape.R")
source("./R/tooth_cutter.R")

ref_file <- read.csv("./data/refFile.csv")
load("./data/data_set_of_full_teeth.RData")
load("./data/ptsTrainList.RData")


#results_list <- list()
for (d in 98:length(ptsTrainList[[tooth]])){
  print(d)
  print(Sys.time())
  partial_shape <- t(tooth_cutter(ptsTrainList[[tooth]][[d]])[[side]])
  complete_shape_list <- lapply(ptsTrainList[[tooth]], t)
  
  ##complete_shape_list <- list(complete_shape_list[[1]],complete_shape_list[[2]],complete_shape_list[[3]],complete_shape_list[[4]],complete_shape_list[[5]],complete_shape_list[[6]],complete_shape_list[[7]],complete_shape_list[[8]],complete_shape_list[[9]],complete_shape_list[[10]])
  #names(complete_shape_list) <- names(ptsTrainList[[tooth]])[1:10]
  
  #I can't impute the partial shape with itself!
  complete_shape_list[[d]]<-NULL
  
  
  
  #Now do classification on the completed shapes just using closest 
  ref_file <- read.csv("./data/refFile.csv")
  DSCN_target <- names(ptsTrainList[["LM1"]])[[d]]
  truth <- subset(ref_file,ref == DSCN_target)
  
  
  
  #Classify based on closest match between partial and all full teeth.  
  fret <- mclapply(complete_shape_list,calc_shape_dist_partial,partial_shape = partial_shape)
  dist_partial <- data.frame(DSCN = names(unlist(fret)), dist = unlist(fret))
  
  dist_partial <- merge(dist_partial,ref_file,by.x = "DSCN",by.y = "ref", all.x = TRUE)
  
  results_list[[DSCN_target]] <- dist_partial
  print(dist)
  
  end_all <- Sys.time()
  end_all-start_all
  outfile <- paste0("./results/results20190514_partial_matching_only_side=",side,"_tooth=",tooth,".RData")
  save.image(outfile)
}
