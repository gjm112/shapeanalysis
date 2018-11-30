######################################################
#This function takes in two main arguments: a complete tooth and a partial tooth.  
#This and it returns k donor teeth
######################################################
#complete_shape: ONE completed shape. p x n. p = 2 in our setting. 
#partial_shape: ONE partial shape.  p x n.

get_cumdel <- function(X){
  n <- dim(X)[1]
  T <- dim(X)[2]
  del <- c()
  del[1] <- 0
  for (r in 2:T){
    del[r] <- norm(as.matrix(X[,r]) - as.matrix(X[,r-1]), type = "F")
  }
  cumdel <- cumsum(del)/sum(del)
  cumdel
}


ShiftF <- function(vec, rotations){
  vec_index <- 1:ncol(vec)
  vec_index <- vec_index - rotations
  vec <- cbind(vec[,sign(vec_index) == 1], vec[,sign(vec_index) != 1])
  vec
}

find_best_rotation <- function(q1, q2){
  n <- dim(q1)[1]
  T <- dim(q1)[2]
  
  A <- q1%*%t(q2)
  svdA <- svd(A)
  #If A is pos def made the diagonl elements all 1?
  if (det(A) > 0){
    svdA$d <- rep(1,n)
  }
  else {
    #don't know what is happening here?  
    #Make the last element negative?  
    svdA$d <- rep(1,n)
    svdA$d[length(svdA$d)] <- -svdA$d[length(svdA$d)]
  }
  R <- svdA$u%*%diag(svdA$d)%*%svdA$v
  q2new <- R%*%q2
  return(list(R = R, q2new = q2new))
}


library(caTools)
#Finds the inner product of functions.  
InnerProd_Q <- function(q1, q2){
  n <-  dim(q1)[1]
  T <-  dim(q1)[2]
  val <- trapz(seq(0,1,length = T),apply(q1*q2,2,sum))
  return(val)
}


library(fdasrvf)
load("/Users/gregorymatthews/Dropbox/StatisticalShapeAnalysis/data_set_of_full_teeth.RData")
#Full shape
complete_shape <- t(ptsTrainList[[1]][[1]])
#Partial Shape.
partial_shape <- t(ptsTrainList[[1]][[1]][11:42,])

calc_shape_dist(template,cm)
calc_shape_dist_partial(complete_shape,partial_shape)

calc_shape_dist_partial <- function(complete_shape, partial_shape){
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
  library(fdasrvf)
  #Note: resamplecurve is using splines 
  #Does this sampl ing need to 
  partial_shape_closed <- resamplecurve(partial_shape_closed,N_complete_new)
  
  newpt <- which(t < olddel[N_partial])
  newpt <- newpt[length(newpt)]
  
  #Resample the complete and missing parts
  partial_shape_closed_obs <- resamplecurve(partial_shape_closed[,1:newpt],N)
  partial_shape_closed_mis <- resamplecurve(partial_shape_closed[,newpt:dim(partial_shape_closed)[2]],N)
  
  #Find the centroid of the observed part
  cent1 <- apply(partial_shape_closed_obs,1, mean)
  
  #Centering
  partial_shape_closed_obs <- partial_shape_closed_obs - cent1
  partial_shape_closed_mis <- partial_shape_closed_mis - cent1
  
  #scale factor
  sc1 <- norm(partial_shape_closed_obs, type = "F")
  
  #Scaling the shape
  partial_shape_closed_obs <- partial_shape_closed_obs/sc1
  partial_shape_closed_mis <- partial_shape_closed_mis/sc1
  
  minE <- Inf
  #I think we are looking across all strting points around the curve?
  for (j in 0:(N_complete-1)){
    #What does shiftF do??
    #Why N_complete - 1 and not just N_complete??????
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
    
    sc2=norm(mu1, type = "F")
    mu1=mu1/sc2
    mu2=mu2/sc2
    
    #Finding the best rotation
    out <- find_best_rotation(partial_shape_closed_obs,mu1)
    R <- out$R
    q2new <- out$q2new
    
    mu1n <- R%*%mu1
    
    Ec <- InnerProd_Q(partial_shape_closed_obs-mu1n,partial_shape_closed_obs-mu1n)
    if (Ec < minE){
      Rbest <- R
      complete_shape_obs <- Rbest%*%mu1
      complete_shape_mis <- Rbest%*%mu2
      minE <- Ec
    }
    
    
  }
  
  
  
  #mu <- cbind(complete_shape_obs,complete_shape_mis[,2:dim(complete_shape_mis)[2]])
  
  #ccompnew <- cbind(partial_shape_obs,partial_shape_mis[,2:dim(partial_shape_mis)[2]])
  
  dist <- calc_shape_dist(complete_shape_obs,partial_shape_closed_obs)  
  
  return(dist)

  
}

distance[s] <- calc_shape_dist(ccompnew, mu, mode = 'C')
