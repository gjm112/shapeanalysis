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