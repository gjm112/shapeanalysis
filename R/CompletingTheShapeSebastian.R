library(fdasrvf)
load("/Users/gregorymatthews/Dropbox/StatisticalShapeAnalysis/data_set_of_full_teeth.RData")
#Full shape
template <- t(ptsTrainList[[1]][[1]])
#Partial Shape.
cm <- t(ptsTrainList[[1]][[1]][11:42,])


#Dimension
d <- dim(template)[1]
#Number of points for template and cm
Ninit <- dim(template)[2]
Ncm <- dim(cm)[2]

#What is this doing? 
#This initializes the line connecting the shape.  
t <- seq(0,1,length = 100)
x0 <- matrix(NA,ncol = 100,nrow = 2)
for (j in 1:d){
  x0[j,] <- (1-t)*cm[j,Ncm] + t*cm[j,1]
}

#Closed shape with linear interpolation.
ccomp <- cbind(cm,x0[,2:100])

Nccomp <- 500
t <- seq(0,1,length = Nccomp)
minE <- Inf

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

#I think this is the percentage along the curve that you have traveled.  
#Find this before resampling the curve
olddel <- get_cumdel(ccomp)

N <- 100
library(fdasrvf)
#Note: resamplecurve is using splines 
#Does this sampl ing need to 
ccomp <- resamplecurve(ccomp,Nccomp)




#What is this doing?  
#This is measuring the distance traveled around the shape!
newpt <- which(t < olddel[Ncm])
newpt <- newpt[length(newpt)]

#Resample the complete and missing parts
ccomp1 <- resamplecurve(ccomp[,1:newpt],N)
ccomp2 <- resamplecurve(ccomp[,newpt:dim(ccomp)[2]],N)

#Find the centroid of the observed part
cent1 <- apply(ccomp1,1, mean)

#Centering
ccomp1 <- ccomp1 - cent1
ccomp2 <- ccomp2 - cent1

#scale factor
sc1 <- norm(ccomp1, type = "F")

#Scaling the shape
ccomp1 <- ccomp1/sc1
ccomp2 <- ccomp2/sc1


#What does this function do? 
ShiftF <- function(p, tau){
  pn <- p
  n <- dim(p)[1]
  T <- dim(p)[2]
  if (tau == 0){
    pn <- p
    return(pn)
  }
  if (tau > 0){
    pn[,1:(T-tau)] <- p[,(tau+1):T]
    pn[,(T-tau+1):T] <- p[,1:tau]
  }
  else 
  {
    t = abs(tau) + 1
    pn[,1:(T-t+1)] <- p[,t:T]
    pn[,(T-t+2):T] <- p[,1:(t-1)]
  }
  return(pn)
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

#I think we are looking across all strting points around the curve?
for (j in 0:(Ninit-1)){print(j)
  #What does shiftF do??
  #Why Ninit - 1 and not just Ninit??????
 mu <- ShiftF(template[,1:(Ninit-1)],j) 
 mu <- cbind(mu,mu[,1])
 
 olddel1 <- get_cumdel(mu)
 
 N <- 100
 library(fdasrvf)
 mu <- resamplecurve(mu,Nccomp)
 
 newpt1 <- which(t < olddel1[Ncm])
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
 out <- find_best_rotation(ccomp1,mu1)
 R <- out$R
 q2new <- out$q2new
 
 mu1n <- R%*%mu1
 
 Ec <- InnerProd_Q(ccomp1-mu1n,ccomp1-mu1n)
 if (Ec < minE){
   Rbest <- R
   mu11 <- Rbest%*%mu1
   mu21 <- Rbest%*%mu2
   minE <- Ec
 }
 
 
}

mu <- cbind(mu11,mu21[,2:dim(mu21)[2]])

ccompnew <- cbind(ccomp1,ccomp2[,2:dim(ccomp2)[2]])




n <- 40
tn <- seq(0,1, length = N)

#Why minus 1 in cos?  
#Defining basic functions
b <- matrix(NA, nrow = 2*n, ncol = length(tn))
for (j in 1:n){
b[j,] <- sin(2*pi*j*tn)/(sqrt(2)*pi*j)
b[j+n,] <- (cos(2*pi*j*tn)-1)/(sqrt(2)*pi*j)
}

###Greg stuff 
# v <- ccomp2 - mu21
# 
# theta[1:80] <- rep(1,80)
# theta[81:160] <- runif(80)
#   
# minimize_theta <- function(theta,mu21){
# xxx <- theta[1:80]%*%b
# yyy <- theta[81:160]%*%b
# 
# tempcomp <- rbind(xxx,yyy)
# v <- tempcomp - mu21
# 
# out <- trapz(tn,abs(v[1,])) + trapz(tn,abs(v[2,]))
# return(out)
# 
# }
# 
# minimize_theta(rep(0.1,160),mu21 = mu21)
# 
# help(optim)
# test <- optim(rep(1,160),minimize_theta,mu21=mu21, method = "SANN")
# 




n <- dim(b)[1]
iter <- 1
eps <- 15

while(iter < 500){print(iter)

  v <- ccomp2 - mu21
  
#Computing each basis component and then adding them.  
gradE <- matrix(0,nrow = 2,ncol = N)
  for (j in 1:n){
    for (k in 1:d){
  val <- trapz(tn,v[k,]*b[j,])*b[j,]
  gradE[k,] <- gradE[k,]+val
    }}
  
  ngE <- trapz(tn,apply(gradE*gradE,2,sum))
  
  ccomp2 <- ccomp2-eps*gradE
  
  ccompnew <- cbind(ccomp1,ccomp2[,2:dim(ccomp2)[2]])
  
  ngE
  
  
  #figure(2),clf;
  plot(ccompnew[1,],ccompnew[2,], pch = 16, xlim = c(-0.4,0.2),ylim = c(-0.2,0.2))
  #axis equal
  #hold on
  
  points(mu[1,],mu[2,], col = "red", pch = 16)
  
  iter=iter+1
    
  
  
  
}

plot(t(ccompnew))
