junk <- list()
for (q in 1:368) {
  junk[[q]] <- list()
}

for (q in 1:368) {
  for (r in 1:2) {
    distance <- c()
    for (s in 1:368) {





#Full shape
template <- t(ptsTrainList[[1]][[s]])


#Partial Shape.
cm <- t(cut_teeth[[q]][[r]])


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
  x0[j,] <- (1 - t) * cm[j, Ncm] + t * cm[j, 1]
}

#Closed shape with linear interpolation.
ccomp <- cbind(cm,x0[,2:100])

Nccomp <- 500
t <- seq(0,1,length = Nccomp)


#I think this is the percentage along the curve that you have traveled.  
#Find this before resampling the curve
olddel <- get_cumdel(ccomp)

N <- 100
library(fdasrvf)
#ccomp <- resamplecurve(ccomp,Nccomp)


#What is this doing?  
#This is measuring the distance traveled around the shape!
#newpt <- which(t < olddel[Ncm])
#newpt <- newpt[length(newpt)]

#Resample the complete and missing parts
ccomp1 <- resamplecurve(ccomp[,1:(dim(ccomp)[2] - (dim(x0)[2] - 1))],N)
ccomp2 <- resamplecurve(ccomp[,(dim(ccomp)[2] - (dim(x0)[2] - 1)):dim(ccomp)[2]],N)

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


minE <- Inf #Moved this here so that I could redo the function more easily.

#I think we are looking across all strting points around the curve?
for (j in 0:(Ninit - 1)){print(j)
  #What does shiftF do??
  
  mu <- ShiftF(template[,1:(Ninit)],j) 
  mu <- cbind(mu, mu[,1]) #Do this, otherwise resampling does not treat the object as a full shape
  olddel1 <- get_cumdel(mu)
  
  N <- 100
  library(fdasrvf)
  mu <- resamplecurve(mu,Nccomp)
  
  newpt1 <- which(t < olddel1[Ncm])
  newpt1 <- newpt1[length(newpt1)]
  
  mu1 <- resamplecurve(mu[,1:newpt1],N) 
  mu2 <- resamplecurve(mu[,newpt1:ncol(mu)],N) 
  
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

muccompnew <- cbind(ccomp1,ccomp2[,2:dim(ccomp2)[2]])




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
  
  gradE <- matrix(0,nrow = 2,ncol = N)
  for (j in 1:n){
    for (k in 1:d){
      val <- trapz(tn,v[k,]*b[j,])%*%b[j,]
      gradE[k,] <- gradE[k,]+val
    }}
  
  ngE <- trapz(tn,apply(gradE*gradE,2,sum))
  
  ccomp2 <- ccomp2-eps*gradE
  
  ccompnew <- cbind(ccomp1,ccomp2[,2:dim(ccomp2)[2]])
  
  print(ngE)
  
  
  #figure(2),clf;
  #axis equal
  #hold on
  
  
  
  
  
  iter=iter+1
}
distance[s] <- calc_shape_dist(ccompnew, mu, mode = 'C')
    }
    junk[[q]][[r]] <- distance
  }
}
