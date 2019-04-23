load("/Users/gregorymatthews/Dropbox/shapeanalysisgit/data/ptsTrainList.RData")
ref_file <- read.csv("/Users/gregorymatthews/Dropbox/shapeanalysisgit/refFile.csv")
source("https://raw.githubusercontent.com/cran/fdasrvf/master/R/curve_functions.R")
source("https://raw.githubusercontent.com/cran/fdasrvf/master/R/utility_functions.R")

curve_to_q <- function (beta, scale = F) 
{
  n = nrow(beta)
  T1 = ncol(beta)
  v = apply(beta, 1, gradient, 1/(T1 - 1))
  v = t(v)
  q = matrix(0, n, T1)
  for (i in 1:T1) {
    L = sqrt(pvecnorm(v[, i], 2))
    if (L > 1e-04) {
      q[, i] = v[, i]/L
    }
    else {
      q[, i] = v[, i] * 1e-04
    }
  }
  
  if (scale == TRUE){
    q = q/sqrt(innerprod_q2(q, q))
    }
  
  return(q)
}


project_curve <- function(q, scale = F){
  T1 = ncol(q)
  n = nrow(q)
  if (n==2){
    dt = 0.35
  }
  
  if (n==3) {
    dt = 0.2
  }
  
  epsilon =- 1e-6
  
  e = diag(1,n)
  iter = 1
  res = rep(1,n)
  J = matrix(0,n,n)
  s = seq(0,1,length.out=T1)
  qnorm = rep(0,T1)
  G = rep(0,n)
  C = rep(0,301)
  
  qnew = q
  if (scale == TRUE){
  qnew = qnew / sqrt(innerprod_q2(qnew,qnew))
  }
  
  while (pvecnorm(res) > epsilon){
    if (iter > 300){
      break
    }
    
    # Compute Jacobian
    for (i in 1:n){
      for (j in 1:n){
        J[i,j]  = 3 * trapz(s, qnew[i,]*qnew[j,])
      }
    }
    J = J + diag(1,n)
    
    for (i in 1:T1){
      qnorm[i] = pvecnorm(qnew[,i])
    }
    
    # Compute the residue
    for (i in 1:n){
      G[i] = trapz(s,qnew[i,]*qnorm)
    }
    
    res = -1*G
    
    if (pvecnorm(res)<epsilon)
      break
    
    x = solve(J,res)
    C[iter] = pvecnorm(res)
    
    delG = find_basis_normal(qnew)
    tmp = 0
    for (i in 1:n){
      tmp = tmp + x[i]*delG[[i]]*dt
    }
    qnew = qnew + tmp
    
    iter = iter + 1
  }
  
  if (scale == TRUE){
    qnew = qnew / sqrt(innerprod_q2(qnew,qnew))
  }
  
  
  return(qnew)
}

library(fdasrvf)
beta1 <- t(ptsTrainList[["LM1"]][[1]])
beta2 <- t(ptsTrainList[["LM1"]][[2]])

inverse_exp_coord <- function(beta1, beta2, mode="O", rotated=T){
  T1 = ncol(beta1)
  centroid1 = calculatecentroid(beta1)
  dim(centroid1) = c(length(centroid1),1)
  beta1 = beta1 - repmat(centroid1, 1, T1)
  centroid2 = calculatecentroid(beta2)
  dim(centroid2) = c(length(centroid2),1)
  beta2 = beta2 - repmat(centroid2, 1, T1)
  
  q1 = curve_to_q(beta1)
  
  if (mode=="C"){
    isclosed = TRUE
  }
  
  # Iteratively optimize over SO(n) x Gamma using old DP
  out = reparam_curve(beta1, beta2, rotated=rotated, isclosed=isclosed, mode=mode)
  if (mode=="C")
    beta2 = shift_f(beta2, out$tau)
  
  beta2 = out$R %*% beta2
  gamI = invertGamma(out$gam)
  beta2 = group_action_by_gamma_coord(beta2, gamI)
  if (rotated){
    out = find_rotation_seed_coord(beta1, beta2, mode)
    q2n = curve_to_q(out$beta2new)
  } else {
    q2n = curve_to_q(beta2)
  }
  
  
  if (mode=="C"){
    q2n = project_curve(q2n)
  }
  
  # Compute geodesic distance
  q1dotq2 = innerprod_q2(q1-q2n, q1-q2n)
  #if (q1dotq2>1){
  #  q1dotq2 = 1.
  #}
  
  #dist = acos(q1dotq2)
  dist <- q1dotq2
  
  # u = q2n - q1dotq2 * q1
  # normu = sqrt(innerprod_q2(u,u))
  # 
  # if (normu > 1e-4){
  #   v = u*acos(q1dotq2)/normu
  # } else {
  #   v = matrix(0, nrow(beta1), T1)
  # }
  
  return(dist=dist)
}


calc_shape_dist <- function (beta1, beta2, mode = "O") 
{
  out = inverse_exp_coord(beta1, beta2, mode)
  return(out)
}

library(fdasrvf)
beta1 <- t(ptsTrainList[["LM1"]][[1]])
beta2 <- 3*t(ptsTrainList[["LM1"]][[2]])

calc_shape_dist(beta1,beta2, mode = "C")
