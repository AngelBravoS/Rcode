#Matrices Q de ejemplos

lambda <- 1
mu <- 0.1
t <- 31

#por filas
Q <- matrix(c(-lambda, lambda, mu, -mu), ncol=2, byrow = TRUE)
Q

Q <- matrix(c(-10,10,0,0,0,0, 
              15,-25,10,0,0,0,
              0,15,-25,10,0,0,
              0,0,15,-25,10,0,
              0,0,0,15,-25,10,
              0,0,0,0,15,-15
), ncol=6, byrow = TRUE)
Q

Q <-matrix(c(-6, 6, 0, 0, 0, 0, 
             5, -11, 6, 0, 0, 0,
             0, 5, -11, 6, 0, 0,
             0, 0, 5, -11, 6, 0, 
             0, 0, 0, 0, -5, 5,
             0, 0, 5, 0, 0, -5
), nrow = 6, byrow = TRUE)
Q

Q <- matrix(c(-1,1,0,0,0, 
              1/72,-73/72,1,0,0,
              0,2/72,-74/72,1,0,
              0,0,3/72,-39/72,1/2,
              0,0,0,4/72,-4/72
), ncol=5, byrow = TRUE)
Q

Q <- matrix(c(-4,4,0,0,0,0,0, 
              0.5,-4.5,4,0,0,0,0,
              0,1,-5,4,0,0,0,
              0,0,1.5,-5.5,4,0,0,
              0,0,0,2,-6,4,0,
              0,0,0,0,2.5,-6.5,4,
              0,0,0,0,0,3,-3
), ncol=7, byrow = TRUE)
Q

#función para el cálculo de P(t)

Pt <- function(t){
  library(expm)
  
  vectoresYValores <- eigen(Q)
  A <- vectoresYValores$vectors
  A1 <- solve(A)
  D <- diag(n)*vectoresYValores$values
  
  Dt <- D*t
  eDt <- expm(Dt)
  Pt <- A%*%eDt%*%A1
  return(Pt)
}

Pt(24)


#función para el cálculo de M(t)
Mt <- function(t){
  library(expm)
  n <-ncol(Q)
  
  vectoresYValores <- eigen(Q)
  A <- vectoresYValores$vectors
  A1 <- solve(A)
  D <- diag(n)*vectoresYValores$values
  
  Dt <- D*t
  eDt <- expm(Dt)
  
  n <- ncol(eDt)
  for (i in 1:n){
    # de no hacer un redondeo, es posible que un valor propio no sea exactamente 0
    #invalidando la comparación
    if (round(D[i,i],5)!=0){ 
      eDt[i,i] = (eDt[i,i]-1)/D[i,i]
    } else {
      eDt[i,i] = t
    }
  }
  Mt <- A%*%eDt%*%A1
  return(Mt)
}

Mt(24)


#vector P
vectorp <- function(){
  n <- ncol(Q)
  vector1y0 <-c(1)
  vector1y0 <- append(vector1y0, c(replicate(n-1, 0)))
  vector1y0
  
  Qest <- Q
  Qest[,1] <- c(1)
  Qest
  return(vector1y0%*%solve(Qest))
}

vectorp()