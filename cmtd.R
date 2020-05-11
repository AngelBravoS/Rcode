#matriz P

P <-matrix(c(0.5, 0.5, 0, 0, 0, 
             0.5, 0, 0.5, 0, 0, 
             0, 0.5, 0, 0.5, 0,
             0, 0, 0.5, 0, 0.5, 
             0, 0, 0, 1, 0
             ), nrow = 5, byrow = TRUE)

P <-matrix(c(0.2, 0.3, 0.5, 
             0.1, 0, 0.9, 
             0.55, 0, 0.45
              ), nrow = 3, byrow = TRUE)
P

P <-matrix(c(0.0009, 0.0582, 0.9409, 
             0.0006, 0.0488, 0.9506, 
             0.0004, 0.0392, 0.9604
), nrow = 3, byrow = TRUE)
P


P <-matrix(c(0.03, 0.97,
             0.02, 0.98 
          ), nrow = 2, byrow = TRUE)
P

P <-matrix(c(0.5, 0.2,
             0.6, 0 
              ), nrow = 2, byrow = TRUE)
Q


Q <-matrix(c(0, 0.2,
             0.3, 0.4 
            ), nrow = 2, byrow = TRUE)
Q

B <-matrix(c(0.3, 0,
             0, 0.3 
), nrow = 2, byrow = TRUE)
B
#CMTD

#Función para el cálculo de la matriz P^n
matrizPn <- function(n){
  if(n == 1) {
    return (P)
  } else {
    return (P%*%matrizPn(n-1))
  }
}

matrizPn(1)

#Función para el cálculo de la matriz Mn 
#número esperado de visitas
matrizMn <- function(n){
  n <- ncol(P)
  I <- diag(n)
  
  Mn <- (I-matrizPn(n+1))*solve(I-P)
  return(Mn)
}

matrizMn(3)

#Función para el cálculo del vector de transiciones
vectorPi <- function(){
  n <- ncol(P)
  I <- diag(n)
  matrizAuxiliar <- P-I
  for(i in 1:n){
    matrizAuxiliar[,n] = 1
  }
  pi<-I[,n]%*%solve(matrizAuxiliar)
  print("vector de la distribución límite")
  print(pi)
}

vectorPi()

# Elemento rj
rj <- function(j){
  Pestr <- P[-j,-j]
  n <- ncol(Pestr)
  vector1 <-c(replicate(n, 1))
  I <- diag(n)
  R <- solve(I-Pestr)
  R <- R*vector1
  rj <- vector()
  for(i in 1:n){
    rj <- append(rj,c(sum(R[i,])))
  }
  return(rj)
}

rj(3)


#Función para el cálculo de la matriz RQ
matrizRQ <- function(n){
  n <- ncol(Q)
  I <- diag(n)
  
  #cuadrante RQ
  RQ = solve(I-Q)
  #return(RQ)
}

RQ <- matrizRQ()
RQ

#Función para el cálculo de la matriz F
matrizF <- function(){
  RQ <- matrizRQ()
  n <- ncol(RQ)
  F <- matrix(nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      if (isTRUE(i==j)){
        F[i,j]= 1-1/RQ[i,j]
      } else {
        F[i,j]= RQ[i,j]/RQ[j,j]
      }
    }
  }
  return(F)
}

F <- matrizF()
F





