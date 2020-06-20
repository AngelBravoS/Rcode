######
#CMTD#
######

#matrices P
P1 <-matrix(c(0.8, 0.2, 0, 
              0.16, 0.68, 0.16, 
              0, 0.16, 0.84
), nrow = 3, byrow = TRUE)
P1

P2 <-matrix(c(0.7, 0.15, 0.15, 
              0.3, 0.6, 0.1, 
              0, 0.4, 0.6
), nrow = 3, byrow = TRUE)

P2

P <- P1
P <- P2

#Función para el cálculo de la matriz P^n
matrizPn <- function(n){
  if(n == 1) {
    return (P)
  } else {
    return (P%*%matrizPn(n-1))
  }
}

#Función para el cálculo de la matriz Mn 
#Tiempo medio de permanencia
#número esperado de visitas en el instante n
matrizMn <- function(n){
  k <- ncol(P)
  I <- diag(k)
  sumP <- matrix(0, nrow = k, ncol = k)
  for (i in 1:n) {
    sumP <- sumP+matrizPn(i) 
  }
  Mn <- I+sumP
  return(Mn)
}

matrizMn(2)

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

#Vector rj
#VIGILA LOS ÍNDICES/ESTADOS
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






#Diplomatura, no interesa
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
