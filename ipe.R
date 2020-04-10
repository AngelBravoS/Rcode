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

Q <-matrix(c(0.5, 0.2,
             0.6, 0 
              ), nrow = 2, byrow = TRUE)
Q


#CMTD

#Función para el cálculo de las matrices RQ y 
#el cuadrante de F que realmente interesa
matricesRQ_y_F <- function(){
  n <- ncol(Q)
  I <- diag(n)
  
  #cuadrante RQ
  RQ = solve(I-Q)
  print('Matriz RQ')
  print(RQ)

  #Cuadrante inferior derecho de la matriz F
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
  print('Cuadrante inferior derecho de la Matriz F')
  print(F)
}

matricesRQ_y_F()

#Función para el cálculo de la función P^n
matrizPn <- function(n){
  if(n == 1) {
    return (P)
  } else {
    return (P%*%matrizPn(n-1))
  }
}

matrizPn(1)

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


#CMTC
