
# Ideen f�r Penaltymatrixc
Dmat <- function(m,n){
  D <- matrix(0, nrow = m, ncol = n)
  for(i in 1:m){
    for (j in 1:n){
       if (i == j){
          D[i,j] <- -1
       }
       if (i == j - 1){
           D[i ,j] <- 1
       }
    }
  }
  return(D)
}
Dmat(3,4)
#bildet Matriten einer bestimmten Form
Dmat2 <- function(m){
  dia <- diag(m)
  D <- cbind(0,dia) + cbind(-dia,0)
  return(D)
}
# m ist die Anzal der Zeilen, r ist die anzahl welches R betrachet wird
r <- function(m,r){
  #die Anzahl der Zahlen die ausgeblendet werden müssen entspricht 2^r/2 auf jeder Seite
  del <- 2^r/2
  #daher müssen auch soviel extrazeilen erzeugt werden, wenn am Ende die Zahl stimmen soll
  #- 1 die bei der Entsteheung von D zusätzlich entsteht
  m <- m + 2^r -1
 #D <- Dmat(m,m+1)
 D <- Dmat2(m)
  for (i in 1:r) {
    D <- crossprod(D)
    #D <- t(D) %*% D
  }
  return(D[(del+1):(m-del+1),])
}
library(microbenchmark)
microbenchmark(r(5,9), times = 4)

t(r(5,1)) %*% r(5,1)

Dmat2(5)
r(5,2)
