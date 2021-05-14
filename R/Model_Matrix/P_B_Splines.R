source("R\\Model_Matrix\\Model_Matrix.R")
# Ideen f?r Penaltymatrixc

#
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
#m ist die Anzahl der länge von beta_hat
Dmat2 <- function(m){
  dia <- diag(m)
  D <- cbind(0,dia) + cbind(-dia,0)
  return(D)
}


#
# p_order_diff      welcher Ordnung soll die penalization Difference sein
diff_mat <- function(kn, order ,p_order_diff)
{
  koe_nr <- kn + order - 1
  if (p_order_diff == 1)
  {
    return(Dmat2(koe_nr)[-koe_nr, 1:koe_nr])
  }
  D <- Dmat2(koe_nr + 2 * p_order_diff)
  D_help <- D
  for (i in 2:p_order_diff)
  {
    if (i %% 2 == 1)
    {
      D_help <- D_help %*% D
    }else
    {
      D_help <- D_help %*% t(D)
    }
  }
  #Zeilenzahl Ausgabe formatieren
  #ab welcher Zeile sind die Einträge nicht mehr kapuut
  ganz <- (floor(p_order_diff)/2) + 1
  # wie viele Zeilen braucht man insgesamt
  D_help <- D_help[ganz:(ganz + koe_nr - p_order_diff -1), ,drop=FALSE]
  #Spaltenzahl formatieren
  D_help <- D_help[,1:koe_nr]
  return(D_help)
}
# difference penalty matrix


# p_order Ordnung der Bestrafung
# lambda  Wie stark wird für Koeffizienten bestraft

spl <- function(z, y, kn, range, order, p_order, lambda)
{
  Z <- model_matrix(z = z, kn = kn,range = range, order = order)
  K <- crossprod(diff_mat(kn = kn, order = order ,p_order_diff = p_order))
  beta_hat <- solve(crossprod(Z) +lambda * K) %*% t(Z) %*% y
  y_hat <- Z %*% beta_hat
  spl_objekt <- list(z = z, y = y, kn = kn, range = range, order = order)
  class(spl_objekt) <- "spl"
  spl_objekt$y_hat <- y_hat
  return(spl_objekt)
  # return(y_hat)
}

x <- runif(1000,0,10)
y <- 0.5 * x + 0.5 * x^2 - 0.07 *x^3 +rnorm(1000, sd = 8)

y_hat <- spl(z = x,y = y, kn = 40,range = c(0,10), order = 2, p_order = 4, lambda = 200000)
print(y_hat)
#library(profvis)
#profvis(spl(z = p,y = o, kn = 50,range = c(0,10), order =3 ))







