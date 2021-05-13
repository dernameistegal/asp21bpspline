####Einstieg konstruieren eines Splines der Ordnung 1#####
set.seed(100)
x <- runif(1000,0,20)
y <- sin(x) +rnorm(1000)
plot(x,y)

Knoten <- seq(1,10, length.out = 10)

B_0 <- function(z,kj1,kj2){
  if (kj1 < z & z < kj2 ){
    return(1)
  }else{return(0)}
}
# der einser Spline
B_1 <- function(z,kj1,kj2,kj3){
  k <- ((z - kj1)/(kj2 - kj1))*B_0(z, kj1, kj2) +
    ((kj3 - z)/(kj3 - kj2))*B_0(z, kj2, kj3)
  return(k)
}


# Zeichnen der Splines
library(ggplot2)

Wrapper <- function(z){
  sapply(z, B_1, kj1 = 1, kj2 = 2, kj3 = 3)
}
p <- ggplot(data = data.frame(x = c(3, 8)), aes(x))# + ylim(c(0,3))
p1_1 <- stat_function(fun = Wrapper,n = 1000, geom = "area", fill = "blue", alpha = 0.5)
test <-function(){
  p + stat_function(fun = Wrapper,n = 1000, geom = "area", fill = "blue", alpha = 0.5)
}

####Systematisieren####
# Welche Informationen braucht eine Splinefunktion? Z_Wert, Gesamtzahl der Knoten, Order
# was will ich jetzt schaffen?  Intervall 0 - 10 sollen Splines eines beliebigen grades gebildet werden

# Definition des Grundsplines, auf dem die anderen aufbauen
B_0 <- function(z,kj1,kj2){
  if (kj1 < z & z < kj2 ){
    return(1)
  }else{return(0)}
}

# wie wird es dann grafisch dargestellt: Eigentlich erst später durchführbar, aber hier schonmal Gerüst
p <- ggplot(data = data.frame(x = c(1, 10)), aes(x))# + ylim(c(0,3))
test <-function(){
  p + stat_function(fun = Wrapper,n = 1000, geom = "area", fill = "blue", alpha = 0.5)
}

#Hier wird jetzt die Splinesfunktion definiert



knotposition <- function(range, kn, order){
  #distanz zwischen zwei Punkten
  distance <- function(object){
    return(object[2] - object[1])
  }
  #abstand zwischen 2 Punkten
  onedist <- distance(range) / (kn - 1)
  seq(from = (range[1] - onedist * order), to = (range[2] + onedist * order),
      length.out = (kn  + 2 * order))
}

kp <- knotposition(range = c(0,10), 11, 2)
#kp     Platz auf der x Achse des Knots
#j      Nummer des entsprechenden Knots
#order  welcher order soll der Spline angehören
#z      der Wert an der Stelle an der ausgewertet wird


Spline <- function(kp, j, z, order){
  if (order == 0){
    return(B_0(z, kp[j], kp[j + 1]))
  }
  k <- ((z - kp[j - order])/(kp[j] - kp[j - order])) * Spline(kp, j-1 , z, order - 1) +
    ((kp[j + 1] - z)/(kp[j + 1] - kp[j + 1 - order]))* Spline(kp, j, z, order - 1)
  return(k)
}

splines <- function(z, range, kn, order, j){
  kp <- knotposition(range, kn, order)
  #damit die richtige Knotenposition angegeben wird
  j <- j + order
  Spline(kp, j, z, order)
}

splines(0.001,range = c(0,10), kn = 2,order = 5,j = 1)

Wrapper <- function(z){
  sapply(z, splines, range = c(0,10), kn = 11,order = 1,j = 3)
}
test()




