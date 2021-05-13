####Einstieg konstruieren eines Splines der Ordnung 1#####
set.seed(100)
x <- runif(100,0,20)
y <- sin(x) +rnorm(100)
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
  if (kj1 <= z & z <= kj2 ){

    return(1)
  }else{return(0)}
}


#Hier wird jetzt die Grundfunktion für die Knotenpunkte definiert

# range     In welchem Bereich soll definiert werden (Vektor mit 2 Elementen)
# kn        Welche Knotenzahl ist gewünscht
# order     Welche order soll der

splinmitte <- function(range, kn, order)
{
  #distanz zwischen zwei Punkten
  distance <- function(object)
  {
    return(object[2] - object[1])
  }
  #abstand zwischen 2 Punkten
  onedist <- distance(range) / (kn - 1)
  return(seq(from = (range[1] - onedist * order), to = (range[2] + onedist * order),
      length.out = (kn  + 2 * order)))
}


#kp     Platz auf der x Achse des Knots
#j      Nummer des entsprechenden Knots
#order  welcher order soll der Spline angehören
#z      der Wert an der Stelle an der ausgewertet wird


splin <- function(kp, j, z, order)
{
  if (order == 0)
  {
    return(B_0(z, kp[j], kp[j + 1]))
  }
  k <- ((z - kp[j - order])/(kp[j] - kp[j - order])) * splin(kp, j-1 , z, order - 1) +
    ((kp[j + 1] - z)/(kp[j + 1] - kp[j + 1 - order]))* splin(kp, j, z, order - 1)
  return(k)
}


splines <- function(z, range, kn, order, j)
{
  kp <- splinmitte(range, kn, order)
  #SPlinmittelpunkt
  j <- j + order
  splin(kp, j, z, order)
}


# wie wird es dann grafisch dargestellt: Eigentlich erst später durchführbar, aber hier schonmal Gerüst

splines(0.1,range = c(0,10), kn = 11,order = 1,j = 1)

p <- ggplot(data = data.frame(x = c(0, 10)), aes(x))# + ylim(c(0,3))
test <-function(){
  p + stat_function(fun = Wrapper,n = 1000, geom = "area", fill = "blue", alpha = 0.5)
}

Wrapper <- function(z){
  sapply(z, splines, range = c(0,10), kn = 11,order = 2,j = 4)
}
test()

z <- c(2.4,3.2,4.4)
kn <- 11
model <- matrix(0, nrow = length(z), ncol = kn - 1)
for ( i in 1:length(z)){
  for( j in 1:(kn - 1)){
    model[i,j] <- splines( z = z[i] ,range = c(0,10), kn = 11,order = 5, j = j )
  }
}

####Model_Matrix####
#jetzt wird die Modell Matrix (Z) definiert
#z    Werte der Kovariaten
#kn   Gewünschte Knotenzahl
model_matrix <- function(z, kn, range, order){
  model <- matrix(0, nrow = length(z), ncol = kn - 1)
  for ( i in 1:length(z))
  {
    for( j in 1:(kn - 1))
    {
      model[i,j] <- splines( z = z[i] ,range = c(0,10), kn = 11,order = 2, j = j )
    }
  }
  return(model)
}
Z <- model_matrix(z = x, kn = 11,range = c(0,10), order = 3)
koeffizienten <- crossprod(Z) %*% t(Z) %*% y
koeffizienten

####Zusammensetzen der FUnktion funktioniert nohc nicht####
# jetzt soll die Funktion zusammengefügt werden

ts <- numeric(20)
for (p in seq(0,10, length.out = 20)) {
  summe <- 0
  #bildet die Summe der einzelnen
  for ( j in 1:10){
    summe <- summe + koeffizienten[j]* splines( z = p,range = c(0,10), kn = 11,order = 2, j = j)
  }
  ts[p] <- summe
}


ts
#splines(z = 0.1,range = c(0,10), kn = 11,order = 1,j = 1)



