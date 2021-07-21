

library(ggplot2)
####B-Splin Erzeugung####

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

splinmitte(c(0,10), 22, 3)
setitup(22, 3, c(0,10), 10/21)

# Definition des Grundsplines, auf dem die anderen aufbauen
B_0 <- function(z,kj1,kj2){
  if (kj1 <= z & z <= kj2 ){

    return(1)
  }else{return(0)}
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


####Grafische Darstellung der Splines####

p <- ggplot(data = data.frame(x = c(0, 10)), aes(x))# + ylim(c(0,3))
test <-function()
{
  p + stat_function(fun = Wrapper,n = 1000, geom = "area", fill = "blue", alpha = 0.5)
}

Wrapper <- function(z)
{
  sapply(z, splines, range = c(0,10), kn = 11,order = 7,j = 1)
}
test()

####Model_Matrix####
#jetzt wird die Modell Matrix (Z) definiert
#z    Werte der Kovariaten
#kn   Gewünschte Knotenzahl
model_matrix <- function(z, kn, range, order)
{
  # Zahl der insgesamt benötigten Splines
  nr_splines <- kn + order - 1
  model <- matrix(0, nrow = length(z), ncol = nr_splines)
  for ( i in 1:length(z))
  {
    for( j in 1:(nr_splines))
    {
      # model Matrix wird befüllt
      model[i,j] <- splines(z = z[i] ,range = range, kn = kn,order = order, j = j )
    }
  }
  return(model)
}
####Spline Modell Schätzen####
# ts tab
set.seed(100)
x <- runif(1000,0,10)
y <- 0.5 * x + cos(x) +rnorm(1000)


"
z     float           Wert der erklärenden Variable
y     float           Wert der zu erklärenden Variable
kn    integer         Zahl der Knoten die verwendet werden soll
range 2 float vector  VOn wo bis wo soll simuliert werden
order interger        Grad der verwendeten B-Splines

"
spl <- function(z, y, kn, range, order)
{
  Z <- model_matrix(z = z, kn = kn,range = range, order = order)
  beta_hat <- solve(crossprod(Z)) %*% t(Z) %*% y
  y_hat <- Z %*% beta_hat
  spl_objekt <- list(z = z, y = y, kn = kn, range = range, order = order)
  class(spl_objekt) <- "spl"
  spl_objekt$y_hat <- y_hat
  return(spl_objekt)
  # return(y_hat)
}
#### Einfacher Print der Funktion ####
print.spl <- function(spl_objekt)
{
  ggplot(mapping = aes(x = spl_objekt[["z"]])) +
    geom_point(aes(y = spl_objekt[["y"]])) +
    geom_line(aes(y = spl_objekt[["y_hat"]]), colour = "green", size = 2)+
    ylab("dependent variable")+
    xlab("explaining variable")
}

#### Model Beispiel ####
y_hat <- spl(z = x,y = y, kn = 10,range = c(0,10), order = 2)
#library(profvis)
#profvis(spl(z = p,y = o, kn = 50,range = c(0,10), order =3 ))

print(y_hat)
#splines(z = 9.9 ,range = c(0,10), kn = 10,order = 2, j = 51 )
