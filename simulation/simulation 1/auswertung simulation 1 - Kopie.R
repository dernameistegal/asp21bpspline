library(simsalapar)

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "grid", value = 1000),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)))



res10 = maybeRead("simulation/simulation 1/take_2")
beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

n = get.n.sim(simulation1)

betares = matrix(0, nrow = 17, ncol = n)
gammares = matrix(0, nrow = 17, ncol = n)
for (i in 1:n)
{
  betares[,i] = res10[[i]]$value[,3]
  gammares[,i] = res10[[i]]$value[,4]
}
meanbeta = rowSums(betares) / n
meangamma = rowSums(gammares) / n

# Estimation of bias
biasbeta = meanbeta - beta
biasgamma = meangamma - gamma


# Estimation of SE of bias

sup = matrix(0, nrow = 17, ncol = n)
sop = matrix(0, nrow = 17, ncol = n)

for (i in 1:n)
{
 sup[,i] = (betares[,i] - meanbeta)^2
 sop[,i] = (gammares[,i] - meangamma)^2
}
SEbeta = sqrt(rowSums(sup) / (n * n -1))
SEgamma = sqrt(rowSums(sop) / (n * n -1))

data.frame(bias = biasbeta, SE = SEbeta)
data.frame(bias = biasgamma, SE = SEgamma)

