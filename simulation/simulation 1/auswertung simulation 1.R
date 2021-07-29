
beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

n = get.n.sim(simulation1)

betares = matrix(0, nrow = 17, ncol = n)
gammares = matrix(0, nrow = 17, ncol = n)
for (i in 1:n)
{
  betares[,i] = res10[[i]]$value[,1]
  gammares[,i] = res10[[i]]$value[,2]
}

# is the estimator unbiased?
mb = rowSums(betares) / n
mg = rowSums(gammares) / n


mb - beta
mg - gamma


