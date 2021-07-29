


n = get.n.sim(simulation1)

pars = cbind(beta, gamma)

betares = matrix(0, nrow = 17, ncol = n)
gammares = matrix(0, nrow = 17, ncol = n)
for (i in 1:n)
{
  betares[,i] = res[[i]]$value[,1]
  gammares[,i] = res[[i]]$value[,2]
}

# is the estimator unbiased?
mb = rowSums(betares) / n
mg = rowSums(gammares) / n


mb - beta
mg - gamma


