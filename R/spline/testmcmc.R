# test
library(profvis)
source("R/spline/mcmc_framework_jonathan.R")

# evaluate code performance with profvis and rprof

profvis({
  mcmc(m)
})

Rprof("mcmc_prof.log", line.profiling = TRUE)
mcmc(m)
Rprof(NULL)
summaryRprof("mcmc_prof.log", lines = "show")

#test without code performance evaluation

lol = mcmc(m)

lol$gamma

