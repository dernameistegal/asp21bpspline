#source necessary files and generate model object

source("R/spline/print_function.R")
source("R/spline/estimation.R")
source("R/spline/init.R")
source("R/spline/spline.R")
source("R/spline/mcmc_framework_jonathan.R")
m = m$spline



# evaluate code performance with profvis and rprof (set its to 1 for this purpose)
library(profvis)
profvis({
  mcmc(m)
})

Rprof("mcmc_prof.log", line.profiling = TRUE)
mcmc(m)
Rprof(NULL)
summaryRprof("mcmc_prof.log", lines = "show")

#test without code performance evaluation
lol = mcmc(m)


