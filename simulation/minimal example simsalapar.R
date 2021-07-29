require(simsalapar)


simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "grid", value = c(10, 20, 50, 100)),
  nvars = list(type = "grid", value = c(1, 2))
)

toLatex(simulation1)
mkGrid(simulation1)

doOne = function(n, nvars)
{
  x = matrix(0, nrow = n, ncol = nvars)
  for (i in 1:nvars)
  {
    x[,i] = rnorm(n)
  }
  y = rowSums(x)
  R = sum(lm(y~x)$coef)
  return(R)
}
#doOne(10, 1)
res = doLapply(simulation1, sfile = "simulation/simtest", doOne = doOne)
array2df(getArray(res))
str(getArray(res))
val <- getArray(res)
err <- getArray(res, "error")
warn <- getArray(res, "warning")
time <- getArray(res, "time")

rv <- c( "nvars")
cv <- c( "n")
ftable(100 * (err + warn), row.vars = rv, col.vars = cv)
ftable(time, row.vars = rv, col.vars = cv)
