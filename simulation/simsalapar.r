

require("simsalapar")
varList <- varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 32),
  n = list(type = "grid", value = c(64, 256)),
  d = list(type = "grid", value = c(5, 20, 100, 500)),
  varWgts = list(type = "frozen", expr = quote(bold(w)),
                 value = list("5" = 1, "20" = 1, "100" = 1, "500" = 1)),
  qF = list(type = "frozen", expr = quote(F^{-1}),
              value = list(qF = qnorm)),
  family = list(type = "grid", expr = quote(C),
                  value = c("Clayton", "Gumbel")),
  tau = list(type = "grid", value = c(0.25, 0.5)),
  alpha = list(type = "inner", value = c(0.95, 0.99, 0.999)))

toLatex(varList, label = "tab:var", caption = "Variables which determine our simulation study.")
