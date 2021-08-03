
data = read.csv("message.txt", sep = "\t")
head(data)
require("stringr")
require(asp21bpspline)
data$Word.Count = stringr::str_remove(data$Word.Count, ",")
data$Word.Count = as.numeric(data$Word.Count)
plot(data$Chapter, data$Word.Count, type = "l")

require(TTR)
plot(SMA(data$Word.Count, n = 27), type = "l")


decompose(data$Word.Count)
data =list(x =data$Chapter, y = as.numeric(data$Word.Count), z =data$Chapter)




m = spline(data,kn = c(10, 10), order = c(3,3), p_order = c(3,3), smooth = c(1, 1))

plot(m)
sample = mcmc.spline(m, it = 10000, burning = 100, thinning = 5)
plot(sample, m)

lol = sample

n = length(lol$tau)
seq1 = seq(1, n, length.out = n)
plot(seq1, lol$beta[, 2], type = "l")
