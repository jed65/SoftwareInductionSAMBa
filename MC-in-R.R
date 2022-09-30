### Project 6
## Monte Carlo integration
g = function(x, y){
  return(1/(1 + (sin(x))^2 + (sin(y))^2))
}

N = 10000
set.seed(1); u = runif(N)
set.seed(2); v = runif(N)
mean(g(u,v))

# Increasing the number of sampling
NN = seq(100, 100000, by = 1000)
MC_int = rep(0, length(NN))
for(i in 1:length(NN)){
  set.seed(1); u = runif(NN[i])
  set.seed(2); v = runif(NN[i])
  MC_int[i] = mean(g(u,v))
}
plot(NN, MC_int, pch = 16, xlab = "Sample dimension", ylab = "MC value")
title("MC Integral value")
lines(NN, MC_int)
abline(h = 0.675024, col = "red")


## Importance sampling with multivariate Normal
library(mvtnorm)
# rmvnorm(1, c(0,0), diag(1, 2)); dmvnorm(c(1,1), c(0,0), diag(1, 2))
N = 1000
set.seed(1); u = rmvnorm(N, c(0,0), diag(1, 2))
mean(g(u[,1], u[,2])*dunif(u[,1])*dunif(u[,2])/(dmvnorm(u, c(0,0), diag(1, 2))))

# Increasing the number of sampling
NN = seq(100, 100000, by = 1000)
MC_int = rep(0, length(NN))
for(i in 1:length(NN)){
  set.seed(1)
  u = rmvnorm(NN[i], c(0,0), diag(1, 2))
  MC_int[i] = mean(g(u[,1], u[,2])*dunif(u[,1])*dunif(u[,2])/(dmvnorm(u, c(0,0), diag(1, 2))))
}
plot(NN, MC_int, pch = 16, xlab = "Sample dimension", ylab = "MC value")
title("MC Integral value - Normal IS")
lines(NN, MC_int)
abline(h = 0.675024, col = "red")


## Importance sampling with multivariate Beta
N = 1000
set.seed(1); u = rbeta(N, 2, 2)
set.seed(2); v = rbeta(N, 2, 2)
mean(g(u, v)/(dbeta(u, 2, 2)*dbeta(v, 2, 2)))

# Increasing the number of sampling
NN = seq(100, 100000, by = 1000)
MC_int = rep(0, length(NN))
for(i in 1:length(NN)){
  set.seed(1); u = rbeta(NN[i], 2, 2)
  set.seed(2); v = rbeta(NN[i], 2, 2)
  MC_int[i] = mean(g(u, v)/(dbeta(u, 2, 2)*dbeta(v, 2, 2)))
}
plot(NN, MC_int, pch = 16, xlab = "Sample dimension", ylab = "MC value")
title("MC Integral value - Beta IS")
lines(NN, MC_int)
abline(h = 0.675024, col = "red")

