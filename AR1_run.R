
library(rethinking)

N <- 50L

alpha <- 2
beta <- 0.93
sigma <- 1.5
y1 <- -10

y <- rep(NA, N)
y[1] <- y1

for (n in 2:N) {
  y[n] <- rnorm(1, mean = alpha + beta * y[n - 1], sd = sigma)
}
  
plot(y, type = "o")

# MLE

model <- stan_model("AR1.stan")

mle <- optimizing(model, data = c("N", "y"))

# Initial log joint probability = -15420.9
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance

mle

# $par
#     alpha      beta     sigma 
# 2.5205258 0.8953923 1.6328562 
# 
# $value
# [1] -49.31251
# 
# $return_code
# [1] 0

fit <- stan("AR1.stan", data = c("N", "y"))

post <- extract(fit)

str(post)
# List of 4
#  $ alpha: num [1:4000(1d)] 2 2.12 2.39 2.22 2.28 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ beta : num [1:4000(1d)] 0.913 0.891 0.89 0.898 0.904 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ sigma: num [1:4000(1d)] 1.84 1.64 1.73 1.52 1.47 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ lp__ : num [1:4000(1d)] -49.9 -50.9 -49.3 -49.8 -49.6 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL

pairs(fit)

dens(post$alpha)
dens(post$beta)
dens(post$sigma)

tracerplot(fit)

# генерация "оболочки" для данных

gen <- function(N, alpha, beta, sigma, y1) {
  y <- rep(NA, N)
  y[1] <- y1
  
  for (n in 2:N) {
    y[n] <- rnorm(1, mean = alpha + beta * y[n - 1], sd = sigma)
  }
  y
}

gendata <- matrix(NA, nrow = 4000, ncol = N)
for (i in 1:4000) {
  gendata[i,] <- gen(N, post$alpha[i], post$beta[i], post$sigma[i], -10)
}

gendata.PI <- apply(gendata, 2, PI)

plot(y, type = "o")
rethinking::shade(gendata.PI, 1:N)
