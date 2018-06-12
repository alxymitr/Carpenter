
library(rethinking)

N <- 50L
alpha <- 2
beta <- 0.93
sigma <- 1.5
y1 <- -10

gen <- function(N, alpha, beta, sigma, y1) {
  y <- rep(NA, N)
  y[1] <- y1
  
  for (n in 2:N) {
    y[n] <- rnorm(1, mean = alpha + beta * y[n - 1], sd = sigma)
  }
  y
}

y <- gen(N, alpha, beta, sigma, y1)

plot(y, type = "o")

model <- stan_model("AR1_y1.stan")

mle <- optimizing(model, data = c("N", "y"))
# Initial log joint probability = -11138.3
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance

mle
# $par
#     alpha      beta     sigma 
# 2.2550675 0.9270759 1.5797678 
# 
# $value
# [1] -47.6605
# 
# $return_code
# [1] 0

# в этой модели стартовое значение y остается неизвестным
# это мешающий параметр

fit <- stan("AR1_y1.stan", data = c("N", "y"))

precis(fit)
#       Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha 2.25   0.47       1.53       3.01  1839    1
# beta  0.93   0.02       0.89       0.96  1826    1
# sigma 1.66   0.18       1.38       1.94  1989    1

post <- extract(fit)

gendata <- matrix(NA, nrow = 4000, ncol = N)
for (i in 1:4000) {
  gendata[i,] <- gen(N, post$alpha[i], post$beta[i], post$sigma[i], rnorm(1, -10, 2))
}

gendata.PI <- apply(gendata, 2, PI)

plot(y, type = "o", ylim = c(-10, 50))
rethinking::shade(gendata.PI, 1:N)
