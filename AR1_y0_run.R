
library(rethinking)

N <- 50L
alpha <- 2
beta <- 0.93
sigma <- 1.5
y0 <- -10

gen <- function(N, alpha, beta, sigma, y0) {
  y <- rep(NA, N)
  
  for (n in 1:N) {
    y[n] <-
      rnorm(1,
            mean = alpha + beta *  ifelse(n == 1, y0, y[n - 1]),
            sd = sigma)
  }
  y
}

y <- gen(N, alpha, beta, sigma, y0)

plot(y, type = "o")

model <- stan_model("AR1_y0.stan")

mle <- optimizing(model, data = c("N", "y"))
mle
# $par
#     alpha      beta     sigma        y0 
#  1.466396  0.950397  1.491995 -8.620545 
# 
# $value
# [1] -45.93383
# 
# $return_code
# [1] 0

fit <- stan("AR1_y0.stan", data = c("N", "y"))

precis(fit)
#        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha  1.47   0.40       0.83       2.08  1942    1
# beta   0.95   0.02       0.92       0.98  2045    1
# sigma  1.57   0.16       1.33       1.84  2969    1
# y0    -8.67   1.33     -10.77      -6.54  2816    1

post <- extract(fit)

dens(post$y0)

gendata <- matrix(NA, nrow = 4000, ncol = N)
for (i in 1:4000) {
  gendata[i,] <- gen(N, post$alpha[i], post$beta[i], post$sigma[i], post$y0[i])
}

gendata.PI <- apply(gendata, 2, PI)

plot(y, type = "o", ylim = c(-10, 40))
rethinking::shade(gendata.PI, 1:N)

