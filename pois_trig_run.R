
// Poisson data with trigonometric trend 

library(rethinking)

N <- 50L

tt <- 1:N

xcos <- cos(2*pi*tt/N)
xsin <- sin(2*pi*tt/N)

alpha <- 2
beta_cos <- 0.5
beta_sin <- -0.3

lamb <- exp(alpha + beta_cos * xcos + beta_sin * xsin)
y <- rpois(N, lamb)

plot(lamb, type = "l", col = "red", ylim = c(0, 20))
lines(y, type = "o")

model <- stan_model("pois_trig.stan")

(mle <- optimizing(model, data = c("N", "xcos", "xsin", "y")))
# Initial log joint probability = -866.097
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance
# $par
#      alpha   beta_cos   beta_sin 
#  2.0584404  0.4559340 -0.2266098 
# 
# $value
# [1] 494.9746
# 
# $return_code
# [1] 0

fit <- stan("pois_trig.stan", data = c("N", "xcos", "xsin", "y"))

precis(fit)
#           Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha     2.06   0.05       1.97       2.14  3287    1
# beta_cos  0.46   0.07       0.34       0.57  3442    1
# beta_sin -0.22   0.07      -0.34      -0.11  3342    1

post <- extract(fit)

dens(post$alpha)
dens(post$beta_cos)
dens(post$beta_sin)

gendata <- matrix(NA, nrow = 4000, ncol = N)
for (i in 1:4000) {
  gendata[i, ] <-
    with(post, rpois(N, exp(alpha[i] + beta_cos[i] * xcos + beta_sin[i] * xsin)))
}

gendata.PI <- apply(gendata, 2, PI)

plot(y, type = "o", ylim = c(0, 20))
rethinking::shade(gendata.PI, 1:N)
