
library(rethinking)
library(MASS)

N <- 50L

tt <- 1:N

xcos <- cos(2*pi*tt/N)
xsin <- sin(2*pi*tt/N)

alpha <- 2
beta_cos <- 0.5
beta_sin <- -0.3
theta <- 2

mu <- exp(alpha + beta_cos * xcos + beta_sin * xsin)
y <- rnegbin(N, mu = mu, theta = theta)

# Function to generate random outcomes from a Negative Binomial distribution,
# with mean mu and variance mu + mu^2/theta.

plot(mu, type = "l", col = "red", ylim = c(0, 20))
lines(y, type = "o")

model <- stan_model("negbin_trig.stan")

(mle <- optimizing(model, data = c("N", "xcos", "xsin", "y")))
# Initial log joint probability = -222.276
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance
# $par
#      alpha   beta_cos   beta_sin      theta 
#  1.8816966  0.3596087 -0.2580791  1.8101928 
# 
# $value
# [1] 389.5196
# 
# $return_code
# [1] 0

fit <- stan("negbin_trig.stan", data = c("N", "xcos", "xsin", "y"))

precis(fit)
#           Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha     1.89   0.12       1.70       2.09  4000    1
# beta_cos  0.36   0.17       0.09       0.63  4000    1
# beta_sin -0.26   0.16      -0.51       0.00  4000    1
# theta     1.88   0.50       1.09       2.57  4000    1

# очень странно: n_eff = 4000!