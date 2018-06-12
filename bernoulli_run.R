
#library(rstan)
library(rethinking)

# maximum penalized likelihood
N <- 5
y <- c(0,1,1,0,0)

model <- stan_model("bernoulli.stan")

mle <- optimizing(model, data = c("N", "y"))

# Initial log joint probability = -3.36507
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance

print(mle, digits = 2)
# $par
# theta 
#   0.4 
# 
# $value
# [1] -3.4
# 
# $return_code
# [1] 0

# posterior distribution
curve(dbeta(x, 1 + 2, 1 + 3), 0, 1)
grid()

# mode 0.4

# mean 
(1 + 2)/(1 + 2 + 1 + 3)
# [1] 0.4285714

# Bayes
fit <- stan("bernoulli.stan", data = c("N", "y"))
print(fit, digits = 2)

# Inference for Stan model: bernoulli.
# 4 chains, each with iter=2000; warmup=1000; thin=1; 
# post-warmup draws per chain=1000, total post-warmup draws=4000.
# 
#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# theta  0.43    0.00 0.17  0.13  0.30  0.43  0.55  0.78  1405    1
# lp__  -5.29    0.02 0.72 -7.33 -5.46 -5.01 -4.83 -4.78  1583    1
# 
# Samples were drawn using NUTS(diag_e) at Mon Jun  4 17:44:13 2018.
# For each parameter, n_eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor on split chains (at 
# convergence, Rhat=1).

#hist(extract(fit)$theta)
curve(dbeta(x, 1 + 2, 1 + 3), 0, 1, ylim = c(0, 2.5))
dens(extract(fit)$theta, col = "red", add = T)
grid()

# vectorized version

fit <- stan("bernoulli_2.stan", data = c("N", "y"))
print(fit, digits = 2)

# Inference for Stan model: bernoulli_2.
# 4 chains, each with iter=2000; warmup=1000; thin=1; 
# post-warmup draws per chain=1000, total post-warmup draws=4000.
# 
#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# theta  0.43    0.00 0.17  0.12  0.31  0.43  0.56  0.77  1682    1
# lp__  -5.32    0.02 0.79 -7.52 -5.48 -5.01 -4.83 -4.78  1260    1
# 
# Samples were drawn using NUTS(diag_e) at Mon Jun  4 18:04:07 2018.
# For each parameter, n_eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor on split chains (at 
# convergence, Rhat=1).

precis(fit)
#       Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# theta 0.43   0.17       0.14        0.7  1682    1

plot(fit)
# ci_level: 0.8 (80% intervals)
# outer_level: 0.95 (95% intervals)

plot(precis(fit))
# simpler, also interval

