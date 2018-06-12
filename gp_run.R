# Gaussian process estimation

library(rethinking)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(mvtnorm)

model <- stan_model("gp.stan")

# generate data -----------------------------------------------------------

N <- 50L

x <- 1:N

eta_sq <- 0.5
inv_rho_sq <- 10
sigma_sq <- 0.7

rho_sq <- 1/inv_rho_sq

Sigma <- matrix(NA, nrow = N, ncol = N)
for (i in 1:(N - 1)) {
  for (j in (i + 1):N) {
    Sigma[i, j] = eta_sq * exp(-rho_sq * (x[i] - x[j]) ^ 2)
    Sigma[j, i] = Sigma[i, j]
  }
}

for (k in 1:N)
  Sigma[k,k] = eta_sq + sigma_sq;

y <- rmvnorm(1, rep(0,N), Sigma)

y <- as.vector(y)

plot(x, y, type = "o")

# fit model ---------------------------------------------------------------

fit <-
  stan("gp.stan",
       data = c("N", "x", "y"),
       iter = 5000)

precis(fit)
#             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# eta_sq      0.45   0.36       0.00       0.83  5297    1
# inv_rho_sq 11.88  25.06       0.01      21.54  5422    1
# sigma_sq    0.75   0.22       0.44       1.08  4095    1
# rho_sq      0.41   2.12       0.00       0.61  4230    1

# very close to theoretical values

pairs(fit)
