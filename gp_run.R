# Gaussian process estimation

library(rethinking)
options(mc.cores = parallel::detectCores())

# don't use: creates big files
# rstan_options(auto_write = TRUE)

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

# predict new values ------------------------------------------------------

# not very good sampling

M <- 10L
x_tilde <- (N + 1):(N + M)

model <- stan_model("gp_pred.stan")
# hash mismatch so recompiling; make sure Stan code ends with a blank line

fit <-
  stan("gp_pred.stan",
       data = c("N", "x", "y", "M", "x_tilde"),
       iter = 5000)

precis(fit, depth = 2)
#              Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# eta_sq       0.46   0.35       0.03       0.84  5047    1
# inv_rho_sq  14.41 147.13       0.02      20.92  5571    1
# sigma_sq     0.74   0.22       0.42       1.10  2373    1
# y_tilde[1]   0.21   1.03      -1.48       1.75 10000    1
# y_tilde[2]   0.14   1.05      -1.58       1.75 10000    1
# y_tilde[3]   0.09   1.07      -1.59       1.81 10000    1
# y_tilde[4]   0.05   1.08      -1.55       1.85  7570    1
# y_tilde[5]   0.03   1.09      -1.70       1.76 10000    1
# y_tilde[6]   0.01   1.09      -1.71       1.75 10000    1
# y_tilde[7]   0.00   1.10      -1.85       1.62 10000    1
# y_tilde[8]   0.00   1.08      -1.68       1.74 10000    1
# y_tilde[9]   0.01   1.07      -1.73       1.67 10000    1
# y_tilde[10]  0.01   1.07      -1.65       1.75 10000    1
# rho_sq       0.42   1.69       0.00       0.62  2054    1
# Предупреждение:
# В precis(fit, depth = 2) :
#   There were 12 divergent iterations during sampling.
# Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.

# show predictions --------------------------------------------------------

post <- extract(fit)

y_tilde.mean <- apply( post$y_tilde , 2 , mean )
y_tilde.PI <- apply( post$y_tilde , 2 , PI )

plot(x, y, type = "o", xlim = c(1, 60), ylim = c(-2.5, 2.5))
lines(x_tilde, y_tilde.mean, col = "red")
grid()
shade(y_tilde.PI, x_tilde)
