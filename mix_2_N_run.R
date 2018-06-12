# mixture of 2 normals

library(rethinking)

N1 <- 20L
N2 <- 30L
N <- N1 + N2

mu1 <- 5
sig1 <- 1

mu2 <- 8
sig2 <- 2

y <- c(rnorm(N1, mu1, sig1), rnorm(N2, mu2, sig2))

dens(y)

# fit model ---------------------------------------------------------------

model <- stan_model("mix_2_N.stan")

fit <-
  stan(
    "mix_2_N.stan",
    data = c("N", "y"),
    iter = 10000,
    init = function() {
      list(
        lambda = 0.6,
        mu = c(mu1, mu2),
        sigma = c(sig1, sig2)
      )
    }
  )

precis(fit, depth = 2)
#          Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# lambda   0.68   0.12       0.50       0.84  2095    1
# mu[1]    6.05   1.74       4.35       9.15   329    1
# mu[2]    7.26   1.15       5.06       8.67   441    1
# sigma[1] 1.15   0.60       0.26       1.94   908    1
# sigma[2] 1.97   0.51       1.02       2.66   707    1

dens(y, ylim = c(0, 0.3))
curve(
  0.68 * dnorm(x, 6.05, 1.15) + 0.32 * dnorm(x, 7.26, 1.97),
  1,
  15,
  add = TRUE,
  col = "red"
)

