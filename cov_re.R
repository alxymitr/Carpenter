# covariance random-effects priors

library(rethinking)

# generate data -----------------------------------------------------------

# number of groups
G <- 5L

# group sizes
nn <- c(10, 20, 30, 20, 30)

# group indicator
gg <- rep(1:G, nn)

table(gg)
# gg
#  1  2  3  4  5 
# 10 20 30 20 30 

N <- length(gg)

# regression coefficients
betas <- matrix(c(5, 1,
                  4, 0.5,
                  3, 0.75,
                  4, 0.5,
                  3, 1), nrow = G, ncol = 2, byrow = TRUE)

betas
#      [,1] [,2]
# [1,]    5 1.00
# [2,]    4 0.50
# [3,]    3 0.75
# [4,]    4 0.50
# [5,]    3 1.00

xx <- NULL
yy <- NULL

for (g in 1:G) {
  x <- 1:nn[g]
  beta0 <- betas[g, 1]
  beta1 <- betas[g, 2]
  y <- beta0 + beta1 * x + rnorm(nn[g], 0.5)
  
  xx <- c(xx, x)
  yy <- c(yy, y)
}

plot(xx, yy, type = "n")
for (g in 1:G) {
  ii <- gg == g
  text(xx[ii], yy[ii], g, col = g)
}

x <- xx
y <- yy

# fit model ---------------------------------------------------------------

model <- stan_model("cov_re.stan")

fit <- stan("cov_re.stan", data = c("N", "G", "x", "y", "gg"))

precis(fit, depth = 2)
#               Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# beta[1,1]     4.86   0.69       3.78       5.97  3026    1
# beta[1,2]     1.05   0.11       0.87       1.22  3208    1
# beta[2,1]     3.63   0.46       2.91       4.33  4000    1
# beta[2,2]     0.60   0.04       0.54       0.66  4000    1
# beta[3,1]     3.81   0.38       3.21       4.42  4000    1
# beta[3,2]     0.74   0.02       0.71       0.77  4000    1
# beta[4,1]     4.38   0.48       3.66       5.20  3302    1
# beta[4,2]     0.54   0.04       0.48       0.61  3441    1
# beta[5,1]     3.80   0.37       3.16       4.33  3451    1
# beta[5,2]     1.00   0.02       0.97       1.03  4000    1
# L_Omega[1,1]  1.00   0.00       1.00       1.00  4000  NaN
# L_Omega[1,2]  0.00   0.00       0.00       0.00  4000  NaN
# L_Omega[2,1] -0.27   0.31      -0.75       0.22  4000    1
# L_Omega[2,2]  0.91   0.10       0.76       1.00  4000    1
# sigma[1]      1.53   0.66       0.66       2.42  4000    1
# sigma[2]      2.52   1.03       1.15       3.61  4000    1
# sigy          1.03   0.07       0.91       1.15  4000    1

