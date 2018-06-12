
library(rethinking)

N <- 50L
alpha <- 2
beta <- 0.93
sigma <- 1.5
y0 <- -10
df <- 5L

gen <- function(N, alpha, beta, sigma, y0, df) {
  y <- rep(NA, N)
  
  for (n in 1:N) {
    mu <- alpha + beta *  ifelse(n == 1, y0, y[n - 1])
    y[n] <- mu + sigma * rt(1, df)
  }
  y
}

y <- gen(N, alpha, beta, sigma, y0, df)

plot(y, type = "o")

model <- stan_model("AR1_y0_t.stan")

(mle <- optimizing(model, data = c("N", "y", "df")))
# Initial log joint probability = -199.242
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance
# $par
#      alpha       beta      sigma         y0 
#  2.1809903  0.9129368  1.6732735 -8.9021754 
# 
# $value
# [1] -64.98455
# 
# $return_code
# [1] 0

fit <- stan("AR1_y0_t.stan", data = c("N", "y", "df"))

precis(fit)
#        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha  2.23   1.09       0.54       4.00  1610    1
# beta   0.91   0.05       0.83       0.99  1772    1
# sigma  1.78   0.24       1.37       2.12  2467    1
# y0    -8.86   2.00     -12.16      -5.76  2608    1

post <- extract(fit)

# dens(post$y0)

gendata <- matrix(NA, nrow = 4000, ncol = N)
for (i in 1:4000) {
  gendata[i,] <- with(post, gen(N, alpha[i], beta[i], sigma[i], y0[i], df))
}

gendata.PI <- apply(gendata, 2, PI)

plot(y, type = "o", ylim = c(-10, 40))
rethinking::shade(gendata.PI, 1:N)

# estimate df -------------------------------------------------------------

model <- stan_model("AR1_y0_tdf.stan")

fit <- stan("AR1_y0_tdf.stan", data = c("N", "y"))

precis(fit)
#        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha  2.39   0.55       1.54       3.30  2196    1
# beta   0.91   0.03       0.86       0.95  2128    1
# sigma  1.51   0.20       1.20       1.83  2663    1
# y0    -9.77   1.46     -11.99      -7.46  2812    1
# df     6.97   2.01       4.16      10.00  2721    1

post <- extract(fit)

dens(post$df)

gendata <- matrix(NA, nrow = 4000, ncol = N)
for (i in 1:4000) {
  gendata[i,] <- with(post, gen(N, alpha[i], beta[i], sigma[i], y0[i], df))
}

gendata.PI <- apply(gendata, 2, PI)

plot(y, type = "o", ylim = c(-10, 40))
rethinking::shade(gendata.PI, 1:N)

