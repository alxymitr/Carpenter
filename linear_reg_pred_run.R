# linear regression with prediction

library(rethinking)

N <- 10L
x <- 1:N
y <- 10 + 0.5 * x + rnorm(N)
plot(x, y)

# predict
Nnew <- 5L
xnew <- 11:15

model <- stan_model("linear_reg_pred.stan")

fit <-
  stan(
    "linear_reg_pred.stan",
    data = c("N", "x", "y", "Nnew", "xnew"),
    iter = 10000
  )

precis(fit, depth = 2)
#          Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha    9.74   0.72       8.66      10.88  5969    1
# beta     0.48   0.12       0.29       0.66  4648    1
# sigma    1.01   0.31       0.57       1.40  4473    1
# ynew[1] 14.98   1.27      13.05      17.01  8659    1
# ynew[2] 15.44   1.34      13.37      17.54  7270    1
# ynew[3] 15.93   1.42      13.68      18.06  6799    1
# ynew[4] 16.40   1.50      14.12      18.76  6460    1
# ynew[5] 16.88   1.55      14.43      19.29  6305    1

post <- extract(fit)

str(post)
# List of 5
#  $ alpha: num [1:20000(1d)] 9.48 10.29 10.64 10.2 8.53 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ beta : num [1:20000(1d)] 0.546 0.376 0.347 0.359 0.601 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ sigma: num [1:20000(1d)] 0.9 0.816 0.791 1.479 1.243 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ ynew : num [1:20000, 1:5] 15.3 13.1 14.1 12.6 13.6 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ iterations: NULL
#   .. ..$           : NULL
#  $ lp__ : num [1:20000(1d)] -5.79 -8.3 -3.74 -9.13 -9.32 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL

dim(post$ynew)
# [1] 20000     5

ynew.PI <- apply( post$ynew , 2 , PI )
ynew.mean <- apply( post$ynew , 2 , mean )

plot(x,y, xlim = c(1, 15), ylim = c(10, 20))
points(xnew, ynew.mean, col = "red")
shade(ynew.PI, xnew)

# prediction with generated quantities ------------------------------------

model <- stan_model("linear_reg_pred_gq.stan")

fit <-
  stan(
    "linear_reg_pred_gq.stan",
    data = c("N", "x", "y", "Nnew", "xnew"),
    iter = 2000
  )

precis(fit, depth = 2)
#          Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha    9.72   0.68       8.74      10.92  1486    1
# beta     0.48   0.11       0.30       0.65  1375    1
# sigma    1.00   0.31       0.59       1.41  1340    1
# ynew[1] 15.01   1.24      13.19      17.11  2834    1
# ynew[2] 15.46   1.34      13.33      17.45  2381    1
# ynew[3] 15.95   1.39      13.92      18.12  2226    1
# ynew[4] 16.45   1.46      13.99      18.60  2221    1
# ynew[5] 16.86   1.55      14.51      19.25  2017    1

post <- extract(fit)
ynew.PI <- apply( post$ynew , 2 , PI )
ynew.mean <- apply( post$ynew , 2 , mean )

plot(x,y, xlim = c(1, 15), ylim = c(10, 20))
points(xnew, ynew.mean, col = "red")
shade(ynew.PI, xnew)
