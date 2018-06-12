
library(rethinking)

# Stan has 5 data types:
#   
# - integer
# - real
# - vector
# - real vector
# - matrix
# 
# + constrained versions thereof

N <- 10L

x <- c(1,2,3,5,4,3,1,2,1,4)
y <- c(2,3,4,2,1,6,4,2,1,2)

plot(x, y)

model <- stan_model("linear_reg.stan")

mle <- optimizing(model, data = c("N", "x", "y"))
# Initial log joint probability = -301.538
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance

mle
# $par
#       alpha        beta       sigma 
#  2.86957450 -0.06523565  1.48395618 
# 
# $value
# [1] -8.947225
# 
# $return_code
# [1] 0

plot(x, y)
abline(a = mle$par[1],
       b = mle$par[2],
       col = "red")

fit <- stan("linear_reg.stan", data = c("N", "x", "y"))

str(extract(fit))
# List of 4
#  $ alpha: num [1:4000(1d)] 4.41 1.59 3.65 3.8 3.68 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ beta : num [1:4000(1d)] -0.91 0.367 -0.167 -0.266 -0.608 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ sigma: num [1:4000(1d)] 1.48 1.62 1.74 1.25 1.93 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL
#  $ lp__ : num [1:4000(1d)] -12.53 -9.24 -9.09 -9.83 -10.08 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL

precis(fit)
#        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# alpha  2.82   1.41       0.58       4.98  1269    1
# beta  -0.05   0.48      -0.78       0.70  1182    1
# sigma  1.98   0.61       1.13       2.79  1319    1

dens(extract(fit)$alpha)

dens(extract(fit)$beta)

dens(extract(fit)$sigma)

dens(extract(fit)$lp__)

pairs(fit)

plot(fit)

traceplot(fit)

xnew <- seq(from = min(x) - 5, to = max(x) + 5, length.out = 31)
xnew
#  [1] -4.0000000 -3.5333333 -3.0666667 -2.6000000 -2.1333333 -1.6666667 -1.2000000
#  [8] -0.7333333 -0.2666667  0.2000000  0.6666667  1.1333333  1.6000000  2.0666667
# [15]  2.5333333  3.0000000  3.4666667  3.9333333  4.4000000  4.8666667  5.3333333
# [22]  5.8000000  6.2666667  6.7333333  7.2000000  7.6666667  8.1333333  8.6000000
# [29]  9.0666667  9.5333333 10.0000000

post <- extract(fit)

str(post)

foo <- matrix(NA, nrow = 4000, ncol = length(xnew))

for (i in 1:4000) {
  foo[i,] <- post$alpha[i] + post$beta[i] * xnew
}

ymean.PI <- apply( foo , 2 , PI )

plot(x, y, xlim = range(xnew), ylim = c(-4, 10))
shade(ymean.PI, xnew)
