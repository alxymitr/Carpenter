
library(rethinking)

N <- 10
K <- 2

x <- matrix(c(rep(1, N), c(2, 3, 1, 2, 0, 2, 3, 1, 1, 1)), nr = N, nc = K)
x
#       [,1] [,2]
#  [1,]    1    2
#  [2,]    1    3
#  [3,]    1    1
#  [4,]    1    2
#  [5,]    1    0
#  [6,]    1    2
#  [7,]    1    3
#  [8,]    1    1
#  [9,]    1    1
# [10,]    1    1

beta <- c(-1, 0.5)

y1 <- x %*% beta
y1
#       [,1]
#  [1,]  0.0
#  [2,]  0.5
#  [3,] -0.5
#  [4,]  0.0
#  [5,] -1.0
#  [6,]  0.0
#  [7,]  0.5
#  [8,] -0.5
#  [9,] -0.5
# [10,] -0.5

p1 <- 1/(1 + exp(-y1))
p1
#            [,1]
#  [1,] 0.5000000
#  [2,] 0.6224593
#  [3,] 0.3775407
#  [4,] 0.5000000
#  [5,] 0.2689414
#  [6,] 0.5000000
#  [7,] 0.6224593
#  [8,] 0.3775407
#  [9,] 0.3775407
# [10,] 0.3775407

y <- rbinom(N, 1, p1)

model <- stan_model("logistic_reg.stan")

mle <- optimizing(model, data = c("K", "N", "x", "y"))
# Initial log joint probability = -7.02589
# Optimization terminated normally: 
#   Convergence detected: relative gradient magnitude is below tolerance
mle
# $par
#    beta[1]    beta[2] 
# -2.2965735  0.6574056 
# 
# $value
# [1] -4.833146
# 
# $return_code
# [1] 0

mle$par
#    beta[1]    beta[2] 
# -2.2965735  0.6574056 
   
phat <- 1 / (1 + exp(-x %*% mle$par))
# phat
#             [,1]
#  [1,] 0.27254225
#  [2,] 0.41961437
#  [3,] 0.16257832
#  [4,] 0.27254225
#  [5,] 0.09140714
#  [6,] 0.27254225
#  [7,] 0.41961437
#  [8,] 0.16257832
#  [9,] 0.16257832
# [10,] 0.16257832

plot(x[, 2], p1, ylim = c(0, 1))
points(x[, 2], y, col = "red")
points(x[, 2], phat, col = "green")

fit <- stan("logistic_reg.stan", data = c("K", "N", "x", "y"))
pairs(fit)

str(extract(fit))
# List of 2
#  $ beta: num [1:4000, 1:2] -3.02 -2.42 -3.24 -4.36 -3.6 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ iterations: NULL
#   .. ..$           : NULL
#  $ lp__: num [1:4000(1d)] -5.01 -5.83 -5.93 -6.37 -5.61 ...
#   ..- attr(*, "dimnames")=List of 1
#   .. ..$ iterations: NULL

dens(extract(fit)$beta[, 1])

dens(extract(fit)$beta[, 2])

cor(extract(fit)$beta)
# [1,]  1.000000 -0.907424
# [2,] -0.907424  1.000000

# оценка вероятностей
x2.seq <- seq(from = 0, to = 3, length.out = 31)
x2.seq
#  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8
# [20] 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0

xnew <- cbind(1, x2.seq)

head(extract(fit)$beta)
# iterations      [,1]        [,2]
#       [1,] -1.908780  1.43596191
#       [2,] -0.781060 -0.06214647
#       [3,] -2.774629  1.12602336
#       [4,] -1.620172  0.65878337
#       [5,] -1.591710  1.32359620
#       [6,] -3.109096  1.34708699

phatnew <- 1 / (1 + exp(-xnew %*%  extract(fit)$beta[1,]))
plot(phatnew)

dim(extract(fit)$beta)[1]
#[1] 4000

# заготовка вероятностей
foo <-
  matrix(NA, nr = dim(extract(fit)$beta)[1], nc = length(x2.seq))

for (i in 1:dim(extract(fit)$beta)[1]) {
  foo[i,] <- 1 / (1 + exp(-xnew %*% extract(fit)$beta[i,]))
}

prob.PI <- apply( foo , 2 , PI )

plot(x[, 2], p1, ylim = c(0, 1))
points(x[, 2], y, col = "red")
shade(prob.PI, x2.seq)

curve(dcauchy(x, 0, 2.5), -10, 10)
