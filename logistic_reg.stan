// logistic regression with x matrix
data{
  int<lower=1> K;
  int<lower=0> N;
  matrix[N,K] x;
  int<lower=0,upper=1> y[N];
}
parameters{
  vector[K] beta;
}
model{
  beta ~ cauchy(0, 2.5);          // prior
  y ~ bernoulli_logit(x * beta);  // likelihood
}
