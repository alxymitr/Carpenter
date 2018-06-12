// linear regression with prediction

data{
  int<lower=0> N;
  vector[N] x;
  vector[N] y;

  // predict  
  int<lower=0> Nnew;
  vector[Nnew] xnew;
}
parameters{
  real alpha;
  real beta;
  real<lower=0> sigma;
  
  // predict
  vector[Nnew] ynew;
}
model {
  y ~ normal(alpha + beta * x, sigma);
  
  // predict
  ynew ~ normal(alpha + beta * xnew, sigma);
}
