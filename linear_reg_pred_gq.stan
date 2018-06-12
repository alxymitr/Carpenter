// linear regression with prediction
// with generated quantities

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
}
model {
  y ~ normal(alpha + beta * x, sigma);
}
generated quantities{
  // predict
  vector[Nnew] ynew;

  // predict
  for (n in 1:Nnew)
    ynew[n] = normal_rng(alpha + beta * xnew[n], sigma);
}
