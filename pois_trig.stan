
// Poisson data with trigonometric trend

data{
  int<lower=0> N;
  vector[N] xcos;
  vector[N] xsin;
  int y[N];
}

parameters{
  real alpha;
  real beta_cos;
  real beta_sin;
}

model{
  y ~ poisson_log(alpha + beta_cos * xcos + beta_sin * xsin);
  
  alpha ~ normal(0, 2);
  beta_cos ~ normal(0, 1);
  beta_sin ~ normal(0, 1);
}
