
// negative binomial with trigonometric trend

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
  real<lower=1,upper=5> theta;
}

model{
  y ~ neg_binomial_2_log(alpha + beta_cos * xcos + beta_sin * xsin, theta);
  
  alpha ~ normal(0, 2);
  beta_cos ~ normal(0, 1);
  beta_sin ~ normal(0, 1);
}
