
// AR(1) model
data{
  int<lower=0> N;
  vector[N] y;
}
parameters{
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model{
  for (n in 2:N)
    y[n] ~ normal(alpha + beta * y[n-1], sigma);
  
  alpha ~ normal(0, 10);
  beta ~ normal(0, 2);
  sigma ~ cauchy(0, 2.5);
}
