
// AR(1) with unknown y0 and t-distributed innovations

data{
  int<lower=0> N;
  vector[N] y;
  int<lower=1> df;
}
parameters{
  real alpha;
  real beta;
  real<lower=0> sigma;
  real y0;
}
model{
  y[1] ~ student_t(df, alpha + beta * y0, sigma);
  for (n in 2:N)
    y[n] ~ student_t(df, alpha + beta * y[n-1], sigma);

  alpha ~ normal(0, 10);
  beta ~ normal(0, 2);
  sigma ~ cauchy(0, 2.5);
  y0 ~ normal(-10, 2);
}

