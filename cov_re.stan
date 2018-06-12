// covariance random-effects priors

data{
  int<lower=1> N;
  int<lower=1> G;
  vector[N] x;
  vector[N] y;
  int gg[N];
}
parameters{
  vector[2] beta[G];
  cholesky_factor_corr[2] L_Omega;
  vector<lower=0>[2] sigma;
  real<lower=0> sigy;
}
model{
  sigma ~ cauchy(0, 2.5);
  L_Omega ~ lkj_corr_cholesky(4);
  beta ~ multi_normal_cholesky(rep_vector(3, 2),
                               diag_post_multiply(L_Omega, sigma));
  for (n in 1:N)
    y[n] ~ normal([1,  x[n]] * beta[gg[n]], sigy);
    
  sigy ~ cauchy(0, 1);
}
