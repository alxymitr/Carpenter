// Gaussian process estimation

data{
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
}
parameters{
  real<lower=0> eta_sq;
  real<lower=0> inv_rho_sq;
  real<lower=0> sigma_sq;
}
transformed parameters{
  real<lower=0> rho_sq;
  rho_sq = inv(inv_rho_sq);
}
model{
  matrix[N,N] Sigma;
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      Sigma[i,j] = eta_sq * exp(-rho_sq * square(x[i] - x[j]));
      Sigma[j,i] = Sigma[i,j];
    }
  }
  for (k in 1:N)
    Sigma[k,k] = eta_sq + sigma_sq;
  
  eta_sq ~ cauchy(0, 5);
  inv_rho_sq ~ cauchy(0, 5);
  sigma_sq ~ cauchy(0, 5);
  
  y ~ multi_normal(rep_vector(0, N), Sigma);
}
