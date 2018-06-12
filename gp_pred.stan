// Gaussian process estimation and prediction

data{
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
  
  // prediction
  int<lower=1> M;
  vector[M] x_tilde;
}
transformed data{
  int<lower=1> NM = N + M;

  vector[NM] xfull;
  xfull = append_row(x, x_tilde);
}
parameters{
  real<lower=0> eta_sq;
  real<lower=0> inv_rho_sq;
  real<lower=0> sigma_sq;
  
  // prediction
  vector[M] y_tilde;
}
transformed parameters{
  real<lower=0> rho_sq;
  rho_sq = inv(inv_rho_sq);
}
model{
  matrix[NM,NM] Sigma;
  for (i in 1:(NM-1)) {
    for (j in (i+1):NM) {
      Sigma[i,j] = eta_sq * exp(-rho_sq * square(xfull[i] - xfull[j]));
      Sigma[j,i] = Sigma[i,j];
    }
  }
  for (k in 1:NM)
    Sigma[k,k] = eta_sq + sigma_sq;
  
  eta_sq ~ cauchy(0, 5);
  inv_rho_sq ~ cauchy(0, 5);
  sigma_sq ~ cauchy(0, 5);
  
  append_row(y, y_tilde) ~ multi_normal(rep_vector(0, NM), Sigma);
}
