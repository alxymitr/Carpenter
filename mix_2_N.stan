// mixture of 2 normals

data{
  int<lower=1> N;
  vector[N] y;
}
parameters{
  real<lower=0.5, upper=0.9> lambda;
  real mu[2];
  real<lower=0> sigma[2];
}
model{
  for (n in 1:N) {
    real lp1; real lp2;
    
    lp1 = bernoulli_lpmf(0 | lambda)
      + normal_lpdf(y[n] | mu[1], sigma[1]);
      
    lp2 = bernoulli_lpmf(1 | lambda)
      + normal_lpdf(y[n] | mu[2], sigma[2]);
    
    target += log_sum_exp(lp1, lp2);
  }
  
  mu ~ normal(6, 2);
  sigma ~ cauchy(0, 0.5);
}
