data {
  int<lower=0> N;
  vector[N] ver;    
  vector[N] edad;     
}
parameters {
  real beta0; 
  real beta1; 
  real<lower=0> sigma;
}
model {
  ver ~ normal(beta0 + beta1 * edad, sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] y_rep;
  vector[N] log_lik;

  mu = beta0 + beta1 * edad;

  for (i in 1:N) {
    y_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(ver[i] | mu[i], sigma);
  }
}