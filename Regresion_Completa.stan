data {
  int<lower=0> N;
  vector[N] ver;    
  vector[N] edad;  
  vector[N] sexo;
  vector[N] res1;
  vector[N] res2;
  vector[N] inte;
}
parameters {
  real beta0; 
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  real<lower=0> sigma;
}
model {
  ver ~ normal(beta0 + beta1 * edad + beta2 * sexo + beta3 * res1 + beta4 * res2 + beta5 * inte, sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] y_rep;
  vector[N] log_lik;

  mu = beta0 + beta1 * edad + beta2 * sexo + beta3 * res1 + beta4 * res2 + beta5 * inte;

  for (i in 1:N) {
    y_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(ver[i] | mu[i], sigma);
  }
}