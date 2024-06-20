data {
  int<lower=0> N;
  int<lower=0, upper=1> diag[N];
  vector[N] age;
  vector[N] vhi;
  vector[N] inte;
}
parameters {
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real b6;
}
model {
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  b2 ~ normal(0, 1);
  b3 ~ normal(0, 1);
  b4 ~ normal(0, 1);
  b5 ~ normal(0, 1);
  b6 ~ normal(0, 1);
  diag ~ bernoulli_logit(b0 + b1*age + b2*vhi + b3*inte + b4*age*vhi + b5*age*inte + b6*vhi*inte);
}
generated quantities {
  vector[N] alpha;
  vector[N] y_rep;
  vector[N] log_lik;

  alpha = b0 + b1*age + b2*vhi + b3*inte + b4*age*vhi + b5*age*inte + b6*vhi*inte;

  for (i in 1:N) {
    y_rep[i] = bernoulli_logit_rng(alpha[i]);
    log_lik[i] = bernoulli_logit_lpmf(diag[i] | alpha[i]);
  }
}