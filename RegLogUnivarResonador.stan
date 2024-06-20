data {
  int<lower=0> N;
  int<lower=0, upper=1> diag[N];
  vector[N] ver1;
  vector[N] ver2;
}
parameters {
  real b0;
  real b1;
  real b2;
}
model {
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  b2 ~ normal(0, 1);
  diag ~ bernoulli_logit(b0 + b1*ver1 + b2*ver2);
}