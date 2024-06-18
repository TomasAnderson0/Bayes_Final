data {
  int<lower=0> N;
  vector[N] vhi;
  vector[N] masc;
  vector[N] fem;
}
parameters {
  real<lower=0> b0;
  real<lower=0> b1;
  real<lower=0> sigma;
}
model {
  vhi ~ normal(b0*masc+b1*fem, sigma);
}