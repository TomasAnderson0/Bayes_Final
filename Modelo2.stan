data {
  int<lower=0> N;
  int<lower=0, upper=1> diag[N];
  vector[N] age;
  int sex[N];
  vector[N] vhi;
  vector[N] vi;
  vector[N] ecsf;
  vector[N] vcf;
  int inte[N];
  int res[N];
}
parameters {
  real b1;
  vector[2] b2;
  real b3;
  real b4;
  real b5;
  real b6;
}
model {
  b1 ~ normal(0, 1);
  b2 ~ normal(0, 1);
  b3 ~ normal(0, 1);
  b4 ~ normal(0, 1);
  b5 ~ normal(0, 1);
  b6 ~ normal(0, 1);
  diag ~ bernoulli_logit(b1*age + b2[sex] + b3*vhi + b4*vi + b5*ecsf + b6*vcf);
}
generated quantities {
  vector[N] alpha;
  vector[N] y_rep;
  vector[N] log_lik;

  alpha = b1*age + b2[sex] + b3*vhi + b4*vi + b5*ecsf + b6*vcf;

  for (i in 1:N) {
    y_rep[i] = bernoulli_logit_rng(alpha[i]);
    log_lik[i] = bernoulli_logit_lpmf(diag[i] | alpha[i]);
  }
}