data {
  int<lower=0> N;
  int<lower=0, upper=1> diag[N];
  vector[N] ver;
}
parameters {
  real b0;
  real b1;
}
model {
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  diag ~ bernoulli_logit(b0 + b1*ver);
}
// generated quantities {
//   vector[N] alpha;
//   vector[N] y_rep;
//   vector[N] log_lik;
// 
//   alpha = b0 + b1*age + b2*sex + b3*vhi + b4*vi + b5*ecsf + b6*vcf + b7*inte + b8*res1 + b9*res2;
// 
//   for (i in 1:N) {
//     y_rep[i] = bernoulli_logit_rng(alpha[i]);
//     log_lik[i] = bernoulli_logit_lpmf(diag[i] | alpha[i]);
//   }
// }