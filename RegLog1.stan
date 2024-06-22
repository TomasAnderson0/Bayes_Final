data {
  int<lower=0> N;
  int<lower=0, upper=1> diag[N];
  vector[N] var1;
  
}
parameters {
  real b0;
  real b1;
}
model {
  diag ~ bernoulli_logit(b0 + b1*var1);
}

