functions {
  real normal_mixture_transport(real z, real sigma, data real q, data real extra);
}
data {
  int<lower=1> T;
  array[T] int<lower=0> s;
  array[T] int<lower=0> n;
  array[T] int<lower=0> y;
  real<lower=0> prior_theta1_a;
  real<lower=0> prior_theta1_b;
  real<lower=0> prior_sigma_sd;
}
transformed data {
  real jump_probability = 0.05;
  real jump_extra_sd = 1.5;
  for (t in 1:T)
    if (y[t] > n[t] || n[t] > s[t]) reject("Require y <= n <= s");
}
parameters {
  real<lower=0, upper=1> theta1;
  real<lower=0> sigma_eta;
  vector[T-1] z_raw;
}
transformed parameters {
  vector[T-1] eta;
  vector[T] mu;
  vector[T] theta;

  if (T > 1)
    for (t in 1:(T-1))
      eta[t] = normal_mixture_transport(z_raw[t], sigma_eta,
                                        jump_probability, jump_extra_sd);
  mu[1] = logit(theta1);
  if (T > 1) mu[2:T] = mu[1] + cumulative_sum(eta);
  theta = inv_logit(mu);
}
model {
  theta1 ~ beta(prior_theta1_a, prior_theta1_b);
  sigma_eta ~ normal(0, prior_sigma_sd);
  z_raw ~ std_normal();
  y ~ binomial_logit(n, mu);
}
generated quantities {
  int y_pred = y[T] + binomial_rng(s[T] - n[T], theta[T]);
  vector[T-1] jump_responsibility;

  if (T > 1)
    for (t in 1:(T-1)) {
      real broad = log(jump_probability)
                   + normal_lpdf(eta[t] | 0, sqrt(square(sigma_eta) + square(jump_extra_sd)));
      real regular = log1m(jump_probability) + normal_lpdf(eta[t] | 0, sigma_eta);
      jump_responsibility[t] = exp(broad - log_sum_exp(broad, regular));
    }
}
