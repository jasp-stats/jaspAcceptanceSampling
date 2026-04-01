data {
  int<lower=1> T;             // number of time points
  array[T] int<lower=0> s;    // total size of each lot
  array[T] int<lower=0> n;    // number of sampled items
  array[T] int<lower=0> y;    // observed defects in sample

  // Hyperparameters for priors
  real<lower=0> prior_theta1_a;   // Beta(alpha) for theta1
  real<lower=0> prior_theta1_b;   // Beta(beta)  for theta1
  real<lower=0> prior_sigma_sd;   // SD for (half-)normal prior on sigma_eta
}

parameters {
  // Initial defect rate on probability scale
  real<lower=0, upper=1> theta1;

  // Scale of day-to-day innovations (constrained > 0)
  real<lower=0> sigma_eta;

  // Non-centered innovations
  vector[T-1] z_raw;
}

transformed parameters {
  vector[T] mu;
  vector[T] theta;
  vector[T-1] eta;

  eta = sigma_eta * z_raw;

  mu[1] = logit(theta1);
  mu[2:T] = mu[1] + cumulative_sum(eta);

  theta = inv_logit(mu);
}

model {

  theta1   ~ beta(prior_theta1_a, prior_theta1_b);  // default 1,1 in JASP
  sigma_eta ~ normal(0, prior_sigma_sd);            // with <lower=0>, this is half-normal
  z_raw    ~ normal(0, 1);

  y ~ binomial(n, theta);
}

generated quantities {
  int y_pred; // predicted total defects in the last lot

  // Predict total defects in lot T:
  // observed defects + random draw for remaining items
  y_pred = y[T] + binomial_rng(s[T] - n[T], theta[T]);
}
