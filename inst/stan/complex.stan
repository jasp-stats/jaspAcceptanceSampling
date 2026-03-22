data {
  int<lower=1> T;                  // number of time points
  array[T] int<lower=0> s;         // total size of each lot
  array[T] int<lower=0> n;         // number of sampled items
  array[T] int<lower=0> y;         // observed defects in sample
  array[T] real temperature;       // predictor (ideally centered / scaled)

  // Hyperparameters for priors
  real<lower=0> prior_theta1_a;    // Beta(alpha) for theta1
  real<lower=0> prior_theta1_b;    // Beta(beta)  for theta1
  real<lower=0> prior_sigma_sd;    // SD for (half-)normal prior on sigma_eta
  real<lower=0> prior_beta_scale;  // Cauchy scale for beta_temperature
}

parameters {
  // Initial defect rate on probability scale
  real<lower=0, upper=1> theta1;

  // Scale of day-to-day innovations (local level)
  real<lower=0> sigma_eta;

  // Non-centered innovations
  vector[T-1] z_raw;

  // Temperature effect on logit defect rate
  real beta_temperature;
}

transformed parameters {
  vector[T] nu;        // latent baseline level on logit scale (no predictor)
  vector[T] mu;        // logit including temperature effect
  vector[T-1] eta;     // actual innovations
  vector[T] theta;     // defect rate on probability scale

  // Random-walk innovations
  eta = sigma_eta * z_raw;

  // Baseline local-level process (no predictor)
  nu[1] = logit(theta1);
  nu[2:T] = nu[1] + cumulative_sum(eta);

  // Add predictor effect on the logit scale
  for (t in 1:T) {
    mu[t] = nu[t] + beta_temperature * temperature[t];
  }

  // Map to probability scale
  theta = inv_logit(mu);
}

model {

  theta1           ~ beta(prior_theta1_a, prior_theta1_b);     // default 1,1
  sigma_eta        ~ normal(0, prior_sigma_sd);                // ∝ half-normal due to <lower=0>
  z_raw            ~ normal(0, 1);
  beta_temperature ~ cauchy(0, prior_beta_scale);              // default 0.707

  y ~ binomial(n, theta);
}

generated quantities {
  int y_pred;

  // Predict total defects in lot T:
  y_pred = y[T] + binomial_rng(s[T] - n[T], theta[T]);
}
