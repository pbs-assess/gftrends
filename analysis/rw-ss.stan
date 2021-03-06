data {
  int<lower=0> T;    // # years
  int<lower=0> J;    // # of stocks
  matrix[T,J] y;     // observations
  matrix[T,J] tau;   // observation measurement-error SD
  real<lower=0> rho_sd;
  int<lower=1> first_obs[J]; // first observation
  int<lower=0> N_miss;
  int<lower=1,upper=2> ar_coef_id[J]; // which AR1 coefs to use
}
parameters {
  real<lower=0> sigma_x;        // RW process SD
  real<lower=0> sigma_eps[2];      // stock-level AR1 SD
  real<lower=-1, upper=1> rho[2];  // stock-level AR1 correlation
  vector[T] x_raw;              // RW mean value
  matrix[T,J] tau_raw;
  vector[J-1] alpha_raw;        // stock-level deviation (without J-th element)
  vector[N_miss] y_miss;        // missing y values
  real x_t_intercept;
}
transformed parameters {
  matrix[T,J] y_true;           // unobserved latent 'true' stock value
  vector[J] alpha = append_row(alpha_raw, -sum(alpha_raw)); // sum to zero constraint
  vector[T] x;
  matrix[T,J] eps;
  x[1] = x_t_intercept + sigma_x * x_raw[1]; // non-centered
  for (t in 2:T) {
    x[t] = x[t-1] + sigma_x * x_raw[t]; // RW process (non-centered)
  }
  {
  int k = 0;
  for (j in 1:J) {
    for (t in 1:T) {
      if (t > first_obs[j] && y[t,j] != 999) {
        y_true[t,j] = y[t,j] - (tau[t,j] * tau_raw[t,j]); // measurement model (non-centered)
        eps[t,j] = y_true[t,j] - (x[t] + alpha[j]);
        // -y_true[t,j] = -eps[t,j] - (x[t] + alpha[j]);
        // y_true[t,j] = x[t] + alpha[j] + eps[t,j];
      }
      if (t > first_obs[j] && y[t,j] == 999) { // estimate missing values
        k += 1;
        y_true[t,j] = y_miss[k];
        eps[t,j] = y_true[t,j] - (x[t] + alpha[j]);
      }
      if (t <= first_obs[j]) { // just fill in to suppress warnings
        y_true[t,j] = 0;
        eps[t,j] = 0;
      }
    }
  }
  }
}
model {
  for (j in 1:J) {
    for (t in 1:T) {
      if (t == first_obs[j]) {
        eps[t,j] ~ normal(0, sigma_eps[ar_coef_id[j]]);
      }
      if (t > first_obs[j]) {
        eps[t,j] ~ normal(rho[ar_coef_id[j]] * eps[t-1,j], sigma_eps[ar_coef_id[j]]); // AR1 epsilon
      }
    }
  }
  x_raw ~ std_normal();
  to_vector(tau_raw) ~ std_normal();
  sigma_x ~ std_normal();
  sigma_eps ~ std_normal();
  rho ~ normal(0, rho_sd);
  alpha_raw ~ normal(0, 5);
  x_t_intercept ~ normal(0, 5);
}
