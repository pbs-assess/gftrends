data {
  int<lower=0> T;    // # years
  int<lower=0> N;    // # observations
  int<lower=0> J;    // # of stocks
  matrix[T,J] y;     // observations
  int s[J];          // group sizes
}
parameters {
  real<lower=0> sigma_x;        // RW process SD
  real<lower=0> sigma_eps;      // stock-level AR1 SD
  real<lower=-1, upper=1> rho;  // stock-level AR1 correlation
  vector[T] x_raw;              // RW mean value
  vector[J-1] alpha_raw;        // stock-level deviation (without J-th element)
}
transformed parameters {
  vector[J] alpha = append_row(alpha_raw, -sum(alpha_raw)); // sum to zero constraint
  vector[T] x;
  x[1] = sigma_x * x_raw[1];
  for (t in 2:T) {
    x[t] = x[t-1] + sigma_x * x_raw[t]; // RW process
  }
}
model {
  for (j in 1:J) {
    vector[s[j]] eps_j;  // residual for stock j, time t
    vector[s[j]] y_j;    // stock j time series
    for (t in 1:s[j]) {
      if (y[t,j] != 999) {
        eps_j[t] = y[t,j] - (x[t] + alpha[j]);
      }
    }
    if (y[1,j] != 999) {
      eps_j[1] ~ normal(0, sigma_eps);
    }
    for (t in 2:s[j]) {
      if (y[t,j] != 999 && y[t-1,j] != 999) {
        eps_j[t] ~ normal(rho * eps_j[t-1], sigma_eps); // AR1 epsilon
      }
    }
    // eps_j[2:s[j]] ~ normal(rho * eps_j[1:(s[j] - 1)], sigma_eps); // AR1 epsilon; sliced
  }
  x_raw ~ std_normal();
  // priors:
  sigma_x ~ std_normal();
  sigma_eps ~ std_normal();
  alpha_raw ~ normal(0, 5);
}
