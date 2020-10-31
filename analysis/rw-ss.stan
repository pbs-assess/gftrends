data {
  int<lower=0> T;    // # years
  int<lower=0> N;    // # observations
  int<lower=0> J;    // # of stocks
  // vector[N] y;       // observations
  matrix[T,J] y;       // observations
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
  // https://mc-stan.org/docs/2_25/stan-users-guide/parameterizing-centered-vectors.html
  vector[J] alpha = append_row(alpha_raw, -sum(alpha_raw));
  vector[T] x;
  x[1] = sigma_x * x_raw[1];
  for (t in 2:T) {
    x[t] = x[t-1] + sigma_x * x_raw[t]; // RW process; 'sliced'
  }

}
model {
  int pos;              // position counter
  // x[2:T] ~ normal(x[1:(N - 1)], sigma_x); // RW process; 'sliced'
  // for (t in 2:T) {
  //   x[t] ~ normal(x[t-1], sigma_x); // RW process; 'sliced'
  // }
  // pos = 1;
  for (j in 1:J) {
    vector[s[j]] eps_j;  // residual for stock j, time t
    // vector[s[j]] eps_raw;  // residual for stock j, time t
    // vector[s[j]] eps_j;  // deviation for stock j, time t, accounting for AR1
    vector[s[j]] y_j;    // stock j time series
    // y_j = segment(y, pos, s[j]);
    for (t in 1:s[j]) {
      // eps_j[t] = y_j[t] - (x[t] + alpha[j]);
      eps_j[t] = y[t,j] - (x[t] + alpha[j]);
    }
    // eps_j[1] = sigma_eps * eps_raw[1]; // AR1 epsilon
    eps_j[1] ~ normal(0, sigma_eps);
    for (t in 2:s[j]) {
      eps_j[t] ~ normal(rho * eps_j[t - 1], sigma_eps); // AR1 epsilon
      // eps_j[t] = rho * eps_j[t - 1] + sigma_eps * eps_raw[t]; // AR1 epsilon
    }
    // eps_raw ~ std_normal();
    // pos = pos + s[j];
  }
  x_raw ~ std_normal();
  // priors
  sigma_x ~ std_normal();
  sigma_eps ~ std_normal();
  alpha_raw ~ normal(0, 5);
}
