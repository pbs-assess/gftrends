data {
  int<lower=0> T;    // # years
  int<lower=0> J;    // # of stocks
  matrix[T,J] y;     // observations
  matrix[T,J] tau;   // observation measurement-error SD
  // real<lower=1> df;  // Student-t df for RW
}
parameters {
  real<lower=0> sigma_x;        // RW process SD
  real<lower=0> sigma_eps;      // stock-level AR1 SD
  real<lower=-1, upper=1> rho;  // stock-level AR1 correlation
  vector[T] x_raw;              // RW mean value
  matrix[T,J] tau_raw;
  vector[J-1] alpha_raw;        // stock-level deviation (without J-th element)
}
transformed parameters {
  matrix[T,J] y_true;           // unobserved latent 'true' stock value
  vector[J] alpha = append_row(alpha_raw, -sum(alpha_raw)); // sum to zero constraint
  vector[T] x;
  matrix[T,J] eps;
  x[1] = sigma_x * x_raw[1];
  for (t in 2:T) {
    x[t] = x[t-1] + sigma_x * x_raw[t]; // RW process
  }
  for (j in 1:J) {
    for (t in 1:T) {
      if (y[t,j] != 999) {
        y_true[t,j] = y[t,j] - (tau[t,j] * tau_raw[t,j]);    // measurement model
        eps[t,j] = y_true[t,j] - (x[t] + alpha[j]);
      }
    }
  }
}
model {
  for (j in 1:J) {
    if (y[1,j] != 999) {
      eps[1,j] ~ normal(0, sigma_eps);
    }
    for (t in 2:T) {
      if (y[t,j] != 999 && y[t-1,j] != 999) {
        eps[t,j] ~ normal(rho * eps[t-1,j], sigma_eps); // AR1 epsilon
      }
    }
  }
  // x_raw ~ student_t(df, 0, 1);
  x_raw ~ std_normal();
  to_vector(tau_raw) ~ std_normal();
  // priors:
  sigma_x ~ student_t(7, 0, 2);
  sigma_eps ~ student_t(7, 0, 2);
  alpha_raw ~ normal(0, 5);
  rho ~ normal(0, 2);
}
