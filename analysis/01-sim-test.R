# library(rstan)
# library(ggplot2)
# theme_set(theme_light())

sim_dat <- function(sigma_eta = 0.3, sigma_eps = 0.8, n_j = 20,
  n_t = 40, phi = 0.8) {
  a_j <- runif(n_j, -0.5, 0.5)
  a_j <- a_j - mean(a_j)
  x_t <- numeric(length = n_t)
  x_t[1] <- rnorm(1, 0, sigma_eta)
  for (i in seq(2, n_t)) {
    x_t[i] <- x_t[i - 1] + rnorm(1, 0, sigma_eta)
  }
  y_jt <- matrix(ncol = n_t, nrow = n_j)
  eps_jt <- matrix(ncol = n_t, nrow = n_j)
  for (i in seq_len(n_t)) {
    for (j in seq_len(n_j)) {
      if (i == 1L) {
        eps_jt[j, i] <- rnorm(1, 0, sigma_eps)
      } else {
        eps_jt[j, i] <- rnorm(1, phi * eps_jt[j, i - 1], sigma_eps)
      }
      y_jt[j, i] <- x_t[i] + a_j[j] + eps_jt[j, i]
    }
  }

  y <- y_jt
  ## number of years
  n <- ncol(y)
  ## number of stocks
  m <- nrow(y)
  ## presence indicator
  ypresent <- ifelse(is.na(y), 0, 1)
  ## index of first and last observations
  ## -1 for start at zero in TMB
  t0 <- apply(ypresent, 1, which.max) - 1

  list(
    x_t = x_t,
    y = y,
    ypresent = ypresent,
    first_obs = t0,
    m = m,
    n = n,
    alpha = a_j
  )
}

set.seed(1)
d <- sim_dat(
  sigma_eta = 0.3,
  sigma_eps = 0.3,
  n_j = 25,
  n_t = 50,
  phi = 0.7
)
# matplot(t((d$y)), type = "l", lty = 1, col = viridisLite::plasma(nrow(d$y)))
# lines(seq_along(d$x_t), (d$x_t), lwd = 3)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat <- list(
  T = ncol(d$y),
  N = length(as.numeric(d$y)),
  J = nrow(d$y),
  y = t(d$y),
  s = rep(ncol(d$y), nrow(d$y)))
m <- stan("analysis/rw-ss.stan",
  data = dat,
  chains = 4, iter = 500,
  pars = c("sigma_eps", "sigma_x", "rho", "x", "alpha"))
m

p <- extract(m)
plot(d$alpha, colMeans(p$alpha))
segments(
  x0 = d$alpha,
  y0 = apply(p$alpha, 2, quantile, probs = 0.05),
  y1 = apply(p$alpha, 2, quantile, probs = 0.95)
)
abline(a = 0, b = 1)

plot(d$x, colMeans(p$x))
segments(
  x0 = d$x,
  y0 = apply(p$x, 2, quantile, probs = 0.05),
  y1 = apply(p$x, 2, quantile, probs = 0.95)
)
abline(a = 0, b = 1)

hist(p$rho);abline(v = 0.7, col = "red")
hist(p$sigma_eps);abline(v = 0.3, col = "red")
hist(p$sigma_x);abline(v = 0.3, col = "red")

