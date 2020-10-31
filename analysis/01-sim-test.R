library(rstan)
library(ggplot2)
library(dplyr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sim_dat <- function(sigma_x = 0.3, sigma_eps = 0.8, n_j = 20,
                    n_t = 40, rho = 0.8, alpha_sd = 0.5) {
  a_j <- rnorm(n_j, 0, alpha_sd)
  a_j <- a_j - mean(a_j) # sum to zero
  x_t <- numeric(length = n_t)
  x_t[1] <- rnorm(1, 0, sigma_x)
  for (i in seq(2, n_t)) {
    x_t[i] <- x_t[i - 1] + rnorm(1, 0, sigma_x)
  }
  y_jt <- matrix(ncol = n_j, nrow = n_t)
  eps_jt <- matrix(ncol = n_j, nrow = n_t)
  for (i in seq_len(n_t)) {
    for (j in seq_len(n_j)) {
      if (i == 1L) {
        eps_jt[i, j] <- rnorm(1, 0, sigma_eps)
      } else {
        eps_jt[i, j] <- rnorm(1, rho * eps_jt[i - 1, j], sigma_eps)
      }
      y_jt[i, j] <- x_t[i] + a_j[j] + eps_jt[i, j]
    }
  }
  list(
    x_t = x_t,
    y = y_jt,
    alpha = a_j,
    rho = rho,
    sigma_eps = sigma_eps,
    sigma_x = sigma_x
  )
}

set.seed(1223)
d <- sim_dat(
  sigma_x = 0.3,
  sigma_eps = 0.5,
  n_j = 12,
  n_t = 50,
  rho = 0.7
)

d$y[1:20,1:9] <- NA # NA
par(mfrow = c(2, 3))
matplot(d$y, type = "l", lty = 1, col = viridisLite::plasma(ncol(d$y)))
lines(seq_along(d$x_t), (d$x_t), lwd = 3)
d$y[is.na(d$y)] <- 999 # fake NA

dat <- list(
  T = nrow(d$y),
  N = length(as.numeric(d$y)),
  J = ncol(d$y),
  y = d$y,
  s = rep(nrow(d$y), ncol(d$y)))

m <- stan("analysis/rw-ss.stan",
  data = dat,
  chains = 4, iter = 800, seed = 182823,
  pars = c("rho", "sigma_eps", "sigma_x", "x", "alpha")
)

p <- extract(m)

plot(d$alpha, colMeans(p$alpha))
lwr <- apply(p$alpha, 2, quantile, probs = 0.05)
upr <- apply(p$alpha, 2, quantile, probs = 0.95)
segments(x0 = d$alpha, y0 = lwr, y1 = upr)
abline(a = 0, b = 1)
cat("alpha_j 90% CI coverage:", mean(lwr < d$alpha & upr > d$alpha), "\n")

plot(d$x, colMeans(p$x))
lwr <- apply(p$x, 2, quantile, probs = 0.05)
upr <- apply(p$x, 2, quantile, probs = 0.95)
segments(x0 = d$x, y0 = lwr, y1 = upr)
abline(a = 0, b = 1)
cat("x_t 90% CI coverage:", mean(lwr < d$x & upr > d$x), "\n")

hist(p$rho);abline(v = d$rho, col = "red")
hist(p$sigma_eps);abline(v = d$sigma_eps, col = "red")
hist(p$sigma_x);abline(v = d$sigma_x, col = "red")

x_t <- tidybayes::gather_draws(m, x[.t])
save_draw <- sample(x_t$.draw, 250)
x_t %>% filter(.draw %in% save_draw) %>%
  ggplot(aes(.t, .value, group = .draw)) + geom_line(alpha = 0.1) +
  geom_line(data =
      data.frame(.t = seq_along(d$x), .value = d$x, .draw = 999), colour = "red", lwd = 1)
