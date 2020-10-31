library(rstan)
library(ggplot2)
library(dplyr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(ggsidekick::theme_sleek())

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
    y_true = y_jt,
    y = y_jt + rnorm(length(as.numeric(y_jt)), 0, 0.2),
    tau = matrix(0.2, nrow = nrow(y_jt), ncol = ncol(y_jt)),
    alpha = a_j,
    rho = rho,
    sigma_eps = sigma_eps,
    sigma_x = sigma_x
  )
}

set.seed(12223)
d <- sim_dat(
  sigma_x = 0.3,
  sigma_eps = 0.5,
  n_j = 12,
  n_t = 50,
  rho = 0.7
)

d$y[1:20,1:9] <- NA # NA
d$y[40:50,9:12] <- NA # NA
d$y_true[1:20,1:9] <- NA # NA
d$y_true[40:50,9:12] <- NA # NA
par(mfrow = c(2, 3))
matplot(d$y, type = "l", lty = 1, col = viridisLite::plasma(ncol(d$y)))
lines(seq_along(d$x_t), (d$x_t), lwd = 3)
matplot(d$y_true, type = "l", lty = 1, col = viridisLite::plasma(ncol(d$y)))
lines(seq_along(d$x_t), (d$x_t), lwd = 3)
d$y[is.na(d$y)] <- 999 # fake NA

dat <- list(
  T = nrow(d$y),
  N = length(as.numeric(d$y)),
  J = ncol(d$y),
  y = d$y,
  tau = d$tau,
  df = 7,
  s = rep(nrow(d$y), ncol(d$y)))

model <- stan_model("analysis/rw-ss.stan")
m <- sampling(
  model,
  data = dat,
  chains = 4, iter = 1000, seed = 182823,
  control = list(adapt_delta = 0.95),
  pars = c("rho", "sigma_eps", "sigma_x", "x", "alpha")
)
# pairs(m, pars = c("sigma_eps", "sigma_x", "rho", "x[1]"))
rstan::check_hmc_diagnostics(m)
rstan::stan_rhat(m)
# rstan::stan_diag(m)
print(m)

p <- extract(m)

# p <- tidybayes::gather_draws(m, alpha[.j], x[.t], sigma_eps, sigma_x, rho)
# true_alpha <- tibble(
#   .j = seq_along(d$alpha),
#   .variable = "alpha",
#   true_value = d$alpha
# )
#
# filter(p, .variable == "alpha") %>%
#   left_join(true_alpha) %>%
#   ggplot(true_value, .value) + geom_point()

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
# save_draw <- sample(x_t$.draw, 250)
# x_t %>% filter(.draw %in% save_draw) %>%
x_t %>% group_by(.t) %>%
  summarize(
    lwr3 = quantile(.value, 0.45),
    upr3 = quantile(.value, 0.55),
    lwr2 = quantile(.value, 0.975),
    upr2 = quantile(.value, 0.025),
    lwr1 = quantile(.value, 0.25),
    upr1 = quantile(.value, 0.75),
    lwr = quantile(.value, 0.1),
    upr = quantile(.value, 0.9),
    med = median(.value), .groups = "drop") %>%
  ggplot(aes(.t, med)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.25) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.25) +
  geom_ribbon(aes(ymin = lwr3, ymax = upr3), alpha = 0.25) +
  # geom_line() +
  geom_line(data =
      data.frame(.t = seq_along(d$x), med = d$x), colour = "red", lwd = 1)
