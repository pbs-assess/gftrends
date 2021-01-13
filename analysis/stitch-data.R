# library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

f <- list.files("data-raw", pattern = ".rds", full.names = TRUE)
f <- f[!grepl("-mcmc", f)]
d <- purrr::map_dfr(f, readRDS)
d <- select(d, year, species, region, log_blrp, sd_log_blrp)

f <- list.files("data-raw", pattern = ".rds", full.names = TRUE)
f <- f[grepl("-mcmc", f)]
d <- purrr::map_dfr(f, readRDS)
d <- select(d, year, species, region, log_blrp, sd_log_blrp)

d %>%
  mutate(stock = paste(species, region)) %>%
  ggplot(aes(
    x = year,
    y = exp(log_blrp),
    ymin = exp(log_blrp - 2 * sd_log_blrp),
    ymax = exp(log_blrp + 2 * sd_log_blrp)
  )) +
  geom_ribbon(colour = NA, fill = "grey50", alpha = 0.4) +
  geom_line() +
  facet_wrap(~stock) +
  scale_y_log10() +
  geom_hline(yintercept = 1, lty = 2) +
  coord_cartesian(xlim = c(1950, 2020)) +
  ylab("B/LRP") +
  theme(axis.title.x = element_blank())

d <- mutate(d, stock = paste(species, region)) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock))

dat_wide <- tidyr::pivot_wider(d,
  id_cols = year,
  values_from = log_blrp, names_from = stock, values_fill = 999
) %>%
  arrange(year) %>%
  filter(year >= 1950, year <= 2020)

dat_wide_tau <- tidyr::pivot_wider(d,
  id_cols = year,
  values_from = sd_log_blrp, names_from = stock, values_fill = 999
) %>%
  arrange(year) %>%
  filter(year >= 1950, year <= 2020)

dd <- as.matrix(t(dat_wide[,-1]))
first_obs <- apply(dd, 1, function(x) which(!(x == 999))[1])
stopifnot(first_obs >= 1)
dat <- list(
  T = nrow(dat_wide),
  J = ncol(dat_wide) - 1,
  y = as.matrix(dat_wide[, -1]),
  tau = as.matrix(dat_wide_tau[, -1]),
  first_obs = first_obs,
  rho_sd = 1
)

model <- stan_model("analysis/rw-ss.stan")
initf <- function() {
  list(
    rho = runif(1, 0.8, 0.98),
    sigma_eps = runif(1, 0.05, 0.3),
    sigma_x = runif(1, 0.05, 0.3),
    sigma_alpha = runif(1, 0.05, 0.3)
  )
}
m <- sampling(
  model,
  data = dat,
  chains = 3, iter = 300, # seed = 1823,
  # control = list(adapt_delta = 0.8, max_treedepth = 10),
  pars = c("rho", "sigma_eps", "sigma_x", "x", "alpha", "eps"),
  init = initf
)

# pairs(m, pars = c("sigma_eps", "sigma_x", "rho", "x[1]", "alpha[1]"))
# rstan::check_hmc_diagnostics(m)
# rstan::stan_rhat(m)
# rstan::stan_diag(m)
print(m, pars = c("sigma_eps", "sigma_x", "rho", "alpha"))
# shinystan::launch_shinystan(m)

p <- extract(m)
apply(p$alpha, 2, mean)
mean(apply(p$alpha, 2, mean))

pp <- apply(p$x, 2, function(x, ...) median(exp(x), ...), na.rm = TRUE)
plot(seq_along(pp), pp, lwd = 3, lty = 2, type = "l")

dd <- as.matrix(dat_wide[, -1])
dd[dd == 999] <- NA
matplot(exp(dd), type = "l", log = "y")

x <- apply(p$eps, c(2, 3), mean, na.rm = TRUE)
matplot(x, type = "l")

# x <- apply(p$y_true, c(2, 3), mean, na.rm = TRUE)
# matplot(exp(x), type = "l", log = "y")

hist(p$rho)
hist(p$sigma_eps)
hist(p$sigma_x)

basic <- group_by(d, year) %>%
  summarise(m = exp(mean(log_blrp))) %>%
  filter(year >= 1950, year <= 2020)

x_t <- tidybayes::gather_draws(m, x[.t])
x_t %>%
  mutate(.value = exp(.value)) %>%
  mutate(year = .t + min(dat_wide$year) - 1L) %>%
  group_by(year) %>%
  summarize(
    lwr2 = quantile(.value, 0.975),
    upr2 = quantile(.value, 0.025),
    lwr1 = quantile(.value, 0.25),
    upr1 = quantile(.value, 0.75),
    lwr = quantile(.value, 0.1),
    upr = quantile(.value, 0.9),
    med = median(.value), .groups = "drop"
  ) %>%
  ggplot(aes(year, med)) +
  geom_ribbon(aes(
    x = year,
    y = exp(log_blrp),
    ymin = exp(log_blrp - 2 * sd_log_blrp),
    ymax = exp(log_blrp + 2 * sd_log_blrp)
  ),
  colour = NA, alpha = 0.1, data = d, fill = "red"
  ) +
  geom_line(aes(
    x = year,
    y = exp(log_blrp)
  ), alpha = 0.6, data = d, colour = "red") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.35) +
  geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.35) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.35) +
  geom_line() +
  # theme_light() +
  ggsidekick::theme_sleek() +
  # geom_line(aes(x = year, y = m), data = basic, colour = "grey30", inherit.aes = FALSE, lty = 2) +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(.4, 15), expand = FALSE) +
  geom_hline(yintercept = 1, lty = 3) +
  scale_y_log10() +
  # scale_fill_viridis_d() +
  # scale_colour_viridis_d() +
  facet_wrap(~stock) +
  ylab("B/LRP") +
  theme(
    axis.title.x = element_blank(), panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_line(colour = "grey98")
  )

