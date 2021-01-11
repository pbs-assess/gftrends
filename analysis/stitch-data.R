# library(tidyverse)
library(dplyr)
library(ggplot2)

f <- list.files("data-raw", pattern = ".rds", full.names = TRUE)

# dl <- purrr::map(f, readRDS)
d <- purrr::map_dfr(f, readRDS)

d <- select(d, year, species, region, log_blrp, sd_log_blrp)

d %>% mutate(stock = paste(species, region)) %>%
  ggplot(aes(
    x = year,
    y = exp(log_blrp),
    ymin = exp(log_blrp - 2 * sd_log_blrp),
    ymax = exp(log_blrp + 2 * sd_log_blrp))) +
  geom_ribbon(colour = NA, fill = "grey50", alpha = 0.4) +
  geom_line() +
  facet_wrap(~stock) +
  scale_y_log10() +
  geom_hline(yintercept = 1, lty = 2) +
  coord_cartesian(xlim = c(1950, 2020)) +
  ylab("B/LRP") + theme(axis.title.x = element_blank())

d <- mutate(d, stock = paste(species, region)) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock))

# d <- filter(d, species != "bocaccio", stock != "sablefish_BC")

dat_wide <- tidyr::pivot_wider(d, id_cols = year,
  values_from = log_blrp, names_from = stock, values_fill = 999) %>%
  arrange(year) %>%
  filter(year >= 1950, year <= 2020)

dat_wide_tau <- tidyr::pivot_wider(d, id_cols = year,
  values_from = sd_log_blrp, names_from = stock, values_fill = 999) %>%
  arrange(year) %>%
  filter(year >= 1950, year <= 2020)

# ypresent <- as.matrix(t(dat_wide[,-1]))
# ypresent[!is.na(ypresent)] <- 1L
# ypresent[is.na(ypresent)] <- 0L
#
# dd <- as.matrix(t(dat_wide[,-1]))
# # first_obs <- apply(dd, 1, function(x) which(!is.na(x))[1]) - 1L
# first_obs <- apply(ypresent, 1, which.max) - 1 ## -1 for start at zero in TMB

# dat_tmb <- list(
#   y = as.matrix(t(dat_wide[,-1])),
#   ypresent = ypresent,
#   first_obs = first_obs
# )


# library(TMB)
# compile("analysis/dlm_ar1.cpp")
# dyn.load(dynlib("analysis/dlm_ar1"))
# y = as.matrix(t(dat_wide[,-1]))
# n <- ncol(y)
# m <- nrow(y)
# obj <- MakeADFun(
#   data = list(
#     y = y,
#     ypresent = ypresent,
#     first_obs = first_obs
#   ),
#   parameters = list(
#     lnsde = log(0.1),
#     lnsdx = log(0.1),
#     logitrho = -log(2/(1 + 0.5) - 1), ## for AR(1) = 0.5
#     x = rep(0, n),
#     Apar = rep(0, m - 1)
#   ),
#   random = c("x"),
#   DLL = "dlm_ar1",
#   silent = FALSE)
#
# opt <- nlminb(objective = obj$fn,
#   gradient = obj$gr,
#   start = obj$par,
#   lower = c(lnsde = log(0.05), lnsdx  = log(0.05)),
#   control = list(iter.max = 1e3, eval.max = 1e3))

# opt
# rep <- sdreport(obj)
# srep <- summary(rep)
# xhat <- srep[rownames(srep) == "x", ]
# rownames(xhat) <- NULL
# xhat <- as.data.frame(xhat)
# head(xhat)
# xhat$year <- dat_wide$year
# xhat$se <- xhat$`Std. Error`
# ggplot(xhat, aes(year, Estimate, ymin = Estimate - 2 * se, ymax = Estimate + 2 * se)) +
#   geom_line() +
#   geom_ribbon(alpha = 0.2)

dat <- list(
  T = nrow(dat_wide),
  J = ncol(dat_wide) - 1,
  y = as.matrix(dat_wide[,-1]),
  tau = as.matrix(dat_wide_tau[,-1]),
  rho_sd = 1
  # tau = matrix(0.2, nrow = nrow(dat_wide), ncol = ncol(dat_wide) - 1))
)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
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
  pars = c("rho", "sigma_eps", "sigma_x", "x", "alpha", "eps", "y_true"),
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
#
# pp <- apply(p$y_true, c(2, 3), function(x, ...) exp(mean(x, ...)), na.rm = TRUE)
# matplot(pp, type = "l", lty = 1, ylab = "B/LRP latent")

pp <- apply(p$x, 2, function(x, ...) median(exp(x), ...), na.rm = TRUE)
# lines(seq_along(pp), pp, lwd = 3, lty = 2)
plot(seq_along(pp), pp, lwd = 3, lty = 2, type = "l")

dd <- as.matrix(dat_wide[,-1])
dd[dd == 999] <- NA
matplot(exp(dd), type = "l", log = "y")

x <- apply(p$eps, c(2, 3), mean, na.rm = TRUE)
matplot(x, type = "l")

x <- apply(p$y_true, c(2, 3), mean, na.rm = TRUE)
matplot(exp(x), type = "l", log = "y")

hist(p$rho)
hist(p$sigma_eps)
hist(p$sigma_x)

basic <- group_by(d, year) %>% summarise(m = exp(mean(log_blrp))) %>%
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
    ymax = exp(log_blrp + 2 * sd_log_blrp)),
    colour = NA, alpha = 0.1, data = d, fill = "red") +
  geom_line(aes(
    x = year,
    y = exp(log_blrp)), alpha = 0.6, data = d, colour = "red") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.35) +
  geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.35) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.35) +
  geom_line() +
  # theme_light() +
  ggsidekick::theme_sleek() +
  geom_line(aes(x = year, y = m), data = basic, colour = "grey30", inherit.aes = FALSE, lty = 2) +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(.3, 12), expand = FALSE) +
  geom_hline(yintercept = 1, lty = 3) +
  scale_y_log10() +
  # scale_fill_viridis_d() +
  # scale_colour_viridis_d() +
  facet_wrap(~stock) +
  ylab("B/LRP") +
  theme(axis.title.x = element_blank(), panel.grid.major = element_line(colour = "grey95"))
