library(dplyr)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
model <- stan_model("analysis/rw-ss.stan")
source("analysis/utils.R")

dat <- readRDS("data-generated/b-status-dat.rds")
d <- format_stan_data(dat, log_bbmsy, sd_log_bbmsy)

m <- sampling(
  model,
  data = d,
  chains = 3, iter = 300, seed = 1823,
  pars = c("rho", "sigma_eps", "sigma_x", "x", "alpha", "eps")
)

# pairs(m, pars = c("sigma_eps", "sigma_x", "rho", "x[1]", "alpha[1]"))
# rstan::check_hmc_diagnostics(m)
# rstan::stan_rhat(m)
# rstan::stan_diag(m)
print(m, pars = c("sigma_eps", "sigma_x", "rho", "alpha"))
# shinystan::launch_shinystan(m)

p <- rstan::extract(m)
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
