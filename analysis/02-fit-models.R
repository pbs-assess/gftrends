library(dplyr)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
model <- stan_model("analysis/rw-ss.stan")
source("analysis/utils.R")

dat <- readRDS("data-generated/b-status-dat.rds")
dat[dat$species == "sablefish","sd_log_bbmsy"] <- dat[dat$species == "sablefish","sd_log_bbmsy"] * 2

d <- format_stan_data(dat, log_bbmsy, sd_log_bbmsy)

initf <- function() {
  list(
    rho = runif(1, 0.7, 0.95),
    sigma_eps = runif(1, 0.03, 0.1),
    sigma_x = runif(1, 0.03, 0.07),
    sigma_alpha = runif(1, 0.05, 0.3),
    x_t_intercept = runif(1, 1, 2)
  )
}

m <- sampling(
  model,
  data = d,
  chains = 6, iter = 300,
  init = initf,
  # control = list(adapt_delta = 0.98),
  pars = c("rho", "sigma_eps", "sigma_x", "x", "alpha", "y_miss", "y_true")
)

# pairs(m, pars = c("sigma_eps", "sigma_x", "rho", "x[1]", "alpha[1]"))
# rstan::check_hmc_diagnostics(m)
# rstan::stan_rhat(m)
# rstan::stan_diag(m)
print(m, pars = c("sigma_eps", "sigma_x", "rho", "alpha", "y_miss"))

p <- rstan::extract(m)

eps <- p$eps
means <- apply(eps, c(2, 3), mean)
sds <- apply(eps, c(2, 3), sd)
matplot(means, type = "l")
matplot(sds, type = "l")

miss <- p$y_m
apply(miss, 2, mean)
apply(miss, 2, sd)

# shinystan::launch_shinystan(m)

apply(p$alpha, 2, mean)
mean(apply(p$alpha, 2, mean))

# pp <- apply(p$x, 2, function(x, ...) median(exp(x), ...), na.rm = TRUE)
# plot(seq_along(pp), pp, lwd = 3, lty = 2, type = "l")

# dd <- as.matrix(dat_wide[, -1])
# dd[dd == 999] <- NA
# matplot(exp(dd), type = "l", log = "y")

# x <- apply(p$eps, c(2, 3), mean, na.rm = TRUE)
# matplot(x, type = "l")

# x <- apply(p$y_true, c(2, 3), mean, na.rm = TRUE)
# matplot(exp(x), type = "l", log = "y")

hist(p$rho)
hist(p$sigma_eps)
hist(p$sigma_x)
