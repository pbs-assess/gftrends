library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("analysis/utils.R")
model <- stan_model("analysis/rw-ss.stan")

dat <- readRDS("data-generated/b-status-dat.rds")
dat[dat$species == "sablefish","sd_log_bbmsy"] <-
  dat[dat$species == "sablefish","sd_log_bbmsy"] * 2
dat[dat$species == "sablefish","sd_log_blrp"] <-
  dat[dat$species == "sablefish","sd_log_blrp"] * 2
dat[dat$species == "sablefish","sd_log_busr"] <-
  dat[dat$species == "sablefish","sd_log_busr"] * 2

d <- list()
d$blrp <- format_stan_data(dat, log_blrp, sd_log_blrp)
d$bbmsy <- format_stan_data(dat, log_bbmsy, sd_log_bbmsy)
d$busr <- format_stan_data(dat, log_busr, sd_log_busr)

pars <- c("rho", "sigma_eps", "sigma_x", "x", "alpha", "y_true", "x_t_intercept")
m <- purrr::map(d, function(.d) {
  rstan::sampling(
    model,
    data = .d$stan_dat,
    chains = 6, iter = 600,
    init = initf,
    pars = pars
  )
})

purrr::walk(m, print, pars = pars[!pars %in% c("y_true", "x")])
# pairs(m[[1]], pars = c("sigma_eps", "sigma_x", "rho", "x[1]", "alpha[1]"))
# pairs(m[[2]], pars = c("sigma_eps", "sigma_x", "rho", "x[1]", "alpha[1]"))
# pairs(m[[3]], pars = c("sigma_eps", "sigma_x", "rho", "x[1]", "alpha[1]"))
# lapply(m, rstan::check_hmc_diagnostics)
# lapply(m, rstan::stan_rhat)
# lapply(m, rstan::stan_diag)

# p <- purrr::map(m, rstan::extract)

saveRDS(m, "data-generated/b-ratio-fits.rds")
saveRDS(d, "data-generated/b-ratio-fits-data.rds")

# eps <- p$eps
# means <- apply(eps, c(2, 3), mean)
# sds <- apply(eps, c(2, 3), sd)
# matplot(means, type = "l")
# matplot(sds, type = "l")
#
# miss <- p$y_m
# apply(miss, 2, mean)
# apply(miss, 2, sd)
#
# # shinystan::launch_shinystan(m)
#
# apply(p$alpha, 2, mean)
# mean(apply(p$alpha, 2, mean))

# pp <- apply(p$x, 2, function(x, ...) median(exp(x), ...), na.rm = TRUE)
# plot(seq_along(pp), pp, lwd = 3, lty = 2, type = "l")

# dd <- as.matrix(dat_wide[, -1])
# dd[dd == 999] <- NA
# matplot(exp(dd), type = "l", log = "y")

# x <- apply(p$eps, c(2, 3), mean, na.rm = TRUE)
# matplot(x, type = "l")

# x <- apply(p$y_true, c(2, 3), mean, na.rm = TRUE)
# matplot(exp(x), type = "l", log = "y")

# hist(p$rho)
# hist(p$sigma_eps)
# hist(p$sigma_x)
