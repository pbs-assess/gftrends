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
    chains = 4L, iter = 2000L,
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

saveRDS(m, "data-generated/b-ratio-fits.rds")
saveRDS(d, "data-generated/b-ratio-fits-data.rds")
