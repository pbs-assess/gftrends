library(dplyr)
options(mc.cores = parallel::detectCores()/2)
source("analysis/utils.R")
# model <- cmdstanr::cmdstan_model("analysis/rw-ss.stan")
model <- rstan::stan_model("analysis/rw-ss.stan")

dat <- readRDS("data-generated/b-status-dat.rds")

dat[dat$species == "sablefish","sd_log_bbmsy"] <-
  dat[dat$species == "sablefish","sd_log_bbmsy"] * 2
dat[dat$species == "sablefish","sd_log_blrp"] <-
  dat[dat$species == "sablefish","sd_log_blrp"] * 2
dat[dat$species == "sablefish","sd_log_busr"] <-
  dat[dat$species == "sablefish","sd_log_busr"] * 2

d <- list()
d$blrp <- format_stan_data(dat, log_blrp, sd_log_blrp, max_year = end_year)
d$bbmsy <- format_stan_data(dat, log_bbmsy, sd_log_bbmsy, max_year = end_year)
d$busr <- format_stan_data(dat, log_busr, sd_log_busr, max_year = end_year)

pars <- c("rho", "sigma_eps", "sigma_x", "x", "alpha", "y_true", "x_t_intercept")
m <- purrr::map(d, function(.d) {
  # fit <- model$sample(
  #   data = .d$stan_dat,
  #   chains = 6L,
  #   iter_sampling = 500L,
  #   iter_warmup = 500L,
  #   seed = 84791L,
  #   adapt_delta = 0.9,
  #   max_treedepth = 20L
  # )
  #
  rstan::sampling(
    model,
    data = .d$stan_dat,
    chains = 6L, iter = 1500L,
    pars = pars,
    control = list(max_treedepth = 12L, adapt_delta = 0.95),
    seed = 84791
  )
  # rstan::read_stan_csv(fit$output_files())
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
