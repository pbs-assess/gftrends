format_stan_data <- function(dat, column_mean, column_sd) {
  dat <- dplyr::filter(dat, !is.na({{column_mean}}), !is.na({{column_sd}})) %>%
    filter(year >= 1950, year <= 2020)
  dat <- arrange(dat, stock, year)
  dat_wide <- tidyr::pivot_wider(dat,
    id_cols = year,
    values_from = {{column_mean}}, names_from = stock, values_fill = 999
  ) %>%
    arrange(year)
  dat_wide_tau <- tidyr::pivot_wider(dat,
    id_cols = year,
    values_from = {{column_sd}}, names_from = stock, values_fill = 999
  ) %>%
    arrange(year)
  dd <- as.matrix(t(dat_wide[,-1]))
  first_obs <- apply(dd, 1, function(x) which(!(x == 999))[1])
  stopifnot(first_obs >= 1)
  num_missing <- apply(dd, 1, function(x) {
    first <- which(!(x == 999))[1]
    sum(x[seq(first, length(x))] == 999)
  })
  num_missing = sum(num_missing)
  stan_dat <- list(
    T = nrow(dat_wide),
    J = ncol(dat_wide) - 1,
    y = as.matrix(dat_wide[, -1]),
    tau = as.matrix(dat_wide_tau[, -1]),
    first_obs = first_obs,
    rho_sd = 1,
    N_miss = num_missing
  )
  list(stan_dat = stan_dat, filtered_dat = dat)
}

initf <- function() {
  list(
    rho = runif(1, 0.6, 0.98),
    sigma_eps = runif(1, 0.03, 0.1),
    sigma_x = runif(1, 0.03, 0.1),
    sigma_alpha = runif(1, -0.5, 0.5),
    x_t_intercept = runif(1, 0.5, 1.5)
  )
}
