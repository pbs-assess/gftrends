format_stan_data <- function(dat, column_mean, column_sd) {
  dat <- dplyr::filter(dat, !is.na({{column_mean}}))
  dat_wide <- tidyr::pivot_wider(dat,
    id_cols = year,
    values_from = {{column_mean}}, names_from = stock, values_fill = 999
  ) %>%
    arrange(year) %>%
    filter(year >= 1950, year <= 2020)
  dat_wide_tau <- tidyr::pivot_wider(dat,
    id_cols = year,
    values_from = {{column_sd}}, names_from = stock, values_fill = 999
  ) %>%
    arrange(year) %>%
    filter(year >= 1950, year <= 2020)
  dd <- as.matrix(t(dat_wide[,-1]))
  first_obs <- apply(dd, 1, function(x) which(!(x == 999))[1])
  stopifnot(first_obs >= 1)
  stan_dat <- list(
    T = nrow(dat_wide),
    J = ncol(dat_wide) - 1,
    y = as.matrix(dat_wide[, -1]),
    tau = as.matrix(dat_wide_tau[, -1]),
    first_obs = first_obs,
    rho_sd = 1
  )
  stan_dat
}

initf <- function() {
  list(
    rho = runif(1, 0.8, 0.98),
    sigma_eps = runif(1, 0.05, 0.3),
    sigma_x = runif(1, 0.05, 0.3),
    sigma_alpha = runif(1, 0.05, 0.3)
  )
}
