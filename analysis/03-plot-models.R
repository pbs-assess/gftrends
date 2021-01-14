library(dplyr)
library(ggplot2)
library(rstan)
source("analysis/utils.R")
dir.create("figs", showWarnings = FALSE)

m <- readRDS("data-generated/b-ratio-fits.rds")
d <- readRDS("data-generated/b-ratio-fits-data.rds")
dat <- readRDS("data-generated/b-status-dat.rds")

plot_x_t <- function(x_t, col_log_mean, col_q0.05, col_q0.95, ylab = "", ylim = c(0, 10)) {
  last_dat <- dat %>%
    group_by(stock) %>%
    mutate(last_status = exp({{col_log_mean}})[n()]) %>%
    ungroup() %>%
    mutate(stock = gsub("_", " ", stock))

  x_t %>%
    mutate(.value = exp(.value)) %>%
    mutate(year = .t + 1949) %>%
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
    geom_ribbon(aes(ymin = lwr2, ymax = upr2), fill = "grey70", alpha = 0.6) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey55", alpha = 0.6) +
    geom_ribbon(aes(ymin = lwr1, ymax = upr1), fill = "grey40", alpha = 0.6) +
    geom_line(lwd = 0.7) +
    geom_ribbon(aes(
      x = year,
      y = exp({{col_log_mean}}),
      ymin = {{col_q0.05}},
      ymax = {{col_q0.95}},
      fill = last_status
    ),
      colour = NA, alpha = 0.3, data = last_dat
    ) +
    geom_line(aes(
      x = year,
      y = exp({{col_log_mean}}),
      colour = last_status,
    ), alpha = 0.8, data = last_dat, lwd = 0.6) +
    scale_colour_viridis_c(direction = -1, option = "C", end = 0.9) +
    scale_fill_viridis_c(direction = -1, option = "C", end = 0.9) +
    ggsidekick::theme_sleek() +
    coord_cartesian(xlim = c(1950, 2020), ylim = ylim, expand = FALSE) +
    geom_hline(yintercept = 1, lty = 2) +
    # scale_y_log10() +
    facet_wrap(~stock) +
    ylab(ylab) +
    theme(
      axis.title.x = element_blank(), panel.grid.major = element_line(colour = "grey92"),
      panel.grid.minor = element_line(colour = "grey98")
    ) +
    labs(fill = "Last\nstatus", colour = "Last\nstatus")
  # theme(legend.position = "none")
}

x_t <- lapply(m, function(.x) tidybayes::gather_draws(.x, x[.t]))
g <- plot_x_t(x_t[["blrp"]], log_blrp, q0.05_blrp, q0.95_blrp,
  ylab = expression(B/LRP), ylim = c(0, 10))
ggsave("figs/blrp-x-t.png", width = 10, height = 7)

g <- plot_x_t(x_t[["busr"]], log_busr, q0.05_busr, q0.95_busr,
  ylab = expression(B/USR), ylim = c(0, 5))
ggsave("figs/busr-x-t.png", width = 10, height = 7)

g <- plot_x_t(x_t[["bbmsy"]], log_bbmsy, q0.05_bmsy, q0.95_bmsy,
  ylab = expression(B/B[MSY]), ylim = c(0, 3.5))
ggsave("figs/bbmsy-x-t.png", width = 10, height = 7)

y_true <- tidybayes::gather_draws(m[[1]], y_true[.t, j], n = 30)
y_true <- filter(y_true, .value != 0) # fake

stock_ids <- distinct(select(d[[1]]$filtered_dat, stock)) %>%
  arrange(stock) %>%
  mutate(j = seq_len(n()))
year_ids <- distinct(select(d[[1]]$filtered_dat, year)) %>%
  filter(year >= 1950, year <= 2020) %>%
  arrange(year) %>%
  mutate(.t = seq_len(n()))
y_true <- left_join(y_true, stock_ids) %>% left_join(year_ids)

g <- ggplot(y_true, aes(year, exp(.value), group = .draw)) +
  geom_line(alpha = 0.3, lwd = 0.2) +
  facet_wrap(~stock) +
  geom_line(aes(x = year, y = exp(log_blrp)),
    data = filter(dat, year >= 1950), colour = "red",
    inherit.aes = FALSE
  ) +
  geom_ribbon(aes(x = year, y = exp(log_blrp), ymin = q0.05_blrp, ymax = q0.95_blrp),
    data = filter(dat, year >= 1950), colour = NA, fill = "red", alpha = 0.2,
    inherit.aes = FALSE
  ) +
  ggsidekick::theme_sleek() +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(0, 10))
ggsave("figs/blrp-stock-latent.png", width = 10, height = 7)
