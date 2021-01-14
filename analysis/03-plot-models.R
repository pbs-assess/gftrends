library(dplyr)
library(ggplot2)
library(rstan)
source("analysis/utils.R")

# dat <- readRDS("data-generated/b-status-dat.rds")

# dat <- filter(dat, !is.na(log_bbmsy))

x_t <- tidybayes::gather_draws(m, x[.t])
x_t %>%
  mutate(.value = exp(.value)) %>%
  mutate(year = .t + 1950) %>%
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
  # geom_ribbon(aes(
  #   x = year,
  #   y = exp(log_blrp),
  #   ymin = exp(log_blrp - 2 * sd_log_blrp),
  #   ymax = exp(log_blrp + 2 * sd_log_blrp)
  # ),
  # geom_ribbon(aes(
  #   x = year,
  #   y = exp(log_bbmsy),
  #   ymin = q0.05_bmsy,
  #   ymax = q0.95_bmsy
  # ),
    geom_ribbon(aes(
      x = year,
      y = exp(log_blrp),
      ymin = q0.05_blrp,
      ymax = q0.95_blrp
    ),
      colour = NA, alpha = 0.3, data = dat, fill = "red"
    ) +
  geom_line(aes(
    x = year,
    y = exp(log_blrp)
  ), alpha = 0.6, data = dat, colour = "red") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.35) +
  geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.35) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.35) +
  geom_line() +
  ggsidekick::theme_sleek() +
  coord_cartesian(xlim = c(1951, 2020), ylim = c(.1, 15), expand = FALSE) +
  geom_hline(yintercept = 1, lty = 3) +
  scale_y_log10() +
  facet_wrap(~stock) +
  ylab(expression(B/B[MSY])) +
  theme(
    axis.title.x = element_blank(), panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_line(colour = "grey98")
  )

y_true <- tidybayes::gather_draws(m, y_true[.t,j], n = 30)

stock_ids <- distinct(select(dat, stock)) %>%
  arrange(stock) %>%
  mutate(j = seq_len(n()))
# dat2 <- left_join(dat, ids)

year_ids <- distinct(select(dat, year)) %>%
  filter(year >= 1950, year <= 2020) %>%
  arrange(year) %>%
  mutate(.t = seq_len(n()))

y_true <- left_join(y_true, stock_ids) %>% left_join(year_ids)

ggplot(y_true, aes(year, exp(.value), group = .draw)) +
  geom_line(alpha = 0.3, lwd = 0.2) +
  facet_wrap(~stock) +
  geom_line(aes(x = year, y = exp(log_blrp)), data = filter(dat, year >= 1950), colour = "red",
    inherit.aes = FALSE) +
  geom_ribbon(aes(x = year, y = exp(log_blrp), ymin = q0.05_blrp, ymax = q0.95_blrp), data = filter(dat, year >= 1950), colour = NA, fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  ggsidekick::theme_sleek() +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(0, 10))
