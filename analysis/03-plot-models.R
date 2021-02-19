library(dplyr)
library(ggplot2)
source("analysis/utils.R")
source("analysis/stock_df.R")
dir.create("figs", showWarnings = FALSE)

m <- readRDS("data-generated/b-ratio-fits.rds")
d <- readRDS("data-generated/b-ratio-fits-data.rds")
dat <- readRDS("data-generated/b-status-dat.rds")

plot_x_t <- function(x_t, .y_true, .fitted_dat, col_log_mean, col_q0.05, col_q0.95, ylab = "", ylim = c(0, 10)) {
  last_dat <- dat %>%
    group_by(stock) %>%
    mutate(
      countn = n(),
      last_status = exp({{col_log_mean}})[n()],
      next2last = exp({{col_log_mean}})[countn-1],
      next3last = exp({{col_log_mean}})[countn-2],
      last3status = (last_status + next2last + next3last)/3
      ) %>%
    ungroup() %>%
    # mutate(stock = gsub("_", " ", stock)) %>%
    left_join(stock_df)

  stock_ids <- distinct(select(.fitted_dat, stock)) %>%
    arrange(stock) %>%
    mutate(j = seq_len(n()))
  year_ids <- distinct(select(.fitted_dat, year)) %>%
    filter(year >= 1950, year <= 2020) %>%
    arrange(year) %>%
    mutate(.t = seq(1, n()))
  .y_true <- left_join(.y_true, stock_ids) %>% left_join(year_ids)
  # .y_true <- mutate(.y_true, stock = gsub("_", " ", stock)) %>%
  .y_true <- .y_true %>%
    left_join(stock_df)

  .y_true <- left_join(.y_true, distinct(select(last_dat, stock_clean, last_status, last3status)))

  x_t <- x_t %>%
    mutate(.value = exp(.value)) %>%
    mutate(year = .t + 1949)

  # summarized <- x_t %>%
  #   mutate(.value = exp(.value)) %>%
  #   mutate(year = .t + 1949) %>%
  #   group_by(year) %>%
  #   summarize(
  #     lwr2 = quantile(.value, 0.975),
  #     upr2 = quantile(.value, 0.025),
  #     lwr1 = quantile(.value, 0.25),
  #     upr1 = quantile(.value, 0.75),
  #     lwr = quantile(.value, 0.1),
  #     upr = quantile(.value, 0.9),
  #     med = median(.value), .groups = "drop"
  #   ) %>%
  set.seed(1234)
  .samples <- sample(unique(x_t$.draw), 100L)
  x_t %>%
    filter(.draw %in% .samples) %>%
    ggplot(aes(year, .value)) +
    geom_line(aes(y = .value, group = .draw), colour = "grey10", alpha = 0.04) +
    # geom_ribbon(aes(ymin = lwr2, ymax = upr2), fill = "grey70", alpha = 0.6) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey55", alpha = 0.6) +
    # geom_ribbon(aes(ymin = lwr1, ymax = upr1), fill = "grey40", alpha = 0.6) +
    geom_line(mapping = aes(x = year, y = exp(.value), group = .draw,
      # colour = last_status
      colour = last3status
      ),
      alpha = 0.3, lwd = 0.15, data = .y_true, inherit.aes = FALSE) +
    # geom_line(lwd = 0.7, alpha = 0.75) +
    geom_ribbon(aes(
      x = year,
      y = exp({{col_log_mean}}),
      ymin = {{col_q0.05}},
      ymax = {{col_q0.95}},
      # fill = last_status
      fill = last3status
    ),
    colour = NA, alpha = 0.3, data = last_dat
    ) +
    geom_line(aes(
      x = year,
      y = exp({{col_log_mean}}),
      # colour = last_status
      colour = last3status
    ), alpha = 0.9, data = last_dat, lwd = 0.6) +
    # scale_colour_gradient2(
    #   low = "red", high = "green", mid="blue",
    #   # midpoint = mean(.y_true$last_status)
    #   midpoint = 1
    #   ) +
    # scale_fill_gradient2(
    #   low = "red", high = "green", mid="blue",
    #   # midpoint = mean(.y_true$last_status)
    #   midpoint = 1
    #   ) +
    # scale_fill_distiller(palette = "Spectral", direction = 1) +
    # scale_colour_distiller(palette = "Spectral", direction = 1) +
    scale_colour_viridis_c(direction = 1, option = "D", end = 0.65) +
    scale_fill_viridis_c(direction = 1, option = "D", end = 0.65) +
    ggsidekick::theme_sleek() +
    coord_cartesian(xlim = c(1950, 2020), ylim = ylim, expand = FALSE) +
    geom_hline(yintercept = 1, lty = 2, col = "grey40") +
    # scale_y_log10() +
    facet_wrap(~stock_clean, ncol = 4) +
    ylab(ylab) +
    theme(
      axis.title.x = element_blank()
      # panel.grid.major = element_line(colour = "grey92"),
      # panel.grid.minor = element_line(colour = "grey98")
    ) +
    labs(fill = "Last\nstatus", colour = "Last\nstatus") +
    guides(fill = FALSE, colour = FALSE) +
    theme(plot.margin = margin(t = 4, r = 13, b = 1, l = 2, unit = "pt"))
  # theme(legend.position = "none")
}

x_t <- lapply(m, function(.x) tidybayes::gather_draws(.x, x[.t]))
y_true <- lapply(m, function(.x) {
  result <- tidybayes::gather_draws(.x, y_true[.t, j], n = 30L)
  filter(result, .value != 0) # fake
})

g <- plot_x_t(x_t[["blrp"]], y_true[["blrp"]], d[["blrp"]]$filtered_dat,
  log_blrp, q0.05_blrp, q0.95_blrp,
  ylab = expression(B / LRP), ylim = c(0, 10)
)
ggsave("figs/blrp-x-t.pdf", width = 7, height = 8)
ggsave("figs/blrp-x-t.png", width = 7, height = 8, dpi = 200)

g <- plot_x_t(x_t[["busr"]], y_true[["busr"]], d[["busr"]]$filtered_dat,
  log_busr, q0.05_busr, q0.95_busr,
  ylab = expression(B / USR), ylim = c(0, 5)
)
ggsave("figs/busr-x-t.pdf", width = 7, height = 8)
ggsave("figs/busr-x-t.png", width = 7, height = 8, dpi = 200)

g <- plot_x_t(x_t[["bbmsy"]], y_true[["bbmsy"]], d[["bbmsy"]]$filtered_dat,
  log_bbmsy, q0.05_bmsy, q0.95_bmsy,
  ylab = expression(B / B[MSY]), ylim = c(0, 3.5)
)
ggsave("figs/bbmsy-x-t.pdf", width = 7, height = 8)
ggsave("figs/bbmsy-x-t.png", width = 7, height = 8, dpi = 200)

# Summary plot --------------------------------------------------------

set.seed(1234)
.samples <- sample(unique(x_t[[1]]$.draw), 25L)

plot_data <- bind_rows(x_t, .id = "ratio") %>%
  mutate(year = .t + 1949) %>%
  mutate(.value = exp(.value)) %>%
  mutate(ratio = gsub("bbmsy", "B/B[MSY]", ratio)) %>%
  mutate(ratio = gsub("blrp", "B/LRP", ratio)) %>%
  mutate(ratio = gsub("busr", "B/USR", ratio)) %>%
  mutate(ratio = factor(ratio, levels = c("B/LRP", "B/USR", "B/B[MSY]")))

summarized_plot_data <- group_by(plot_data, ratio, year) %>%
  summarize(
    lwr = quantile(.value, probs = 0.025),
    upr = quantile(.value, probs = 0.975),
    med = median(.value), .groups = "drop"
  )

plot_data_sub <- plot_data %>% filter(.draw %in% .samples)

g <- plot_data_sub %>%
  ggplot(aes(year, .value, group = paste(ratio, .draw), color = ratio)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, y = med, x = year, fill = ratio), inherit.aes = FALSE, data = summarized_plot_data, alpha = 0.3) +
  geom_line(aes(y = med, x = year, color = ratio),
    inherit.aes = FALSE, data = summarized_plot_data, alpha = 1, lwd = 1
  ) +
  geom_line(alpha = 0.3, lwd = 0.3) +
  # ITQ introduced
  # geom_vline(xintercept = 1997, linetype="dotted",
  #   color="grey50") +
  # annotate(geom="text", x=1999, y=5.2, label="Trawl ITQ", angle = 90,
  #   color="grey30") +
  # # synoptic trawl surveys begin
  # geom_vline(xintercept = 2003, linetype="dotted",
  #   color="grey50") +
  # annotate(geom="text", x=2005, y=5.2, label="Synoptic trawl surveys", angle = 90,
  #   color="grey30") +
  geom_vline(xintercept = 1997, linetype = "dashed", color = "grey40") +
  annotate(
    geom = "text", x = 1999, y = 5.8, label = "Trawl ITQs introduced", angle = 90,
    color = "grey30", hjust = 1, size = 3.5
  ) +
  # synoptic trawl surveys begin
  geom_vline(xintercept = 2003, linetype = "dashed", color = "grey40") +
  annotate(
    geom = "text", x = 2005, y = 5.8, label = "Synoptic surveys begin", angle = 90,
    color = "grey30", hjust = 1, size = 3.5
  ) +
  # geom_vline(xintercept = 1996, linetype = "dashed", color = "grey40") +
  # annotate(
  #   geom = "text", x = 1998, y = 5.8, label = "Trawl observerer coverage", angle = 90,
  #   color = "grey30", hjust = 1, size = 3.5
  # ) +
  ggsidekick::theme_sleek() +
  coord_cartesian(expand = FALSE, ylim = c(0.7, 6.7)) +
  # scale_y_continuous(trans = "sqrt") +
  ylab("Ratio value") +
  xlab("Year") +
  labs(color = "Ratio", fill = "Ratio") +
  theme(legend.position = c(0.13, 0.2), plot.margin = margin(t = 8, r = 13, b = 1, l = 2, unit = "pt")) +
  # scale_colour_brewer(palette = "Dark2",
  #   labels = c(expression(B/LRP), expression(B/USR), expression(B/B[MSY]))
  #   ) +
  # scale_fill_brewer(palette = "Dark2",
  #   labels = c(expression(B/LRP), expression(B/USR), expression(B/B[MSY]))
  # ) +
  scale_colour_viridis_d(
    labels = c(expression(B/LRP), expression(B/USR), expression(B/B[MSY]))
  ) +
  scale_fill_viridis_d(
    labels = c(expression(B/LRP), expression(B/USR), expression(B/B[MSY]))
  ) +
  geom_hline(yintercept = 1, lty = 2, col = "grey60")

ggsave("figs/ts-summary.pdf", width = 4.5, height = 4)
ggsave("figs/ts-summary.png", width = 4.5, height = 4)

saveRDS(plot_data, "data-generated/x_t_posterior.rds")
