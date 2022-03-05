library(dplyr)
library(ggplot2)
source("analysis/utils.R")
source("analysis/stock_df.R")
dir.create("figs", showWarnings = FALSE)

m <- readRDS("data-generated/b-ratio-fits-2022.rds")
d <- readRDS("data-generated/b-ratio-fits-data-2022.rds")
dat <- readRDS("data-generated/b-status-dat.rds")

plot_x_t <- function(x_t, .y_true, .fitted_dat, col_log_mean, col_q0.05, col_q0.95,
                     # .type = c("Rockfish", "Flatfish", "Cod", "Cod allies"), # in case you want a subset
                     ylab = "", ylim = c(0, 10), example = FALSE) {

  last_dat <- dat %>%
    group_by(stock) %>%
    mutate(
      last_status = exp({
        {
          col_log_mean
        }
      })[n()]
    ) %>%
    ungroup() %>%
    left_join(stock_df)

  stock_ids <- distinct(select(.fitted_dat, stock)) %>%
    arrange(stock) %>%
    mutate(j = seq_len(n()))
  year_ids <- distinct(select(.fitted_dat, year)) %>%
    filter(year >= 1950) %>%
    arrange(year) %>%
    mutate(.t = seq(1, n()))

  .y_true <- left_join(.y_true, stock_ids) %>% left_join(year_ids)

  .y_true <- .y_true %>%
    left_join(stock_df)

  .y_true <- left_join(.y_true, distinct(select(last_dat, stock_clean, last_status)))

  x_t <- x_t %>%
    mutate(.value = exp(.value)) %>%
    mutate(year = .t + 1949)

  if (example) {
    dat <- filter(dat, stock == "pcod_5abcd")
    .y_true <- filter(.y_true, stock == "pcod_5abcd")
  }

  set.seed(1234)
  .samples <- sample(unique(x_t$.draw), 100L)
  x_t %>%
    filter(.draw %in% .samples) %>%
    ggplot(aes(year, .value)) +
    geom_line(aes(y = .value, group = .draw), colour = "grey10", alpha = 0.04) +
    geom_line(
      mapping = aes(
        x = year, y = exp(.value), group = .draw,
        colour = last_status
      ),
      alpha = 0.3, lwd = 0.15, data = .y_true, inherit.aes = FALSE
    ) +
    geom_ribbon(aes(
      x = year,
      y = exp({
        {
          col_log_mean
        }
      }),
      ymin = {
        {
          col_q0.05
        }
      },
      ymax = {
        {
          col_q0.95
        }
      },
      fill = last_status
    ),
    colour = NA, alpha = 0.3, data = last_dat
    ) +
    geom_line(aes(
      x = year,
      y = exp({
        {
          col_log_mean
        }
      }),
      colour = last_status
    ), alpha = 0.9, data = last_dat, lwd = 0.6) +
    scale_colour_viridis_c(direction = 1, option = "D", end = 0.82) +
    scale_fill_viridis_c(direction = 1, option = "D", end = 0.82) +
    ggsidekick::theme_sleek() +
    coord_cartesian(xlim = c(1950, 2022), ylim = ylim, expand = FALSE) +
    geom_hline(yintercept = 1, lty = 2, col = "grey40") +
    # scale_y_log10() +
    facet_wrap(~stock_clean, ncol = 5L) +
    ylab(ylab) +
    theme(
      axis.title.x = element_blank()
    ) +
    labs(fill = "Last\nstatus", colour = "Last\nstatus") +
    guides(fill = "none", colour = "none") +
    theme(plot.margin = margin(t = 4, r = 13, b = 1, l = 2, unit = "pt"))
}

x_t <- lapply(m, function(.x) tidybayes::gather_draws(.x, x[.t]))
y_true <- lapply(m, function(.x) {
  result <- tidybayes::gather_draws(.x, y_true[.t, j], ndraws = 30L)
  dplyr::filter(result, .value != 0) # fake
})

# # change order of facets to group more like species together
stock_df <- stock_df %>% arrange(desc(type), stock) # for grouping by taxa/type first
# stock_df <- stock_df %>% arrange(stock) # for alphabetical order
stock_df <- stock_df %>% mutate(stock_clean = factor(stock_clean,
  levels = as.character(unique(stock_df$stock_clean))
))

g <- plot_x_t(x_t[["blrp"]], y_true[["blrp"]], d[["blrp"]]$filtered_dat,
  log_blrp, q0.05_blrp, q0.95_blrp,
  ylab = expression(B / LRP), ylim = c(0, 11.5)
)
ggsave("figs/blrp-x-t-2022.pdf", width = 8, height = 8)
ggsave("figs/blrp-x-t-2022.png", width = 8, height = 8)
# ggsave("figs/blrp-x-t-example-2022.png", width = 4, height = 3, dpi = 200)

g <- plot_x_t(x_t[["busr"]], y_true[["busr"]], d[["busr"]]$filtered_dat,
  log_busr, q0.05_busr, q0.95_busr,
  ylab = expression(B / USR), ylim = c(0, 5.5)
)
ggsave("figs/busr-x-t.pdf", width = 8, height = 8)
ggsave("figs/busr-x-t.png", width = 8, height = 8)

g <- plot_x_t(x_t[["bbmsy"]], y_true[["bbmsy"]], d[["bbmsy"]]$filtered_dat,
  log_bbmsy, q0.05_bmsy, q0.95_bmsy,
  ylab = expression(B / B[MSY]), ylim = c(0, 4.75)
)
ggsave("figs/bbmsy-x-t.pdf", width = 7, height = 8)
ggsave("figs/bbmsy-x-t.png", width = 7, height = 8)

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

pal <- c("#08d9d6", "#252a34", "#ff2e63")

g <- plot_data_sub %>%
  ggplot(aes(year, .value, group = paste(ratio, .draw), color = ratio)) +
  coord_cartesian(expand = FALSE, ylim = c(0.7, 7)) +
  # Trawl ITQ introduced
  annotate(
    geom = "rect", xmin = 1992, xmax = 1997, ymin = 0.7, ymax = 7,
    fill = "grey", alpha = 0.4
  ) +
  annotate(
    geom = "text", x = 1994, y = 6.9, label = "Trawl ITQs begin",
    angle = 90, color = "grey30", hjust = 1, size = 3.5
  ) +
  # synoptic trawl surveys begin
  annotate(
    geom = "rect", xmin = 2003, xmax = 2005, ymin = 0.7, ymax = 7,
    fill = "grey", alpha = 0.4
  ) +
  annotate(
    geom = "text", x = 2004, y = 6.9, label = "Synoptic surveys begin",
    angle = 90, color = "grey30", hjust = 1, size = 3.5
  ) +
  # ITQs and monitoring for line and trap begin
  # annotate(geom = "rect", xmin = 2006, xmax = 2007, ymin = 0.7, ymax= 7,
  # fill="grey", alpha = 0.4, inherit.aes = F) +
  geom_vline(xintercept = 2006, linetype = "solid", color = "grey", alpha = 0.4, lwd = 1.4) +
  annotate(
    geom = "text", x = 2007.8, y = 6.9, label = "Line & trap ITQs begin",
    angle = 90, color = "grey30", hjust = 1, size = 3.5
  ) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, y = med, x = year, fill = ratio), inherit.aes = FALSE, data = summarized_plot_data, alpha = 0.3) +
  geom_line(aes(y = med, x = year, color = ratio),
    inherit.aes = FALSE, data = summarized_plot_data, alpha = 1, lwd = 1
  ) +
  geom_line(alpha = 0.3, lwd = 0.3) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  ylab("Ratio value") +
  xlab("Year") +
  labs(color = "Ratio", fill = "Ratio") +
  ggsidekick::theme_sleek() +
  theme(legend.position = c(0.13, 0.65), plot.margin = margin(t = 8, r = 13, b = 1, l = 2, unit = "pt")) +
  scale_colour_viridis_d(
    labels = c(expression(B / LRP), expression(B / USR), expression(B / B[MSY])),
    option = "A", end = 0.85, direction = 1
  ) +
  scale_fill_viridis_d(
    labels = c(expression(B / LRP), expression(B / USR), expression(B / B[MSY])),
    option = "A", end = 0.85, direction = 1
  ) +
  geom_hline(yintercept = 1, lty = 2, col = "grey60")


ggsave("figs/ts-summary.pdf", width = 4.5, height = 4)
ggsave("figs/ts-summary.png", width = 4.5, height = 4)

saveRDS(plot_data, "data-generated/x_t_posterior.rds")
