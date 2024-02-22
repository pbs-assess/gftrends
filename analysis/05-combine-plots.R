# run instead of 03 and 04
library(ggplot2)
library(dplyr)
library(ggsidekick)
library(ggridges)
library(patchwork)
source("analysis/utils.R")
source("analysis/stock_df.R")
dir.create("figs", showWarnings = FALSE)

m <- readRDS("data-generated/b-ratio-fits.rds")
d <- readRDS("data-generated/b-ratio-fits-data.rds")
dat <- readRDS("data-generated/b-status-dat.rds")
dm <- readRDS("data-generated/all-mcmc.rds")

plot_x_t <- function(x_t, .y_true, .fitted_dat, col_log_mean, col_q0.05, col_q0.95,
  # .type = c("Rockfish", "Flatfish", "Cod", "Cod allies"), # in case you want a subset
  ylab = "", ylim = c(0, 10)
) {
  last_dat <- dat %>%
    group_by(stock) %>%
    mutate(
      last_status = exp({{col_log_mean}})[n()]
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

  set.seed(1234)
  .samples <- sample(unique(x_t$.draw), 100L)
  x_t %>%
    filter(.draw %in% .samples) %>%
    ggplot(aes(year, .value)) +
    geom_line(aes(y = .value, group = .draw), colour = "grey10", alpha = 0.04) +
    geom_line(mapping = aes(x = year, y = exp(.value), group = .draw,
      colour = last_status
    ),
      alpha = 0.3, lwd = 0.15, data = .y_true, inherit.aes = FALSE) +
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
  levels = as.character(unique(stock_df$stock_clean))))

g2 <- plot_x_t(x_t[["busr"]], y_true[["busr"]], d[["busr"]]$filtered_dat,
  log_busr, q0.05_busr, q0.95_busr,
  ylab = expression(B / USR), ylim = c(0, 5.5)
)


# # ridges
stock_df <- stock_df %>% arrange(stock) # for grouping by taxa/type fir
stock_df <- stock_df %>% mutate(stock_clean = factor(stock_clean,
  levels = as.character(unique(stock_df$stock_clean))))

dat2 <- dm %>%
  group_by(species, region) %>%
  filter(year <= 2022) %>%
  filter(year == max(year)) %>%
  # sample_n(2000L, replace = TRUE) %>%
  mutate(
    mean_blrp = mean(blrp, na.rm = TRUE),
    mean_bbmsy = mean(bbmsy, na.rm = TRUE),
    mean_busr = mean(busr, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(stock = paste(species, region, sep = "_")) %>%
  arrange(year, species) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock)) %>%
  left_join(stock_df)%>%
  arrange(year, desc(type), stock_clean)

dat_sum <- group_by(dat2, stock_clean) %>%
  summarize(p_lrp = mean(blrp < 1), p_usr = mean(busr < 1),
    p_bbmsy = mean(bbmsy < 1, na.rm = TRUE)) %>%
  arrange(-p_lrp)

lines <- tibble(
  ratio = c("B/B[MSY]", "B/B[MSY]", "B/B[MSY]", "B/LRP", "B/USR"),
  ratio_value = c(0.4, 0.8, 1, 1, 1)
) %>%
  # mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]"))))
  filter(ratio %in% c("B/LRP", "B/USR")) %>%
  mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR"))))

data_plot <- dat2 %>%
  tidyr::pivot_longer(
    cols = c(blrp, busr, bbmsy),
    names_to = "ratio", values_to = "ratio_value"
  ) %>%
  mutate(stock_clean = forcats::fct_reorder(stock_clean, year)) %>%
  mutate(ratio = gsub("bbmsy", "B/B[MSY]", ratio)) %>%
  mutate(ratio = gsub("blrp", "B/LRP", ratio)) %>%
  mutate(ratio = gsub("busr", "B/USR", ratio)) %>%
  mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]")))) %>%
  group_by(ratio) %>%
  dplyr::filter(
    ratio_value < quantile(ratio_value, probs = 0.98, na.rm = TRUE),
    ratio_value > quantile(ratio_value, probs = 0.005, na.rm = TRUE)
  )

years <- select(data_plot, ratio, stock_clean, year) %>%
  distinct() %>%
  filter(ratio == "B/LRP") %>%
  mutate(ratio_value = 9.6) %>%
  # mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]"))))
  mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR"))))

unique(data_plot$ratio)

(g1 <- data_plot %>%
    filter(ratio %in% c("B/LRP", "B/USR")) %>%
    mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR")))) %>%
  ggplot(aes(x = ratio_value, y = stock_clean, fill = (mean_blrp), group = stock_clean)) +
  geom_vline(
    data = lines, mapping = aes(xintercept = ratio_value),
    lty = 2, lwd = 0.4, colour = "grey70"
  ) +
  facet_wrap(vars(ratio), labeller = label_parsed, scales = "free_x") +
  geom_density_ridges2(scale = 4, alpha = 0.7, lwd = 0.4, colour = "grey30") +
  scale_x_continuous(trans = "sqrt", breaks = c(0.2, 1, 2, 5)) +
  scale_fill_viridis_c(direction = 1, option = "D", end = 0.82) +
  theme_sleek() +
  coord_cartesian(expand = FALSE) +
  labs(x = "Ratio value", fill = "Mean\nB/LRP") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 9.5),
    axis.text.x = element_text(size = 7.5),
    axis.text.y = element_text(size = 8.25),
    axis.text.y.left = element_text(vjust = -0.8)
  ) +
  geom_text(
    mapping = aes(x = ratio_value, y = stock_clean, label = year),
    data = years, size = 2.5, color = "grey30", nudge_y = 0.7,
    inherit.aes = FALSE
  ) +
  guides(fill = FALSE))

# ygrob <- grid::textGrob("Rockfish",
#   gp = grid::gpar(fontsize = 12),
#   hjust = 0.5,
#   vjust = 0.7,
#   rot = 270
# )
#
# ygrob2 <- grid::textGrob("Other species",
#   gp = grid::gpar(fontsize = 12),
#   hjust = 1,
#   vjust = 0.7,
#   rot = 270
# )
#
# layout <- "
#       ABC
#       ABC
#       ABD
#       "
#
# wrap_plots(g1, g2, ygrob, ygrob2) +
#   plot_layout(design = layout, ncol = 3,
#     widths = c(1, 2, 0.02))

wrap_plots(g1, g2) +
  plot_layout(ncol = 2, widths = c(1, 2))

ggsave("figs/combined.pdf", width = 10, height = 8)