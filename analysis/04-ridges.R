library(ggplot2)
library(dplyr)
library(ggsidekick)
library(ggridges)
source("analysis/stock_df.R")

d <- readRDS("data-generated/all-mcmc.rds")

dat <- d %>%
  group_by(species, region) %>%
  filter(year <= 2020) %>%
  filter(year == max(year)) %>%
  # sample_n(2000L, replace = TRUE) %>%
  mutate(
    mean_blrp = mean(blrp, na.rm = TRUE),
    mean_bbmsy = mean(bbmsy, na.rm = TRUE),
    mean_busr = mean(busr, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(stock = paste(species, region, sep = "_")) %>%
  arrange(year, stock) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock)) %>%
  left_join(stock_df)

dat_sum <- group_by(dat, stock_clean) %>%
  summarize(p_lrp = mean(blrp < 1), p_usr = mean(busr < 1),
    p_bbmsy = mean(bbmsy < 1, na.rm = TRUE)) %>%
  arrange(-p_lrp)

saveRDS(dat_sum, "data-generated/p-thresh.rds")

lines <- tibble(
  ratio = c("B/B[MSY]", "B/B[MSY]", "B/B[MSY]", "B/LRP", "B/USR"),
  ratio_value = c(0.4, 0.8, 1, 1, 1)
) %>% mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]"))))

data_plot <- dat %>%
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
  mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]"))))

g <- data_plot %>%
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
  guides(fill = FALSE)

ggsave("figs/ridges.pdf", width = 5.8, height = 5.8)
ggsave("figs/ridges.png", width = 5.8, height = 5.8)
