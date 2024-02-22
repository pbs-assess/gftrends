library(dplyr)
library(ggplot2)

source('analysis/04-ridges.R')
pres_dir <- file.path("figs", "SOPO_presentation", format(Sys.Date(), "%Y"))
dir.create(pres_dir, showWarnings = FALSE)

# Set colour for source column
cols <- RColorBrewer::brewer.pal(3, name = "Dark2")
cols <- c("#00000050", cols)

d <- readRDS("data-generated/assessments-surveys-joined.rds")
d <- mutate(d, gear_type = ifelse(!grepl("trawl", gear), "Longline", "Trawl"))
d <- mutate(d, facet_label = paste(panel_title1, panel_title2, sep = "\n"))
d <- mutate(d, source = ifelse(is.na(est), "Assessement", gear)) %>% 
     mutate(source = factor(source, levels = c("Assessement", "HBLL (inside)", "HBLL (outside)", "Synoptic trawl")))

plot_pres_trends <- function(data, ncol, base_size = 16) {
  ggplot(data = data) +
  geom_ribbon(aes(year, ymin = q0.05_blrp / mean_blrp, ymax = q0.95_blrp / mean_blrp), 
    fill = "black", alpha = 0.2
  ) +
  geom_line(aes(year, exp(log_blrp) / mean_blrp), 
    linetype = 1, alpha = 0.4, linewidth = 1.1
  ) +
  geom_line(aes(year, est / mean_est, colour = source), , linewidth = 1.1) +
  geom_ribbon(aes(year, ymin = lwr / mean_est, ymax = upr / mean_est, fill = source), 
    alpha = 0.3
  ) +
  ggsidekick::theme_sleek(base_size = base_size) +
  theme(
    axis.text.y = element_blank(), axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linetype = 2),
    panel.spacing.x = unit(30, "points"),
    panel.spacing.y = unit(5, "points"), 
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020)) +
  scale_colour_manual(values = cols, drop = FALSE) + 
  scale_fill_manual(values = cols, drop = FALSE) + 
  labs(colour = "Type", fill = "Type") +
  ylab("Relative biomass or abundance estimate") +
  xlab("Year") +
  facet_wrap(vars(facet_label), scales = "free_y", ncol = ncol)
}

# Roundfish trends
# ----------------
# dev.new(width = 7.5, height = 6)
g <- d %>%
  filter(type == "Roundfish") %>% 
  filter(species != "Sablefish") %>% 
  mutate(species_order = factor(species, levels = c('Pacific Cod', 'Walleye Pollock', 'Lingcod'))) %>%
  mutate(facet_label = forcats::fct_reorder(facet_label, as.numeric(species_order))) %>%
  plot_pres_trends(ncol = 2, base_size = 16)
ggsave(file.path(pres_dir, "roundfish_assesses-indices-join.png"), width = 7.5, height = 6)

g <- d %>%
  filter(type == "Inshore rockfish") %>% 
  plot_pres_trends(ncol = 2, base_size = 16)
ggsave(file.path(pres_dir, "inshore-rockfish_assesses-indices-join.png"), width = 7.5, height = 5)

g <- d %>%
  filter(type == "Shelf rockfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 2, base_size = 16)
ggsave(file.path(pres_dir, "shelf-rockfish_assesses-indices-join.png"), width = 7.5, height = 6)


g <- d %>%
  filter(type == "Slope rockfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 16)
ggsave(file.path(pres_dir, "slope-rockfish_assesses-indices-join.png"), width = 9, height = 6)

g <- d %>%
  filter(type == "Flatfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 16)
ggsave(file.path(pres_dir, "flatfish_assesses-indices-join.png"), width = 9, height = 6)

g <- d %>%
  filter(type == "Sharks, skates, chimeras") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 16)
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020), limits = c(1960, 2022))
ggsave(file.path(pres_dir, "sharks-skates-chimeras_assesses-indices-join.png"), width = 9, height = 6)




g <- data_plot %>%
  mutate(stock_clean = paste0(stock_clean, "   ", year)) |>
  mutate(stock_clean = forcats::fct_reorder(stock_clean, year)) |>
  ggplot(aes(x = ratio_value, y = stock_clean, fill = mean_blrp, group = stock_clean)) +
  geom_vline(
    data = lines, mapping = aes(xintercept = ratio_value),
    lty = 2, lwd = 0.45, colour = "grey55"
  ) +
  facet_wrap(vars(ratio), labeller = label_parsed, scales = "free_x") +
  geom_density_ridges2(scale = 4, alpha = 0.7, size = 0.4, colour = "grey30") +
  scale_x_continuous(trans = "sqrt", breaks = c(0.2, 1, 2, 5, 10), labels = scales_fun) +
  scale_fill_viridis_c(direction = -1, option = "B", end = 0.71) +
  theme_sleek() +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Ratio value", fill = "Mean\nB/LRP") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.text.y.left = element_text(vjust = -1),
    strip.text = element_text(size = 14), 
    panel.spacing.x = unit(15, "points"),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(colour = "grey70", linewidth = 0.5)
  ) +
  guides(fill = "none")  
ggsave(file.path(pres_dir, "status-as-of-last-assessment.png"), width = 8.3, height = 7)