library(dplyr)
library(ggplot2)

dir.create("figs/SOPO_presentation", showWarnings = FALSE)

# Set colour for source column
cols <- RColorBrewer::brewer.pal(3, name = "Dark2")
cols <- c("#00000050", cols)

d <- readRDS("data-generated/assessments-surveys-joined.rds")
d <- mutate(d, gear_type = ifelse(!grepl("trawl", gear), "Longline", "Trawl"))
d <- mutate(d, facet_label = paste(panel_title1, panel_title2, sep = "\n"))
d <- mutate(d, source = ifelse(is.na(est), "Assessement", gear)) %>% 
     mutate(source = factor(source, levels = c("Assessement", "HBLL (inside)", "HBLL (outside)", "Synoptic trawl")))

d %>% 
filter(type == "Roundfish") %>%
distinct(stock_clean, gear, year, est, log_blrp, mean_blrp, facet_label, mean_est) %>% view

plot_pres_trends <- function(data, ncol, base_size = 11) {
  ggplot(data = data) +
  geom_ribbon(aes(year, ymin = q0.05_blrp / mean_blrp, ymax = q0.95_blrp / mean_blrp), 
    fill = "black", alpha = 0.2
  ) +
  geom_line(aes(year, exp(log_blrp) / mean_blrp), 
    linetype = 1, alpha = 0.4
  ) +
  geom_line(aes(year, est / mean_est, colour = source)) +
  geom_ribbon(aes(year, ymin = lwr / mean_est, ymax = upr / mean_est, fill = source), 
    alpha = 0.3
  ) +
  ggsidekick::theme_sleek(base_size = base_size) +
  theme(
    axis.text.y = element_blank(), axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linetype = 2),
    panel.spacing.x = unit(25, "points"),
    panel.spacing.y = unit(5, "points"), 
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020)) +
  scale_colour_manual(values = cols, drop = FALSE) + 
  scale_fill_manual(values = cols, drop = FALSE) + 
  labs(colour = "Type", fill = "Type") +
  ylab("Relative biomass or abundance") +
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
  plot_pres_trends(ncol = 2, base_size = 14)
ggsave("figs/SOPO_presentation/roundfish_assesses-indices-join.png", width = 7.5, height = 6)


g <- d %>%
  filter(type == "Inshore rockfish") %>% 
  plot_pres_trends(ncol = 2, base_size = 14)
ggsave("figs/SOPO_presentation/inshore-rockfish_assesses-indices-join.png", width = 7.5, height = 5)

g <- d %>%
  filter(type == "Shelf rockfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 2, base_size = 14)
ggsave("figs/SOPO_presentation/shelf-rockfish_assesses-indices-join.png", width = 7.5, height = 6)


g <- d %>%
  filter(type == "Slope rockfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 14)
ggsave("figs/SOPO_presentation/slope-rockfish_assesses-indices-join.png", width = 9, height = 6)

g <- d %>%
  filter(type == "Flatfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 14)
ggsave("figs/SOPO_presentation/flatfish_assesses-indices-join.png", width = 9, height = 6)

g <- d %>%
  filter(type == "Sharks, skates, chimeras") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 14) + 
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020), limits = c(1960, 2022))
ggsave("figs/SOPO_presentation/sharks-skates-chimeras_assesses-indices-join.png", width = 9, height = 6)