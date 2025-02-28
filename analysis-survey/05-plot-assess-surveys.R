library(dplyr)
library(ggplot2)
library(tidyr)

d <- readRDS("data-generated/assessments-surveys-joined.rds")
d <- mutate(d, gear_type = ifelse(!grepl("trawl", gear), "Longline", "Trawl"))
d <- mutate(d, facet_label = paste(panel_title1, panel_title2, sep = "\n"))

gg <- d %>%
  ggplot() +
  geom_ribbon(data = d |> drop_na(log_blrp),
    aes(year, ymin = q0.05_blrp / mean_blrp, ymax = q0.95_blrp / mean_blrp,
        group = stock_clean),
    fill = "black", alpha = 0.2
  ) +
  geom_line(data = d |> drop_na(log_blrp),
    aes(year, exp(log_blrp) / mean_blrp, group = stock_clean),
    linetype = 1, alpha = 0.4, colour = "black"
  ) +
  geom_line(aes(year, est / mean_est, colour = type, lty = gear_type)) +
  geom_ribbon(aes(year,
    ymin = lwr / mean_est, ymax = upr / mean_est,
    fill = type, lty = gear_type
  ), alpha = 0.3) +
  ylab("Relative biomass or abundance") +
  ggsidekick::theme_sleek() +
  theme(
    axis.text.y = element_blank(), axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linetype = 2),
    panel.spacing.x = unit(15, "points"),
    panel.spacing.y = unit(3, "points")
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1980, 2000, 2020)) +
  labs(colour = "Type", fill = "Type") +
  xlab("Year") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("Trawl" = 1, "Longline" = 2)) +
  facet_wrap(vars(forcats::fct_reorder(facet_label, -slope)), scales = "free_y", ncol = 5) +
  labs(fill = "Species group", colour = "Species group", linetype = "Survey gear") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.spacing.x = unit(5, "pt"), axis.title.x = element_blank()) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 1), linetype = guide_legend(order = 2))

ggsave("figs/assesses-indices-join.pdf", width = 9, height = 10)
ggsave("figs/assesses-indices-join.png", width = 9, height = 10)
