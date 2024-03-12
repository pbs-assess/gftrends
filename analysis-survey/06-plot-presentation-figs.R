library(dplyr)
library(ggplot2)
library(sf)

source('analysis/04-ridges.R')
pres_dir <- file.path("figs", "SOPO_presentation", format(Sys.Date(), "%Y"))
dir.create(pres_dir, showWarnings = FALSE)

# Plot wide format of gg
source("analysis-survey/05-plot-assess-surveys.R")

gg + 
  ggsidekick::theme_sleek(base_size = 13) +
  theme(
    axis.text.y = element_blank(), axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing.x = unit(2, "points"),
    panel.spacing.y = unit(1, "points")
  ) +
  facet_wrap(vars(forcats::fct_reorder(facet_label, -slope)), scales = "free_y", ncol = 8) +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.text = element_text(size = 11), 
        legend.margin = margin(t = -1, b = -5)) +
  guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

ggsave(file.path(pres_dir, "assesses-indices-join.png"), width = 14, height = 10)

# Set colour for source column
cols <- RColorBrewer::brewer.pal(4, name = "Dark2")
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
ggsave(file.path(pres_dir, "roundfish_assesses-indices-join.png"), width = 8, height = 6.3)

g <- d %>%
  filter(type == "Inshore rockfish") %>% 
  plot_pres_trends(ncol = 2, base_size = 16)
ggsave(file.path(pres_dir, "inshore-rockfish_assesses-indices-join.png"), width = 8, height = 5.8)

g <- d %>%
  filter(type == "Shelf rockfish") %>% 
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>% 
  plot_pres_trends(ncol = 3, base_size = 16)
ggsave(file.path(pres_dir, "shelf-rockfish_assesses-indices-join.png"), width = 9, height = 6)


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
  plot_pres_trends(ncol = 3, base_size = 16) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020), limits = c(1960, 2023))
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
g
ggsave(file.path(pres_dir, "status-as-of-last-assessment.png"), width = 8.3, height = 7)


max_stock_nchar <- max(nchar(as.character(data_plot$stock_clean))) + 3 + 4

ridge_dat_formatted <- data_plot |> 
  mutate(stock_clean = paste0(stock_clean, "   ", year)) |>
  mutate(stock_clean = forcats::fct_reorder(stock_clean, year))

stock_thisyear <- c(
    "Quillback Rockfish BC \\(Outside\\)", 
    "Pacific Cod 3CD", 
    "Pacific Ocean Perch 3CD", 
    "Pacific Ocean Perch 5ABC", 
    "Pacific Ocean Perch 5DE"
) |> paste(collapse = '|')


g <- ggplot(data = ridge_dat_formatted, aes(x = ratio_value, y = stock_clean, fill = mean_blrp, group = stock_clean)) +
  geom_vline(
    data = lines, mapping = aes(xintercept = ratio_value),
    lty = 2, lwd = 0.45, colour = "grey55"
  ) +
  facet_wrap(vars(ratio), labeller = label_parsed, scales = "free_x") +
  geom_density_ridges2(data = ridge_dat_formatted, scale = 4, alpha = 0.1, size = 0.4, colour = "grey90") +
  geom_density_ridges2(data = ridge_dat_formatted |> filter(str_detect(stock_clean, stock_thisyear)), 
    alpha = 0.7, size = 0.4, colour = "grey30", 
    scale = 1.45) + # This scale parameter is fussy to get right, not sure if there is a more systematic way to do this
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
g
ggsave(file.path(pres_dir, "status-as-of-last-assessment-this-year.png"), width = 8.3, height = 7)

# Plot survey set locations
# ------------------------------------------------------------------------------
# 'data-raw/survey-set-names.csv' was created by getting the unique surveys sampled 
# in 2022 and 2023, then manually adding the map_labels.
# bind_rows(
#   readRDS('data-raw/2022-survey-locations.rds'), 
#   readRDS('data-raw/2023-survey-locations.rds')
#   ) |> 
#   distinct(surveys, survey_series_id, survey_desc, survey_series_name, survey_series_desc) |>
#   readr::write_csv('data-raw/survey-set-names.csv')

survey_labels <- readr::read_csv("data-raw/survey-set-names.csv") |>
  select(survey_series_id, map_label) |>
  distinct()

survey_list <- c(
  "Dogfish",
  "Hard Bottom Longline Hook",
  #"IPHC FISS", # Omit for now because the data are not found in GFBio every year
  "Multispecies Small-mesh Bottom Trawl",
  "Pacific Hake Hydroacoustic",
  "Sablefish Research and Assessment",
  "SOGERI Acoustic",
  "Synoptic Bottom Trawl"
)

shapes <- c(
  "Dogfish" = 8,
  "Hard Bottom Longline Hook" = 17,
  "Multispecies Small-mesh Bottom Trawl" = 15,
  "Pacific Hake Hydroacoustic" = 19,
  "Sablefish Research and Assessment" = 7,
  "Synoptic Bottom Trawl" = 1, 
  "SOGERI Acoustic" = 10
)

surveys <- readRDS("data-raw/2023-survey-locations.rds") |> 
  tidyr::drop_na(survey_series_id, latitude) |> 
  left_join(survey_labels) |>
  filter(map_label %in% survey_list)

survey_sf <- surveys |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

bbox <- st_bbox(survey_sf)

coast <- rnaturalearth::ne_states(c("canada", "united states of america"), returnclass = "sf")

ggplot() + 
  geom_sf(data = survey_sf, aes(colour = map_label, shape = map_label)) + 
  geom_sf(data = coast) +
  coord_sf(xlim = c(-134.5, -122.5), ylim = c(48, 54.5)) +
  gfplot::theme_pbs(base_size = 16) +
  theme(legend.position = c(0.25, 0.19), 
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14)) + 
  guides(color = guide_legend(title = "Groundfish Survey Set Locations", override.aes = list(size = 3, stroke = 1)), 
         shape = guide_legend(title = "Groundfish Survey Set Locations")) +
  scale_colour_manual(values = as.vector(palette.colors(palette = "Okabe-Ito"))[-c(3, 5)]) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(breaks = c(-134, -130, -126, -122)) +
  scale_y_continuous(breaks = c(48, 50, 52, 54))

ggsave(file.path(pres_dir, "survey-set-locations.png"), width = 9, height = 7)
