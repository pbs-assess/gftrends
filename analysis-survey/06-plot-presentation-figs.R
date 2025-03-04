library(dplyr)
library(ggplot2)
library(sf)

# source('analysis/04-ridges.R')
# source("analysis-survey/05-plot-assess-surveys.R")
pres_dir <- file.path("figs", "SOPO_presentation", format(Sys.Date(), "%Y"))
dir.create(pres_dir, showWarnings = FALSE)

# Presentation text
refpt <- readRDS(here::here("data-generated/p-thresh.rds"))
filter(refpt, stock_clean == "Arrowtooth Flounder BC")

n_assessed <- length(unique(refpt$stock_clean))
n_assessed
n_below_usr <- sum(refpt$p_usr >= 0.25)
n_below_usr
# 1/5 assessed stocks had a > 25% chance of being in “Cautious Zone”.
n_above_usr <- sum(1 - refpt$p_usr >= 0.75)
n_above_usr
# 3/4 of assessed stocks had a high (>75%) probability of being in “healthy zone”.

# Time series of assessed trends
# ------------------------------------------------------------------------------
ts_plot <- readRDS("data-generated/ts-summary-plot.rds") # can then be loaded in <06-plot-presentation-figs.R>

ts_plot +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  # gfplot::theme_pbs(base_size = 14)
ggsave(file.path(pres_dir, "ts-summary.png"), width = 7, height = 5.4)

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

# Reference point ridges
# ------------------------------------------------------------------------------
max_stock_nchar <- max(nchar(as.character(data_plot$stock_clean))) + 3 + 4

ridge_dat_formatted <- data_plot |>
  mutate(stock_clean = paste0(stock_clean, "   ", year)) |>
  mutate(stock_clean = forcats::fct_reorder(stock_clean, year))

stock_thisyear <- c(
  "Petrale Sole BC",
  "Bocaccio BC",
  "Pacific Spiny Dogfish BC \\(Outside\\)",
  "Arrowtooth Flounder BC"
) |> paste(collapse = '|')


g <- ggplot(data = ridge_dat_formatted, aes(x = ratio_value, y = stock_clean, fill = mean_blrp, group = stock_clean)) +
  geom_vline(
    data = lines, mapping = aes(xintercept = ratio_value),
    lty = 2, lwd = 0.45, colour = "grey55"
  ) +
  facet_wrap(vars(ratio), labeller = label_parsed, scales = "free_x") +
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

g # blank background plot
ggsave(file.path(pres_dir, "status-as-of-last-assessment-blank.png"), width = 8.3, height = 7)

# all ridges
g + geom_density_ridges2(data = ridge_dat_formatted, scale = 3.5, alpha = 0.1, colour = "grey90") +
  geom_density_ridges2(data = ridge_dat_formatted |> filter(stringr::str_detect(stock_clean, stock_thisyear)),
    alpha = 0.7, colour = "grey30",
    scale = 1.9) # This scale parameter is fussy to get right, not sure if there is a more systematic way to do this
ggsave(file.path(pres_dir, "status-as-of-last-assessment-this-year.png"), width = 8.3, height = 7)

# latest updated assessment ridges
g + geom_density_ridges2(data = ridge_dat_formatted, scale = 3.5, alpha = 0.8, colour = "grey30") +
  geom_density_ridges2(data = ridge_dat_formatted |> filter(stringr::str_detect(stock_clean, stock_thisyear)),
    alpha = 0, colour = NA,
    scale = 1.9) # This scale parame
ggsave(file.path(pres_dir, "status-as-of-last-assessment-all.png"), width = 8.3, height = 7)



# All species survey trends
# ------------------------------------------------------------------------------
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

# Species group assessment and survey trends
# ------------------------------------------------------------------------------
# Set colour for source column
# cols <- RColorBrewer::brewer.pal(4, name = "Dark2")
cols <- c(
  "Assessment" = "#00000050",
  "HBLL (inside)" = "#1B9E77",
  "HBLL (outside)" = "#D95F02",
  "Synoptic trawl" = "#7570B3"
)

d <- readRDS("data-generated/assessments-surveys-joined.rds")
d <- mutate(d, gear_type = ifelse(!grepl("trawl", gear), "Longline", "Trawl"))
# d <- mutate(d, facet_label = paste(panel_title1, panel_title2, sep = "\n"))
d <- mutate(d, source = ifelse(is.na(est), "Assessment", gear)) %>%
     mutate(source = factor(source, levels = c("Assessment", "HBLL (inside)", "HBLL (outside)", "Synoptic trawl"))) |>
     mutate(facet_label = paste0(panel_title1, " - ", panel_title2))

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
    legend.margin = margin(0, 0, 0.1, 0),
    legend.direction = "horizontal"
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020)) +
  scale_colour_manual(values = cols, drop = TRUE) +
  scale_fill_manual(values = cols, drop = TRUE) +
  labs(colour = "Type", fill = "Type") +
  # ylab("Relative biomass or abundance estimate") +
  ylab("Relative biomass") +
  xlab("Year") +
  facet_wrap(vars(facet_label), scales = "free_y", ncol = ncol)
}

# Roundfish trends
# ----------------
# dev.new(width = 7.5, height = 6)
g <- d %>%
  filter(type == "Roundfish") %>%
  filter(species != "Sablefish") %>%
  # mutate(species_order = factor(species, levels = c('Pacific Cod', 'Walleye Pollock', 'Lingcod'))) %>%
  # mutate(facet_label = forcats::fct_reorder(facet_label, as.numeric(species_order))) %>%
  mutate(facet_label = forcats::fct_reorder(facet_label, as.numeric(-slope))) %>%
  plot_pres_trends(ncol = 3, base_size = 16) +
  theme(panel.spacing.x = unit(25, "points"),
    legend.position = "top"
    )
g
ggsave(file.path(pres_dir, "roundfish_assesses-indices-join.png"), width = 9, height = 6.3)

g <- d %>%
  filter(type == "Inshore rockfish") %>%
  mutate(facet_label = forcats::fct_reorder(facet_label, as.numeric(-slope))) %>%
  plot_pres_trends(ncol = 2, base_size = 16) +
  theme(strip.clip = "off",
    legend.position = "top"
  )
g
ggsave(file.path(pres_dir, "inshore-rockfish_assesses-indices-join.png"), width = 8, height = 6.3)

g <- d %>%
  filter(type == "Shelf rockfish") |>
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>%
  plot_pres_trends(ncol = 3, base_size = 16) +
  theme(legend.position = "inside",
    legend.position.inside = c(0.7, 0.1))
g
ggsave(file.path(pres_dir, "shelf-rockfish_assesses-indices-join.png"), width = 9.2, height = 6.3)

g <- d %>%
  filter(type == "Slope rockfish") %>%
  mutate(facet_label = gsub("- ", "", facet_label)) |>
  mutate(facet_label = gsub("Blacksp.", "Blackspotted", facet_label)) |>
  mutate(facet_label = gsub("\\bBC", "- BC", facet_label, perl = TRUE)) |>
  mutate(facet_label = gsub("5", "- 5", facet_label, perl = TRUE)) |>
  mutate(facet_label = gsub("3", "- 3", facet_label, perl = TRUE)) |>
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>%
  plot_pres_trends(ncol = 3, base_size = 16) +
  theme(legend.position = "inside",
    legend.position.inside = c(0.85, 0.1))
g
ggsave(file.path(pres_dir, "slope-rockfish_assesses-indices-join.png"), width = 9.2, height = 6)

g <- d %>%
  filter(type == "Flatfish") %>%
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>%
  plot_pres_trends(ncol = 3, base_size = 16) +
  theme(legend.position = "inside",
    legend.position.inside = c(0.7, 0.1))
g
ggsave(file.path(pres_dir, "flatfish_assesses-indices-join.png"), width = 9.2, height = 6)

g <- d %>%
  filter(type == "Sharks, skates, chimeras") %>%
  mutate(facet_label = forcats::fct_reorder(facet_label, -slope)) %>%
  plot_pres_trends(ncol = 3, base_size = 16) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1960, 1980, 2000, 2020), limits = c(1960, 2023)) +
  theme(legend.position = "inside",
    legend.position.inside = c(0.7, 0.1),
    strip.clip = "off")
g
ggsave(file.path(pres_dir, "sharks-skates-chimeras_assesses-indices-join.png"), width = 9, height = 6)



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

# For 2024 data I wanted to make it clear it was a dogfish gear comparison, not dogfish survey
survey_labels <- readr::read_csv("data-raw/survey-set-names.csv") |>
  select(survey_series_id, map_label) |>
  distinct() |>
  mutate(map_label = ifelse(map_label == "Dogfish", "Dogfish gear comparison", map_label)) |>
  mutate(map_label = ifelse(map_label == "Multispecies Small-mesh Bottom Trawl", "Small-mesh Multispecies Bottom Trawl", map_label))

survey_list <- c(
  "Dogfish gear comparison",
  "Hard Bottom Longline Hook",
  #"IPHC FISS", # Omit for now because the data are not found in GFBio every year
  "Small-mesh Multispecies Bottom Trawl",
  "Pacific Hake Hydroacoustic",
  "Sablefish Research and Assessment",
  "SOGERI Acoustic",
  "Synoptic Bottom Trawl"
)

shapes <- c(
  "Dogfish gear comparison" = 8,
  "Hard Bottom Longline Hook" = 17,
  "Small-mesh Multispecies Bottom Trawl" = 15,
  "Pacific Hake Hydroacoustic" = 19,
  "Sablefish Research and Assessment" = 7,
  "Synoptic Bottom Trawl" = 1,
  "SOGERI Acoustic" = 10
)

survey_cols <- c(
  "Synoptic Bottom Trawl" = "#7570B3",
  "Hard Bottom Longline Hook" = "#E69F00",
  "Sablefish Research and Assessment" = "#0072B2",
  "Small-mesh Multispecies Bottom Trawl" = "#CC79A7",
  "SOGERI Acoustic" = "#000000",
  # "#D55E00",
  "Dogfish gear comparison" = "#009E73"
  # "#999999"
)

bc_coast <- pacea::bc_coast
bc_coast_bbox <- st_bbox(bc_coast)

surveys <- readRDS("data-raw/2024-survey-locations.rds") |>
  tidyr::drop_na(survey_series_id, latitude) |>
  left_join(survey_labels) |>
  filter(map_label %in% survey_list) |>
  mutate(year = lubridate::year(fe_begin_retrieval_time)) |>
  as_tibble()

survey_sf <- surveys |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_crop(bc_coast_bbox)

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
  scale_colour_manual(values = survey_cols, drop = TRUE) +
  scale_shape_manual(values = shapes, drop = TRUE) +
  scale_x_continuous(breaks = c(-134, -130, -126, -122)) +
  scale_y_continuous(breaks = c(48, 50, 52, 54))

ggsave(file.path(pres_dir, "survey-set-locations.png"), width = 9, height = 7)
