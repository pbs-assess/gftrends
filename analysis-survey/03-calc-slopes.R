# Steps:
# 1. calculate index slopes; if there are multiple indexes take the average slope
# 2. join the assessment and index data; required fields:
#    - assessment stock name
#    - index common stock name
#    - survey type (longline vs. trawl) - joined by survey name
#    - species group (flatfish etc.)
# (some of these could be separate lookup table .csvs that all join together)
# 3. center everything that overlaps by the overlapping geometric mean
# 4. plot

library(dplyr)
library(ggplot2)

all_indices <- readRDS(file.path("data-generated", "sopo-combined-indices.rds")) |>
  rename(gear = type)

se_check <- all_indices %>%
  group_by(species, gear, region, surveys, model) %>%
  summarise(avg_se = mean(se, na.rm = TRUE), .groups = "drop")

group_by(se_check, species, gear, region, surveys) %>%
  filter(avg_se == min(avg_se)) %>%
  pull(model) %>%
  table()

keep <- group_by(se_check, species, gear, region, surveys) %>%
  filter(avg_se == min(avg_se)) %>%
  ungroup()
nrow(keep)
# keep$model[keep$species == "Redstripe Rockfish" & keep$region == "WCHG only"] <- "Tweedie"

# nrow(all_indices)
# all_indices <- left_join(keep, all_indices, multiple = "all")  # should have half as many all_indices after binding to keep
# nrow(all_indices)

g <- all_indices |>
ggplot(data = _, aes(year, est, group = model)) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(
    ymin = lwr, ymax = upr, fill = model
  ), alpha = 0.4) +
  ylab("Relative Biomass (log scale)") +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(paste(species, region)), scales = "free_y", nrow = 10) +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank(),
        legend.position = 'bottom')
g

slopes <- all_indices %>%
  group_by(species, region) %>%
  group_split() %>%
  purrr::map_dfr(function(.x) {
    .x <- dplyr::filter(.x, year >= 2005)
    m <- lm(log(est) ~ year, data = .x)
    data.frame(slope = coef(m)[2], species = .x$species[1], region = .x$region[1])
  })

all_indices <- left_join(all_indices, slopes)

ind <- select(all_indices, species, gear, region, year, est, lwr, upr, slope, model)
# ind <- filter(ind, species != "Sablefish")
saveRDS(ind, file = "data-generated/survey-dat-minimal.rds")

dat <- readRDS("data-generated/b-status-dat.rds") |>
  filter(year >= 1960) |>
  rename(dfo_area = region) |>
  select(assess_species = species, assess_stock = stock, dfo_area, year, q0.05_blrp, q0.95_blrp, log_blrp)

saveRDS(dat, file = "data-generated/assess-dat-minimal.rds")
