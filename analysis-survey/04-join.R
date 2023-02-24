library(dplyr)
library(ggplot2)

ind <- readRDS("data-generated/survey-dat-minimal.rds")
dat <- readRDS("data-generated/assess-dat-minimal.rds")

lu <- readr::read_csv("data-raw/surveys_to_assessments.csv")
ind <- left_join(ind, lu)
dat <- left_join(dat, lu)

d <- full_join(ind, dat) |>
  mutate(stock_clean = paste(panel_title1, panel_title2)) |>
  arrange(stock_clean, year)

# filter(d, assess_species == "arrowtooth") |> View()

d$est[d$species == "Sablefish"] <- NA
d$lwr[d$species == "Sablefish"] <- NA
d$upr[d$species == "Sablefish"] <- NA

# find the geometric mean of the overlapping years for both the assessment and survey time series
d <- group_by(d, stock_clean) %>%
  group_split() %>%
  purrr::map_dfr(function(.x) {
    if (sum(!is.na(.x$est)) == 0L) {
      return(mutate(.x, min_geo_mean = NA, max_geo_mean = NA, mean_blrp = 1, mean_est = 1))
    } else {
      .x <- mutate(.x, min_surv_year = min(year[!is.na(est)], na.rm = TRUE))
      .x <- mutate(.x, max_surv_year = max(year[!is.na(est)], na.rm = TRUE))
      .x <- mutate(.x, min_assess_year = min(year[!is.na(log_blrp)], na.rm = TRUE))
      .x <- mutate(.x, max_assess_year = max(year[!is.na(log_blrp)], na.rm = TRUE))
      .x <- mutate(.x, min_geo_mean = max(min_surv_year, min_assess_year, na.rm = TRUE))
      .x <- mutate(.x, max_geo_mean = min(max_surv_year, max_assess_year, na.rm = TRUE))
      .x <- mutate(.x, mean_blrp = exp(mean(log_blrp[year >= min_geo_mean & year <= max_geo_mean], na.rm = TRUE)))
      # could be multiple survey gears:
      if (sum(!is.na(.x$log_blrp)) == 0L) { # no assessment
        .x$min_geo_mean <- min(.x$year, na.rm = TRUE)
        .x$max_geo_mean <- max(.x$year, na.rm = TRUE)
      }
      .x <- group_by(.x, gear) %>%
        mutate(mean_est = exp(mean(log(est[year >= min_geo_mean & year <= max_geo_mean]), na.rm = TRUE))) %>%
        ungroup()
      .x <- select(.x, -min_surv_year, -max_surv_year, -min_assess_year, -max_assess_year)
      .x
    }
  })

d <- mutate(d, gear_type = ifelse(grepl("trawl", gear), "Longline", "Trawl"))

d <- mutate(d, facet_label = paste(panel_title1, panel_title2, sep = "\n"))

gg <- d %>%
    ggplot() +
    geom_ribbon(aes(year,
      ymin = q0.05_blrp / mean_blrp, ymax = q0.95_blrp / mean_blrp,
      group = stock_clean
    ), fill = "black", alpha = 0.2) +
    geom_line(aes(year, exp(log_blrp) / mean_blrp, group = stock_clean),
      linetype = 1, alpha = 0.4, colour = "black"
    ) +
    geom_line(aes(year, est / mean_est, colour = type, lty = gear_type)) +
    geom_ribbon(aes(year,
      ymin = lwr / mean_est, ymax = upr / mean_est,
      fill = type, lty = gear_type
    ), alpha = 0.3) +
    ylab("Relative biomass or abundance estimate") +
    # scale_colour_manual(values = cols) +
    # scale_fill_manual(values = cols) +
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
    scale_x_continuous(expand = c(0, 0)) +
    labs(colour = "Type", fill = "Type")

  # if (arrange_by == "slope") {
    # gg <- gg +
      # facet_wrap(vars(forcats::fct_reorder(stock_clean, -slope)),
      # facet_wrap(vars(facet_label), ncol = 5L, scales = "free_y")
  # } else {
  #   gg <- gg + facet_wrap(vars(forcats::fct_inorder(stock_clean)),
  #     ncol = ncol, scales = "free_y")
  # }
  # geom_vline(aes(xintercept = min_geo_mean), lty = 1) + # testing
  # geom_vline(aes(xintercept = max_geo_mean), lty = 1) # testing

  gg <- gg + xlab("Year") +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    scale_linetype_manual(values = c("Trawl" = 1, "Longline" = 2)) +
    facet_wrap(vars(forcats::fct_reorder(facet_label, -slope)), scales = "free_y", ncol = 5) +
    labs(fill = "Species\ngroup", colour = "Species\ngroup", linetype = "Gear")+
    ggsidekick::theme_sleek() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  print(gg)
