library(dplyr)
library(ggplot2)

ind <- readRDS("data-generated/survey-dat-minimal.rds")
dat <- readRDS("data-generated/assess-dat-minimal.rds")

lu <- readr::read_csv("data-raw/surveys_to_assessments.csv")

intersect(names(lu), names(ind))

test <- anti_join(ind, lu)
test
test <- anti_join(lu, ind)
test |> as.data.frame()
if (nrow(test) > 0L) stop("Mismatched lookup table and indices")

test2 <- anti_join(dat, lu)
if (nrow(test2) > 0L) stop("Mismatched lookup table and assessment data")

ind <- left_join(ind, lu)
dat <- left_join(dat, lu)

d <- full_join(ind, dat) |>
  mutate(stock_clean = paste(panel_title1, panel_title2)) |>
  arrange(stock_clean, year)

# This survey isn't usually used:
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

saveRDS(d, "data-generated/assessments-surveys-joined.rds")
