library(dplyr)

# From within pcod repo:

source(file.path(here::here(), "R", "all.R"))
build(ovwrt.base = FALSE,
  ovwrt.sens = FALSE,
  ovwrt.retro = FALSE,
  burnin = 1000,
  thin = 1)
load.models.into.parent.env()
source(file.path(rootd.R, "custom-knitr-variables.R"))

tidy_bratio_dat <- function(ratio, catch) {
  ratio %>% reshape2::melt() %>% rename(year = variable) %>%
    group_by(year) %>%
    summarise(
      b_med = median(value, 0.025),
      bref_lwr = quantile(value, 0.025),
      bref_upr = quantile(value, 0.975),
      bref_med = quantile(value, 0.5),
      bref_log_mean = mean(log(value)),
      bref_log_sd = sd(log(value)),
      .groups = "drop"
    ) %>%
    mutate(year = as.numeric(as.character(year)))
}

x <- avg.model.5abcd[[1]]$mcmccalcs$sbt.dat
ratio <- x / x[,"2000"]

dat <- catch.5 %>%
  select(-total_catch) %>%
  group_by(year) %>%
  summarize(usa_catch = sum(usa_catch),
    canada_catch = sum(canada_catch)) %>%
  mutate(catch = usa_catch + canada_catch) %>%
  select(year, catch) %>%
  mutate(across(-1, round, digits = 4L))

out <- tidy_bratio_dat(x) %>%
  left_join(dat)

readr::write_csv(out, "../gftrends/data-raw/PCOD-5ABCD-2018/ts.csv")

