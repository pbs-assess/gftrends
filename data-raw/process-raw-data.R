library(ggplot2)
library(dplyr)
library(tidyr)


# arrowtooth base model -----------------------------------------------------

base_model <- readRDS("data-raw/model-output/arrowtooth-2015.rds")
lrp <- base_model$mcmc$params.est %>%
  as_tibble() %>%
  select("bmsy") %>%
  mutate(iter = seq_len(n())) %>%
  mutate(lrp = 0.4 * bmsy)
arrowtooth <- base_model$mcmc$sbt[[1]] %>%
  pivot_longer(everything(), names_to = "year", values_to = "ssb") %>%
  mutate(iter = rep(seq_len(2000), each = length(unique(year)))) %>%
  arrange(year) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(lrp)
arrowtooth %>% ggplot(aes(year, ssb / lrp, group = iter)) + geom_line(alpha = 0.05)

arrowtooth_sum <- arrowtooth %>%
  mutate(blrp = ssb / lrp) %>%
  group_by(year) %>%
  summarise(species = "arrowtooth", region = "3CD",
    log_blrp = mean(log(blrp)), sd_log_blrp = sd(log(blrp)))

arrowtooth_sum %>% saveRDS("data-raw/arrowtooth-3cd.rds")

# pcod average model ------------------------------------------------------

d3cd <- readRDS("data-raw/model-output/pcod-avg-3cd.rds")
d5abcd <- readRDS("data-raw/model-output/pcod-avg-3cd.rds")

tidy_bratio_dat <- function(ratio) {
  ratio %>% reshape2::melt() %>% rename(year = variable) %>%
    group_by(year) %>%
    summarise(
      # b_med = median(value, 0.025),
      # bref_lwr = quantile(value, 0.025),
      # bref_upr = quantile(value, 0.975),
      # bref_med = quantile(value, 0.5),
      log_blrp = mean(log(value)),
      sd_log_blrp = sd(log(value)),
      .groups = "drop"
    ) %>%
    mutate(year = as.numeric(as.character(year)))
}

x <- d3cd[[1]]$mcmccalcs$sbt.dat
ratio <- x / x[,"1986"]

# dat <- catch.3 %>%
#   select(-total_catch) %>%
#   group_by(year) %>%
#   summarize(usa_catch = sum(usa_catch),
#     canada_catch = sum(canada_catch)) %>%
#   mutate(catch = usa_catch + canada_catch) %>%
#   select(year, catch)

pcod3cd <- tidy_bratio_dat(ratio)
head(pcod3cd)
pcod3cd <- mutate(pcod3cd, species = "pcod", region = "3CD") %>%
  select(species, region, everything())
head(pcod3cd)

x <- d5abcd[[1]]$mcmccalcs$sbt.dat
ratio <- x / x[,"2000"]
pcod5abcd <- tidy_bratio_dat(ratio)
head(pcod5abcd)
pcod5abcd <- mutate(pcod5abcd, species = "pcod", region = "5ABCD") %>%
  select(species, region, everything())
head(pcod5abcd)

pcod3cd %>% saveRDS("data-raw/pcod-3cd.rds")
pcod5abcd %>% saveRDS("data-raw/pcod-5abcd.rds")

  # left_join(dat) %>%
  # mutate(across(-1, round, digits = 4L))
