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

# inside yelloweye --------------------------------------------------------

d <- readRDS("data-raw/model-output/ye-inside-b-lrp.rds")
group_by(d, year) %>%
  summarise(species = "yelloweye", region = "4B", log_blrp = mean(log(b_lrp)), sd_log_blrp = sd(log(b_lrp))) %>%
  select(species, region, everything()) %>%
  saveRDS("data-raw/yelloweye-4b.rds")

# POP 5ABC 2017 -----------------------------------------------------------

format_rowan_raw_data <- function(sheet1, sheet2) {
  b <- tidyr::pivot_longer(d1, cols = -1, names_to = "year", values_to = "B")
  d <- dplyr::left_join(b, d2) %>%
    mutate(year = as.character(year))
  # ggplot(d, aes(year, B / Bmsy, group = sample)) + geom_line()
  mutate(d, species = "pacific-ocean-perch", region = "5ABC") %>%
    select(species, region, everything()) %>%
    group_by(year) %>%
    summarise(
      species = species[1],
      region = region[1],
      log_blrp = mean(log(B/(Bmsy*0.4))),
      sd_log_blrp = sd(log(B/(Bmsy*0.4))),
      .groups = "drop"
    ) %>%
    mutate(year = as.integer(as.character(year)))
}

d1 <- readxl::read_xls("data-raw/model-output/POP.5ABC.2017.MCMC.forSean.xls", sheet = 1)
d2 <- readxl::read_xls("data-raw/model-output/POP.5ABC.2017.MCMC.forSean.xls", sheet = 2)
d <- format_rowan_raw_data(d1, d2)
d %>% saveRDS("data-raw/pop-5abcd.rds")

# Bocaccio ------------------------------------------------------------------

# multiple 'Runs':
format_rowan_raw_data2 <- function(sheet1, sheet2, .species, .region) {
  b <- tidyr::pivot_longer(d1, cols = -c(1, 2), names_to = "year", values_to = "B")
  d <- dplyr::left_join(b, d2) %>%
    mutate(year = as.character(year))
  # ggplot(d, aes(year, B / Bmsy, group = sample)) + geom_line()
  mutate(d, species = .species, region = .region) %>%
    select(species, region, everything()) %>%
    group_by(year) %>%
    summarise(
      species = species[1],
      region = region[1],
      log_blrp = mean(log(B/(Bmsy*0.4))),
      sd_log_blrp = sd(log(B/(Bmsy*0.4))),
      .groups = "drop"
    ) %>%
    mutate(year = as.integer(as.character(year)))
}

d1 <- readxl::read_xlsx("data-raw/model-output/BOR.CST.2019.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/BOR.CST.2019.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "bocaccio", "5ABCD")
d %>% saveRDS("data-raw/bocaccio-5abcd.rds")

# walleye -----------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/WWR.CST.2019.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/WWR.CST.2019.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "walleye-pollock", "BC")
d %>% saveRDS("data-raw/walleye-bc.rds")

# REBS --------------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/REBS.BCN.2020.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/REBS.BCN.2020.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "rougheye/blackspotted", "BC North")
d %>% saveRDS("data-raw/rebs-bc-north.rds")

d1 <- readxl::read_xlsx("data-raw/model-output/REBS.BCS.2020.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/REBS.BCS.2020.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "rougheye/blackspotted", "BC South")
d %>% saveRDS("data-raw/rebs-bc-south.rds")

# sable -------------------------------------------------------------------

d <- readRDS("data-raw/model-output/sable.rds")
d <- mutate(d, region = "BC") %>% as_tibble()
d %>% saveRDS("data-raw/sable-bc.rds")
