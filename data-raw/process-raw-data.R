library(ggplot2)
library(dplyr)
library(tidyr)

# pcod average model ------------------------------------------------------

# from pcod project:
# saveRDS(avg.model.5abcd, file = "../gftrends/data-raw/model-output/pcod-avg-5abcd-2021.rds")
# saveRDS(avg.model.3cd, file = "../gftrends/data-raw/model-output/pcod-avg-3cd-2021.rds")

d3cd <- readRDS("data-raw/model-output/pcod-avg-3cd-2021.rds")
d5abcd <- readRDS("data-raw/model-output/pcod-avg-5abcd-2021.rds")

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

# remove projection:
pcod3cd <- dplyr::filter(pcod3cd, year <= 2020)
pcod5abcd <- dplyr::filter(pcod5abcd, year <= 2020)

pcod3cd %>% saveRDS("data-raw/pcod-3cd.rds")
pcod5abcd %>% saveRDS("data-raw/pcod-5abcd.rds")

# MCMC:
x <- d3cd[[1]]$mcmccalcs$sbt.dat
x <- as.data.frame(x)
x$iter <- seq_len(nrow(x))
x <- tidyr::pivot_longer(x, cols = -iter, names_to = "year", values_to = "b") %>%
  mutate(year = as.integer(year))
lrp <- filter(x, year == 1986L) %>% rename(lrp = b) %>% select(-year)
x <- left_join(x, lrp)
usr <- filter(x, year %in% seq(1956, 2004)) %>%
  group_by(iter) %>%
  summarise(usr = mean(b))
x <- left_join(x, usr) %>%
  mutate(species = "pcod", region = "3CD", run = 1)
x %>% saveRDS("data-raw/pcod-3cd-mcmc.rds")

x <- d5abcd[[1]]$mcmccalcs$sbt.dat
x <- as.data.frame(x)
x$iter <- seq_len(nrow(x))
x <- tidyr::pivot_longer(x, cols = -iter, names_to = "year", values_to = "b") %>%
  mutate(year = as.integer(year))
lrp <- filter(x, year == 2000L) %>% rename(lrp = b) %>% select(-year)
x <- left_join(x, lrp)
usr <- filter(x, year %in% seq(1956, 2004)) %>%
  group_by(iter) %>%
  summarise(usr = mean(b))
x <- left_join(x, usr) %>%
  mutate(species = "pcod", region = "5abcd", run = 1)
x %>% saveRDS("data-raw/pcod-5abcd-mcmc.rds")

# inside yelloweye --------------------------------------------------------

d <- readRDS("data-raw/model-output/ye-inside-b-lrp.rds")
group_by(d, year) %>%
  summarise(species = "yelloweye", region = "4B", log_blrp = mean(log(b_lrp)), sd_log_blrp = sd(log(b_lrp))) %>%
  select(species, region, everything()) %>%
  saveRDS("data-raw/yelloweye-4b.rds")

d <- readRDS("data-raw/model-output/ye-inside-b-mcmc.rds")
d <- mutate(d, species = "yelloweye", region = "4B")
d <- mutate(d, lrp = bmsy * 0.4, usr = bmsy * 0.8)
d %>% saveRDS("data-raw/yelloweye-4b-mcmc.rds")

# POP 5ABC 2017 -----------------------------------------------------------

# one 'Run':
format_rowan_raw_data <- function(sheet1, sheet2, .species, .region) {
  b <- tidyr::pivot_longer(sheet1, cols = -1, names_to = "year", values_to = "B")
  d <- dplyr::left_join(b, sheet2) %>%
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

d1 <- readxl::read_xls("data-raw/model-output/POP.5ABC.2017.MCMC.forSean.xls", sheet = 1)
d2 <- readxl::read_xls("data-raw/model-output/POP.5ABC.2017.MCMC.forSean.xls", sheet = 2)
d <- format_rowan_raw_data(d1, d2, .species = "pacific-ocean-perch", .region = "5ABC")
d %>% saveRDS("data-raw/pop-5abcd.rds")

b <- tidyr::pivot_longer(d1, cols = -1, names_to = "year", values_to = "B")
d <- dplyr::left_join(b, d2) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(b = B, bmsy = Bmsy) %>%
  mutate(run = 1) %>%
  mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
  mutate(species = "pacific-ocean-perch", region = "5ABC") %>%
  rename(iter = sample)
d %>% saveRDS("data-raw/pop-5abcd-mcmc.rds")

# POP 3CD 2012  -------------------------------------------------------------

format_rowan_mcmc_data1 <- function(sheet1, sheet2, .species, .region) {
  b <- tidyr::pivot_longer(sheet1, cols = -1, names_to = "year", values_to = "B")
  dplyr::left_join(b, sheet2) %>%
    mutate(year = as.integer(as.character(year))) %>%
    rename(b = B, bmsy = Bmsy) %>%
    mutate(run = 1) %>%
    mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
    mutate(species = .species, region = .region) %>%
    rename(iter = sample)
}
d1 <- readxl::read_xlsx("data-raw/model-output/POP.3CD.2012.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/POP.3CD.2012.MCMC.forSean.xlsx", sheet = 2)
format_rowan_mcmc_data1(d1, d2, "pacific-ocean-perch", "3CD") %>%
  saveRDS("data-raw/pop-3cd-mcmc.rds")

# POP 5DE -----------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/POP.5DE.2012.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/POP.5DE.2012.MCMC.forSean.xlsx", sheet = 2)
format_rowan_mcmc_data1(d1, d2, "pacific-ocean-perch", "5DE") %>%
  saveRDS("data-raw/pop-5de-mcmc.rds")

# SGR BC ------------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/SGR.CST.2013.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/SGR.CST.2013.MCMC.forSean.xlsx", sheet = 2)
format_rowan_mcmc_data1(d1, d2, "silvergray", "BC") %>%
  saveRDS("data-raw/sgr-bc-mcmc.rds")

# Bocaccio ------------------------------------------------------------------

# multiple 'Runs':
format_rowan_raw_data2 <- function(sheet1, sheet2, .species, .region, lrp = Bmsy) {
  b <- tidyr::pivot_longer(sheet1, cols = -c(1, 2), names_to = "year", values_to = "B")
  d <- dplyr::left_join(b, sheet2) %>%
    mutate(year = as.character(year))
  # ggplot(d, aes(year, B / Bmsy, group = sample)) + geom_line()
  mutate(d, species = .species, region = .region) %>%
    select(species, region, everything()) %>%
    group_by(year) %>%
    summarise(
      species = species[1],
      region = region[1],
      log_blrp = mean(log(B/({{lrp}}*0.4))),
      sd_log_blrp = sd(log(B/({{lrp}}*0.4))),
      .groups = "drop"
    ) %>%
    mutate(year = as.integer(as.character(year)))
}
format_rowan_raw_data2_mcmc <- function(sheet1, sheet2, .species, .region) {
  b <- tidyr::pivot_longer(d1, cols = -c(1, 2), names_to = "year", values_to = "B")
  dplyr::left_join(b, d2) %>%
    mutate(year = as.integer(as.character(year))) %>%
    rename(b = B, bmsy = Bmsy) %>%
    mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
    rename(run = Run) %>%
    mutate(species = .species, region = .region) %>%
    rename(iter = sample)
}
format_rowan_raw_data2_mcmc_lrp <- function(sheet1, sheet2, .species, .region) {
  b <- tidyr::pivot_longer(d1, cols = -c(1, 2), names_to = "year", values_to = "B")
  dplyr::left_join(b, d2) %>%
    mutate(year = as.integer(as.character(year))) %>%
    rename(b = B, lrp = Bmin) %>%
    mutate(usr = lrp * 2) %>%
    rename(run = Run) %>%
    mutate(species = .species, region = .region) %>%
    rename(iter = sample)
}

# d1 <- readxl::read_xlsx("data-raw/model-output/BOR.CST.2019.MCMC.forSean.xlsx", sheet = 1)
# d2 <- readxl::read_xlsx("data-raw/model-output/BOR.CST.2019.MCMC.forSean.xlsx", sheet = 2)
# d <- format_rowan_raw_data2(d1, d2, "bocaccio", "BC")
# d %>% saveRDS("data-raw/bocaccio-5-bc.rds")
# format_rowan_raw_data2_mcmc(d1, d2, "bocaccio", "BC") %>% saveRDS("data-raw/bocaccio-5-bc-mcmc.rds")

# shortspine BC -----------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/SST.CST.2015.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/SST.CST.2015.MCMC.forSean.xlsx", sheet = 2)
d2$BtBmsy <- NULL
d2$B2016 <- NULL
names(d1)[1] <- "Run"
format_rowan_raw_data2_mcmc(d1, d2, "shortspine thornyhead", "BC") %>%
  saveRDS("data-raw/shortspine-bc-mcmc.rds")

# redstripe ---------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/RSR.BCN.2018.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/RSR.BCN.2018.MCMC.forSean.xlsx", sheet = 2)
format_rowan_mcmc_data1(d1, d2, "redstripe rockfish", "BC North") %>%
  saveRDS("data-raw/redstripe-bc-north-mcmc.rds")

d1 <- readxl::read_xlsx("data-raw/model-output/RSR.BCS.2018.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/RSR.BCS.2018.MCMC.forSean.xlsx", sheet = 2)
format_rowan_mcmc_data1(d1, d2, "redstripe rockfish", "BC South") %>%
  saveRDS("data-raw/redstripe-bc-south-mcmc.rds")

# widow ---------------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/WWR.CST.2019.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/WWR.CST.2019.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "widow-rockfish", "BC")
d %>% saveRDS("data-raw/widow-bc.rds")
format_rowan_raw_data2_mcmc(d1, d2, "widow-rockfish", "BC") %>% saveRDS("data-raw/widow-bc-mcmc.rds")

# REBS --------------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/REBS.BCN.2020.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/REBS.BCN.2020.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "rougheye/blackspotted", "BC North")
d %>% saveRDS("data-raw/rebs-bc-north.rds")
format_rowan_raw_data2_mcmc(d1, d2, "rougheye/blackspotted", "BC North") %>% saveRDS("data-raw/rebs-bc-north-mcmc.rds")

d1 <- readxl::read_xlsx("data-raw/model-output/REBS.BCS.2020.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/REBS.BCS.2020.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "rougheye/blackspotted", "BC South")
d %>% saveRDS("data-raw/rebs-bc-south.rds")
format_rowan_raw_data2_mcmc(d1, d2, "rougheye/blackspotted", "BC South") %>% saveRDS("data-raw/rebs-bc-south-mcmc.rds")

# walleye -----------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/WAP.BCN.2017.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/WAP.BCN.2017.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "walleye-pollock", "BC North", lrp = Bmin)
d %>% saveRDS("data-raw/walleye-bc-north.rds")
format_rowan_raw_data2_mcmc_lrp(d1, d2, "walleye-pollock", "BC North") %>%
  saveRDS("data-raw/walleye-bc-north-mcmc.rds")

d1 <- readxl::read_xlsx("data-raw/model-output/WAP.BCS.2017.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/WAP.BCS.2017.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data2(d1, d2, "walleye-pollock", "BC South", lrp = Bmin)
d %>% saveRDS("data-raw/walleye-bc-south.rds")
format_rowan_raw_data2_mcmc_lrp(d1, d2, "walleye-pollock", "BC South") %>%
  saveRDS("data-raw/walleye-bc-south-mcmc.rds")

# yellowtail --------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/YTR.CST.2014.MCMC.forSean.xlsx", sheet = 1)
d2 <- readxl::read_xlsx("data-raw/model-output/YTR.CST.2014.MCMC.forSean.xlsx", sheet = 2)
d <- format_rowan_raw_data(d1, d2, "yellowtail", "BC")
d %>% saveRDS("data-raw/yellowtail-bc.rds")

b <- tidyr::pivot_longer(d1, cols = -1, names_to = "year", values_to = "B")
d <- dplyr::left_join(b, d2) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(b = B, bmsy = Bmsy) %>%
  mutate(run = 1) %>%
  mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
  mutate(species = "yellowtail", region = "BC") %>%
  rename(iter = sample)
d %>% saveRDS("data-raw/yellowtail-bc-mcmc.rds")

# sablefish -----------------------------------------------------------------

d <- readr::read_csv("data-raw/model-output/Sablefish_MCMC_weightedAve_biomass.csv")
d <- select(d, starts_with("spawnB"), SBmsy) |>
  mutate(iter = seq_len(nrow(d)))
d2 <- pivot_longer(d, -c(SBmsy, iter)) %>%
  mutate(year_i = gsub("spawnB_", "", name)) %>%
  mutate(year = as.integer(year_i)) %>%
  mutate(species = "sablefish", region = "BC") %>%
  select(species, region, year, SSB = value, SBmsy, iter) |>
  rename(Bmsy = SBmsy)

d3 <- d2 %>% group_by(species, region, year) %>%
  summarise(
    log_blrp = mean(log(SSB/(Bmsy*0.4))),
    sd_log_blrp = sd(log(SSB/(Bmsy*0.4))),
    log_bbmsy = mean(log(SSB/Bmsy)),
    sd_log_bbmsy = sd(log(SSB/Bmsy)),
    p_lrp = mean(SSB < (Bmsy * 0.4)),
    .groups = "drop"
  ) %>%
  filter(is.finite(log_blrp))
d3 %>% saveRDS("data-raw/sable-bc.rds")


# downsample:
set.seed(123)
iter_sample <- sample(unique(d2$iter), 800L) # downsample
d2 <- dplyr::filter(d2, iter %in% iter_sample) |>
  arrange(iter, year)
d2$iter <- NULL

d2 %>% rename(b = SSB, bmsy = Bmsy) %>%
  mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy, run = 1) %>%
  # filter(b > 0) %>%
  saveRDS("data-raw/sable-bc-mcmc.rds")

# d <- readRDS("data-raw/model-output/sable.rds")
# d <- mutate(d, region = "BC") %>% as_tibble()
# d %>% saveRDS("data-raw/sable-bc.rds")
#
# ggplot(d2, aes(year, log_blrp)) + geom_line() +
#   geom_line(data = d)
# ggplot(d2, aes(year, sd_log_blrp)) + geom_line() +
#   geom_line(data = d, colour = "grey")

# quillback outside ---------------------------------------------------

d <- readr::read_csv("data-raw/model-output/quill-out.csv")
plot(d$x, d$med)

d_out_med <- as.data.frame(approx(d$x, d$med, xout = seq(1921, 2009))) %>%
  rename(year = x, med = y)
d_out_upr <- as.data.frame(approx(d$x, d$upr, xout = seq(1921, 2009))) %>%
  rename(year = x, upr = y)
d_out_lwr <- as.data.frame(approx(d$x, d$lwr, xout = seq(1921, 2009))) %>%
  rename(year = x, lwr = y)
d <- left_join(d_out_med, d_out_lwr) %>% left_join(d_out_upr) %>%
  as_tibble()
d$log_B <- log(d$med)
d$log_B_lwr <- log(d$lwr)
d$log_B_upr <- log(d$upr)
d <- mutate(d, sd_log_B = (log_B_upr - log_B_lwr) / (qnorm(0.975) * 2))

ggplot(d, aes(year, exp(log_B), ymin = exp(log_B + 1.96 * sd_log_B), ymax = exp(log_B - 1.96 * sd_log_B))) +
  geom_line(lwd = 2) +
  geom_ribbon(alpha = 0.2) +
  geom_line(aes(year, med), colour = "red") +
  geom_ribbon(aes(year, med, ymin = lwr, ymax = upr), alpha = 0.2, colour = "red")

# Bmsy is 11718 in Table 4
bmsy <- 11718
lrp <- bmsy * 0.4
usr <- bmsy * 0.8

out <- d %>% transmute(species = "quillback", region = "BC Outside", year = year,
  log_blrp = log(med / lrp), sd_log_blrp = sd_log_B,
  log_busr = log(med / usr), sd_log_busr = sd_log_B,
  log_bbmsy = log(med / bmsy), sd_log_bbmsy = sd_log_B,
  q0.05_blrp = lwr / lrp, q0.95_blrp = upr / lrp,
  q0.05_busr = lwr / usr, q0.95_busr = upr / usr,
  q0.05_bmsy = lwr / bmsy, q0.95_bmsy = upr / bmsy,
  p_lrp = NA, p_usr = NA
  )

out %>% saveRDS("data-raw/quillback-outside.rds")

# lingcod -----------------------------------------------------------------

d <- readr::read_csv("data-raw/model-output/Lingcod_forSean/mcmcPost_SSB_overLRP.csv")
d <- tidyr::pivot_longer(d, cols = -1, names_to = "year", values_to = "blrp")
d <- mutate(d,
  year = as.integer(gsub("status_", "", year)),
  busr = blrp * 0.5,
  bbmsy = blrp * 0.4) %>%
  rename(run = scenario) %>%
  mutate(iter = rep(seq_len(2000), each = length(unique(run)) * length(unique(year)))) %>%
  mutate(species = "lingcod", region = "4B") %>%
  mutate(run = as.integer(as.factor(run)))
set.seed(1829494)
iter_sample <- sample(unique(d$iter), 200L) # downsample
d <- filter(d, iter %in% iter_sample)
saveRDS(d, "data-raw/lingcod-inside-mcmc.rds")

# rocksole ----------------------------------------------------------------

blim <- readr::read_csv("data-raw/model-output/RockSole_forSean/RockSole_BLim_Bmcmc-ROL-5AB.22.03.csv")
b <- readr::read_csv("data-raw/model-output/RockSole_forSean/RockSole_SSB_Bmcmc-ROL-5AB.22.03.csv")
bmsy <- readr::read_csv("data-raw/model-output/RockSole_forSean/RockSole_Bmsy_Bmcmc-ROL-5AB.22.03.csv")
b <- tidyr::pivot_longer(b, cols = -1, names_to = "year", values_to = "b")

b <- mutate(b, year = as.integer(year)) %>%
  rename(iter = X1) %>%
  mutate(species = "rocksole", region = "5AB") %>%
  mutate(run = 1L)
blim <- rename(blim, iter = X1, blim = B.Lim)
bmsy <- rename(bmsy, iter = X1, bmsy = Bmsy)
# b <- left_join(b, blim)
# b <- rename(b, lrp = blim)
b <- left_join(b, bmsy)
b <- mutate(b, lrp = 0.4 * bmsy, usr = 0.8 * bmsy)
saveRDS(b, "data-raw/rocksole-5AB-mcmc.rds")

blim <- readr::read_csv("data-raw/model-output/RockSole_forSean/RockSole_BLim_Bmcmc-ROL-5CD.08.03.csv")
b <- readr::read_csv("data-raw/model-output/RockSole_forSean/RockSole_SSB_Bmcmc-ROL-5CD.08.03.csv")
bmsy <- readr::read_csv("data-raw/model-output/RockSole_forSean/RockSole_Bmsy_Bmcmc-ROL-5CD.08.03.csv")
b <- tidyr::pivot_longer(b, cols = -1, names_to = "year", values_to = "b")
b <- mutate(b, year = as.integer(year)) %>%
  rename(iter = X1) %>%
  mutate(species = "rocksole", region = "5CD") %>%
  mutate(run = 1L)
# blim <- rename(blim, iter = X1, blim = B.Lim)
# b <- left_join(b, blim)
# b <- rename(b, lrp = blim)
bmsy <- rename(bmsy, iter = X1, bmsy = Bmsy)
b <- left_join(b, bmsy)
b <- mutate(b, lrp = 0.4 * bmsy, usr = 0.8 * bmsy)
saveRDS(b, "data-raw/rocksole-5CD-mcmc.rds")

# 2022-02-23 update -------------------------------------------------------

# YMR 2021 ----------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/YMR.CST.2021.MCMC.forSean.xlsx", sheet = 1L)
d2 <- readxl::read_xlsx("data-raw/model-output/YMR.CST.2021.MCMC.forSean.xlsx", sheet = 2L)
names(d1)[1] <- "sample"
names(d2)[1] <- "sample"
d <- format_rowan_raw_data(d1, d2, "yellowmouth", "BC")
d %>% saveRDS("data-raw/yellowmouth-bc.rds")
b <- tidyr::pivot_longer(d1, cols = -1, names_to = "year", values_to = "B")
d <- dplyr::left_join(b, d2) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(b = B, bmsy = Bmsy) %>%
  mutate(run = 1) %>%
  mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
  mutate(species = "yellowmouth", region = "BC") %>%
  rename(iter = sample)
set.seed(123)
iter_sample <- sample(unique(d$iter), 500L) # downsample
d <- filter(d, iter %in% iter_sample)
d$iter <- as.numeric(as.factor(as.character(d$iter)))
d <- arrange(d, iter)

d %>% saveRDS("data-raw/yellowmouth-bc-mcmc.rds")

d %>% ggplot(aes(year, b / bmsy, group = iter)) + geom_line(alpha = 0.05)

# BOR 2021 ----------------------------------------------------------------

d1 <- readxl::read_xlsx("data-raw/model-output/BOR.CST.2021.MCMC.forSean.xlsx", sheet = 1L)
d2 <- readxl::read_xlsx("data-raw/model-output/BOR.CST.2021.MCMC.forSean.xlsx", sheet = 2L)
names(d1)[1] <- "sample"
names(d2)[1] <- "sample"
d <- format_rowan_raw_data(d1, d2, "bocaccio", "BC")
head(d)
d %>% saveRDS("data-raw/bocaccio-bc.rds")
b <- tidyr::pivot_longer(d1, cols = -1, names_to = "year", values_to = "B")
d <- dplyr::left_join(b, d2) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(b = B, bmsy = Bmsy) %>%
  mutate(run = 1) %>%
  mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
  mutate(species = "bocaccio", region = "BC") %>%
  rename(iter = sample)
head(d)
set.seed(123)
iter_sample <- sample(unique(d$iter), 500L) # downsample
d <- filter(d, iter %in% iter_sample)
d$iter <- as.numeric(as.factor(as.character(d$iter)))
d <- arrange(d, iter)
d %>% saveRDS("data-raw/bocaccio-bc-mcmc.rds")
d %>% ggplot(aes(year, b / bmsy, group = iter)) + geom_line(alpha = 0.05)

# canary 2023 ---------------------------------------------------------

d1 <- readr::read_csv("data-raw/model-output/CAR-CST-2022-MCMC(B)-forSean.csv")
d2 <- readr::read_csv("data-raw/model-output/CAR-CST-2022-MCMC(Bmsy)-forSean.csv")
names(d1)[1] <- "sample"
names(d2)[1] <- "sample"

b <- tidyr::pivot_longer(d1, cols = -1, names_to = "year", values_to = "B")
.species <- "canary"
.region <- "BC"
d <- dplyr::left_join(b, d2) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(b = B, bmsy = Bmsy) %>%
  mutate(run = 1) %>%
  mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
  mutate(species = .species, region = .region) %>%
  rename(iter = sample)

set.seed(123)
iter_sample <- sample(unique(d$iter), 500L) # downsample
d <- dplyr::filter(d, iter %in% iter_sample)
d$iter <- as.numeric(as.factor(as.character(d$iter)))
d <- arrange(d, iter)

d %>% ggplot(aes(year, b / bmsy, group = iter)) + geom_line(alpha = 0.05)
d %>% saveRDS("data-raw/canary-bc-mcmc.rds")

# # arrowtooth 2023 -----------------------------------------------------
#
# d1 <- readRDS("data-raw/model-output/arrowtooth-2022-ssb.rds")
# d2 <- readRDS("data-raw/model-output/arrowtooth-2022-params.rds")
# d1$sample <- seq(1, nrow(d1))
#
# b <- tidyr::pivot_longer(d1, cols = seq(1, ncol(d1)-1), names_to = "year", values_to = "B")
# d2 <- data.frame(B0 = d2$sbo, sample = seq(1, length(d2$sbo)))
#
# .species <- "arrowtooth"
# .region <- "BC"
# d <- left_join(b, d2) %>%
#   mutate(year = as.integer(as.character(year))) %>%
#   rename(b = B) %>%
#   mutate(run = 1) %>%
#   mutate(lrp = 0.2 * B0, usr = 0.4 * B0) %>%
#   mutate(species = .species, region = .region) %>%
#   rename(iter = sample) |>
#   select(-B0)
#
# set.seed(123)
# iter_sample <- sample(unique(d$iter), 500L) # downsample
# d <- dplyr::filter(d, iter %in% iter_sample)
# d$iter <- as.numeric(as.factor(as.character(d$iter)))
# d <- arrange(d, iter)
#
# d %>% ggplot(aes(year, b / lrp, group = iter)) + geom_line(alpha = 0.05)
# d %>% ggplot(aes(year, b / usr, group = iter)) + geom_line(alpha = 0.05)
# d %>% saveRDS("data-raw/arrowtooth-bc-mcmc.rds")

# quillback 2023 ------------------------------------------------------

d1 <- readRDS("data-raw/model-output/quillback-inside-2022-bbmsy.rds")
unique(d1$scenario)
d1 <- filter(d1, scenario != "(A) No Jig survey") # robustness

d <- mutate(d1, blrp = bbmsy/0.4, busr = bbmsy/0.8) |>
  mutate(iter = rep(rep(seq_len(200), 104), 3)) |>
  mutate(species = "quillback", region = "4B") |>
  mutate(run = as.integer(as.factor(scenario))) |>
  select(-scenario)

d %>% ggplot(aes(year, blrp, group = paste(iter, run))) + geom_line(alpha = 0.05) + scale_y_log10()
d %>% ggplot(aes(year, busr, group = paste(iter, run))) + geom_line(alpha = 0.05) + scale_y_log10()

d %>% saveRDS("data-raw/quillback-inside-mcmc.rds")

# quillback outside 2023 ------------------------------------------------------
d1 <- readRDS("data-raw/model-output/quillback-outside-2023-bbmsy.rds")
d <- d1 |>
  filter(scen != "(A) Low steepness") |>
  mutate(bbmsy = SpawnBio / SSBMSY) |>
  mutate(blrp = bbmsy / 0.4, busr = bbmsy / 0.8) |>
  mutate(species = "quillback", region = "BC Outside") |>
  mutate(run = as.integer(as.factor(scen))) |>
  rename(year = "Yr", iter = "Sim")

d %>% saveRDS("data-raw/quillback-outside-mcmc.rds")
# d %>% ggplot(aes(year, blrp, group = paste(iter, run))) + geom_line(alpha = 0.05) + scale_y_log10()
# d %>% ggplot(aes(year, busr, group = paste(iter, run))) + geom_line(alpha = 0.05) + scale_y_log10()

# POP 3CD 2024  ----------------------------------------------------------------
d1 <- readr::read_csv('data-raw/model-output/POP-(5ABC,3CD,5DE)-2023-SS3-output-forSean-240215/POP-3CD-2023-MCMC(B)-forSean.csv') |>
  select("...1":"2024")

d2 <- readr::read_csv('data-raw/model-output/POP-(5ABC,3CD,5DE)-2023-SS3-output-forSean-240215/POP-3CD-2023-MCMC(Bmsy)-forSean.csv')

format_rowan_mcmc_data_2024 <- function(sheet1, sheet2, .species, .region) {
  b <- tidyr::pivot_longer(sheet1, cols = -1, names_to = "year", values_to = "B")
  dplyr::left_join(b, sheet2) %>%
    mutate(year = as.integer(as.character(year))) %>%
    rename(b = B, bmsy = Bmsy) %>%
    mutate(run = "1") %>%
    mutate(lrp = 0.4 * bmsy, usr = 0.8 * bmsy) %>%
    mutate(species = .species, region = .region) %>%
    mutate(iter = as.numeric(as.factor(`...1`))) |>
    select(-"...1") |>
    filter(!is.na(bmsy))
}

format_rowan_mcmc_data_2024(sheet1 = d1, sheet2 = d2, .species = "pacific-ocean-perch", .region = "3CD") |>
saveRDS("data-raw/pop-3cd-mcmc.rds")

# POP 5ABC 2024  ----------------------------------------------------------------
d1 <- readr::read_csv('data-raw/model-output/POP-(5ABC,3CD,5DE)-2023-SS3-output-forSean-240215/POP-5ABC-2023-MCMC(B)-forSean.csv') |>
  select("...1":"2024")

d2 <- readr::read_csv('data-raw/model-output/POP-(5ABC,3CD,5DE)-2023-SS3-output-forSean-240215/POP-5ABC-2023-MCMC(Bmsy)-forSean.csv') |>
  filter(!is.na(Bmsy))

format_rowan_mcmc_data_2024(sheet1 = d1, sheet2 = d2, .species = "pacific-ocean-perch", .region = "5ABC") |>
saveRDS("data-raw/pop-5abc-mcmc.rds")

# POP 5DE 2024  ----------------------------------------------------------------
d1 <- readr::read_csv('data-raw/model-output/POP-(5ABC,3CD,5DE)-2023-SS3-output-forSean-240215/POP-5DE-2023-MCMC(B)-forSean.csv') |>
  select("...1":"2024")

d2 <- readr::read_csv('data-raw/model-output/POP-(5ABC,3CD,5DE)-2023-SS3-output-forSean-240215/POP-5DE-2023-MCMC(Bmsy)-forSean.csv') |>
  filter(!is.na(Bmsy))

format_rowan_mcmc_data_2024(sheet1 = d1, sheet2 = d2, .species = "pacific-ocean-perch", .region = "5DE") |>
saveRDS("data-raw/pop-5de-mcmc.rds")

# PCOD 3CD 2024  ----------------------------------------------------------------
d <- readRDS("data-raw/model-output/pcod-2023.rds")

d |>
  select(year, blrp = b_lrp, busr = b_usr, iter) |>
  mutate(species = 'pcod', region = '3CD', run = 1) |>
  saveRDS("data-raw/pcod-3cd-mcmc.rds")

# arrowtooth 2023 data SR, maybe be published ultimately in 2024 or 2025 -------

d <- readRDS("data-raw/model-output/arrowtooth-2023-data-SR-01-base-model.rds")
if (FALSE) {
  d$mcmc |> names()
  d$mcmc$sbt |> str()
  d$mcmc$proj |> str()
  d$mcmccalcs |> names()
  d$mcmccalcs$depl_quants
}
yrs <- as.numeric(colnames(d$mcmc$sbt))
yrs <- yrs[-length(yrs)]
sbo <- d$mcmccalcs$params$sbo
sbt <- d$mcmc$sbt
depletion <- matrix(ncol = length(yrs), nrow = nrow(sbt))
blrp <- matrix(ncol = length(yrs), nrow = nrow(sbt))
busr <- matrix(ncol = length(yrs), nrow = nrow(sbt))
for (i in seq_along(yrs)) {
  depletion[,i] <- sbt[,i,drop=TRUE] / sbo
}
for (i in seq_along(yrs)) {
  blrp[,i] <- depletion[,i] / 0.2
  busr[,i] <- depletion[,i] / 0.4
}
busr_long <- reshape2::melt(busr) |> rename(iter = Var1, year_i = Var2)
blrp_long <- reshape2::melt(blrp) |> rename(iter = Var1, year_i = Var2)
year_lu <- data.frame(year_i = seq_along(yrs), year = yrs)
busr_long <- left_join(busr_long, year_lu)
blrp_long <- left_join(blrp_long, year_lu)
set.seed(123)
# i <- sample(1:1000, size = 800)
dd <- dplyr::bind_cols(select(busr_long, year, busr = value, iter), select(blrp_long, blrp = value))
# dd <- dplyr::filter(dd, iter %in% i)
dd$run <- 1
dd$region <- "BC"
dd$species <- "arrowtooth"
ggplot(dd, aes(factor(year), busr)) + geom_violin()
ggplot(dd, aes(factor(year), blrp)) + geom_violin()
saveRDS(dd, file = "data-raw/arrowtooth-bc-mcmc-2023.rds")

## # petrale 2024
## d0 <- readRDS("data-raw/model-output/Petrale_Sole_output-2024.rds")
##
## d <- d0 |>
##   select(year, run = M, b, bmsy, lrp, usr, species, region, iter) |>
##   mutate(year = as.numeric(year), species = "petrale sole", region = "BC", run = as.integer(as.factor(run))) |>
##   filter(!is.na(lrp)) ## Not sure about this FIXME
##
## iter_sample <- sample(unique(d$iter), 2000L) # downsample
##
## d <- filter(d, iter %in% iter_sample)
## group_by(d, year) |> summarise(n = n()) |> as.data.frame()
##
## saveRDS(d, file = "data-raw/petrale-sole-bc-mcmc-2024.rds")

# petrale 2024 take 2 ----------------------------------------------------------

d1 <- readr::read_csv("data-raw/model-output/petrale_nuisanceposteriors_med.csv")
d2 <- readr::read_csv("data-raw/model-output/petrale_nuisanceposteriors_low.csv")
d3 <- readr::read_csv("data-raw/model-output/petrale_nuisanceposteriors_high.csv")
d0 <- bind_rows(list(d1, d2, d3))
s <- select(d0, num_range("SSB_", 1938:2024)) |> mutate(iter = seq_len(n()))
b <- tidyr::pivot_longer(s, cols = -iter) |>
  mutate(year = as.numeric(gsub("SSB_", "", name))) |>
  rename(b = value) |>
  select(-name)
bmsy <- select(d0, SSB_MSY) |> mutate(iter = seq_len(n()))
bmsy <- tidyr::pivot_longer(bmsy, cols = -iter) |>
  rename(bmsy = value) |>
  select(-name)
set.seed(1)
iter_sample <- sample(unique(b$iter), 2000L) # downsample
bmsy <- filter(bmsy, iter %in% iter_sample)
stopifnot(sum(is.na(bmsy$bmsy)) == 0L)
b <- filter(b, iter %in% iter_sample)
d <- left_join(b, bmsy)
d <- mutate(d, lrp = bmsy * 0.4, usr = bmsy * 0.8, species = "petrale sole", region = "BC", run = 1)
glimpse(d)

saveRDS(d, file = "data-raw/petrale-sole-bc-mcmc-2024.rds")
# ggplot(d, aes(factor(year), b / usr)) +
#   geom_violin()
# ggplot(dd, aes(factor(year), b / blrp)) +
#   geom_violin()

# bocaccio 2024 ----------------------------------------------------------------
b <- readr::read_csv("data-raw/model-output/BOR-CST-2024-MCMC(B)-forSean.csv")
bmsy <- readr::read_csv("data-raw/model-output/BOR-CST-2024-MCMC(Bmsy)-forSean.csv")

d <- b |>
  pivot_longer(cols = -Run.Sample, names_to = "year", values_to = "b") |>
  left_join(bmsy) |>
  rename(run = "Run.Sample", bmsy = "Bmsy") |>
  mutate(year = as.numeric(year),
    lrp = bmsy * 0.4,
    usr = bmsy * 0.8,
    species = "bocaccio",
    region = "BC",
    run = as.numeric(as.factor(run))
  )

saveRDS(d, file = "data-raw/bocaccio-bc-mcmc-2024.rds")
