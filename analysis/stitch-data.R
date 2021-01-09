library(tidyverse)

f <- list.files("data-raw", pattern = ".rds", full.names = TRUE)

# dl <- purrr::map(f, readRDS)
d <- purrr::map_dfr(f, readRDS)

d <- select(d, year, species, region, log_blrp, sd_log_blrp)

d %>% mutate(stock = paste(species, region)) %>%
  ggplot(aes(
    x = year,
    y = exp(log_blrp),
    ymin = exp(log_blrp - 2 * sd_log_blrp),
    ymax = exp(log_blrp + 2 * sd_log_blrp))) +
  geom_ribbon(colour = NA, fill = "grey50", alpha = 0.4) +
  geom_line() +
  facet_wrap(~stock) +
  scale_y_log10() +
  geom_hline(yintercept = 1, lty = 2) +
  coord_cartesian(xlim = c(1960, 2020)) +
  ylab("B/LRP") + theme(axis.title.x = element_blank())
