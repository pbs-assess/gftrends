library(dplyr)
library(ggplot2)
dir.create("data-generated", showWarnings = FALSE)

# f <- list.files("data-raw", pattern = ".rds", full.names = TRUE)
# f <- f[!grepl("-mcmc", f)]
# d <- purrr::map_dfr(f, readRDS)
# d <- select(d, year, species, region, log_blrp, sd_log_blrp)
#
# d %>% mutate(stock = paste(species, region)) %>%
#   ggplot(aes(
#     x = year,
#     y = exp(log_blrp),
#     ymin = exp(log_blrp - 2 * sd_log_blrp),
#     ymax = exp(log_blrp + 2 * sd_log_blrp)
#   )) +
#   geom_ribbon(colour = NA, fill = "grey50", alpha = 0.4) +
#   geom_line() +
#   facet_wrap(~stock) +
#   scale_y_log10() +
#   geom_hline(yintercept = 1, lty = 2) +
#   coord_cartesian(xlim = c(1950, 2020)) +
#   ylab("B/LRP") +
#   theme(axis.title.x = element_blank())

f <- list.files("data-raw", pattern = ".rds", full.names = TRUE)
f <- f[grepl("-mcmc", f)]
.readRDS <- function(x) {
  res <- readRDS(x)
  res$run <- as.character(res$run)
  res
}

quant <- function(x, probs, ...) {
  as.numeric(quantile(x = x, probs = probs, na.rm = TRUE, ...))
}
d <- purrr::map_dfr(f, .readRDS)

d <- d %>%
  group_by(species, region, year) %>%
  mutate(
    blrp = if_else(!is.na(b), b / lrp, blrp),
    busr = if_else(!is.na(b), b / usr, busr),
    bbmsy = if_else(!is.na(b), b / bmsy, bbmsy)
  ) %>%
  ungroup()

saveRDS(d, "data-generated/all-mcmc.rds")

out <- d %>%
  group_by(species, region, year) %>%
  summarise(
    log_blrp = mean(log(blrp)),
    sd_log_blrp = sd(log(blrp)),
    q0.05_blrp = quant(blrp, probs = 0.05),
    q0.95_blrp = quant(blrp, probs = 0.95),

    log_busr = mean(log(busr)),
    sd_log_busr = sd(log(busr)),
    q0.05_busr = quant(busr, probs = 0.05),
    q0.95_busr = quant(busr, probs = 0.95),

    log_bbmsy = mean(log(bbmsy)),
    sd_log_bbmsy = sd(log(bbmsy)),
    q0.05_bmsy = quant(bbmsy, probs = 0.05),
    q0.95_bmsy = quant(bbmsy, probs = 0.95),

    p_lrp = mean(blrp < 1),
    p_usr = mean(busr < 1),

    .groups = "drop"
  )
out <- bind_rows(out, readRDS("data-raw/quillback-outside.rds"))

out <- mutate(out, stock = paste(species, region)) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock)) %>%
  select(species, region, stock, year, everything())
# out <- filter(out, !(species == "bocaccio" & year >= 2021))

g <- out %>%
  ggplot(aes(
    x = year,
    y = exp(log_blrp),
    ymin = exp(log_blrp - 2 * sd_log_blrp),
    ymax = exp(log_blrp + 2 * sd_log_blrp)
  )) +
  geom_ribbon(
    colour = NA, fill = "red", alpha = 0.2,
    mapping = aes(ymin = q0.05_blrp, ymax = q0.95_blrp)
  ) +
  geom_ribbon(
    colour = NA, fill = "blue", alpha = 0.2,
    mapping = aes(ymin = q0.05_busr, ymax = q0.95_busr)
  ) +
  geom_ribbon(
    colour = NA, fill = "green", alpha = 0.2,
    mapping = aes(ymin = q0.05_bmsy, ymax = q0.95_bmsy)
  ) +
  geom_line(aes(y = exp(log_blrp)), col = "red") +
  geom_line(aes(y = exp(log_busr)), col = "blue") +
  geom_line(aes(y = exp(log_bbmsy)), col = "green") +
  # geom_line() +
  facet_wrap(~stock) +
  scale_y_log10() +
  geom_hline(yintercept = 1, lty = 2) +
  coord_cartesian(xlim = c(1950, as.numeric(substr(Sys.Date(), 1, 4)))) +
  ylab("B status ratio") +
  theme(axis.title.x = element_blank())
print(g)

saveRDS(out, "data-generated/b-status-dat.rds")
