library(dplyr)
library(ggplot2)
dir.create("data-generated", showWarnings = FALSE)

end_year <- 2025
message("Stitching up to year: ", end_year)

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
# these get stitched on later:
f <- f[!grepl("dogfish-bc-2023\\.rds", f)]
f <- f[!grepl("dogfish-bc-mcmc-2023\\.rds", f)] # below
f <- f[!grepl("dover-bc-mcmc-2025\\.rds", f)] # below
f <- f[!grepl("yellowtail-bc-mcmc\\.rds", f)] # old and replaced
f <- f[!grepl("bocaccio-bc-mcmc\\.rds", f)] # old and replaced
f <- f[!grepl("pop-5abcd-mcmc\\.rds", f)] # bad file - wrong name from past
f <- f[grepl("-mcmc", f)]
f

.readRDS <- function(x) {
  res <- readRDS(x)
  stopifnot('`run` column missing' = 'run' %in% names(res))
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

d <- mutate(d, stock = paste(species, region)) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock)) %>%
  select(species, region, stock, year, everything())

dog <- readRDS("data-raw/dogfish-bc-mcmc-2023.rds") |>
  rename(scen = model, blrp = b_lrp, busr = b_usr) |>
  mutate(species = "north pacific spiny dogfish", region = "BC", stock = "north_pacific_spiny_dogfish_BC")

dover <- readRDS("data-raw/dover-bc-mcmc-2025.rds") |>
  rename(blrp = b_lrp, busr = b_usr) |>
  mutate(species = "dover sole", region = "BC", stock = "dover_sole_BC")

d <- bind_rows(d, dog)
d <- bind_rows(d, dover)

d$Area <- NULL
d$SpawnBio <- NULL

readr::write_rds(d, "data-generated/all-mcmc.rds")

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

# dogfish mcmc was only for 2023 above; drop it and use the MLE-based versions:
out <- filter(out, species != "north pacific spiny dogfish")
out <- bind_rows(out, readRDS("data-raw/dogfish-bc-2023.rds") |> mutate(species = "north pacific spiny dogfish"))

out <- mutate(out, stock = paste(species, region)) %>%
  mutate(stock = gsub(" ", "_", stock)) %>%
  mutate(stock = gsub("-", "_", stock)) %>%
  select(species, region, stock, year, everything())

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
  coord_cartesian(xlim = c(1950, as.numeric(substr(Sys.Date(), 1, 4))), expand = FALSE) +
  ylab("B status ratio") +
  theme(axis.title.x = element_blank()) +
  gfplot::theme_pbs() +
  geom_vline(xintercept = 2024, lty = 2, colour = "grey70")
ggsave("figs/stitch-summary-plot.pdf", width = 11, height = 8)
ggsave("figs/stitch-summary-plot.png", width = 11, height = 8)

saveRDS(out, "data-generated/b-status-dat.rds")
