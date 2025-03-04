# Rowan was away, need to extract from the SAR
# https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2024/2024_033-eng.html

m <- readr::read_csv("data-raw/model-output/boc-2024-data-med.csv")
l <- readr::read_csv("data-raw/model-output/boc-2024-data-lwr.csv")
u <- readr::read_csv("data-raw/model-output/boc-2024-data-upr.csv")

range(m$x)
m$x[1] <- 1935
l$x[1] <- 1935
u$x[1] <- 1935

u$y[1] <- u$y[2]

l$x[length(l$x)] <- 2024
u$x[length(u$x)] <- 2024

med <- approx(m$x, m$y, xout = 1935:2024)
med
lwr <- approx(l$x, l$y, xout = 1935:2024)
lwr
upr <- approx(u$x, u$y, xout = 1935:2024)
upr

d <- data.frame(year = med$x, med = round(med$y, 6), lwr = round(lwr$y, 6), upr = round(upr$y, 6))
d

d$lwr[d$lwr > d$med]

plot(d$year, d$med, ylim = c(0, 8), type = "l")
lines(d$year, d$upr)
lines(d$year, d$lwr)

d <- mutate(d, logmed = log(med), loglwr = log(lwr), logupr = log(upr))

r <- qnorm(0.975) * 2
r

qnorm(0.95)
r <- qnorm(0.95) * 2
r

d$se <- (d$upr - d$lwr) / r
d$logse <- (d$logupr - d$loglwr) / r
d
glimpse(d)

dd <- transmute(
  d,
  species = "bocaccio",
  region = "BC",
  year = year,
  log_blrp = log(med / 0.4),
  sd_log_blrp = logse,
  q0.05_blrp = lwr / 0.4,
  q0.95_blrp = upr / 0.4,
  log_busr = log(med / 0.8),
  sd_log_busr = logse,
  q0.05_busr = lwr / 0.8,
  q0.95_busr = upr / 0.8,
  log_bbmsy = logmed,
  sd_log_bbmsy = logse,
  q0.05_bmsy = lwr,
  q0.95_bmsy = upr
)
head(dd)

## starts at 4 * Bmsy
## which is 4/0.4 = 10 times LRP
library(ggplot2)
ggplot(dd, aes(year, exp(log_bbmsy), ymin = q0.05_bmsy, ymax = q0.95_bmsy)) +
  geom_ribbon() +
  geom_line(colour = "white")

ggplot(dd, aes(year, exp(log_blrp), ymin = q0.05_blrp, ymax = q0.95_blrp)) +
  geom_ribbon() +
  geom_line(colour = "white")

ggplot(dd, aes(year, exp(log_busr), ymin = q0.05_busr, ymax = q0.95_busr)) +
  geom_ribbon() +
  geom_line(colour = "white")

d2 <- mutate(dd,
  year = year, species = "bocaccio", region = "BC",
  log_blrp = log_blrp,
  sd_log_blrp = sd_log_blrp,
  .keep = "none"
)
d2
ggplot(d2, aes(year, log_blrp, ymin = log_blrp - 2 * sd_log_blrp, ymax = log_blrp + 2 * sd_log_blrp)) +
  geom_ribbon() +
  geom_line(colour = "white")
ggplot(d2, aes(year, exp(log_blrp), ymin = exp(log_blrp - 2 * sd_log_blrp), ymax = exp(log_blrp + 2 * sd_log_blrp))) +
  geom_ribbon() +
  geom_line(colour = "white")
head(d2)
saveRDS(d2, file = "data-raw/bocaccio-bc-2024.rds")
saveRDS(dd, file = "data-raw/bocaccio-bc-mcmc-summarized-2024.rds")
