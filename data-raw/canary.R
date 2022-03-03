library(ggplot2)
library(dplyr)
library(ramlegacy)
download_ramlegacy()
d <- load_ramlegacy()
names(d)
d$stock[grep("Canary", d$stock$commonname),]

x <- d$timeseries %>% dplyr::filter(stockid == "CROCKWCVANISOGQCI")
unique(x$tsid)

names(d)

d$timeseries_ids_views %>% head
d$timeseries_ids_views %>% filter(stockid == "CROCKWCVANISOGQCI")

filter(x, tsid == "BdivBmsytouse-dimensionless") %>%
  ggplot(aes(tsyear, tsvalue)) + geom_line()

filter(x, tsid == "BdivBmsypref-dimensionless") %>%
  ggplot(aes(tsyear, tsvalue)) + geom_line()

