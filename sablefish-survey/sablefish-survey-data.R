# explore sablefish survey data
library(tidyverse)
theme_set(
  ggsidekick::theme_sleek()
)

d <- readRDS("data-raw/sablefishsurveys.rds")

unique(d$reason_desc)

set_count <- d %>% group_by(reason_desc, trip_id, fe_major_level_id, year) %>%
  summarize(fe = 1) %>% group_by(reason_desc, year) %>% summarise(sets = n())

d <- d[!is.na(d$species_common_name),]
d <- d %>% filter(catch_verification_desc == "SORTED AND WEIGHED") %>%
  filter(reason_desc %in% c("SABLEFISH RANDOM STRATIFIED SURVEY",
                            # "SABLEFISH STANDARDIZED INLET SURVEY",
                            # "SABLEFISH DEEPWATER SURVEY", "SABLEFISH OFFSHORE SURVEY",
                            "SABLEFISH STANDARDIZED OFFSHORE SURVEY"))

unique(d$species_common_name)

spp <- d %>% group_by(species_common_name, species_science_name) %>%
  summarize(sets = n(),
            count = sum(catch_count, na.rm = T),
            biomass = sum(catch_weight, na.rm = T)) %>%
  filter(sets > 70 & count > 100)

d1 <- d %>% group_by(species_common_name, species_science_name, year,reason_desc) %>%
  summarize(presence = n(),
            count = sum(catch_count, na.rm = T),
            biomass = sum(catch_weight, na.rm = T)) %>%
  filter(species_common_name %in% spp$species_common_name)

d2 <- inner_join(d1, set_count)

ggplot(d2, aes(year, presence/sets, colour = reason_desc)) + geom_line() +
  facet_wrap(~species_common_name, scales = "free_y")
ggsave("sablefishpresent.pdf")
ggplot(d2, aes(year, count/sets, colour = reason_desc)) + geom_line() +
  facet_wrap(~species_common_name, scales = "free_y")
ggsave("sablefishcounts.pdf")

ggplot(d2, aes(year, biomass/sets, colour = reason_desc)) + geom_line() +
  facet_wrap(~species_common_name, scales = "free_y")
ggsave("sablefishbiomass.pdf")

set_count %>%
  filter(reason_desc %in% c("SABLEFISH RANDOM STRATIFIED SURVEY",
                            "SABLEFISH STANDARDIZED OFFSHORE SURVEY")
         )%>% ggplot( aes(year, sets, colour = reason_desc)) + geom_line() +
  theme(legend.position = c(0.7, 0.1))
