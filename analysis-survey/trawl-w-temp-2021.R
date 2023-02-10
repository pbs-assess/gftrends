# for Eric
library(tidyverse)

d <- readRDS("~/github/dfo/gftrends/data-raw/all-survey-sets-2021.rds")
bc_data_example <- readRDS("~/github/dfo/gftrends/data-raw/bc_data_example.rds")

unique(bc_data_example$species)
glimpse(bc_data_example)

d1 <- d %>% mutate(cpue_kg_km2 = density_kgpm2 * 1000000) %>%
  select(year, survey = survey_abbrev,
         species = species_common_name, scientific_name = species_science_name,
         longitude_dd = longitude, latitude_dd = latitude, depth = depth_m,
         cpue_kg_km2, fishing_event_id) %>%
  filter(survey %in% unique(bc_data_example$survey)) %>%
  filter(species %in% unique(bc_data_example$species))

# data checks
sort(unique(bc_data_example$species))
unique(d1$species)

unique(bc_data_example$survey)
unique(d1$survey)
unique(bc_data_example$year)
unique(d1$year)
range(d1$cpue_kg_km2)
range(bc_data_example$cpue_kg_km2)
length(unique(bc_data_example$fishing_event_id))

d2 <- filter(d1, year<2021)
length(unique(d2$fishing_event_id))

d3 <- bc_data_example %>% distinct()

length(unique(d3$fishing_event_id))

## save without temp?
# saveRDS(d1, "bc_data_2021.rds")

t <- readRDS("~/github/dfo/gftrends/data-raw/trawl_temp.rds")
t <- t %>% select(year, fishing_event_id, temperature = avg, start_time, count, sensor_name)

# two kinds of sensors sometimes used on same fishing event
t1 <- t %>% filter(sensor_name == "SBE39")
t2 <- t %>% filter(sensor_name != "SBE39")

# only use t2 data when t1 missing
t1_ids <- t1 %>% select(fishing_event_id)
t2_only <- anti_join(t2, t1_ids)
t2 <- t %>% filter(fishing_event_id %in% c(t2_only$fishing_event_id))

tdat <- bind_rows(t1, t2)

dat <- left_join(d1,tdat)

# proportion of events with temp data
dat2 <- na.omit(dat)
length(unique(dat2$fishing_event_id))/length(unique(dat$fishing_event_id))

# what survey years have missing data
dat3 <- anti_join(dat,dat2) %>% filter(species == "arrowtooth flounder") %>% group_by(year, survey) %>% summarise(n = n())

# save with temp
saveRDS(dat, "bc_data_2021.rds")
