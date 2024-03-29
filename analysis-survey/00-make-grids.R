library(dplyr)

# grab an example of survey data from synoptic report cache
all_data <- readRDS(here::here("data-raw/all_surv_catch.rds")) %>%
  filter(species_common_name == tolower("Quillback Rockfish"))
# grid for whole coast synoptic indices
grid_locs <- gfplot::synoptic_grid %>%
      dplyr::select(X, Y, depth, survey)
# For now interested in only the trawl surveys
d <- all_data %>% dplyr::filter(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"))
# Need to find all the years that there was some trawl data collected in one of these
# four surveys
time <- sort(unique(d$year))
# Building out the grid for each year a survey existed (function is now in sdmTMB)
nd_whole_coast_index <- sdmTMB::replicate_df(grid_locs, "year", time) |>
  mutate(area = 4)

saveRDS(nd_whole_coast_index, file = paste0(here::here("data-generated/synoptic-grid.rds")))

# Use better version of grids for HBLL? Check that they are different from gfplot::hbll_n_grid$grid?
grid_locs <- readRDS(here::here("data-raw/All_HBLL_Blocks_Area_Water.rds")) %>%
  mutate(depth = (MAX_DEPTH_ + MIN_DEPTH_) / 2) %>%  # get midpoint depth
  select(survey = SS_NAME, ssid = SS_ID, latitude = LATITUDE, longitude = LONGITUDE, area = Area_Water_km2, depth)  # rename to match what is in the synoptic data

# grid for outside HBLL indices
nd_out <- filter(grid_locs, ssid %in% c(22, 36))
d <- all_data %>% filter(survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N"))
time <- sort(unique(d$year))  # get year of the outside LL surveys
nd_outside_index <- sdmTMB::replicate_df(nd_out, "year", time)
nd_outside_index <- sdmTMB::add_utm_columns(nd_outside_index, c("longitude", "latitude"), utm_crs = 32609)

saveRDS(nd_outside_index, file = here::here("data-generated/hbll-outside-grid.rds"))

# grid for inside HBLL indices
nd_in <- filter(grid_locs, ssid %in% c(39, 40))
# grab an example of inside HBLL data
d <- all_data %>% filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N"))
time <- sort(unique(d$year))
nd_inside_index <- sdmTMB::replicate_df(nd_in, "year", time)
nd_inside_index <- sdmTMB::add_utm_columns(nd_inside_index, c("longitude", "latitude"), utm_crs = 32609)

saveRDS(nd_inside_index, file = here::here("data-generated/hbll-inside-grid.rds"))
# plot(nd_all)
