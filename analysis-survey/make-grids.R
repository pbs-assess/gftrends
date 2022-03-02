library(dplyr)

# grab an example of survey data from synoptic report cache
all_data <- readRDS(here::here("data-raw/all-survey-sets-2021.rds")) %>%
  filter(species_common_name == tolower("Quillback Rockfish"))
# grid for whole coast synoptic indices
grid_locs <- gfplot::synoptic_grid %>%
      dplyr::select(X, Y, depth, survey)
d <- all_data %>% dplyr::filter(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"))
original_time <- sort(unique(d$year))
nd_whole_coast_index <- do.call("rbind",
  replicate(length(original_time), grid_locs, simplify = FALSE))
nd_whole_coast_index[["year"]] <- rep(original_time, each = nrow(grid_locs))

saveRDS(nd_whole_coast_index, file = paste0(here::here("data-generated/nd_whole_coast_index.rds")))

# # grid for outside HBLL indices
# nd_N <- gfplot::hbll_n_grid$grid %>% mutate(survey = "HBLL OUT N")
# nd_S <- gfplot::hbll_s_grid$grid %>% mutate(survey = "HBLL OUT S")
# nd_all1 <- bind_rows(nd_N, nd_S) %>% rename(longitude = X, latitude = Y) %>% mutate(area = 4)
# d <- all_data$survey_sets %>% filter(survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N"))
# time <- sort(unique(d$year))
# nd_all <- do.call("rbind", replicate(length(time), nd_all1, simplify = FALSE))
# nd_all[["year"]] <- rep(time, each = nrow(nd_all1))
# nd_all <- sdmTMB::add_utm_columns(nd_all, c("longitude", "latitude"), utm_crs = 32609)
#
# saveRDS(nd_all, file = paste0("analysis-survey/data/nd_hbll_outside_index.rds"))

grid_locs <- readRDS(here::here("data-raw/All_HBLL_Blocks_Area_Water.rds")) %>%
  mutate(depth = (MAX_DEPTH_ + MIN_DEPTH_)/2) %>%
  select(survey = SS_NAME, ssid = SS_ID, latitude = LATITUDE, longitude = LONGITUDE, area = Area_Water_km2, depth)


# grid for outside HBLL indices
nd_out <- filter(grid_locs, ssid %in% c(22, 36))
d <- all_data %>% filter(survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N"))
time <- sort(unique(d$year))
nd_all <- do.call("rbind", replicate(length(time), nd_out, simplify = FALSE))
nd_all[["year"]] <- rep(time, each = nrow(nd_out))
nd_all <- sdmTMB::add_utm_columns(nd_all, c("longitude", "latitude"), utm_crs = 32609)

saveRDS(nd_all, file = here::here("data-generated/nd_hbll_outside_index.rds"))

# grid for inside HBLL indices
nd_in <- filter(grid_locs, ssid %in% c(39, 40))
# grab an example of inside HBLL data
d <- readRDS(here::here("data-raw/all-survey-sets-2021.rds")) %>%
  filter(species_common_name == tolower("Quillback Rockfish"))
time <- sort(unique(d$year))
nd_all <- do.call("rbind", replicate(length(time), nd_in, simplify = FALSE))
nd_all[["year"]] <- rep(time, each = nrow(nd_in))
nd_all <- sdmTMB::add_utm_columns(nd_all, c("longitude", "latitude"), utm_crs = 32609)

saveRDS(nd_all, file = here::here("data-generated/nd_hbll_inside_index.rds"))
# plot(nd_all)
