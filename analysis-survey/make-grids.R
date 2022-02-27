# grid for whole coast synoptic indices
grid_locs <- gfplot::synoptic_grid %>%
      dplyr::select(X, Y, depth, survey)
original_time <- sort(unique(data$year))
nd_whole_coast_index <- do.call("rbind",
  replicate(length(original_time), grid_locs, simplify = FALSE))
nd_whole_coast_index[["year"]] <- rep(original_time, each = nrow(grid_locs))

saveRDS(nd_whole_coast_index, file = paste0("analysis-survey/data/nd_whole_coast_index.rds"))

# grid for outside HBLL indices
nd_N <- gfplot::hbll_n_grid$grid %>% mutate(survey = "HBLL OUT N")
nd_S <- gfplot::hbll_s_grid$grid %>% mutate(survey = "HBLL OUT S")
nd_all1 <- bind_rows(nd_N, nd_S) %>% rename(longitude = X, latitude = Y) %>% mutate(area = 4)
d <- all_data$survey_sets %>% filter(survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N"))
time <- sort(unique(d$year))
nd_all <- do.call("rbind", replicate(length(time), nd_all1, simplify = FALSE))
nd_all[["year"]] <- rep(time, each = nrow(nd_all1))
nd_all <- sdmTMB::add_utm_columns(nd_all, c("longitude", "latitude"))

saveRDS(nd_all, file = paste0("analysis-survey/data/nd_hbll_outside_index.rds"))
