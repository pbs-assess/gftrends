library(dplyr)
library(ggplot2)
source("analysis/utils.R")
# source("analysis/stock_df2.R")

# ---------------------------------------------------------------------
# Run first part only if you have all indices saved in separate files
# and they have been updated
mydir <- paste0("data-generated/indices/")
myfiles <- list.files(path = mydir, pattern = "*.rds", full.names = TRUE)
all_indices <- do.call(bind_rows, lapply(myfiles, readRDS))
saveRDS(all_indices, file = paste0("data-generated/sopo-2021-indices.rds"))

# ---------------------------------------------------------------------

all_indices <- readRDS(paste0("data-generated/sopo-2021-indices.rds")) %>%
  rename(gear = type)
dat <- readRDS("data-generated/b-status-dat.rds") %>%
  filter(year >= 1960) %>%
  rename(dfo_area = region) %>%
  select(-species)
dat <- left_join(dat, stock_df)

g <- ggplot(all_indices, aes(year, est, group = model)) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(
    ymin = lwr, ymax = upr, fill = model
  ), alpha = 0.4) +
  ylab("Relative Biomass (log scale)") +
  # scale_y_log10() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(paste(species, region)), scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())
ggsave("figs/delta-vs-tweedie-all.pdf", width = 22, height = 12)

se_check <- all_indices %>%
  group_by(species, gear, region, surveys, model) %>%
  summarise(avg_se = mean(se, na.rm = TRUE), .groups = "drop")

group_by(se_check, species, gear, region, surveys) %>%
  filter(avg_se == min(avg_se)) %>%
  pull(model) %>%
  table()

keep <- group_by(se_check, species, gear, region, surveys) %>%
  filter(avg_se == min(avg_se))
nrow(keep)
# keep$model[keep$species == "Redstripe Rockfish" & keep$region == "WCHG only"] <- "Tweedie"

all_indices <- left_join(keep, all_indices)

g <- ggplot(all_indices, aes(year, est, group = model)) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(
    ymin = lwr, ymax = upr, fill = model
  ), alpha = 0.4) +
  ylab("Relative Biomass (log scale)") +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(paste(species, region)), scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())
g

ggsave("figs/delta-vs-tweedie-best.pdf", width = 22, height = 12)

slopes <- all_indices %>%
  group_by(species, region) %>%
  group_split() %>%
  purrr::map_dfr(function(.x) {
    .x <- filter(.x, year >= 2000)
    m <- lm(log(est) ~ year, data = .x)
    data.frame(slope = coef(m)[2], species = .x$species[1], region = .x$region[1])
  })

g <- all_indices %>%
  left_join(slopes) %>%
  mutate(id = paste(species, region)) %>%
  # mutate(id = forcats::fct_reorder(id, slope)) %>%
  ggplot(aes(year, est, group = model)) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(
    ymin = lwr, ymax = upr, fill = model
  ), alpha = 0.4) +
  ylab("Relative Biomass") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(forcats::fct_reorder(id, -slope)), scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())
ggsave("figs/index-best-by-slope.pdf", width = 17, height = 12)

all_indices <- left_join(all_indices, slopes)

d <- left_join(all_indices, stock_df, by = c("species", "region")) %>%
  rename(index = region, model_type = model) %>%
  mutate(model = paste(index, model_type)) %>%
  group_by(species, index, model_type, model) %>%
  mutate(mean_est = exp(mean(log(est), na.rm = TRUE))) %>%
  ungroup()

unique(dat$stock_clean)
d1 <- full_join(dat, d)
unique(d1$stock_clean)


# # change order of facets to group more like species together
d2 <- d1 %>% arrange(desc(type), stock) # for grouping by taxa/type first
d2$stock_clean <- ifelse(!is.na(d2$stock_clean), d2$stock_clean, paste(d2$species, d2$gear))
# d2$region <- ifelse(!is.na(d2$region), d2$region, d2$index)

unique(d2$stock_clean)

d2 <- d2 %>%
  group_by(stock_clean) %>%
  mutate(slope = ifelse(is.na(slope), mean(slope, na.rm = TRUE), slope)) # fill in assessment rows

select(d2, species, region, index, slope) %>% distinct()

cols <- RColorBrewer::brewer.pal(3, name = "Dark2")
cols <- c("#00000050", cols)
names(cols) <- c("Assessment", "HBLL (inside)", "HBLL (outside)", "Synoptic trawl")

# d2$short_index <- stringr::str_remove_all(d2$index, " surveys")

e <- exp(d2$log_blrp[d2$stock == "quillback_BC_Outside"])
d2$q0.95_blrp[d2$stock == "quillback_BC_Outside" & d2$q0.95_blrp > max(e, na.rm = T) * 1.2] <- max(e, na.rm = T) * 1.2

e <- d2$est[d2$stock == "redstripe_rockfish_BC_North"]
d2$upr[d2$stock == "redstripe_rockfish_BC_North" & d2$upr > max(e, na.rm = T) * 2] <- max(e, na.rm = T) * 2

# e <- d2$est[d2$species == "Shortbelly Rockfish"]
# d2$upr[d2$species == "Shortbelly Rockfish" & d2$upr > max(e, na.rm = T) * 1.5] <- max(e, na.rm = T) * 1.5

d2$est[d2$stock == "sablefish_BC"] <- NA # not trap survey
d2$lwr[d2$stock == "sablefish_BC"] <- NA # not trap survey
d2$upr[d2$stock == "sablefish_BC"] <- NA # not trap survey

d3 <- group_by(d2, stock_clean) %>%
  group_split() %>%
  purrr::map_dfr(function(.x) {
    if (sum(!is.na(.x$est)) == 0L) {
      return(mutate(.x, min_geo_mean = NA, max_geo_mean = NA, mean_blrp = 1, mean_est = 1))
    } else {
      .x <- mutate(.x, min_surv_year = min(year[!is.na(est)]), na.rm = TRUE)
      .x <- mutate(.x, max_surv_year = max(year[!is.na(est)]), na.rm = TRUE)
      .x <- mutate(.x, min_assess_year = min(year[!is.na(log_blrp)]), na.rm = TRUE)
      .x <- mutate(.x, max_assess_year = max(year[!is.na(log_blrp)]), na.rm = TRUE)
      .x <- mutate(.x, min_geo_mean = max(min_surv_year, min_assess_year))
      .x <- mutate(.x, max_geo_mean = min(max_surv_year, max_assess_year))
      .x <- mutate(.x, mean_blrp = exp(mean(log_blrp[year >= min_geo_mean & year <= max_geo_mean], na.rm = TRUE)))
      # could be multiple survey gears:
      if (sum(!is.na(.x$log_blrp)) == 0L) { # no assessment
        .x$min_geo_mean <- min(.x$year, na.rm = TRUE)
        .x$max_geo_mean <- max(.x$year, na.rm = TRUE)
      }
      # if (unique(.x$stock_clean)[1] == "North Pacific Spiny Dogfish BC") browser()
      .x <- group_by(.x, gear) %>%
        mutate(mean_est = exp(mean(log(est[year >= min_geo_mean & year <= max_geo_mean]), na.rm = TRUE))) %>%
        ungroup()
      .x <- select(.x, -min_surv_year, -max_surv_year, -min_assess_year, -max_assess_year)
      .x
    }
  })

lh <- d3$slope[d3$stock_clean == "Lingcod HBLL (outside)"]
lt <- d3$slope[d3$stock_clean == "Lingcod Synoptic trawl"]
d3$slope[d3$stock_clean == "Lingcod HBLL (outside)"] <- mean(c(mean(lh), mean(lt)))
d3$slope[d3$stock_clean == "Lingcod Synoptic trawl"] <- mean(c(mean(lh), mean(lt)))
d3$stock_clean[d3$stock_clean == "Lingcod Synoptic trawl"] <- "Lingcod BC Outside"
d3$stock_clean[d3$stock_clean == "Lingcod HBLL (outside)"] <- "Lingcod BC Outside"


d3$species <- gsub("rockfish", "Rockfish", d3$species)
d3$stock_clean <- gsub("rockfish", "Rockfish", d3$stock_clean)
d3$stock_clean <- gsub("Synoptic trawl", "BC", d3$stock_clean)
d3$stock_clean <- gsub("HBLL (outide)", "BC Outside", d3$stock_clean)
d3$stock_clean <- gsub("VI Inside", "4B (VI Inside)", d3$stock_clean)
# d3$stock_clean <- gsub("4B", "4B (VI Inside)", d3$stock_clean)
d3$stock_clean <- gsub("HBLL \\(inside\\)", "4B (VI Inside)", d3$stock_clean)
d3$stock_clean <- gsub("HBLL \\(outside\\)", "BC (Outside)", d3$stock_clean)
# d3$stock_clean <- gsub("Dogfish BC", "DogfishBC", d3$stock_clean)

lh <- d3$slope[d3$stock_clean == "Big Skate BC"]
lt <- d3$slope[d3$stock_clean == "Big Skate BC (Outside)"]
d3$slope[d3$stock_clean == "Big Skate BC (Outside)"] <- mean(c(mean(lh), mean(lt)))
d3$stock_clean <- gsub("^Big Skate BC$", "Big Skate BC (Outside)", d3$stock_clean)

lh <- d3$slope[d3$stock_clean == "Longnose Skate BC"]
lt <- d3$slope[d3$stock_clean == "Longnose Skate BC (Outside)"]
d3$slope[d3$stock_clean == "Longnose Skate BC (Outside)"] <- mean(c(mean(lh), mean(lt)))
d3$stock_clean <- gsub("^Longnose Skate BC$", "Longnose Skate BC (Outside)", d3$stock_clean)

d3$stock_clean <- gsub("^Sandpaper Skate BC$", "Sandpaper Skate BC (Outside)", d3$stock_clean)
# d3$stock_clean[d3$stock_clean == "Sandpaper Skate BC (Outside)"] <- "Sandpaper Skate BC"
d3$stock_clean <- gsub("^Copper Rockfish BC$", "Copper Rockfish BC (Outside)", d3$stock_clean)
d3$stock_clean <- gsub("^Yelloweye Rockfish BC Outside$", "Yelloweye Rockfish BC (Outside)", d3$stock_clean)
d3$stock_clean <- gsub("Quillback BC Outside", "Quillback Rockfish BC (Outside)", d3$stock_clean)
d3$stock_clean <- gsub("Quillback 4B", "Quillback Rockfish 4B", d3$stock_clean)
d3$stock_clean <- gsub("^Yelloweye Rockfish 4B$", "Yelloweye Rockfish 4B (VI Inside)", d3$stock_clean)

d3$stock_clean <- gsub("Butter Sole BC", "Butter Sole (QCS & HS)", d3$stock_clean)
d3$stock_clean <- gsub("Longspine Thornyhead BC", "Longspine Thornyhead (WCHG)", d3$stock_clean)


d3$type[d3$species %in% c(
  "North Pacific Spiny Dogfish",
  "Big Skate",
  "Longnose Skate",
  "Sandpaper Skate",
  "Spotted Ratfish"
)] <- "Sharks and allies"
d3$type[d3$species %in% c(
  "Butter Sole",
  "Dover Sole",
  "English Sole",
  "Flathead Sole",
  "Petrale Sole",
  "Rex Sole"
)] <- "Flatfish"

d3$type[d3$species %in% c(
  "Canary Rockfish",
  "Copper Rockfish",
  "Darkblotched rockfish",
  "Greenstriped Rockfish",
  "Longspine Thornyhead",
  "Sharpchin Rockfish",
  "Shortraker Rockfish",
  "Shortbelly Rockfish",
  "Splitnose Rockfish"
)] <- "Rockfish"

d3$type[d3$species %in% c("Walleye Pollock", "Pacific Cod", "Sablefish", "Kelp Greenling", "Lingcod")] <- "Cods and allies"

d3$species_common_name <- tolower(d3$species)
d3 <- left_join(d3, select(gfsynopsis::get_spp_names(), species_common_name, species_code))

select(d3, stock_clean) %>%
  distinct() %>%
  saveRDS(here::here("data-generated/assess-index-plotted-data.rds"))

make_surv_assess_plot <- function(dat, ylab = "Relative biomass or abundance estimate", ncol = 5L, arrange_by = c("slope", "code"), colour_by = gear, lty_by = NULL) {

  arrange_by <- match.arg(arrange_by)

  if (arrange_by == "code") {
    dat <- arrange(dat, species_code, region) #%>%
      # mutate(.order = as.numeric((forcats::fct_inorder(species_code))))
  }
  # browser()

  gg <- dat %>%
    mutate(
      stock_clean =
        gsub("([a-zA-Z]+[ \\/]+[0-9a-zA-Z\\.]+) ([a-zA-Z 0-9]+)", "\\1\\\n\\2", stock_clean)
    ) %>%
    mutate(gear = ifelse(is.na(gear), "Assessment", gear)) %>%
    ggplot() +
    geom_ribbon(aes(year,
      ymin = q0.05_blrp / mean_blrp, ymax = q0.95_blrp / mean_blrp,
      group = stock_clean
    ), fill = "black", alpha = 0.2) +
    geom_line(aes(year, exp(log_blrp) / mean_blrp, group = stock_clean),
      linetype = 1, alpha = 0.4, colour = "black"
    ) +
    geom_line(aes(year, est / mean_est, colour = {{colour_by}}, lty = {{lty_by}})) +
    geom_ribbon(aes(year,
      ymin = lwr / mean_est, ymax = upr / mean_est,
      fill = {{colour_by}}, lty = {{lty_by}}
    ), alpha = 0.3) +
    ylab(ylab) +
    # scale_colour_manual(values = cols) +
    # scale_fill_manual(values = cols) +
    ggsidekick::theme_sleek() +
    theme(
      axis.text.y = element_blank(), axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(colour = "grey85", linetype = 2),
      panel.spacing.x = unit(15, "points"),
      panel.spacing.y = unit(3, "points")
    ) +
    coord_cartesian(expand = FALSE) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(colour = "Type", fill = "Type")

  if (arrange_by == "slope") {
    gg <- gg + facet_wrap(vars(forcats::fct_reorder(stock_clean, -slope)),
      ncol = ncol, scales = "free_y")
  } else {
    gg <- gg + facet_wrap(vars(forcats::fct_inorder(stock_clean)),
      ncol = ncol, scales = "free_y")
  }
  gg
  # geom_vline(aes(xintercept = min_geo_mean), lty = 1) + # testing
  # geom_vline(aes(xintercept = max_geo_mean), lty = 1) # testing
}



d3$type[d3$species %in% c("Darkblotched Rockfish", "Longspine Thornyhead","Pacific Ocean Perch", "Rougheye/Blackspotted Rockfish Complex", "Sharpchin Rockfish", "Shortraker Rockfish", "Shortspine Thornyhead", "Splitnose Rockfish","Yellowmouth Rockfish")] <- "Slope rockfish"
d3$type[d3$species %in% c("Bocaccio", "Canary Rockfish", "Greenstriped Rockfish", "Redstripe Rockfish", "Silvergray Rockfish", "Widow Rockfish", "Yellowtail Rockfish")] <- "Shelf rockfish"
d3$type[d3$species %in% c("Copper Rockfish", "Quillback Rockfish", "Yelloweye Rockfish")] <- "Inshore rockfish"


d4 <- mutate(d3, gear_simple = ifelse(grepl("trawl", gear), "Trawl", "Long line"))
d4 <- mutate(d4, type = ifelse(grepl("Cods", type), "Roundfish", type))
d4 <- mutate(d4, type = ifelse(grepl("Sharks", type), "Sharks, skates, chimeras", type))
g <- make_surv_assess_plot(d4, ncol = 5, colour_by = type, lty_by = gear_simple) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("Trawl" = 1, "Long line" = 2)) +
  labs(fill = "Species\ngroup", colour = "Species\ngroup", linetype = "Gear")
g
ggsave("figs/stock-vs-indices.pdf", width = 9.6, height = 11)
ggsave("figs/stock-vs-indices.png", width = 9.6, height = 11)


d5 <- d4 %>% filter(!is.na(gear)) %>%
  filter(stock_clean != "Sablefish BC")

d5 %>% mutate(
  stock_clean =
    gsub("([a-zA-Z]+[ \\/]+[0-9a-zA-Z\\.]+) ([a-zA-Z 0-9]+)", "\\1\\\n\\2", stock_clean)
)%>%
  # mutate(gear = ifelse(is.na(gear), "Assessment", gear)) %>%
  ggplot(aes(year, est/ mean_est, group = gear)) +
  geom_line(aes(colour = type, lty = gear_simple)) +
  geom_ribbon(aes(
    ymin = lwr/ mean_est, ymax = upr/ mean_est, fill = type
  ), alpha = 0.3) +
  ylab("Relative Biomass or abundance estimate") +
  xlab("Year") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("Trawl" = 1, "Long line" = 2)) +
  facet_wrap(vars(forcats::fct_reorder(stock_clean, -slope)), scales = "free_y", ncol = 6) +
  labs(fill = "Species\ngroup", colour = "Species\ngroup", linetype = "Gear")+
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())

ggsave("figs/survey-indices.pdf", width = 12, height = 12)


d5 %>% select(stock_clean, species, gear_simple, index, surveys, est, lwr, upr) %>%
  mutate(est = round(est), lwr = round(lwr), upr = round(upr)) %>%
  readr::write_csv("data-generated/survey_indices.csv")



# # plot just sablefish before survey removed
# g <- d3 %>%
#   filter(species == "Sablefish") %>%
#   make_surv_assess_plot() +
#   xlim(1960, 2021) +
#   theme(legend.position = "none", axis.title.y = element_blank())
# g
# ggsave("figs/stock-vs-indices-sablefish.pdf", width = 1.9, height = 1.35)
# ggsave("figs/stock-vs-indices-sablefish.png", width = 1.9, height = 1.35)
#
# d3$est[d3$stock == "sablefish_BC"] <- NA # not trap survey
# d3$lwr[d3$stock == "sablefish_BC"] <- NA # not trap survey
# d3$upr[d3$stock == "sablefish_BC"] <- NA # not trap survey

# plot just possible range expansion
# g <- d3 %>%
#   filter(species %in% c("Shortbelly Rockfish", "Chilipepper")) %>%
#   make_surv_assess_plot() +
#   # xlim(1960, 2021) +
#   theme(legend.position = "none", axis.title.y = element_blank())
# ggsave("figs/range-expansion.pdf", width = 4.5, height = 1.5)
# ggsave("figs/range-expansion.png", width = 4.5, height = 1.5)

# d3 <- d3 %>% filter(!species %in% c("Shortbelly Rockfish", "Chilipepper"))


g <- d3 %>%
  filter(year >= 2000) %>%
  make_surv_assess_plot()
# ggsave("figs/stock-vs-indices-recent.pdf", width = 10, height = 12)
ggsave("figs/stock-vs-indices-recent.png", width = 10, height = 12)

# g <- d3 %>%
#   mutate(stock_clean = gsub("Quillback 4B \\(VI Inside\\)", "Quillback 4B Inside", stock_clean)) %>%
#   filter(year >= 2000) %>%
#   mutate(q0.05_blrp = NA, q0.95_blrp = NA, log_blrp = NA) %>%
#   filter(stock %in% c("quillback_BC_Outside", "quillback_WCVI_Inside")) %>%
#   make_surv_assess_plot(ylab = "Relative abundance estimate")
# g
# ggsave("figs/stock-vs-indices-recent.pdf", width = 10, height = 12)
# ggsave("figs/stock-vs-indices-recent.png", width = 10, height = 12)


# just rockfish
g <- d3 %>%
  filter(type %in% c("Inshore rockfish", "Shelf rockfish", "Slope rockfish")) %>%
  make_surv_assess_plot(ncol = 5, arrange_by = "code")
g
# ggsave("figs/stock-vs-indices-rockfish.pdf", width = 10, height = 5)
ggsave("figs/stock-vs-indices-rockfish.png", width = 10, height = 5)

g <- d3 %>%
  filter(type %in% c("Inshore rockfish", "Shelf rockfish", "Slope rockfish")) %>%
  make_surv_assess_plot(ncol = 5, arrange_by = "slope")
g
# ggsave("figs/stock-vs-indices-rockfish-by-slope.pdf", width = 10, height = 5)
ggsave("figs/stock-vs-indices-rockfish-by-slope.png", width = 10, height = 5)

g <- d3 %>%
  filter(type == "Inshore rockfish") %>%
  make_surv_assess_plot(ncol = 2, arrange_by = "slope") +
  facet_wrap(vars(stock_clean),
             ncol = 2L,
             scales = "free_y"
  )
g
# ggsave("figs/stock-vs-indices-rockfish-inshore.pdf", width = 10, height = 5)
ggsave("figs/stock-vs-indices-rockfish-inshore.png", width = 5, height = 3, dpi = 400)

g <- d3 %>%
  filter(type == "Shelf rockfish") %>%
  make_surv_assess_plot(ncol = 2, arrange_by = "slope")
g
# ggsave("figs/stock-vs-indices-rockfish-shelf.pdf", width = 10, height = 5)
ggsave("figs/stock-vs-indices-rockfish-shelf.png", width = 5, height = 5)

g <- d3 %>%
  filter(type == "Slope rockfish") %>%
  make_surv_assess_plot(ncol = 3, arrange_by = "slope")
g
# ggsave("figs/stock-vs-indices-rockfish-shelf.pdf", width = 10, height = 5)
ggsave("figs/stock-vs-indices-rockfish-slope.png", width = 7, height = 4.5)


# just sharks and allies
g <- d3 %>%
  filter(type == "Sharks and allies") %>%
  make_surv_assess_plot(ncol = 3, arrange_by = "code") +
  xlim(range(d3$year))
g
# ggsave("figs/stock-vs-indices-sharkco.pdf", width = 7, height = 4.5)
ggsave("figs/stock-vs-indices-sharkco.png", width = 7, height = 4.5)

# just sharks and allies
g <- d3 %>%
  filter(type == "Sharks and allies") %>%
  make_surv_assess_plot(ncol = 3L) +
  xlim(range(d3$year))
g
# ggsave("figs/stock-vs-indices-sharkco-by-slope.pdf", width = 7, height = 4.5)
ggsave("figs/stock-vs-indices-sharkco-by-slope.png", width = 7, height = 4.5)

# just cods and allies
g <- d3 %>%
  filter(type == "Cods and allies" & species != "Sablefish") %>%
  make_surv_assess_plot(arrange_by = "code", ncol = 2)
# facet_wrap(vars(stock_clean),
#   ncol = 2,
#   scales = "free_y"
# )
g
# ggsave("figs/stock-vs-indices-cods.pdf", width = 5, height = 4)
ggsave("figs/stock-vs-indices-cods.png", width = 5, height = 4)

# just flatfish
g <- d3 %>%
  filter(type == "Flatfish") %>%
  make_surv_assess_plot(arrange_by = "code", ncol = 3)
# g
# ggsave("figs/stock-vs-indices-flatfish.pdf", width = 5, height = 4)
ggsave("figs/stock-vs-indices-flatfish.png", width = 7, height = 4)

g <- d3 %>%
  filter(type == "Flatfish") %>%
  make_surv_assess_plot(ncol = 3)
# g
# ggsave("figs/stock-vs-indices-flatfish-by-slope.pdf", width = 5, height = 4)
ggsave("figs/stock-vs-indices-flatfish-by-slope.png", width = 7, height = 4.5)


