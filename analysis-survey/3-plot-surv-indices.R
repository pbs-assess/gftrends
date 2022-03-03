library(dplyr)
library(ggplot2)
source("analysis/utils.R")
source("analysis/stock_df2.R")

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
  select(-species) %>%
  group_by(stock) %>%
  mutate(
    mean_blrp = exp(mean(log_blrp[year >= 2003], na.rm = TRUE)),
    ratio_ci = q0.95_blrp / q0.05_blrp
  ) %>%
  ungroup()

dat <- left_join(dat, stock_df)

g <- ggplot(all_indices, aes(year, est, group = model)) +
  geom_line(aes(colour = model)) +
  geom_ribbon(aes(
    ymin = lwr, ymax = upr, fill = model
  ), alpha = 0.4) +
  ylab("Relative Biomass") +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(paste(species, region)), scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())
ggsave("figs/delta-vs-tweedie-all.pdf", width = 17, height = 12)

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
  ylab("Relative Biomass") +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(paste(species, region)), scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())
ggsave("figs/delta-vs-tweedie-best.pdf", width = 17, height = 12)

slopes <- all_indices %>%
  group_by(species, gear) %>%
  group_split() %>%
  purrr::map_dfr(function(.x) {
    .x <- filter(.x, year >= 2003)
    m <- lm(log(est) ~ year, data = .x)
    data.frame(slope = coef(m)[2], species = .x$species[1], gear = .x$gear[1])
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

d <- left_join(all_indices, stock_df, by = c("species", "region")) %>%
  rename(index = region, model_type = model) %>%
  mutate(model = paste(index, model_type)) %>%
  group_by(species, index, model_type, model) %>%
  mutate(mean_est = exp(mean(log(est), na.rm = TRUE))) %>%
  ungroup()

unique(dat$stock_clean)
d1 <- full_join(dat, d)
unique(d1$stock_clean)

d1 <- left_join(d1, slopes)

# # change order of facets to group more like species together
d2 <- d1 %>% arrange(desc(type), stock) # for grouping by taxa/type first
d2$stock_clean <- ifelse(!is.na(d2$stock_clean), d2$stock_clean, paste(d2$species, d2$gear))

unique(d2$stock_clean)

d2 <- d2 %>%
  group_by(stock_clean) %>%
  mutate(slope = ifelse(is.na(slope), mean(slope, na.rm = TRUE), slope)) # fill in assessment rows

select(d2, species, gear, slope) %>% distinct()

cols <- RColorBrewer::brewer.pal(3, name = "Dark2")
cols <- c("#00000050", cols)
names(cols) <- c("Assessment", "HBLL (inside)", "HBLL (outside)", "Synoptic trawl")

# d2$short_index <- stringr::str_remove_all(d2$index, " surveys")

e <- exp(d2$log_blrp[d2$stock == "quillback_BC_Outside"])
d2$q0.95_blrp[d2$stock == "quillback_BC_Outside" & d2$q0.95_blrp > max(e, na.rm = T) * 1.2] <- max(e, na.rm = T) * 1.2

e <- d2$est[d2$stock == "redstripe_rockfish_BC_North"]
d2$upr[d2$stock == "redstripe_rockfish_BC_North" & d2$upr > max(e, na.rm = T) * 2] <- max(e, na.rm = T) * 2

d2$est[d2$stock == "sablefish_BC"] <- NA # not trap survey
d2$lwr[d2$stock == "sablefish_BC"] <- NA # not trap survey
d2$upr[d2$stock == "sablefish_BC"] <- NA # not trap survey

make_surv_assess_plot <- function(dat) {
  dat %>%
    mutate(
      stock_clean =
        gsub("([a-zA-Z]+ [a-zA-Z]+) ([a-zA-Z 0-9]+)", "\\1\\\n\\2", stock_clean)
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
    geom_line(aes(year, est / mean_est, colour = gear)) +
    geom_ribbon(aes(year,
      ymin = lwr / mean_est, ymax = upr / mean_est,
      fill = gear
    ), alpha = 0.3) +
    ylab("Relative biomass or abundance estimate") +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    facet_wrap(vars(forcats::fct_reorder(stock_clean, -slope)),
      ncol = 5, scales = "free_y"
    ) + #
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
}

g <- make_surv_assess_plot(d2)
ggsave("figs/stock-vs-indices.pdf", width = 12, height = 12)
ggsave("figs/stock-vs-indices.png", width = 12, height = 12)

g <- d2 %>%
  filter(year >= 2000) %>%
  make_surv_assess_plot()
ggsave("figs/stock-vs-indices-recent.pdf", width = 10, height = 12)
ggsave("figs/stock-vs-indices-recent.png", width = 10, height = 12)
