library(dplyr)
library(ggplot2)
source("analysis/utils.R")
source("analysis/stock_df2.R")

####################
# Run first part only if you have all indices saved in separate files
# and they have been updated
mydir <- paste0("analysis-survey/indices/")
myfiles <- list.files(path = mydir, pattern = "*.rds", full.names = TRUE)
myfiles
all_indices <- do.call(bind_rows, lapply(myfiles, readRDS))
glimpse(all_indices)
saveRDS(all_indices, file = paste0("analysis-survey/data/sopo-2021-indices.rds"))
####################

all_indices <- readRDS(paste0("analysis-survey/data/sopo-2021-indices.rds")) %>%
  rename(gear = type)
dat <- readRDS("data-generated/b-status-dat.rds") %>%
  filter(year >= 2000) %>%
  rename(dfo_area = region) %>%
  select(-species) %>%
  group_by(stock) %>%
  mutate(mean_blrp = mean(exp(log_blrp), na.rm = T),
         ratio_ci = q0.95_blrp/q0.05_blrp ) %>%
  ungroup()

dat <- left_join(dat, stock_df)

d <- left_join(all_indices, stock_df, by = c("species", "region")) %>%
  rename(index = region, model_type = model) %>%
  mutate(model = paste(index, model_type))%>%
  group_by(species, index, model_type, model) %>%
  mutate(mean_est = mean(est, na.rm = T)) %>%
  ungroup()

d1 <- full_join(dat, d)

d1 <- filter(d1, !is.na(stock_clean))

# # change order of facets to group more like species together
d2 <- d1 %>% arrange(desc(type), stock) # for grouping by taxa/type first
# stock_df <- stock_df %>% arrange(stock) # for alphabetical order
d2 <- d2 %>% mutate(stock_clean = factor(stock_clean,
  levels = as.character(unique(stock_df$stock_clean))
))

# clean up CI that were too wide?
d2$upr[is.na(d2$se) ] <- NA
# d2$upr[d2$se > 1.2 ] <- NA
d2$q0.95_blrp[d2$ratio_ci > 15 ] <- NA

# range(d2$sd_log_blrp, na.rm = T)
d2 %>% filter(model_type == "delta-gamma") %>%
ggplot() +
  geom_ribbon(aes(year,
    ymin = q0.05_blrp / mean_blrp, ymax = q0.95_blrp / mean_blrp,
    group = stock_clean
  ), fill = "black", alpha = 0.2) +
  geom_line(aes(year, exp(log_blrp) / mean_blrp, group = stock_clean),
    linetype = 3, alpha = 0.4, colour = "black"
  ) +
  geom_line(aes(year, est / mean_est, #linetype = model_type, # linetype = gear,
                group = model, colour = gear)) +
  geom_ribbon(aes(year,
    ymin = lwr / mean_est, ymax = upr / mean_est,
    group = model, fill = gear
  ), alpha = 0.3) +
  ylab("Relative biomass estimates") +
  # scale_linetype_manual(name = "Index type", values = c(2,3,1,4)) +
  scale_color_brewer(name = "Index type", palette = "Dark2") +
  scale_fill_brewer(name = "Index type", palette = "Dark2") +
  facet_wrap(~stock_clean, ncol = 5, scales = "free_y") + #
  ggsidekick::theme_sleek() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("figs/stock_vs_indices.pdf", width = 12, height = 10)
ggsave("figs/stock_vs_indices.png", width = 12, height = 10)


d %>% filter(index == "Coast-wide trawl surveys") %>%
ggplot(aes(year, est / mean_est, group = model)) +
  geom_line(aes(linetype = gear, colour = model_type)) +
  geom_ribbon(aes(
    ymin = lwr / mean_est, ymax = upr / mean_est, fill = model_type
  ), alpha = 0.4) +
  ylab("Relative Biomass") +
  # scale_linetype_manual(values = c(2, 3, 1)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~species, scales = "free_y") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank())

ggsave("analysis-survey/figs/stock_indices.pdf", width = 14, height = 8)
