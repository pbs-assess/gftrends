library("dplyr")
library("ggplot2")
library("grid")
options(scipen = 999)


####################
# Run first part only if you have all indices saved in separate files
# AND the combined file needs updating
mydir = paste0("analysis-survey/indices/")
myfiles <- list.files(path = mydir, pattern = "*.rds", full.names = TRUE)
myfiles
all_indices <- do.call(rbind, lapply(myfiles, readRDS))
glimpse(all_indices)
saveRDS(all_indices, file = paste0("analysis-survey/data/sopo-2021-indices.rds"))
####################
#
all_indices <- readRDS(paste0("analysis-survey/data/sopo-2021-indices.rds"))
# View(all_indices)

all_indices$se_test <- all_indices$se/all_indices$est

# haven't saved any without good gradients
all_indices$upr[all_indices$max_gradient > 0.01 ] <- NA
all_indices$upr[is.na(all_indices$se) ] <- NA
all_indices$upr[all_indices$se > 1.25 ] <- NA
# all_indices$upr[all_indices$se_test > 10 ] <- NA

all_indices$reliable_est <- all_indices$est
all_indices$reliable_est[is.na(all_indices$se)] <- NA
all_indices$reliable_est[all_indices$max_gradient > 0.01 ] <- NA

all_indices$reliable_est[all_indices$se > 1.25 ] <- NA
# all_indices$reliable_est[all_indices$se_test > 	10 ] <- NA

ggplot(all_indices, aes(year, est / 1000), group = region, fill = type, colour = type) +
  geom_line(aes(linetype = region, group = region, colour = type)) +
  geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000,
                  group = region, fill = type), alpha = 0.4) +
  ylab("Biomass (tonnes)") +
  facet_wrap(~species, scales = "free")

ggsave("analysis-survey/figs/pooled_survey_indices.pdf", width = 14, height = 8)
