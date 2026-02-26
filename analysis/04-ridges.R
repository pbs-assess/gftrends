library(ggplot2)
library(dplyr)
library(ggsidekick)
library(ggridges)
# source("analysis/stock_df.R")

d0 <- readr::read_rds("data-generated/all-mcmc.rds")

# d |> group_by(species, region) |>
#   summarise(last_mcmc_year = max(year)) |>
#   readr::write_csv("~/Downloads/spp.csv")

d <- rename(d0, assess_stock = stock)

lu <- readr::read_csv("data-raw/surveys_to_assessments.csv")

x1 <- lu$assess_stock |> unique() |> sort()
x2 <- d$assess_stock |> unique() |> sort()
stopifnot(identical(x1, x2))

d <- left_join(d, select(lu, -species, -region) |> distinct(), by = join_by(assess_stock))

d <- mutate(d, stock_clean = paste(panel_title1, panel_title2))
# d2 |> select(stock_clean) |> distinct()

d$stock_clean <- gsub(" \\(VI Inside\\)", "", d$stock_clean)

d |>
  select(species, region, type, stock_clean) |>
  distinct() |>
  as.data.frame()

last <- readr::read_csv("data-raw/last-assess-years.csv")

x1 <- select(last, species, region) |> distinct() |> arrange(species, region) |> as.data.frame()
x2 <- select(d, species, region) |> distinct() |> arrange(species, region) |> as.data.frame()
stopifnot(identical(x1$species, x2$species))
stopifnot(identical(x1$region, x2$region))

d <- left_join(d, last, by = join_by(species, region))

d <- group_by(d, species, region) |>
  mutate(last_year_use = last_mcmc_year)
  # mutate(last_year_use = ifelse(last_mcmc_year > max(year), last_mcmc_year, max(year)), max_year = max(year))

d |>
  select(species, region, type, stock_clean, last_year_use) |>
  arrange(-last_year_use) |>
  distinct() |>
  as.data.frame()

dat <- d %>%
  group_by(species, region) %>%
  filter(year == last_year_use) %>%
  mutate(
    mean_blrp = mean(blrp, na.rm = TRUE),
    mean_bbmsy = mean(bbmsy, na.rm = TRUE),
    mean_busr = mean(busr, na.rm = TRUE)
  )
# ungroup() %>%
# mutate(stock = paste(species, region, sep = "_")) %>%
# arrange(year, stock) %>%
# mutate(stock = gsub(" ", "_", stock)) %>%
# mutate(stock = gsub("-", "_", stock)) %>%
# left_join(stock_df)

dat |>
  select(species, region, type, stock_clean) |>
  distinct() |>
  as.data.frame()

dat_sum <- group_by(dat, stock_clean) %>%
  summarize(
    p_lrp = mean(blrp < 1), p_usr = mean(busr < 1),
    p_bbmsy = mean(bbmsy < 1, na.rm = TRUE)
  ) %>%
  arrange(-p_lrp)

saveRDS(dat_sum, "data-generated/p-thresh.rds")

dat_sum |>
  ungroup() |>
  knitr::kable(digits = 2, format = "markdown")

lines <- tibble(
  ratio = c("B/B[MSY]", "B/B[MSY]", "B/B[MSY]", "B/LRP", "B/USR"),
  ratio_value = c(0.4, 0.8, 1, 1, 1)
) %>% mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]"))))

data_plot <- dat %>%
  tidyr::pivot_longer(
    cols = c(blrp, busr, bbmsy),
    names_to = "ratio", values_to = "ratio_value"
  ) %>%
  mutate(stock_clean = forcats::fct_reorder(stock_clean, year)) %>%
  mutate(ratio = gsub("bbmsy", "B/B[MSY]", ratio)) %>%
  mutate(ratio = gsub("blrp", "B/LRP", ratio)) %>%
  mutate(ratio = gsub("busr", "B/USR", ratio)) %>%
  mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]")))) %>%
  group_by(ratio) %>%
  dplyr::filter(
    ratio_value < quantile(ratio_value, probs = 0.98, na.rm = TRUE),
    ratio_value > quantile(ratio_value, probs = 0.005, na.rm = TRUE)
  )

years <- select(data_plot, ratio, stock_clean, year) %>%
  distinct() %>%
  filter(ratio == "B/USR") %>%
  mutate(ratio_value = 5) %>%
  mutate(ratio = factor(ratio, levels = (c("B/LRP", "B/USR", "B/B[MSY]"))))

scales_fun <- function(x) sprintf("%.1f", x)

data_plot <- data_plot |>
  filter(ratio != "B/B[MSY]") |>
  droplevels()
years <- years |>
  filter(ratio != "B/B[MSY]") |>
  droplevels()
lines <- lines |>
  filter(ratio != "B/B[MSY]") |>
  droplevels()

data_plot <- data_plot |>
  group_by(stock_clean) |>
  mutate(prob_usr_0.5 = mean(b > usr)) |>
  ungroup()

years2 <- years
years2$ratio_value <- 0

g <- data_plot %>%
  mutate(stock_clean = paste0(stock_clean, " (", last_year_use, ")")) |>
  # mutate(stock_clean = paste0(stock_clean, "\n", year, ")")) |>
  mutate(stock_clean = forcats::fct_reorder(stock_clean, year)) |>
  ggplot(aes(x = ratio_value, y = stock_clean, fill = mean_blrp, group = stock_clean)) +
  # ggplot(aes(x = ratio_value, y = stock_clean, fill = prob_usr_0.5, group = stock_clean)) +
  geom_vline(
    data = lines, mapping = aes(xintercept = ratio_value),
    lty = 2, lwd = 0.45, colour = "grey55"
  ) +
  facet_wrap(vars(ratio), labeller = label_parsed, scales = "free_x") +
  geom_density_ridges2(scale = 4, alpha = 0.7, colour = "grey30") +
  scale_x_continuous(trans = "sqrt", breaks = c(0.2, 1, 2, 5, 10), labels = scales_fun) +
  # scale_fill_viridis_c(direction = 1, option = "D", end = 1) +
  scale_fill_viridis_c(direction = -1, option = "B", end = 0.71) +
  # scale_fill_distiller(palette = "RdBu", direction = 1) +
  # scale_fill_distiller(palette = "Spectral", direction = 1) +
  # scale_fill_gradient2(midpoint = 0.5) +
  theme_sleek() +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Ratio value", fill = "Mean\nB/LRP") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 9.5),
    axis.text.x = element_text(size = 7.5),
    axis.text.y = element_text(size = 7.5),
    axis.text.y.left = element_text(vjust = -1),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(colour = "grey70", linewidth = 0.5)
  ) +
  # geom_text(
  #   mapping = aes(x = ratio_value, y = stock_clean, label = year),
  #   data = years, size = 2.6, color = "grey20", nudge_y = 0.50,
  #   inherit.aes = FALSE
  # ) +
  guides(fill = "none")
# geom_text(
#   mapping = aes(x = ratio_value, y = stock_clean, label = year),
#   data = years2, size = 2.6, color = "grey20", nudge_y = 0.50,
#   inherit.aes = FALSE
# )
# annotate(geom = "text", x = 0, y = years$stock_clean, label = "test")

ggsave("figs/ridges.pdf", width = 5.5, height = 7.6)
ggsave("figs/ridges.png", width = 5.5, height = 7.6)

saveRDS(data_plot, file = "data-generated/ridges-data.rds")
