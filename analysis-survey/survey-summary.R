library("dplyr")
library("ggplot2")
library("grid")
library("gfranges")
options(scipen = 999)

hbll <- readRDS("data-raw/all-inside-hbll-2021.rds")
sb <- readRDS("data-raw/shortbelly.rds")
all_data <- readRDS("data-raw/all-survey-sets-2021.rds") %>%
  rename(survey_series_id = survey_series_id.x) %>%
  bind_rows(hbll) %>%
  bind_rows(sb)

data <- all_data %>% filter(year == 2021)

# number of tows
length(unique(data$fishing_event_id))

# which surveys
unique(data$survey_series_id)

# trawl summary stats
data2 <- data %>% filter(survey_series_id %in% c(1,3,4, 16)) %>% select(survey_series_id, fishing_event_id, species_common_name, catch_weight) %>% unique()
# total biomass
sum(data2$catch_weight)
# total species
length(unique(data2$species_common_name[data2$catch_weight>0]))

#hbll summary stats
data3 <- data %>% filter(!(survey_series_id %in% c(1,3,4, 16))) %>% select(survey_series_id, fishing_event_id, species_common_name, catch_count) %>% unique()
# total count
sum(data3$catch_count)
# total species
length(unique(data3$species_common_name[data3$catch_count>0]))

# split by survey area
HS <- data2 %>% filter(survey_series_id == 3)

sum(HS$catch_weight)
sum(HS$catch_weight)/length(unique(HS$fishing_event_id))

HS <- HS %>% group_by(species_common_name) %>% mutate(total_catch = sum(catch_weight))
HS %>% select(species_common_name, total_catch) %>% unique() %>% View

# split by survey area
QCS <- data2 %>% filter(survey_series_id == 1)

sum(QCS$catch_weight)
sum(QCS$catch_weight)/length(unique(QCS$fishing_event_id))

QCS <- QCS %>% group_by(species_common_name) %>% mutate(total_catch = sum(catch_weight))
QCS %>% select(species_common_name, total_catch) %>% unique() %>% View



raw_dat <- filter(data, species_common_name == "north pacific spiny dogfish") %>%
  tidy_survey_sets(survey = c("SYN HS","SYN WCVI", "SYN QCS", "SYN WCHG"),
                   # survey = c("HBLL INS N","HBLL INS S"),
                   years = c(2021),
                   density_column = "density_kgpm2")

raw_HBLL <- filter(data, species_common_name == "north pacific spiny dogfish") %>%
  tidy_survey_sets(survey = c("HBLL INS N","HBLL INS S"),
                 years = c(2021),
                 density_column = "density_ppkm2")

nd_hbll <- readRDS(("data-generated/nd_hbll_inside_index.rds"))%>%
  mutate(combined = depth)

nd_all <- readRDS(here::here("data-generated/nd_whole_coast_index.rds")) %>%
  mutate(combined = depth, akima_depth = depth)

##### just HBLL MAP
gfranges:::plot_survey_sets(nd_hbll, raw_HBLL, fill_column = "combined",
  fill_scale = ggplot2::scale_fill_viridis_c(
    option = "D", direction = -1, begin = 0.4, end = 0.7,
    guide = guide_colorbar(reverse = TRUE)), #trans= "sqrt",
  colour_scale = ggplot2::scale_colour_viridis_c(
      option = "D", direction = -1, begin = 0.4, end = 0.7,
      guide = guide_colorbar(reverse = TRUE)),
  pos_pt_col = "black", #"#FFFFFF60",
  bin_pt_col = "black",#"#FFFFFF40",
  pos_pt_fill = "black",#"#FFFFFF05",
  pt_size_range = c(2, 2),
  show_legend = T,
  extrapolate_depth = T,
  extrapolation_buffer = 0,
  show_model_predictions = F,
  show_raw_data = TRUE,
  utm_zone = 9,
  fill_label = "Depth (m)",
  pt_label = "Tow density (kg/km^2)",
  rotation_angle = 0,
  rotation_center = c(500, 5700),
  show_axes = TRUE,
  xlim = NULL,
  ylim = NULL,
  x_buffer = c(-5, 5),
  y_buffer = c(-5, 5),
  north_symbol = F,
  north_symbol_coord = c(810, 5630),
  north_symbol_length = 10,
  cell_size = 2,
  circles = T) + guides(size = "none") + theme(legend.position = c(0.2,0.2))

ggsave("map-tows-2021-hbll-ins.png", width = 6, height = 6)
ggsave("map-tows-2021-hbll-ins.pdf", width = 6, height = 6)


# adds the HBLL sets to the synoptic map
raw_dat <- bind_rows(raw_dat, raw_HBLL)

# plots depth for synoptic and set points for both
gfranges:::plot_survey_sets(nd_all, raw_dat, fill_column = "combined",
  fill_scale = ggplot2::scale_fill_viridis_c(
    option = "D", direction = -1, guide = guide_colorbar(reverse = TRUE)), #trans= "sqrt",
  colour_scale = ggplot2::scale_colour_viridis_c(
    option = "D", direction = -1, guide = guide_colorbar(reverse = TRUE)),
  pos_pt_col = "black", #"#FFFFFF60",
  bin_pt_col = "black",#"#FFFFFF40",
  pos_pt_fill = "black",#"#FFFFFF05",
  pt_size_range = c(0.8, 0.8),
  show_legend = T,
  extrapolate_depth = T,
  extrapolation_buffer = 0,
  show_model_predictions = T,
  show_raw_data = TRUE,
  utm_zone = 9,
  fill_label = "Depth (m)",
  pt_label = "Tow density (kg/km^2)",
  rotation_angle = 0,
  rotation_center = c(500, 5700),
  show_axes = TRUE,
  xlim = NULL,
  ylim = NULL,
  x_buffer = c(-65, 5),
  y_buffer = c(-5, 5),
  north_symbol = T,
  north_symbol_coord = c(550, 6000),
  north_symbol_length = 30,
  cell_size = 0.6, circles = T) + guides(size = "none") + theme(legend.position = c(0.2,0.2))

ggsave("map-tows-2021-all.png", width = 6, height = 6)
ggsave("map-tows-2021-all.pdf", width = 6, height = 6)


# interesting species?
chili <- data %>% filter(species_common_name == "chilipepper")
sum(chili$catch_weight)
all_chili <- all_data %>% filter(species_common_name == "chilipepper")
sum(chili$catch_weight)/sum(all_chili$catch_weight)
unique(all_chili$year[all_chili$catch_weight>0])

sb <- data %>% filter(species_common_name == "shortbelly rockfish")
sum(sb$catch_weight)
all_sb <- all_data %>% filter(species_common_name == "shortbelly rockfish")
sum(sb$catch_weight)/sum(all_sb$catch_weight)
unique(all_sb$year[all_sb$catch_weight>0])



