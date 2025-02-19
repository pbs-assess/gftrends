dir.create("data-generated", showWarnings = FALSE)

library(dplyr)
library(ggplot2)
library(gfplot)
library(gfdata)
library(sdmTMB)

theme_set(gfplot::theme_pbs(base_size = 14))

# Load grids
synoptic_grid <- readRDS(here::here("data-generated/synoptic-grid.rds"))
hbll_out_grid <- readRDS(here::here("data-generated/hbll-outside-grid.rds"))
hbll_ins_grid <- readRDS(here::here("data-generated/hbll-inside-grid.rds"))

filter_grid <- function(region) {
  out_grid <- switch(region, 
    "Both odd year trawl surveys" = synoptic_grid |> filter(survey %in% c("SYN QCS", "SYN HS")), 
    "QCS only" = synoptic_grid |> filter(survey == "SYN QCS"), 
    "HS only" = synoptic_grid |> filter(survey == "SYN HS"), 
    "WCVI only" = synoptic_grid |> filter(survey == "SYN WCVI"), 
    "WCHG only" = synoptic_grid |> filter(survey == "SYN WCHG"), 
    "HS & WCHG" = synoptic_grid |> filter(survey %in% c("SYN HS", "SYN WCHG")),
    "QCS & WCVI" = synoptic_grid |> filter(survey %in% c("SYN QCS", "SYN WCVI")),
    "Coast-wide trawl surveys" = synoptic_grid |> filter(survey %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG")), 
    "North (PMFC 5DE)" = synoptic_grid |> filter((survey == "SYN HS" & Y > 5907.814) | 
                                                 survey == "SYN WCHG"), 
    "South (PMFC 3CD5ABC)" = synoptic_grid |> filter((survey == "SYN HS" & Y < 5907.814) | 
                                                     survey %in% c("SYN QCS", "SYN WCVI")), 
    "South 5 only (PMFC 5ABC)" = synoptic_grid |> filter((survey == "SYN HS" & Y < 5907.814) | 
                                                         survey == c("SYN QCS")), 

    "HBLL outside surveys" = hbll_out_grid, 
    "HBLL inside surveys" = hbll_ins_grid
  )
}

# Prepare data
survey_dat <- readRDS("data-raw/all_surv_catch.rds")
bait_counts <- readRDS("data-raw/bait-counts.rds")

survey_dat <- 
  left_join(survey_dat, bait_counts) |>
  mutate(units )


prep_data <- function(survey_dat, species, region)
  syn_regions <- c(
    "Both odd year trawl surveys",
    "QCS only", 
    "HS only", 
    "WCVI only", 
    "WCHG only", 
    "Coast-wide trawl surveys"
  )

  if (region %in% syn_regions) {
    d <- survey_dat %>%
      dplyr::filter(survey_abbrev %in% survey)
    units <- "density_kgpm2"
    d$density <- d[[units]] * 1e6 # change from per m2 to per km2
    family <- tweedie(link = "log") # to be used if not a delta model
} else {
  d <- all_data %>%
    dplyr::filter(survey_abbrev %in% survey)
  units <- "density_ppkm2"
  d$density <- d[[units]] # change from per m2 to per km2
  family <- tweedie(link = "log")
  # family <- nbinom2(link = 'log') # could use this instead, but would need to change units and add offset
}

if (region == "North (PMFC 5DE)") {
  d <- d %>% dplyr::filter(survey_abbrev == "SYN WCHG" | latitude > 53.3)
}

if (region == "South (PMFC 3CD5ABC)") {
  d <- d %>% dplyr::filter(survey_abbrev != "SYN WCHG" & latitude < 53.3)
}

if (region == "South 5 only (PMFC 5ABC)") {
  d <- d %>% dplyr::filter(survey_abbrev %in% c("SYN QCS", "SYN HS") & latitude < 53.3)
}

positive_sets <- dplyr::filter(d, density != 0)
prop_pos <- round(nrow(positive_sets) / nrow(d), digits = 3)
data <- d %>% dplyr::filter(survey_abbrev %in% unique(positive_sets$survey_abbrev))
ssid_string <- paste0(unique(data$survey_series_id), collapse = "n")
surv_string <- paste0(unique(data$survey_abbrev), collapse = ", ")
data <- sdmTMB::add_utm_columns(data, c("longitude", "latitude"), utm_crs = 32609)


# make mesh
cutoff <- 20
if (region %in% c("HBLL outside surveys")) {
  cutoff <- 15
}
if (region %in% c("HBLL inside surveys", "QCS only", "HS only", "WCVI only")) {
  cutoff <- 10
}
if (region %in% c("WCHG only")) {
  cutoff <- 5
}

data <- data %>% mutate(present = ifelse(density > 0, 1, 0))
spde <- make_mesh(data, xy_cols = c("X", "Y"), cutoff = cutoff)

dir.create(here::here("data-generated/models"), showWarnings = FALSE)


Run sdmTMB model

```{r}
year_range <- range(data$year)
all_years <- data.frame(year = year_range[1]:year_range[2])
missing_years <- anti_join(all_years, data, by = "year") %>%
  select(year) %>%
  unique()
missing_years <- missing_years$year
if (length(missing_years) == 0L) missing_years <- NULL

tictoc::tic()
try({
  m1 <- sdmTMB(
    density ~ 1,
    data,
    mesh = spde,
    time = "year",
    extra_time = missing_years,
    family = delta_gamma(),
    spatial = "on",
    spatiotemporal = "RW",
    silent = params$silent,
    control = sdmTMBcontrol(newton_loops = 1L)
  )
})
tictoc::toc()
if (!exists("m1")) { m1 <- NULL}

saveRDS(m1, file = here::here(paste0("data-generated/models/m-", spp, "-", ssid_string, name, "-delta.rds")))
ok <- all(unlist(sanity(m1)))

if (!ok) {
  tictoc::tic()
  try({
    m <- sdmTMB(
      density ~ 1,
      data,
      mesh = spde,
      time = "year",
      extra_time = missing_years,
      family = family,
      spatial = "on",
      spatiotemporal = "RW",
      silent = params$silent,
      control = sdmTMBcontrol(newton_loops = 1L)
    )
  })
  tictoc::toc()
  if (!exists("m")) { m <- NULL}
  saveRDS(m, file = here::here(paste0("data-generated/models/m-", spp, "-", ssid_string, name, ".rds")))
} else {
  m <- m1
}
```

```{r}
p <- function(x) {
  out <- tryCatch(
    {
      print(x)
    },
    error = function(e) NULL
  )
  return(out)
}
p(m)
sanity(m)
```

Get simulated index
```{r}
dir.create(here::here("data-generated/indices"), showWarnings = FALSE)

make_grid <- function(x, years) {
  .x <- filter(x, year == min(year)) # take one
  years <- sort(unique(years))
  .nd <- do.call(
    "rbind",
    replicate(length(years), .x, simplify = FALSE)
  )
  .nd[["year"]] <- rep(years, each = nrow(.x))
  .nd
}

converged <- function(fit) {
  all(unlist(sanity(fit)))
}

if (!is.null(m)) {
  if (converged(m)) {
    fitted_yrs <- sort(unique(m$data$year))
    nd <- make_grid(nd_all, years = fitted_yrs)

    if (type == "Synoptic trawl") {
      nd <- filter(nd, survey %in% unique(positive_sets$survey_abbrev))
    } else {
      nd <- filter(nd, ssid %in% unique(positive_sets$survey_series_id))
    }
    nd <- na.omit(nd)
    nd$year <- as.integer(nd$year)

    samples <- data %>%
      count(year, survey_abbrev) %>%
      group_by(year) %>%
      mutate(freq = n / sum(n), total = sum(n)) %>%
      filter(n == max(n)) %>%
      select(year, dominant_surv = survey_abbrev, total, freq)
    
    p1 <- predict(m, newdata = nd, return_tmb_object = TRUE)
    i1 <- get_index(p1, area = nd$area, bias_correct = TRUE)
    
    i1 <- i1 %>% mutate(
      species = species,
      type = type,
      region = region,
      surveys = paste(unique(positive_sets$survey_abbrev), collapse = ", "),
      ssids = paste(unique(positive_sets$survey_series_id), collapse = ", "),
      ssid_string = ssid_string,
      model = "delta-gamma",
      max_grad = max(m$gradients)
    )
    i1 <- left_join(i1, samples)
    saveRDS(i1, here::here(paste0("data-generated/indices/i-", spp, "-", ssid_string, name, ".rds")))
  }
}
```

```{r}
dir.create(here::here("figs"), showWarnings = FALSE)

if (exists("i1")) {
  g <- ggplot(i1, aes(year, est / 1000)) +
    geom_line(colour = "red") +
    geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000), alpha = 0.2, fill = "red") +
    coord_cartesian(ylim = c(0, NA))
  g <- g + ylab("Biomass (tonnes)") +
    ggtitle(paste(species), subtitle = paste0(region, " (", surv_string))
  ggsave(here::here(paste0("figs/", spp, "-", ssid_string, name, ".png")), height = 4, width = 6)
  g
}
```
