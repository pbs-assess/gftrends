# update or call a previous version of sdmTMB
# remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
library(dplyr)

source("analysis-survey/make-grids.R")

to_fit <- readr::read_csv('data-raw/species-regions-tofit.csv')

# # # # add interesting (possibly expanding north?) species
# to_fit <- tribble(
#   ~species, ~region,
# # "Sandpaper Skate", "HBLL outside surveys"
# # "Kelp Greenling", "HBLL outside surveys"
#   "Shortbelly Rockfish", "Coast-wide trawl surveys"
#   # "Chilipepper", "QCS & WCVI",
#     # "Pacific Sand Lance", "Coast-wide trawl surveys"
#   )


# https://github.com/rstudio/rmarkdown/issues/1673
render_separately <- function(...) callr::r(
  function(...) rmarkdown::render(..., envir = globalenv()),
  args = list(...), show = TRUE)

fit_index <- function(region, species) {
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
  name <- "-RW-no-covs" # string describing model covariates
  region_name <- region
  try({
    render_separately("analysis-survey/01-index-new-deltas.Rmd",
      params = list(
        species = species,
        region = region,
        silent = TRUE
      ),
      output_file = paste0(spp, name, "-", region_name, ".html")
    )
  })
}

# test
# purrr::pwalk(to_fit4[1,], fit_index)
# beepr::beep()

is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"
cores <- parallel::detectCores() - 2
if (is_unix && !is_rstudio) {
  future::plan(future::multicore, workers = cores)
} else {
  future::plan(future::multisession, workers = cores)
}
options(future.rng.onMisuse = "ignore")

# furrr::future_pwalk(to_fit[c(36, 26),,drop = FALSE], fit_index)

furrr::future_pwalk(to_fit, fit_index)

future::plan(future::sequential)

# Run first part only if you have all indices saved in separate files
# AND the combined file needs updating
ind_dir = file.path("data-generated", "indices")
f <- list.files(path = ind_dir, pattern = "*.rds", full.names = TRUE)
f
all_indices <- do.call(rbind, lapply(f, readRDS))
glimpse(all_indices)
saveRDS(all_indices, file = file.path("data-generated", "sopo-combined-indices.rds"))


# # full list from 2 years ago
# list_species <- c(
# "Aleutian Skate",
# "Big Skate",
# "Longnose Skate",
# "Sandpaper Skate",
# "North Pacific Spiny Dogfish",
# "Brown Cat Shark",
# "Spotted Ratfish",
#
# "Pacific Tomcod",
# "Walleye Pollock",
# "Pacific Cod",
# "Lingcod",
# "Pacific Hake",
# "Buffalo Sculpin",
# "Cabezon",
# #"Pacifc Staghorn Sculpin",
# "Red Irish Lord",
# "Sturgeon Poacher",
# "Bigmouth Sculpin",
# "Kelp Greenling",
# "Threadfin Sculpin",
# "Bigfin Eelpout",
# "Black Eelpout",
# "Wattled Eelpout",
# "Blackbelly Eelpout",
# "Shiner Perch",
# "Snake Prickleback",
# # "Wolf Eel"
# "Pacific Sand Lance",
# "Pacific Herring",
# "Sablefish",
# "Bocaccio",
# "Canary Rockfish",
# "Chilipepper",
# "Copper Rockfish", # small sample
# "Darkblotched Rockfish", # need predictions still
# "Dusky Rockfish",
# "Greenstriped Rockfish",
# "Harlequin Rockfish",
# "Pacific Ocean Perch",
# "Pygmy Rockfish",
# "Quillback Rockfish",
# "Redbanded Rockfish",
# "Redstripe Rockfish",
# "Rosethorn Rockfish",
# "Rougheye/Blackspotted Rockfish Complex",
# "Sharpchin Rockfish",
# "Shortbelly Rockfish", # small sample
# "Shortraker Rockfish",
# "Silvergray Rockfish",
# "Splitnose Rockfish",
# "Widow Rockfish", # schooling
# "Yellowmouth Rockfish",
# "Yellowtail Rockfish", # schooling
# "Yelloweye Rockfish",
# "Longspine Thornyhead",
# "Shortspine Thornyhead",
#
# "Pacific Halibut",
# "Arrowtooth Flounder",
# "Butter Sole",
# "C-O Sole",
# "Curlfin Sole",
# "Dover Sole",
# "English Sole",
# "Flathead Sole",
# "Pacific Sanddab",
# "Petrale Sole",
# "Rex Sole",
# "Southern Rock Sole",
# "Slender Sole",
# "Sand Sole",
# "Starry Flounder"
# )
