# update or call a previous version of sdmTMB
# remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
library(dplyr)

source("analysis-survey/00-make-grids.R")

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

is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"
cores <- parallel::detectCores()/2 - 1
if (is_unix && !is_rstudio) {
  future::plan(future::multicore, workers = cores)
} else {
  future::plan(future::multisession, workers = cores)
}
options(future.rng.onMisuse = "ignore")

furrr::future_pwalk(to_fit, fit_index)

future::plan(future::sequential)

# Run first part only if you have all indices saved in separate files
# AND the combined file needs updating
ind_dir = file.path("data-generated", "indices")
f <- list.files(path = ind_dir, pattern = "*.rds", full.names = TRUE)
f
x <- lapply(f, readRDS)
all_indices <- do.call(rbind, x)
glimpse(all_indices)
saveRDS(all_indices, file = file.path("data-generated", "sopo-combined-indices.rds"))
