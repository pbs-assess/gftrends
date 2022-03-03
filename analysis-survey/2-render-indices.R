library(dplyr)

to_fit1 <- tribble(
  ~species, ~region,
  "North Pacific Spiny Dogfish", "Coast-wide trawl surveys",
  "Arrowtooth Flounder", "Coast-wide trawl surveys",
  "Bocaccio", "Coast-wide trawl surveys",
  "Sablefish", "Coast-wide trawl surveys",
  "Shortspine Thornyhead", "Coast-wide trawl surveys",
  "Silvergray Rockfish", "Coast-wide trawl surveys",
  "Widow Rockfish", "Coast-wide trawl surveys",
  "Yellowmouth Rockfish", "Coast-wide trawl surveys",
  "Yellowtail Rockfish", "Coast-wide trawl surveys",
  "Big Skate", "Coast-wide trawl surveys",
  "Longnose Skate", "Coast-wide trawl surveys",
  "Spotted Ratfish", "Coast-wide trawl surveys",
  "Lingcod", "Coast-wide trawl surveys",
  "Petrale Sole", "Coast-wide trawl surveys",
  "Rex Sole", "Coast-wide trawl surveys",
  "Dover Sole", "Coast-wide trawl surveys",
  "English Sole", "Coast-wide trawl surveys",
  "Canary Rockfish", "Coast-wide trawl surveys",
  "Shortraker Rockfish", "Coast-wide trawl surveys"
)

to_fit2 <- tribble(
  ~species, ~region,
  "North Pacific Spiny Dogfish", "HBLL outside surveys",
  "Big Skate", "HBLL outside surveys",
  "Longnose Skate", "HBLL outside surveys",
  "Lingcod", "HBLL outside surveys",
  "Quillback Rockfish", "HBLL outside surveys",
  "Yelloweye Rockfish", "HBLL outside surveys"
)

to_fit3 <- to_fit2
to_fit3$region <- "HBLL inside surveys"

to_fit4 <- expand.grid(
  species = c(
    "Redstripe Rockfish",
    "Rougheye/Blackspotted Rockfish Complex"
  ),
  region = c("WCHG only", "QCS & WCVI"), stringsAsFactors = FALSE
)

to_fit4 <- expand.grid(
  species = c(
    "Walleye Pollock"
  ),
  region = c("HS & WCHG", "QCS & WCVI"),
  stringsAsFactors = FALSE
)

make_dat <- function(r, s) {
  expand.grid(
    species = s,
    region = r,
    stringsAsFactors = FALSE
  )
}

list_regions <- c("Both odd year trawl surveys", "WCVI only")
list_species <- c(
  "Pacific Cod"
)
to_fit5 <- make_dat(list_regions, list_species)

list_regions <- c("WCHG only", "QCS & WCVI")
list_species <- c(
  "Redstripe Rockfish",
  "Rougheye/Blackspotted Rockfish Complex"
)
to_fit5 <- bind_rows(to_fit5, make_dat(list_regions, list_species))

list_regions <- c("QCS only", "WCVI only", "WCHG only")
list_species <- c(
  "Pacific Ocean Perch"
)
to_fit5 <- bind_rows(to_fit5, make_dat(list_regions, list_species))

list_regions <- c("QCS only", "HS only")
list_species <- c(
  "Southern Rock Sole"
)
to_fit5 <- bind_rows(to_fit5, make_dat(list_regions, list_species))

list_regions <- c("HS & WCHG", "QCS & WCVI")
list_species <- c(
  "Walleye Pollock"
)
to_fit5 <- bind_rows(to_fit5, make_dat(list_regions, list_species))

to_fit <- bind_rows(
  list(
    to_fit1,
    to_fit2,
    to_fit3,
    to_fit4,
    to_fit5
  )
)

# https://github.com/rstudio/rmarkdown/issues/1673
render_separately <- function(...) callr::r(
  function(...) rmarkdown::render(..., envir = globalenv()),
  args = list(...), show = TRUE)

fit_index <- function(region, species) {
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
  name <- "-RW-no-covs" # string describing model covariates
  region_name <- region
  try({
    render_separately("analysis-survey/1-index-standardization.Rmd",
      params = list(
        species = species,
        region = region,
        delta_model = TRUE,
        update_model = TRUE,
        update_index = TRUE,
        silent = TRUE
        # update_index = FALSE
      ),
      output_file = paste0(spp, name, "-", region_name, "-delta.html")
    )
  })
}

# purrr::pwalk(to_fit[31,,drop=FALSE], fit_index)

set.seed(1)
i <- sample(seq_len(nrow(to_fit)), nrow(to_fit))
to_fit <- to_fit[i, ]

# if (Sys.info()[["user"]] == "seananderson") {
#   to_fit <- to_fit[1:28, ]
# } else {
#   to_fit <- to_fit[29:nrow(to_fit), ]
# }

is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"
if (is_unix && !is_rstudio) {
  future::plan(future::multicore, workers = 6L)
} else {
  future::plan(future::multisession, workers = 3L)
}
options(future.rng.onMisuse = "ignore")

# furrr::future_pwalk(to_fit[c(36, 26),,drop = FALSE], fit_index)
furrr::future_pwalk(to_fit, fit_index)

future::plan(future::sequential)

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
