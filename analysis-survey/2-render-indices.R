# getwd()


dir.create(file.path(here::here("analysis-survey", "data")))
dir.create(file.path(here::here("analysis-survey", "figs")))
dir.create(file.path(here::here("analysis-survey", "indices")))

dir.create(file.path(here::here("analysis-survey", "html")))


list_regions <- c("Both odd year trawl surveys", "WCVI only")
list_species <- c(
  "Pacific Cod" #3CD (WCVI), 5ABCD (Both odd year)
)

list_regions <- c("WCHG only", "QCS & WCVI") #"North (PMFC 5DE)", "South (PMFC 3CD5ABC)",
list_species <- c(
  "Redstripe Rockfish", # N (PMFC 5DE) and South (PMFC 3CD5ABC)
  "Rougheye/Blackspotted Rockfish Complex"# N = HS (technically only north half) and WCHG, and S + QCS and WCVI
)

list_regions <- c("QCS only", "WCVI only", "WCHG only") #"North (PMFC 5DE)", "South 5 only (PMFC 5ABC)",
list_species <- c(
  "Pacific Ocean Perch"
)

list_regions <- c("QCS only", "HS only")
list_species <- c(
  "Southern Rock Sole" # 5AB (QCS),  5CD (HS) low density along boundary so close enough
)

list_regions <- c("HS & WCHG", "QCS & WCVI")
list_species <- c(
  "Walleye Pollock" # N = HS and WCHG, and S + QCS and WCVI
)


# initial coastwide trawl models
list_regions <- c("Coast-wide trawl surveys")
list_species <- c(
  "North Pacific Spiny Dogfish",
  "Arrowtooth Flounder",
  "Bocaccio",
  "Sablefish",
  "Shortspine Thornyhead",
  "Silvergray Rockfish",
  "Widow Rockfish",
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish"

  # "Quillback Rockfish", # outside and WCVI_Inside?
  # redo with specific subsets of surveys?
  # "Pacific Ocean Perch", #3CD (WCVI), 5ABC, 5DE (Both odd year + HG)
  # "Pacific Cod", #3CD (WCVI), 5ABCD (Both odd year)
  # "Southern Rock Sole", # 5AB (QCS),  5CD (HS)
  # redo with N-S split?
  # "Walleye Pollock", # N and S?
  # "Redstripe Rockfish", # N and S?
  # "Rougheye/Blackspotted Rockfish Complex"# N and S?
)

# list_regions <- c("HBLL outside surveys")
# list_species <- c(
#   "North Pacific Spiny Dogfish",
#   # "Arrowtooth Flounder",
#   # "Bocaccio",
#   "Sablefish",
#   # "Shortspine Thornyhead",
#   # "Silvergray Rockfish",
#   # "Widow Rockfish",
#   # "Yellowtail Rockfish",
#   # "Yellowmouth Rockfish",
#   ## ones with potentially awkward splits
#   # "Walleye Pollock", # N and S?
#   # "Pacific Cod", #3CD (WCVI), 5ABCD (Both odd year)
#   # "Southern Rock Sole", # 5AB (QCS),  5CD (HS)
#   # "Redstripe Rockfish", # N and S?
#   # "Rougheye/Blackspotted Rockfish Complex",# N and S?
#   # "Lingcod", #4B = inside
#   "Quillback Rockfish", # outside and WCVI_Inside?
#   "Yelloweye Rockfish" # 4B = inside and outside
# )
#
# list_regions <- c("HBLL inside surveys")
# list_species <- c(
#   "North Pacific Spiny Dogfish",
#   "Lingcod", #4B = inside
#   "Quillback Rockfish", # outside and WCVI_Inside?
#   "Yelloweye Rockfish" # 4B = inside
#   # "Big Skate"
# )

# also need to come up with a list of other species of interest...

list_regions <- c("Coast-wide trawl surveys")
list_species <- c(
"Big Skate",
"Longnose Skate",
"Spotted Ratfish",
"Lingcod",
"Petrale Sole",
"Rex Sole",
"Dover Sole",
"English Sole",
"Canary Rockfish",
# "Shortbelly Rockfish",
"Shortraker Rockfish"
)

list_regions <- c("HBLL outside surveys", "HBLL inside surveys")
list_regions <- c("HBLL inside surveys")
list_species <- c("Big Skate", "Longnose Skate", "Lingcod")

# #HBLL surveyed species
# list_regions <- c("Coast-wide trawl surveys")
# list_species <- c(
#     "Quillback Rockfish", # outside and WCVI_Inside?
#     "Yelloweye Rockfish" # 4B = inside and outside
# )


env <- new.env() # parent = baseenv()

for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    name <- "-RW-no-covs" # string describing model covariates
    region_name <- list_regions[r_h]
    try({
      rmarkdown::render("analysis-survey/1-index-standardization.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          delta_model = FALSE,
          update_model = FALSE,
          update_index = TRUE
          # update_index = FALSE
        ),
        output_file = paste0(spp, name, "-", region_name, "-delta.html"),
        envir = env
      )
    })
  }
}


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
