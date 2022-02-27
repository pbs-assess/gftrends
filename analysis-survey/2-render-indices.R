# getwd()


dir.create(file.path(here::here("analysis-survey", "data")))
dir.create(file.path(here::here("analysis-survey", "figs")))
dir.create(file.path(here::here("analysis-survey", "html")))

env <- new.env() # parent = baseenv()

# initial coastwide trawl models
list_regions <- c("All synoptic surveys")
list_species <- c(
  "Arrowtooth Flounder",
  "Bocaccio",
  "Sablefish",
  "Shortspine Thornyhead",
  "Silvergray Rockfish",
  "Widow Rockfish",
  "Yellowtail Rockfish",
  "Yellowmouth Rockfish",
  "Quillback Rockfish", # outside and WCVI_Inside?

  # redo with specific subsets of surveys?
  "Pacific Ocean Perch", #3CD (WCVI), 5ABC, 5DE (Both odd year + HG)
  "Pacific Cod", #3CD (WCVI), 5ABCD (Both odd year)
  "Southern Rock Sole", # 5AB (QCS),  5CD (HS)

  # redo with N-S split?
  "Walleye Pollock", # N and S?
  "Redstripe Rockfish", # N and S?
  "Rougheye/Blackspotted Rockfish Complex"# N and S?
)

# list_regions <- c("HBLL outside surveys")
# list_species <- c(
#   "Arrowtooth Flounder",
#   "Bocaccio",
#   "Sablefish",
#   "Shortspine Thornyhead",
#   "Silvergray Rockfish",
#   "Widow Rockfish",
#   "Yellowtail Rockfish",
#   "Yellowmouth Rockfish",
#   "Walleye Pollock", # N and S?
#   "Pacific Cod", #3CD (WCVI), 5ABCD (Both odd year)
#   "Southern Rock Sole", # 5AB (QCS),  5CD (HS)
#   "Redstripe Rockfish", # N and S?
#   "Rougheye/Blackspotted Rockfish Complex",# N and S?
#   "Lingcod", #4B = inside
#   "Quillback Rockfish", # outside and WCVI_Inside?
#   "Yelloweye Rockfish" # 4B = inside
# )


# list_regions <- c("HBLL inside surveys")
# list_species <- c(
# "Lingcod", #4B = inside
# "Quillback Rockfish", # outside and WCVI_Inside?
# "yelloweye" # 4B = inside
# )

# also need to come up with a list of other species of interest...


for (r_h in seq_along(list_regions)) {
  for (spp_i in seq_along(list_species)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(list_species[spp_i])))
    name <- "-RW-no-covs" # string describing model covariates

    try({
      rmarkdown::render("analysis-survey/1-index-standardization.Rmd",
        params = list(
          species = list_species[spp_i],
          region = list_regions[r_h],
          update_model = TRUE,
          update_index = TRUE
          # update_index = FALSE
        ),
        output_file = paste0(spp, name, "-", region, ".html"),
        envir = env
      )
    })
  }
}


# full list from 2 years ago
list_species <- c(
"Aleutian Skate",
"Big Skate",
"Longnose Skate",
"Sandpaper Skate",
"North Pacific Spiny Dogfish",
"Brown Cat Shark",
"Spotted Ratfish",

"Pacific Tomcod",
"Walleye Pollock",
"Pacific Cod",
"Lingcod",
"Pacific Hake",
"Buffalo Sculpin",
"Cabezon",
#"Pacifc Staghorn Sculpin",
"Red Irish Lord",
"Sturgeon Poacher",
"Bigmouth Sculpin",
"Kelp Greenling",
"Threadfin Sculpin",
"Bigfin Eelpout",
"Black Eelpout",
"Wattled Eelpout",
"Blackbelly Eelpout",
"Shiner Perch",
"Snake Prickleback",
# "Wolf Eel"
"Pacific Sand Lance",
"Pacific Herring",
"Sablefish",
"Bocaccio",
"Canary Rockfish",
"Chilipepper",
"Copper Rockfish", # small sample
"Darkblotched Rockfish", # need predictions still
"Dusky Rockfish",
"Greenstriped Rockfish",
"Harlequin Rockfish",
"Pacific Ocean Perch",
"Pygmy Rockfish",
"Quillback Rockfish",
"Redbanded Rockfish",
"Redstripe Rockfish",
"Rosethorn Rockfish",
"Rougheye/Blackspotted Rockfish Complex",
"Sharpchin Rockfish",
"Shortbelly Rockfish", # small sample
"Shortraker Rockfish",
"Silvergray Rockfish",
"Splitnose Rockfish",
"Widow Rockfish", # schooling
"Yellowmouth Rockfish",
"Yellowtail Rockfish", # schooling
"Yelloweye Rockfish",
"Longspine Thornyhead",
"Shortspine Thornyhead",

"Pacific Halibut",
"Arrowtooth Flounder",
"Butter Sole",
"C-O Sole",
"Curlfin Sole",
"Dover Sole",
"English Sole",
"Flathead Sole",
"Pacific Sanddab",
"Petrale Sole",
"Rex Sole",
"Southern Rock Sole",
"Slender Sole",
"Sand Sole",
"Starry Flounder"
)
