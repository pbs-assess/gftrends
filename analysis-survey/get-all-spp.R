library(dplyr)
library(gfdata)

spp_table <- get_species()
saveRDS(spp_table , file = "data-raw/allspp.rds")


fish_table <- spp_table %>% filter(species_grouping == "fish")
species_list <- c(fish_table$species_common_name)
species_list <- na.omit(species_list)
species_list <- unique(species_list)

# invert_table <- spp_table %>% filter(species_grouping == "invertebrate")
# invert_list <- c(invert_table$species_common_name)
# invert_list <- na.omit(invert_list)
# invert_list <- unique(invert_list)

all_spp_trawl <- list()

species_list <- c("Arrowtooth Flounder", "Big Skate", "Bocaccio", "Butter Sole",
  "Canary Rockfish", "Copper Rockfish", "Darkblotched Rockfish",
  "Dover Sole", "English Sole", "Flathead Sole", "Greenstriped Rockfish",
  "Kelp Greenling", "Lingcod", "Longnose Skate", "Longspine Thornyhead",
  "North Pacific Spiny Dogfish", "Pacific Cod", "Pacific Ocean Perch",
  "Petrale Sole", "Quillback Rockfish", "Redstripe Rockfish", "Rex Sole",
  "Rougheye/Blackspotted Rockfish Complex", "Sablefish", "Sandpaper Skate",
  "Sharpchin Rockfish", "Shortraker Rockfish", "Shortspine Thornyhead",
  "Silvergray Rockfish", "Southern Rock Sole", "Splitnose Rockfish",
  "Spotted Ratfish", "Walleye Pollock", "Widow Rockfish", "Yelloweye Rockfish",
  "Yellowmouth Rockfish", "Yellowtail Rockfish")
species_list <- tolower(species_list)

for (i in species_list) {
  cat(i, "\n")
  species <- i
  all_spp_trawl[[i]] <- try({gfdata::get_survey_sets(species, ssid = c(1,3,4,16,22,36,39,40))})
}

all_surv <- do.call("rbind", all_spp_trawl)
#all_trawl_catch <- all_trawl %>% filter(catch_weight > 0)
all_catch_2021 <- all_surv %>% filter(year == 2022)

saveRDS(all_surv, file = "data-raw/all_surv_catch.rds")
saveRDS(all_spp_trawl, file = "data-raw/all_spp_trawl.rds")
saveRDS(all_catch_2021, file = "data-raw/all_trawl_catch_2022.rds")

####

#species codes not captured with above method

# 66S = blue mussel (exact species undetermined),	mytilus edulis complex
# 68P	= pacific scallop,	patinopecten caurinus x mizuhopecten yessoensis,	hybrid
# 394	= rougheye/blackspotted rockfish complex,	sebastes aleutianus/melanostictus complex
# 013	= unidentified shark,	unidentified shark
# 	015	unknown fish
# 390	NA	sebastosomus-type larvae
