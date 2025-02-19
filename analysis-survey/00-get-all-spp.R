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
saveRDS(all_surv, file = "data-raw/all_surv_catch.rds")

# Get bait counts so that the nbinom2() with hook competition can be used. 
ll_hook_data <- gfdata::get_ll_hook_data(species = 'yelloweye rockfish', ssid = c(22, 36, 39, 40)) 

bait_counts <- ll_hook_data |>
  dplyr::select(ssid, year, fishing_event_id, count_bait_only)
saveRDS(bait_counts, file.path(path, "bait-counts.rds"))

# Get survey locations for current year for presentation plot
select_year <- 2023

survey_locations <- gfdata::run_sql(database = "GFBioSQL",
  glue::glue_sql("SELECT
    t.trip_id,
    ts.survey_id,
    s.survey_desc,
    s.survey_series_id,
    ss.survey_series_desc,
    fe.fishing_event_id,
    fe.fe_begin_retrieval_time,
    fe.fe_start_lattitude_degree + fe.fe_start_lattitude_minute / 60 as latitude,
    -(fe.fe_start_longitude_degree + fe.fe_start_longitude_minute / 60) as longitude,
    fe.fe_end_lattitude_degree + fe.fe_end_lattitude_minute / 60 as latitude_end,
    -(fe.fe_end_longitude_degree + fe.fe_end_longitude_minute / 60) as longitude_end
  FROM trip AS t
    LEFT JOIN trip_survey AS ts ON t.trip_id = ts.trip_id
    LEFT JOIN survey AS s ON ts.survey_id = s.survey_id
    LEFT JOIN survey_series AS ss ON s.survey_series_id = ss.survey_series_id
    LEFT JOIN fishing_event AS fe ON t.trip_id = fe.trip_id
  WHERE YEAR(t.trip_start_date) = {select_year};"
  )
)

survey_locations |> saveRDS(file.path("data-raw", paste0(select_year, '-survey-locations.rds')))

#species codes not captured with above method

# 66S = blue mussel (exact species undetermined),	mytilus edulis complex
# 68P	= pacific scallop,	patinopecten caurinus x mizuhopecten yessoensis,	hybrid
# 394	= rougheye/blackspotted rockfish complex,	sebastes aleutianus/melanostictus complex
# 013	= unidentified shark,	unidentified shark
# 	015	unknown fish
# 390	NA	sebastosomus-type larvae
