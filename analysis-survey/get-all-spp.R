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
for (i in species_list) {
  species <- i
  all_spp_trawl[[i]] <- try({gfdata::get_survey_sets(species, ssid = c(1,3,4,16,22,36,39,40))})
}

all_surv <- do.call("rbind", all_spp_trawl)
#all_trawl_catch <- all_trawl %>% filter(catch_weight > 0)
all_catch_2021 <- all_surv %>% filter(year == 2021)

saveRDS(all_surv, file = "data-raw/all_surv_catch.rds")
saveRDS(all_catch_2021, file = "data-raw/all_trawl_catch_2021.rds")

####

#species codes not captured with above method

# 66S = blue mussel (exact species undetermined),	mytilus edulis complex
# 68P	= pacific scallop,	patinopecten caurinus x mizuhopecten yessoensis,	hybrid
# 394	= rougheye/blackspotted rockfish complex,	sebastes aleutianus/melanostictus complex
# 013	= unidentified shark,	unidentified shark
# 	015	unknown fish
# 390	NA	sebastosomus-type larvae
