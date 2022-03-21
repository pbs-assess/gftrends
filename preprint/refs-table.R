source(here::here("analysis/stock_df.R"))

# bib <- bib2df::bib2df("preprint/refs.bib")
# stock_df$bib_key <- stringr::str_trim(unique(bib$BIBTEXKEY))

refs <- tibble::tribble(
  ~stock_clean, ~reference,
  "Arrowtooth Flounder BC","grandin2017arrowtooth",
  "Bocaccio BC","dfo2020bocaccio",
  "Canary BC","stanly2009canary",
  "Lingcod 4B","holt2016lingsog",
  "Pacific Ocean Perch 3CD","edwards2014popwcvi",
  "Pacific Ocean Perch 5ABC","edwards2013pophg",
  "Pacific Ocean Perch 5DE","edwards2011popqcs",
  "Pacific Cod 3CD","dfo2021pcod",
  "Pacific Cod 5ABCD","dfo2021pcod",
  "Quillback BC Outside","yamanaka2011quillback",
  "Quillback VI Inside","yamanaka2011quillback",
  "Redstripe Rockfish BC N","starr2021redstripe",
  "Redstripe Rockfish BC S","starr2021redstripe",
  "Rocksole 5AB","holt2016rocksole",
  "Rocksole 5CD","holt2016rocksole",
  "Rougheye/Blacksp. BC N","dfo2020rougheye",
  "Rougheye/Blacksp. BC S","dfo2020rougheye",
  "Sablefish BC","dfo2020sablefish",
  "Shortspine Thornyhead BC","starr2015shortspineidiots",
  "Silvergray Rockfish BC","starr2016silvergray",
  "Walleye Pollock BC N","starr2021pollock",
  "Walleye Pollock BC S","starr2021pollock",
  "Widow Rockfish BC","dfo2019widow",
  "Yelloweye Rockfish 4B","dfo2020yelloweyeinside",
  "Yellowmouth Rockfish BC","dfo2022yellowmouth",
  "Yellowtail Rockfish BC", "dfo2015yellowtail")

refs$reference <- paste0("@", refs$reference)
