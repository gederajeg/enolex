library(tidyverse)

## This code file prepares the sources and EnoLEX data used inside the enolex directory for the EnoLEX app.

## Sources data ====
sourcesmini <- read_tsv("sourcesmini.tsv") |>
  rename(BIBTEXKEY = SourcesKey)
sourcesdf <- read_tsv("sources.tsv")
sources_all <- sourcesmini |>
  left_join(sourcesdf) |>
  select(Sources, BIBTEXKEY, AUTHOR, YEAR, TITLE, URL, CITATION) |>
  mutate(across(where(is.character), ~replace_na(., ""))) |>
  mutate(YEAR_URL = if_else(URL == "", YEAR, paste("<a href='", URL, "' target='_blank'>", YEAR, "</a>", sep = "")))
sources_all |>
  write_rds("enolex/sources.rds")

## EnoLEX data ====
### This "EnoLEX data" needs to be re-run every time there is an update from "data/dummy...tsv" file
### This "EnoLEX data" needs to be run from the root directory (not from inside the `enolex` app directory)
enolex <- read_tsv("data/dummy_for_pak_cok_20240731.tsv") |>
  # mutate(across(where(is.character), ~replace_na(., ""))) |>
  left_join(sources_all) |>
  filter(Year != "ms.") |>
  mutate(Year = replace(Year, Year == "2023", "2022")) |>
  mutate(Sources = replace(Sources, Sources == "Zakaria et al. 2023", "Zakaria et al. 2022")) |>
  mutate(Year = factor(Year, levels = c("<1855", "1854", "1855", "1864", "1870", "1878",
                                        "1879", "1888", "1891", "1894", "1895",
                                        "1916", "1979", "1982", "1987", "2011",
                                        "2019", "2022"))) |>
  mutate(Concepticon_Gloss = if_else(!is.na(Concepticon_Gloss),
                                     paste("<a href='", Concepticon, "' target='_blank'>", Concepticon_Gloss, "</a>", sep = ""),
                                     Concepticon_Gloss)) |>
  mutate(across(matches("(Etymon|^Note)"), ~str_replace_all(., '""', '"')))
enolex |>
  write_rds("enolex/enolex.rds")

### Image data =====
### Run in the terminal git bash without the double quote!
"cp '../../Enggano-Fieldwork/2nd-fieldwork-2024-02-01/photos/easter-monday_ibadah-padang/IMG_E3668.JPG' enolex/estuary.JPG"
