library(tidyverse)

## This code file prepares the sources and EnoLEX data used inside the enolex directory for the EnoLEX app.

## Sources data ====
sourcesmini <- read_tsv("sourcesmini.tsv") |>
  rename(BIBTEXKEY = SourcesKey) |> 
  mutate(LexemesCount = prettyNum(LexemesCount, big.mark = ","))
sourcesdf <- read_tsv("sources.tsv")
sources_all <- sourcesmini |>
  left_join(sourcesdf) |>
  select(Sources, BIBTEXKEY, AUTHOR, YEAR, TITLE, URL, CITATION, LexemesCount) |>
  mutate(across(where(is.character), ~replace_na(., ""))) |>
  mutate(YEAR_URL = if_else(URL == "", YEAR, paste("<a href='", URL, "' target='_blank'>", YEAR, "</a>", sep = "")))
sources_all

year_arranged <- c("< 1855", "1854", "1855", "1864", "1870", "1878", "1879", 
                   "1888", "1891", "1894", "1916", "1979", "1982", "1987", 
                   "2011", "2019", "2022")

sources_enolex <- sources_all |> 
  filter(BIBTEXKEY != "NothoferMS") |> 
  mutate(YEAR = replace(YEAR, YEAR == "n.d.", "< 1855")) |> 
  mutate(YEAR = factor(YEAR, levels = year_arranged)) |> 
  arrange(YEAR, AUTHOR) |> 
  mutate(Sources = str_replace(Sources, "et al\\. 2023", "et al. 2022")) |> 
  mutate(Sources = replace(Sources, Sources == "vd Straten & S. 1855", "vd Straaten & Severijn 1855"))
  
## EnoLEX data ====
### This "EnoLEX data" needs to be re-run every time there is an update from "data/dummy...tsv" file
### This "EnoLEX data" needs to be run from the root directory (not from inside the `enolex` app directory)
enolex <- read_tsv("data/dummy_for_pak_cok_20240903.tsv") |>
  # mutate(across(where(is.character), ~replace_na(., ""))) |>
  left_join(sources_enolex) |>
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
  mutate(across(matches("(Etymon|^Note)"), ~str_replace_all(., '""', '"'))) |> 
  mutate(Sources = replace(Sources, 
                           Sources == "vd Straten & S. 1855", 
                           "vd Straaten & Severijn 1855")
  ) |> 
  mutate(Sources = replace(Sources, 
                           Sources == "Stockhof 1987", 
                           "Stokhof 1987")
  ) |> 
  mutate(Note_for_Year = if_else(str_detect(Note_for_Year, '^[^" ]+?"'),
                                 str_replace(Note_for_Year,
                                             '(^[^" ]+?")',
                                             '"\\1'),
                                 Note_for_Year)
  ) |> 
  mutate(Etymology_Source = str_replace_all(Etymology_Source, "^(Lafeber)(1922)",
                                            "\\1 \\2"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "\\, ",
                                            "; "),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(\\bACD\\b)\\s([0-9]+)",
                                            "\\1 <a href='https://acd.clld.org/cognatesets/\\2' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Edwards )(2015)",
                                            "\\1<a href='https://openresearch-repository.anu.edu.au/server/api/core/bitstreams/5bd92bd2-ff85-4e76-92c0-8f52593a4654/content' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(1987)",
                                            "<a href='https://books.google.co.id/books/about/Enggano_deutsches_Wörterbuch.html?id=OEsOAAAAYAAJ&redir_esc=y' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Smith )(2020)",
                                            "\\1<a href='https://www.austronesianist.com/_files/ugd/fb0c2e_c7954bbefb464344a104aa45fecc6d24.pdf' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Nothofer )(1986)",
                                            "\\1<a href='https://openresearch-repository.anu.edu.au/server/api/core/bitstreams/749ab386-9a3e-49d8-bd0e-7929cec4c069/content' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Nothofer )(1994)",
                                            "\\1<a href='https://doi.org/10.1515/9783110883091.389' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(?<=Zorc )(1995)",
                                            "<a href='https://zorc.net/RDZorc/publications/093=GlossaryOfAustronesianReconstructions[ACD].pdf' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(1975)",
                                            "<a href='https://search.worldcat.org/title/Texte-von-der-Insel-Enggano-:-(Berichte-uber-eine-untergehende-Kultur)/oclc/2333004' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(1940)",
                                            "<a href='https://glottolog.org/resource/reference/id/38922' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Mahdi )(1988)",
                                            "\\1<a href='https://books.google.co.id/books/about/Morphophonologische_Besonderheiten_und_h.html?id=RWMOAAAAYAAJ&redir_esc=y' target='_blank'>\\2</a>"))



## Dialect data =====
dialect_info <- enolex |> 
  select(Sources, Doculect) |> 
  distinct() |> 
  rename(Dialect_Info = Doculect) |> 
  mutate(Dialect_Info = replace(Dialect_Info,
                                Sources %in% c("Zakaria et al. 2022",
                                               "Aron 2019"),
                                "Enggano Meok"),
         Dialect_Info = replace(Dialect_Info,
                                Dialect_Info == "Enggano",
                                "?")) |> 
  mutate(Dialect_Info = replace(Dialect_Info,
                                Sources %in% c("Helfrich & Pieters 1891") &
                                  Dialect_Info == "Enggano Kèfoe",
                                "Southeast"),
         Dialect_Info = replace(Dialect_Info,
                                Sources %in% c("Helfrich & Pieters 1891") &
                                  Dialect_Info == "Enggano Barohia",
                                "Northwest")) |> 
  group_by(Sources) |> 
  mutate(Dialect_Info = str_c(Dialect_Info, collapse = " ; ")) |> 
  ungroup() |> 
  distinct() |> 
  mutate(Place = "?") |> 
  mutate(Dialect_Info = replace(Dialect_Info,
                                Sources == "Brouwer <1855",
                                "Northwest"),
         Place = replace(Place,
                         Sources == "Brouwer <1855",
                         "Barhau"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "vd Straaten & Severijn 1855",
                                "Northwest"),
         Place = replace(Place,
                         Sources == "vd Straaten & Severijn 1855",
                         "Karkau"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "v. Rosenberg 1855",
                                "Northwest or South"),
         Place = replace(Place,
                         Sources == "v. Rosenberg 1855",
                         "Barhau"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Francis 1870",
                                "Northwest"),
         Place = replace(Place,
                         Sources == "Francis 1870",
                         "Barhau?"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Helfrich 1888",
                                "South"),
         Place = replace(Place,
                         Sources == "Helfrich 1888",
                         "Kioyo"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Modigliani 1894",
                                "Southeast?"),
         Place = replace(Place,
                         Sources == "Modigliani 1894",
                         "Kayaapu"),
         
         Place = replace(Place,
                         Sources == "Helfrich & Pieters 1891",
                         "Pulau Dua ; Karkua"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Stokhof 1987",
                                "Southeast?"),
         Place = replace(Place,
                         Sources == "Stokhof 1987",
                         "Pulau Dua"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Helfrich 1916",
                                "Southeast ; Northwest"),
         Place = replace(Place,
                         Sources == "Helfrich 1916",
                         "Pulau Dua ; Karkua"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Kähler 1987",
                                "South"),
         Place = replace(Place,
                         Sources == "Kähler 1987",
                         "Kioyo"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources == "Kasim et al. 1987",
                                "West"),
         Place = replace(Place,
                         Sources == "Kasim et al. 1987",
                         "Malakoni ; Banjar Sari"),
         
         Dialect_Info = replace(Dialect_Info,
                                Sources %in% c("Zakaria et al. 2022",
                                               "Aron 2019",
                                               "Yoder 2011"),
                                "West"),
         Place = replace(Place,
                         Sources %in% c("Zakaria et al. 2022",
                                        "Aron 2019",
                                        "Yoder 2011"),
                         "Meok"),
         
         Dialect_Info = str_replace(Dialect_Info,
                                    "^Enggano ",
                                    "")) |> 
  mutate(Collected = "-",
         Collected = replace(Collected,
                             Sources %in% c("Brouwer <1855"),
                             "ca. 1850"),
         Collected = replace(Collected,
                             Sources %in% c("Boewang 1854"),
                             "1840-1850"),
         Collected = replace(Collected,
                             Sources %in% c("vd Straaten & Severijn 1855"),
                             "1854"),
         Collected = replace(Collected,
                             Sources %in% c("v. Rosenberg 1855"),
                             "1852"),
         Collected = replace(Collected,
                             Sources %in% c("Walland 1864"),
                             "1863"),
         Collected = replace(Collected,
                             Sources %in% c("Francis 1870"),
                             "1865-1870"),
         Collected = replace(Collected,
                             Sources %in% c("Helfrich 1888"),
                             "1885"),
         Collected = replace(Collected,
                             Sources %in% c("Helfrich & Pieters 1891"),
                             "1891"),
         Collected = replace(Collected,
                             Sources %in% c("Modigliani 1894"),
                             "1891"),
         Collected = replace(Collected,
                             Sources %in% c("Stokhof 1987"),
                             "1895"),
         Collected = replace(Collected,
                             Sources %in% c("Helfrich 1916"),
                             "1891"),
         Collected = replace(Collected,
                             Sources %in% c("Amran et al. 1979"),
                             "1978"),
         Collected = replace(Collected,
                             Sources %in% c("Kähler 1987"),
                             "1937-1938"),
         Collected = replace(Collected,
                             Sources %in% c("Kasim et al. 1987"),
                             "1983?"),
         Collected = replace(Collected,
                             Sources %in% c("Yoder 2011"),
                             "2010"),
         Collected = replace(Collected,
                             Sources %in% c("Aron 2019"),
                             "2019"),
         Collected = replace(Collected,
                             Sources %in% c("Zakaria et al. 2022"),
                             "2018-2024"))

### join dialect info into EnoLEX and save into enolex main data
write_rds(select(left_join(enolex, dialect_info), -Doculect, -YEAR), "enolex/enolex.rds")
read_rds("enolex/enolex.rds") |> 
  rename(Original_gloss = English_Original,
         # Concepticon = Concepticon_Gloss,
         Dialect = Dialect_Info) |> 
  select(-CITATION, -URL, -BIBTEXKEY, -Concepticon, -AUTHOR, -TITLE, -YEAR_URL, 
         -Number_of_Cognates, -matches("Segments"), -Collected, -LexemesCount) |> 
  mutate(across(matches("^(PAN_Etymon|PMP_Etymon|Etymology_Source|Concepticon_Gloss)"),
                ~str_replace_all(., "\\<[^>]+?\\>", ""))) |> 
  write_rds("enolex/enolex_glb.rds")
write_rds(left_join(sources_enolex, dialect_info), "enolex/sources.rds")
write_rds(dialect_info, "enolex/dialect_info.rds")
bibs <- read_rds("enolex/sources.rds")
bibs1 <- select(bibs,
                # -Sources,
                -BIBTEXKEY,
                -YEAR,
                -URL) |> 
  mutate(CITATION = str_replace_all(CITATION, "(\\s)_", "\\1<em>"), 
         CITATION = str_replace_all(CITATION, "_(\\s|[[:punct:]])", "</em>\\1"),
         CITATION = if_else(str_detect(CITATION, "\\<https"),
                            str_replace_all(CITATION, "\\<(https[^>]+?)\\>", 
                                            "<a href='\\1' target='_blank'>URL</a>"),
                            CITATION)) |> 
  rename(YEAR = YEAR_URL) |> 
  select(Collected, Published = YEAR, Sources, 
         # Form_Count = Count_of_Original_Form, 
         Form_Count = LexemesCount,
         Dialect = Dialect_Info, Place, Citation = CITATION)
write_rds(bibs1, "enolex/bibs1.rds")

## Create SQLite version =====
enolex_db <- DBI::dbConnect(RSQLite::SQLite(), "enolex/enolex.sqlite")
DBI::dbWriteTable(enolex_db, "enolex", readr::read_rds("enolex/enolex.rds"),
                  overwrite = TRUE)
DBI::dbWriteTable(enolex_db, "enolex_glb", readr::read_rds("enolex/enolex_glb.rds"),
                  overwrite = TRUE)
DBI::dbWriteTable(enolex_db, "dialect_info", 
                  readr::read_rds("enolex/dialect_info.rds"),
                  overwrite = TRUE)
DBI::dbWriteTable(enolex_db, "sources", 
                  readr::read_rds("enolex/sources.rds"),
                  overwrite = TRUE)
DBI::dbWriteTable(enolex_db, "bibs1", 
                  readr::read_rds("enolex/bibs1.rds"),
                  overwrite = TRUE)
DBI::dbListTables(enolex_db)
DBI::dbDisconnect(enolex_db)


### Image data =====
### Run the following line in the terminal git bash without the double quote!
# "cp '../../Enggano-Fieldwork/2nd-fieldwork-2024-02-01/photos/easter-monday_ibadah-padang/IMG_E3668.JPG' enolex/estuary.JPG"


















## Count the forms in von Rosenberg (1878), Aron (2019), and Zakaria et al. (2022) ====
enolex |> 
  filter(str_detect(Sources, "Rosenberg"), Year == "1878") |> 
  pull(Original_Form) |> 
  str_count("([^ ;])+") |> 
  sum()
# [1] 79
enolex |> 
  filter(str_detect(Sources, "Aron"), Year == "2019") |> 
  pull(Original_Form) |> str_count("([^ ;])+") |> 
  sum()
# [1] 694
enolex |> 
  filter(str_detect(Sources, "Zakaria")) |> 
  pull(Original_Form) |> 
  str_count("([^ ;])+") |> 
  sum()
# [1] 57