library(tidyverse)
source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")
source("codes/r-code_03-ACD.R")
source("codes/r-code_04-1-orthography-profiling.R")

# load the etymology table
proto_distinct1 <- read_rds("data/proto_distinct1.rds")

# load the orthography table
orthofiles <- dir("ortho", pattern = "strings\\-ipa", full.names = TRUE)
orthodfs <- map(orthofiles, read_tsv)
orthodfs1 <- orthodfs |> 
  map(\(x) mutate(x, across(starts_with("year"), as.character))) |> 
  map(\(x) mutate(x, across(starts_with("entry_id"), as.integer))) |> 
  map(\(x) mutate(x, across(matches("^id$"), as.integer)))
orthocombined <- orthodfs1 |> 
  list_rbind() |> 
  mutate(year = factor(year, 
                       levels = c("<1855", "1854", "1855", "1864", "1870", "1878", 
                                  "1879", "1888", "1891", "1894", "1895", 
                                  "1916", "1979", "1982", "ms.", "1987", "2011", 
                                  "2019", "2023")))

# combine the orthography and the ACD with the main data
enolex1 <- eno_etym_long_mini8 |> 
  left_join(orthocombined |> 
              select(entry_id, id, commons, commons_tokenised, ipa, ipa_tokenised),
            by = join_by(entry_id, id)) |> 
  arrange(id, year)

enolex2 <- enolex1 |> 
  left_join(proto_distinct1 |> 
              select(id, PAN_url, PAN_gloss, PAN_source, PMP_url, PMP_gloss, PMP_source, Etymological_source)) |> 
  mutate(note_etc = str_replace_all(note_etc, "([^ ]),([^ ])?", "\\1 , \\2")) |> 
  mutate(note_etc = str_replace_all(note_etc, "\\s{2,}", " ")) |> 
  mutate(note_etc = if_else(str_detect(note_etc, "\\bcf\\."),
                            str_replace_all(note_etc, "\\, ", ", cf. "),
                            note_etc)) |> 
  mutate(note_etc = str_replace_all(note_etc, "lit\\.\\:", "lit."))

enolex3 <-  enolex2 |> 
  mutate(note_etc = if_else(str_detect(note_etc, "\\bcf\\."),
                            str_replace_all(note_etc, "\\b(cf\\.\\s)([^,?]+)", "<re>\\1<link target='#\\2'>\\2</link></re>"),
                            note_etc)) |> 
  mutate(note_etc = str_replace_all(note_etc, "\\s+?(?=<\\/link><\\/re>)", ""))

enolex4 <- enolex3 |> 
  mutate(across(starts_with("note_"), ~str_replace_all(., "\\<\\/?[^>]+?>", ""), .names = "{.col}_cleaned"))
enolex5 <- enolex4 |> 
  mutate(xref_id = if_else(!is.na(note_id_cleaned) & !is.na(note_etc_cleaned), note_etc_cleaned, "")) |> 
  mutate(xref_id = if_else(is.na(note_id_cleaned) & !is.na(note_etc_cleaned) & str_detect(note_etc_cleaned, "^cf\\."),
                           note_etc_cleaned,
                           xref_id)) |> 
  mutate(note_etc_cleaned = if_else(xref_id != "", NA, note_etc_cleaned)) |> 
  mutate(xref_id = if_else(xref_id == "" & str_detect(note_etc_cleaned, "\\bcf\\."), 
                           str_extract(note_etc_cleaned, "\\bcf.+$"), 
                           xref_id)) |> 
  mutate(note_etc_cleaned = if_else(!is.na(note_etc_cleaned) & xref_id != "",
                                    str_replace(note_etc_cleaned, "(\\s;)?\\scf\\..+$", ""),
                                    note_etc_cleaned)) |> 
  mutate(note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "is this a loanword?", "loanword?"),
         note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "Buginese loanword?", "loanword from Buginese?"),
         note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "loanword BI tuan?", "loanword from Indonesian tuan?"),
         note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "loanword BI", "loanword from Indonesian"),
         # note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "Mod. loanword from Minang?", "Modigliani (1894) loanword from Minang?"),
         note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "H&P loanword", "Helfrich & Pieters (1891) loanword"),
         note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "Edwards (2015:68) for split of /o/ Yoder (2011) says this is a loanword", "cf. Edwards (2015:68) for split of /o/ ; Yoder (2011) says this is a loanword"),
         note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "loanword from Indonesian or is this inherited?", "loanword from Indonesian? Or is this inherited?"),
         note_id_cleaned = if_else(is.na(note_id_cleaned) & str_detect(note_etc_cleaned, "loanword"), note_etc_cleaned, note_id_cleaned),
         note_etc_cleaned = if_else(str_detect(note_etc_cleaned, "loanword"), NA, note_etc_cleaned),
         note_id_cleaned = replace(note_id_cleaned, note_id_cleaned == "early loanword from MAL tali?", "early loanword from Malay tali?")) |> 
  mutate(lit_temp = if_else(str_detect(note_etc_cleaned, "\\blit\\.") & is.na(note_id_cleaned),
                            note_etc_cleaned,
                            ""),
         lit_temp = str_replace_all(lit_temp, "(?<=\\blit\\.\\s)([^'?]+)", "\\'\\1\\'"),
         note_id_cleaned = if_else(lit_temp != "", note_etc_cleaned, note_id_cleaned),
         note_etc_cleaned = if_else(lit_temp != "", NA, note_etc_cleaned)) |> 
  select(-lit_temp) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, stri_trans_nfc("Kähler"), "Kähler (1987)")) |> 
  mutate(note_id_cleaned = str_replace_all(note_id_cleaned, stri_trans_nfc("Kähler"), "Kähler (1987)")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, ", in Walland 1864 as 'bird'", "; in Walland (1864) eʔũmãõ means 'bird'")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "H\\&P", "Helfrich & Pieters (1891)")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "van Rosenberg", "cf. von Rosenberg (1855)")) |> 
  # mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "\\bMod\\.", "Modigliani (1894)")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "Yoder", "Yoder (2011)")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "^ter Keurs", "cf. ter Keurs")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "^or (famine|learn|parrot)", "or '\\1'")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "Clercq 1909\\:359", "cf. Clercq (1909:359)")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "BI 'jorok'", "cf. Indonesian 'jorok'")) |> 
  mutate(note_etc_cleaned = str_replace_all(note_etc_cleaned, "^(Kähler)", "cf. \\1")) |> 
  mutate(note_etc_cleaned = replace(note_etc_cleaned, note_etc_cleaned == "Edwards (2015:68) for split of /o/", "cf. Edwards (2015:68) for split of /o/")) |>  
  mutate(xref_temp = if_else(str_detect(note_etc_cleaned, "^cf\\.") & xref_id != "" & !is.na(xref_id), paste(note_etc_cleaned, " ; ", xref_id, sep = ""), "")) |> 
  mutate(xref_id = if_else(xref_temp != "" & !is.na(xref_temp), xref_temp, xref_id)) |> 
  select(-xref_temp) |> 
  mutate(xref_temp = if_else(str_detect(note_etc_cleaned, "\\bcf\\.") & xref_id == "", note_etc_cleaned, xref_id)) |> 
  mutate(xref_id = if_else(!is.na(xref_temp) & xref_id == "", xref_temp, xref_id)) |> 
  select(-xref_temp) |> 
  mutate(note_etc_cleaned = if_else(str_detect(note_etc_cleaned, "^cf\\."), NA, note_etc_cleaned)) |> 
  mutate(note_id_cleaned = if_else(!is.na(note_etc_cleaned) & is.na(note_id_cleaned), note_etc_cleaned, note_id_cleaned)) |> 
  mutate(note_etc_cleaned = if_else(!is.na(note_etc_cleaned), NA, note_etc_cleaned))
  
  # |> 
  # mutate(note_id_cleaned = if_else(is.na(note_id_cleaned) & !is.na(note_etc_cleaned),
  #                                 note_etc_cleaned,
  #                                 note_id_cleaned))# |> 
  #select(-note_year, -note_id, -note_etc) 

enolex6 <- enolex5 |> 
  mutate(english_new = if_else(english_gloss == english_new, "", english_new)) |> 
  select(entry_id,
         id,
         year,
         words,
         commons,
         commons_tokenised,
         ipa,
         ipa_tokenised,
         indonesian_gloss,
         english_gloss,
         english_new,
         semantic_field,
         EngganoLanguage,
         EngganoSource,
         note_year_cleaned,
         note_id_cleaned,
         xref_id,
         PAN_url,
         PAN_gloss,
         PAN_source,
         PMP_url,
         PMP_gloss,
         PMP_source,
         Etymological_source) |> 
  rename(ID = entry_id,
         Year = year,
         `Given as` = words,
         `Common transcription` = commons,
         `Common transcription tokenised` = commons_tokenised,
         `IPA phonemic transcription` = ipa,
         `IPA phonmeic transcription tokenised` = ipa_tokenised,
         `Indonesian` = indonesian_gloss,
         `English` = english_gloss,
         `Semantic field` = semantic_field,
         `Doculect info` = EngganoLanguage,
         `Sources` = EngganoSource,
         `Cognate ID` = id,
         `Original English gloss in source` = english_new,
         `Note for each year` = note_year_cleaned,
         `Note for Cognate ID` = note_id_cleaned,
         `Crossreference` = xref_id,
         `PAN etymon` = PAN_url,
         `PAN English gloss` = PAN_gloss,
         `PAN source` = PAN_source,
         `PMP etymon` = PMP_url,
         `PMP English gloss` = PMP_gloss,
         `PMP source` = PMP_source,
         `Etymological sources` = Etymological_source)

# Prepare the English gloss for Concepticon Mapping
concepts_gloss <- enolex6 |> 
  select(GLOSS =English) |> 
  distinct()
# concepts_gloss |> 
#   mutate(NUMBER = row_number()) |> 
#   write_tsv(paste("data/enolex-gloss-to-map_2024-", nrow(concepts_gloss), ".tsv", sep = ""))

concepticon_url <- "https://concepticon.clld.org/parameters/"
concepts_gloss_edit <- read_tsv("data/enolex-gloss-mapped-to-edit_2024-1810.tsv")
# For June 6 2024 version
concepts_mapped <- concepts_gloss_edit |> 
  filter(NUMBER %in% 1:822) |> 
  mutate(Concepticon = if_else(!is.na(CONCEPTICON_ID), 
                               paste(concepticon_url, CONCEPTICON_ID, sep = ""),
                               NA)) |> 
  select(English = GLOSS, `Concepticon gloss` = CONCEPTICON_GLOSS, Concepticon)

# joined with the main EnoLEX database

enolex7 <- enolex6 |> 
  left_join(concepts_mapped) |> 
  group_by(`Cognate ID`) |> 
  mutate(`Number of Cognates` = n_distinct(`Given as`)) |> 
  ungroup()

enolex7 |> 
  write_tsv("data/dummy_for_pak_cok_20240606.tsv")
