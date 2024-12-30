library(tidyverse)
source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")
source("codes/r-code_03-ACD.R")
# source("codes/r-code_04-1-orthography-profiling.R") # Only run this when there is update on the orthography and the spreadsheet!

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
         `IPA phonemic transcription tokenised` = ipa_tokenised,
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

# the reason why we read the raw original data from Concepticon in the code line below is to get the data for Concepticon Semantic Field based on matching the Concepticon ID!
concepsem <- read_tsv("https://raw.githubusercontent.com/concepticon/concepticon-data/master/concepticondata/concepticon.tsv")
concepts_gloss_edit <- read_tsv("data/enolex-gloss-mapped-to-edit_2024-1810.tsv")
# For June 2024 version
concepts_mapped <- concepts_gloss_edit |> 
  # filter(NUMBER %in% 1:1263) |> 
  mutate(Concepticon = if_else(!is.na(CONCEPTICON_ID), 
                               paste(concepticon_url, CONCEPTICON_ID, sep = ""),
                               NA)) |> 
  select(English = GLOSS, `Concepticon gloss` = CONCEPTICON_GLOSS, Concepticon, CONCEPTICON_ID)
# Get the Concepticon's Semantic Field via the CONCEPTICON_ID
concepts_mapped <- concepts_mapped |> 
  left_join(concepsem |> 
              rename(CONCEPTICON_ID = ID) |> 
              select(CONCEPTICON_ID, SEMANTICFIELD)) |> 
  select(English, `Concepticon gloss`, SEMANTICFIELD, Concepticon)

# joined with the main EnoLEX database

enolex7 <- enolex6 |> 
  left_join(concepts_mapped) |> 
  group_by(`Cognate ID`) |> 
  mutate(`Number of Cognates` = n_distinct(`Given as`)) |> 
  ungroup() |> 
  mutate(English = if_else(English == "kuat", "strong", English),
         Indonesian = if_else(Indonesian == "strong", "kuat", Indonesian),
         English = if_else(English == "sea form", "sea foam", English)) |> 
  select(-`Semantic field`) |> 
  rename(`Semantic field` = SEMANTICFIELD) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "first word.+register\\, second"), 
                                        str_replace(`Note for each year`, "(register)\\,\\s(second)", "\\1 ; \\2"), 
                                        `Note for each year`),
         `Note for each year` = if_else(str_detect(`Note for each year`, "^apparently first word"),
                                        str_replace(`Note for each year`, "first word", paste('"', str_extract(`Given as`, "^[^ ]+?(?=\\s;)"), '" is', sep = "")),
                                        `Note for each year`),
         `Note for each year` = if_else(str_detect(`Note for each year`, "^apparently .+second\\sword\\shigh\\sregister"),
                                        str_replace(`Note for each year`, "second word", paste('"', str_extract(`Given as`, "(?<=\\;\\s).+$"), '" is', sep = "")),
                                        `Note for each year`),
         `Note for each year` = if_else(str_detect(`Note for each year`, stringi::stri_trans_nfc("cáua")),
                                        str_replace_all(`Note for each year`, stringi::stri_trans_nfc("(cáua|caúa)"), "\\1 (KI- form)"),
                                        `Note for each year`),
         `Note for each year` = if_else(str_detect(`Note for each year`, "abaua\\s+means\\s+'good'"),
                                        str_replace(`Note for each year`, "abaua", "abaua (BU- form)"),
                                        `Note for each year`),
         `Note for each year` = if_else(str_detect(`Note for each year`, "\\s\\;\\sfirst word is the ki\\-form.+"),
                                        str_replace(`Note for each year`, "\\s\\;\\sfirst word is the ki\\-form.+", ""),
                                        `Note for each year`)) |> 
  # check if the Given as contains ";" when the Indonesian also contains ";", if The Indonesian contains ";" and the Given as is not, change the Indonesian ";" into ","
  mutate(Indonesian = if_else(str_detect(Indonesian, ";") & str_detect(`Given as`, ";", negate = TRUE),
                              str_replace_all(Indonesian, ";", ","),
                              Indonesian),
         Indonesian = replace(Indonesian, Indonesian == "mug , mangkok", "mangkok")) |> 
  mutate(across(matches("(Common trans|phonemic trans|Given as)"), ~str_replace_all(., "^\\<\\s?", ""))) |> 
  
  ## Fixing the order of Notes ====
  ### Note for each year =====
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "djoeba[-beri-berri] means 'ship' ; joeba means 'village'__[-beri-berri] in \"djoeba[-beri-berri]\" unclear  but cf. boat",
                                        "djoeba[-beri-berri] means 'ship'__[-beri-berri] in \"djoeba[-beri-berri]\" unclear  but cf. boat ; joeba means 'village'")) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "[kahara] koewo means 'tree' ; ekoewo means 'wood'__[kahara] in \"[kahara] koewo\" unclear",
                                        "[kahara] koewo means 'tree'__[kahara] in \"[kahara] koewo\" unclear ; ekoewo means 'wood'")) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "[moko-]dobo means 'wood' ; [panoekoe-am]dobo means 'box'__[moko-] in \"[moko-]dobo\" means 'many'__dobo in \"[moko-]dobo\" means 'goods'",
                                        "[moko-]dobo means 'wood'__[moko-] in \"[moko-]dobo\" means 'many'__dobo in \"[moko-]dobo\" means 'goods' ; [panoekoe-am]dobo means 'box'")) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "léwo léwo  means 'water' ; lèwo lewo [djewé]   means 'rain' ; literal translation of  'water from above'",
                                        "léwo léwo  means 'water' ; lèwo lewo [djewé]  means 'rain' ,, literal translation of  'water from above'")) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "lit. 'a lot of wood' ; second word means 'red acacia'",
                                        "lit. 'a lot of wood' ; pidjoe means 'red acacia'")) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`,
                                                   "third word means 'tailbone'"),
                                        str_replace_all(`Note for each year`,
                                                        "\\,\\sthird word means 'tailbone'",
                                                        ""),
                                        `Note for each year`)) |> 
  # fix error in Daniel's spreadsheet
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Given as` == "ebbo" & Year == "1879",
                                        NA)) |> 
  mutate(`Note for each year` = if_else(`Note for each year` == "third word means 'brave' and literally means 'not fear'",
                                        str_c("  ;   ;  ", str_extract(`Given as`, "(?<=;\\s)\\[.+$"), str_replace(`Note for each year`, "^third word ", " "), sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "kahafie and kahapie are taken from a wordlist ; [wa]kafie literally means 'I wish'",
                                        "kahafie is taken from a wordlist ; kahapie is taken from a wordlist ; [wa]kafie literally means 'I wish'")) |> 
  mutate(`Note for each year` = if_else(`Note for each year` == "first word means 'water', second word means 'rain' ; literal translation of second word 'water from above'",
                                        str_c(str_extract(`Given as`, "^[^ ;]+?(?=\\s;)"),
                                              str_replace_all(`Note for each year`, "(^first word|\\, second word.+$)", ""),
                                              " ; ",
                                              str_replace_all(`Given as`, "(^[^ ;]+?\\s\\;\\s|\\s\\;\\s[^;]+$)", ""),
                                              str_replace_all(`Note for each year`, "(^first word.+?second word| ; literal translation.+$)", ""),
                                              " ,, ",
                                              str_extract(`Note for each year`, "literal translation of "),
                                              str_replace_all(`Given as`, "(^[^ ;]+?\\s\\;\\s|\\s\\;\\s[^;]+$)", ""),
                                              str_replace_all(`Note for each year`, "^first word.+literal translation of second word", ""),
                                              " ; ",
                                              sep = ""),
                                        `Note for each year`)
         ) |> 
  # mutate(across(matches("Given|transcription"), ~str_replace_all(., "(; | \\# ;)", ""))) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "\\[uuaha\\] mena means"),
                                        str_c(`Note for each year`, " ; ", sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = if_else(str_detect(`Given as`, "mŏkŏ .èfōka."),
                                        str_c(" ; ", `Note for each year`, sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = if_else(str_detect(`Given as`, "^kaptuh.. ; puh.."),
                                        str_c(`Note for each year`, " ; ", sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "^these forms are somewhat unclear but must belong"),
                                        # str_replace(`Note for each year`, "^these forms are somewhat unclear but must belong to the same root", "  ;  "),
                                        str_c("These forms (i.e., ",
                                              str_replace_all(`Given as`, " ; ", " , "),
                                              ")",
                                              str_extract(`Note for each year`, " are somewhat unclear but must belong to the same root"),
                                              " ; ",
                                              "These forms (i.e., ",
                                              str_replace_all(`Given as`, " ; ", " , "),
                                              ")",
                                              str_extract(`Note for each year`, " are somewhat unclear but must belong to the same root"),
                                              " ; ",
                                              "These forms (i.e., ",
                                              str_replace_all(`Given as`, " ; ", " , "),
                                              ")",
                                              str_extract(`Note for each year`, " are somewhat unclear but must belong to the same root"),
                                              str_replace_all(`Note for each year`, "^these forms.+root ; ", "__"),
                                              sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "\"kakèbara\" is perhaps a mishearing of \"kakènèbaka\"",
                                        " ; \"kakèbara\" is perhaps a mishearing of \"kakènèbaka\"")) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "ki\\-?koh must be a more conservative form"),
                                        str_c(`Note for each year`, " than ", str_extract(`Given as`, "(?<=\\;\\s)[^;]+$"), " ; ", sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = replace(`Note for each year`,
                                        `Note for each year` == "ho means 'already'",
                                        " ; ho means 'already'")) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "maybe meaning 'forest'"),
                                        str_c(
                                          str_replace(`Note for each year`, "and .+?(?=unclear)", ""),
                                          " ; ",
                                          str_replace(`Note for each year`, "^.+? and ", ""),
                                          " ; ",
                                          sep = ""
                                        ),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "literally means 'head hair'"),
                                        str_c(`Note for each year`, " ; ", sep = ""),
                                        `Note for each year`)) |> 
  mutate(`Note for each year` = if_else(str_detect(`Note for each year`, "missing its second part"),
                                        str_c(" ; ", `Note for each year`, sep = ""),
                                        `Note for each year`))
  


enoSemiCol <- enolex7 |> mutate(across(matches("Given|transcription|English|Indonesian|^Note"), ~str_count(., ";"), .names = "{col}_nsemi"))

## Check entries where the note for each year has more semicolons
enoSemiCol |> 
  filter(`Note for each year_nsemi`<`Given as_nsemi`) |> 
  select(`Given as`, `Note for each year`)

# When the `Given as` contains less number of semicolons than the Note for Cognate ID and Note for year, separate_longer_delim() works
enoSemiCol |> filter(`Given as_nsemi` < `Note for Cognate ID_nsemi`) |>  separate_longer_delim(cols = where(is.character), delim = ";") |> select(`Given as`, `Note for Cognate ID`)
# enoSemiCol |> filter(`Given as_nsemi` < `Note for each year_nsemi`) |>  separate_longer_delim(cols = where(is.character), delim = ";")  |> select(`Given as`, `Note for each year`)


enoSemiCol1 <- enoSemiCol |> 
  # replace semi colon in Note for Cognate ID when the number of semi colon in Note for Cognate ID is larger than or equal with the Given as
  mutate(`Note for Cognate ID` = if_else(`Note for Cognate ID_nsemi` > `Given as_nsemi`,
                                         
                                         str_replace_all(`Note for Cognate ID`, ";", ",,"),
                                         
                                         `Note for Cognate ID`)) |> 
  
  # filter entries that have non-zero, equal number of semi colons in the Note for Cognate ID and Original form
  mutate(`Note for Cognate ID` = if_else(`Note for Cognate ID_nsemi` == `Given as_nsemi` & `Note for Cognate ID_nsemi` > 0 & `Given as_nsemi` > 0,
                                         
                                         str_replace_all(`Note for Cognate ID`, ";", ",,"),
                                         
                                         `Note for Cognate ID`)) |> 
  
  mutate(`Note for each year` = if_else(`Note for each year_nsemi` > `Given as_nsemi` & `Given as_nsemi` == 0,
                                         
                                        str_replace_all(`Note for each year`, ";", ",,"),
                                        
                                        `Note for each year`)) |> 
  
  mutate(`PMP English gloss` = str_replace_all(`PMP English gloss`, ";", ",,")) |> 
  
  mutate(`Note for each year` = if_else(`Note for each year_nsemi` < `Given as_nsemi` & `Note for each year_nsemi` > 0,
                                        
                                        str_replace_all(`Note for each year`, ";", ",,"),
                                        
                                        `Note for each year`)) |> 
  
  mutate(`Note for each year` = str_replace_all(`Note for each year`, "__", " -- ")) |> 
  
  # These codes handle combination of forms to be delimited with underscore
  mutate(`Common transcription` = if_else(str_detect(`Given as`, ";", TRUE),
                                          
                                          str_replace_all(`Common transcription`, " ", "_"),
                                          
                                          `Common transcription`),
         
         `Common transcription tokenised` = if_else(str_detect(`Given as`, ";", TRUE),
                                                    
                                                    str_replace_all(`Common transcription tokenised`, "\\#", "_"),
                                                    
                                                    `Common transcription tokenised`),
         
         `IPA phonemic transcription` = if_else(str_detect(`Given as`, ";", TRUE),
                                                
                                                str_replace_all(`IPA phonemic transcription`, " ", "_"),
                                                
                                                `IPA phonemic transcription`),
         
         `IPA phonemic transcription tokenised` = if_else(str_detect(`Given as`, ";", TRUE),
                                                          
                                                          str_replace_all(`IPA phonemic transcription tokenised`, "\\#", "_"),
                                                          
                                                          `IPA phonemic transcription tokenised`),
         
         `Given as` = if_else(str_detect(`Given as`, ";", TRUE),
                              str_replace_all(`Given as`, " ", "_"),
                              `Given as`))

enoSemiCol1$`Given as`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")] <- str_replace_all(enoSemiCol1$`Given as`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")], "; ", "")
enoSemiCol1$`Common transcription`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")] <- str_replace_all(enoSemiCol1$`Common transcription`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")], "; ", "")
enoSemiCol1$`IPA phonemic transcription`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")] <- str_replace_all(enoSemiCol1$`IPA phonemic transcription`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")], "; ", "")
enoSemiCol1$`Common transcription tokenised`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")] <- str_replace_all(enoSemiCol1$`Common transcription tokenised`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")], "; \\# ", "")
enoSemiCol1$`IPA phonemic transcription tokenised`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")] <- str_replace_all(enoSemiCol1$`IPA phonemic transcription tokenised`[str_which(enoSemiCol1$`Note for each year`, "5x10x20")], "; \\# ", "")

# This is the code when the splitting happens
enoSemiCol2 <- enoSemiCol1 |> 
  
  separate_longer_delim(where(is.character), delim = " ; ") |> # This is the code when the splitting happens
  mutate(across(matches("tokenised$"), ~str_replace_all(., "(^\\#\\s|\\s\\#$)", ""))) |> # run this code line IF the cells with multiple forms get split
  
  # mutate(across(matches("tokenised$"), ~str_replace_all(., "(\\s\\#\\s)", " "))) |> # this is the code to run when the cells with multiple forms are not split
  select(!matches("_nsemi")) |> 
  rename(ID_old = ID,
         Cognate_ID = `Cognate ID`,
         Original_Form = `Given as`,
         Orthography = `Common transcription`,
         Ortho_Segments = `Common transcription tokenised`,
         IPA = `IPA phonemic transcription`,
         IPA_Segments = `IPA phonemic transcription tokenised`,
         English_Original = `Original English gloss in source`,
         Doculect = `Doculect info`,
         Note_for_Year = `Note for each year`,
         Note_for_Cognate = `Note for Cognate ID`,
         PAN_Etymon = `PAN etymon`,
         PAN_English = `PAN English gloss`,
         PAN_Source = `PAN source`,
         PMP_Etymon = `PMP etymon`,
         PMP_English = `PMP English gloss`,
         PMP_Source = `PMP source`,
         Etymology_Source = `Etymological sources`,
         Concepticon_Gloss = `Concepticon gloss`,
         Semantic_Field = `Semantic field`,
         Number_of_Cognates = `Number of Cognates`) |> 
  mutate(ID = row_number()) |> 
  select(ID, everything()) |> 
  select(-ID_old)

enolex8 <- enoSemiCol2
# enolex8 <- enolex7 |> 
#   rename(ID_old = ID,
#          Cognate_ID = `Cognate ID`,
#          Original_Form = `Given as`,
#          Orthography = `Common transcription`,
#          Ortho_Segments = `Common transcription tokenised`,
#          IPA = `IPA phonemic transcription`,
#          IPA_Segments = `IPA phonemic transcription tokenised`,
#          English_Original = `Original English gloss in source`,
#          Doculect = `Doculect info`,
#          Note_for_Year = `Note for each year`,
#          Note_for_Cognate = `Note for Cognate ID`,
#          PAN_Etymon = `PAN etymon`,
#          PAN_English = `PAN English gloss`,
#          PAN_Source = `PAN source`,
#          PMP_Etymon = `PMP etymon`,
#          PMP_English = `PMP English gloss`,
#          PMP_Source = `PMP source`,
#          Etymology_Source = `Etymological sources`,
#          Concepticon_Gloss = `Concepticon gloss`,
#          Semantic_Field = `Semantic field`,
#          Number_of_Cognates = `Number of Cognates`) |> 
#   mutate(ID = row_number()) |> 
#   select(ID, everything()) |> 
#   select(-ID_old)

enolex8 |> 
  write_rds("data/dummy_for_pak_cok_20240903.rds")

# enolex7 |>
#   write_tsv("data/dummy_for_pak_cok_20240621.tsv")
# enolex8 |> select(Sources) |> distinct() |> write_tsv("sourcesmini.tsv")
enolex8 |>
  write_tsv("data/dummy_for_pak_cok_20240903.tsv")
