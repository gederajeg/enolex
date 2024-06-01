library(tidyverse)
library(readxl)
library(stringi)

# orthpath <- "/Users/Primahadi/Library/CloudStorage/GoogleDrive-primahadi_wijaya@unud.ac.id/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/data/Enggano-transcriptions.xlsx"

# read_xlsx(orthpath, sheet = "consonants", range = "A1:W20") |> 
#   write_rds("data/consonants.rds")
# consonant ====
consonant <- read_rds("data/consonants.rds") |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\\n", " ; "))) |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " "))) |> 
  (\(x) x[1:18, -23])() |> 
  pivot_longer(cols = -c(phoneme, `Common transcription all texts`), names_to = "source", values_to = "grapheme") |> 
  rename(commons = `Common transcription all texts`)
consonant1 <- consonant |> 
  mutate(grapheme = str_replace(grapheme, "(\\/|and) trema ", "; trema ")) |> 
  mutate(remark = NA,
         remark = if_else(str_detect(grapheme, "^(WA|CH)"), grapheme, remark),
         grapheme = str_remove(grapheme, "^(WA|CH).+"),
         remark = if_else(str_detect(grapheme, "^allophone of h\\,"),
                          grapheme,
                          remark),
         grapheme = str_replace(grapheme, "^allophone of h\\,.+", "h"),
         commons = str_remove(commons, "\\s\\(glottal\\sstop\\)"),
         grapheme = str_remove(grapheme, "\\s\\((glottal\\sstop|hyphen)\\)")) |> 
  mutate(remark = if_else(str_detect(grapheme, "\\s\\(one occur+ence of .+"), str_extract(grapheme, "\\s\\(one occur+ence of .+"), remark),
         grapheme = str_remove(grapheme, "\\s\\(one occur+ence of.+"),
         remark = str_trim(remark, side = "both")) |> 
  mutate(remark = if_else(str_detect(grapheme, "^typically.+$"), str_extract(grapheme, "^typically.+$"), remark),
         grapheme = if_else(str_detect(grapheme, "^typically h"), "h", grapheme)) |> 
  mutate(remark = if_else(str_detect(grapheme, "^\\(not written"), grapheme, remark),
         grapheme = if_else(str_detect(grapheme, "^\\(not written"), "", grapheme)) |> 
  mutate(remark = if_else(str_detect(grapheme, "\\(sometimes occurs where.+"), str_extract(grapheme, "\\(sometimes occurs where.+"), remark),
         grapheme = str_remove(grapheme, "\\s\\(sometimes occurs.+")) |> 
  mutate(remark = if_else(str_detect(phoneme, "\\(semi.+\\)$"), str_extract(phoneme, "\\(semi.+\\)$"), remark),
         phoneme = str_remove(phoneme, "\\s\\(semi\\-.+$")) |> 
  mutate(grapheme = if_else(is.na(grapheme), "", grapheme)) |>
  mutate(grapheme = str_replace(grapheme, "\\;\\s(?=trema)", "/ ")) |> 
  mutate(grapheme = str_replace(grapheme, "(?<=ä)\\,", " /")) |> 
  mutate(grapheme = str_replace(grapheme, "trema", "trema on following vowel"),
         grapheme = str_replace(grapheme, "(?<=\\)\\s)on following vowel", "trema on following vowel")) |> 
  mutate(grapheme = str_split(grapheme, "\\/")) |> 
  unnest_longer(grapheme)
consonant2 <- consonant1 |>  
  mutate(remark = if_else(str_detect(grapheme, "trema on following vowel"), str_extract(grapheme, "trema on following vowel(\\,\\soccasionally.+$)?"), remark),
         grapheme = str_remove(grapheme, "trema on following vowel(\\,\\soccasionally.+$)?"),
         remark = if_else(str_detect(grapheme, "\\(perhaps not written.+$"), str_extract(grapheme, "\\(perhaps not written.+$"), remark),
         grapheme = str_remove(grapheme, "\\(perhaps not written.+$")) |> 
  mutate(remark = if_else(str_detect(grapheme, "\\(before.+$"), str_extract(grapheme, "\\(before.+$"), remark),
         grapheme = str_remove(grapheme, "\\s\\(before.+$")) |> 
  mutate(grapheme = str_replace_all(grapheme, "\\s+", ""),
         remark = str_trim(remark, side = "both"),
         remark = str_replace_all(remark, "\\s{2,}", " "),
         remark = str_replace_all(remark, "\\(|\\)", ""),
         grapheme = str_replace_all(grapheme, "\\(|\\)", "")) |> 
  mutate(grapheme = replace(grapheme, source == "Brouwer <1855" & commons == "c" & grapheme == "dj", "tj")) |> 
  mutate(cats = "consonant")


# read_xlsx(orthpath, sheet = "vowels", range = "A1:V19") |>
#   write_rds("data/vowels.rds")
# vowels ====
vowels <- read_rds("data/vowels.rds") |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\\n", " ; "))) |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " "))) |> 
  (\(x) x[1:17, -22])() |> 
  pivot_longer(cols = -c(phoneme, `Common transcription for all texts`), names_to = "source", values_to = "grapheme") |> 
  # filter(str_detect(grapheme, "^(?i)not?", negate = TRUE)) |> 
  mutate(remark = str_extract(grapheme, "(?<=;\\s)\\/ī\\/ is indicated.+$|^\\/ī\\/ is indicated.+"),
         remark = if_else(str_detect(grapheme, "^not clear"),
                          grapheme,
                          remark),
         grapheme = if_else(str_detect(grapheme, "^not clear"),
                            "",
                            grapheme)) |> 
  mutate(grapheme = str_replace(grapheme, "^?\\/ī\\/ is indicated.+", ""),
         grapheme = str_replace(grapheme, "\\s\\;\\s$", ""),
         remark = if_else(str_detect(grapheme, "\\(only ā\\)") & phoneme == "VV",
                           str_extract(grapheme, "\\(only ā\\)"),
                           remark),
         grapheme = str_replace(grapheme, "\\(only ā\\)", ""),
         remark = if_else(str_detect(grapheme, "^sometimes") & phoneme == "VV",
                           grapheme,
                           remark)) |> 
  mutate(grapheme = str_remove(grapheme, "^sometimes adds.+$"),
         remark = if_else(source == "Helfrich (1916)" & str_detect(grapheme, "(\\(but not |\\(this often corresponds)"),
                           str_extract(grapheme, "\\(.+$"),
                           remark),
         grapheme = if_else(source == "Helfrich (1916)" & str_detect(grapheme, "(\\(but not |\\(this often corresponds)"),
                            str_remove_all(grapheme, "\\(.+$"),
                            grapheme),
         remark = if_else(source == "Texts (1957-64)" & str_detect(grapheme, "\\(sometimes"),
                           str_extract(grapheme, "\\(.+$"),
                           remark),
         grapheme = if_else(source == "Texts (1957-64)" & str_detect(grapheme, "\\(sometimes"),
                            str_remove_all(grapheme, "\\(.+$"),
                            grapheme)) |> 
  mutate(remark = if_else(str_detect(grapheme, "possibly"),
                           str_extract(grapheme, "possibly"),
                           remark),
         grapheme = if_else(str_detect(grapheme, "possibly"),
                            str_remove_all(grapheme, "(\\(possibly\\s|\\))"),
                            grapheme)) |> 
  mutate(remark = if_else(str_detect(grapheme, "allophone"),
                           grapheme,
                           remark),
         grapheme = if_else(str_detect(grapheme, "allophone"),
                            "e",
                            grapheme),
         
         remark = if_else(str_detect(grapheme, "tilde"),
                           str_extract(grapheme, "\\[dot.+$"),
                           remark),
         grapheme = if_else(str_detect(grapheme, "tilde"),
                            str_remove(grapheme, "\\s\\[dot.+$"),
                            grapheme)) |> 
  mutate(grapheme = str_trim(grapheme, side = "both")) |> 
  mutate(phoneme = replace(phoneme, remark == "[dot and tilde]", "ə̃")) |> 
  mutate(phoneme = str_split(phoneme, "\\/")) |> 
  unnest_longer(phoneme) |> 
  mutate(grapheme = str_split(grapheme, "\\/")) |> 
  unnest_longer(grapheme) |> 
  mutate(phoneme = replace(phoneme, grapheme == "õ", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ė̃̃", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ɘ̃", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ə̃", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ė̃", "ə̃")) |> 
  rename(commons = `Common transcription for all texts`) |> 
  mutate(cats = "vowel") |> 
  # fix ä for V Rosenberg
  mutate(phoneme = replace(phoneme, source == "v. Rosenberg 1855" & 
                             grapheme %in% c(stringi::stri_trans_nfc("ä")),
                           "a"),
         commons = replace(commons, source == "v. Rosenberg 1855" & 
                             grapheme %in% c(stringi::stri_trans_nfc("ä")),
                           "a"))

# Helfrich's (1916) `oö` in common orthography becomes "o'o"

# tribble(
#   ~phoneme, ~source, ~grapheme,
#   "ī", "Brouwer <1855", "ie",
#   "ī", "vd Straten & S. 1855", "ie",
#   "ī", "Walland 1864", "ie"
# )

# read_xlsx("/Users/Primahadi/Downloads/Enggano-transcriptions.xlsx", sheet = 3, range = "A1:V24") |>
#   write_rds("data/diphthong.rds")
# diphthong ====
diphthong <- read_rds("data/diphthong.rds") |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\\n", " ; "))) |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " "))) |> 
  (\(x) x[1:20, -22])() |> 
  pivot_longer(cols = -c(phoneme, `Common transcription for all texts`), names_to = "source", values_to = "grapheme") |> 
  mutate(across(where(is.character), ~replace_na(., "")))
diphthong1 <- diphthong |> 
  mutate(notes = if_else(str_detect(grapheme, "(^(?i)not?|^NB|^B says|^at the moment|^Only)"),
                          grapheme,
                          ""),
         grapheme = if_else(str_detect(grapheme, "(^(?i)not?|^NB|^B says|^at the moment|^Only)"),
                            "",
                            grapheme),
         `Common transcription for all texts` = if_else(notes != "",
                                                        "",
                                                        `Common transcription for all texts`),
         phoneme = if_else(notes != "",
                           "",
                           phoneme)) |> 
  mutate(remark = if_else(source == "Texts (1957-64)" & str_detect(grapheme, "_y"),
                          "attested in the 1955 text. Note that there is also ɛu listed with the   ͜   below to indicate a diphthong but this appears to be the result of the process whereby a vowel is repeated after a glottal stop. There is also an example of the sequence ɔi but without the   ͜   ",
                          "")) |> 
  mutate(grapheme = if_else(phoneme == "ɛ͂i͂" & source == "Yoder (2011)",
                            str_c(grapheme, " _g", sep = ""),
                            grapheme)) |> 
  mutate(notes = if_else(str_detect(remark, "1955 text"), 
                         str_extract(remark, "Note that.+"), 
                         notes)) |> 
  mutate(remark = if_else(str_detect(remark, "1955 text"),
                          str_remove(remark, "\\.\\sNote that.+"),
                          remark)) |> 
  mutate(grapheme = if_else(source == "Texts (1957-64)" & str_detect(grapheme, "_y"),
                            str_replace(grapheme, "\\s_y", ""),
                            grapheme)) |> 
  mutate(remark = if_else(source == "Yoder (2011)" & str_detect(grapheme, "_y"),
                          "not attested/not described as a diphthong phoneme",
                          remark)) |> 
  mutate(remark = if_else(source == "Yoder (2011)" & str_detect(grapheme, "_g"),
                          "not attested in quick scan of Yoder appendices. He only lists the non-nasal versions as phonemes",
                          remark)) |> 
  mutate(grapheme = if_else(source == "Yoder (2011)" & str_detect(grapheme, "(\\b\\s?_[yg]\\b)"),
                            str_remove(grapheme, "(\\b\\s?_[yg]\\b)"),
                            grapheme)) |> 
  mutate(notes = if_else(source == "Enggano dictionary (1987)" & notes != "",
                         str_c(notes, " ; NB The diphthong eu is also attested", sep = ""),
                         notes)) |> 
  mutate(notes = if_else(source == "Yoder (2011)" & notes != "",
                         str_c(notes, " ; He has both /oi/ and /ɘi/ which is not listed anywhere else. In phonetic/surface transcription he uses  ̯ below the final vowel to indicate a diphthong. In phonemic transcription just written as a sequence of two vowels", sep = ""),
                         notes)) |> 
  mutate(grapheme = str_trim(grapheme, side = "both")) |> 
  mutate(graphemes = str_split(grapheme, "\\/")) |> 
  unnest_longer(graphemes) |> 
  rename(commons = `Common transcription for all texts`) |> 
  select(-grapheme) |> 
  rename(grapheme = graphemes) |> 
  mutate(cats = "glide")

trx1 <- bind_rows(vowels, consonant2, diphthong1) |> 
  select(cats, phoneme, grapheme, commons, source, remark, notes) |> 
  mutate(across(where(is.character), ~replace_na(., "")))
  

## add further notes

trx <- trx1 |> 
  mutate(notes = if_else(str_detect(remark, "^WA"),
                          "This is a dialect variant of /p/ according to Kähler & Crowley",
                          notes)) |> 
  mutate(notes = if_else(remark == "semi-vowel",
                          str_c("CH: allophone of /i/ ; ",
                                "NB: Nothofer also lists /w/ as a semi-vowel phoneme", sep = ""),
                          notes)) |> 
  mutate(notes = if_else(phoneme == "l",
                          "This is a dialect variant of /d/",
                          notes)) |> 
  mutate(notes = if_else(str_detect(remark, "^CH"),
                          "Maybe 'ny' would be good?",
                          notes)) |> 
  mutate(notes = if_else(phoneme == "r",
                          "A dialect feature",
                          notes),
         notes = if_else(phoneme == "t",
                          "Possibly a dialect feature - possibly a phoneme!",
                          notes)) |> 
  mutate(notes = if_else(phoneme == "ç",
                         "This is probably an allophone of [h] that occurs following [i]. Therefore, might be possible to write as 'h'?",
                         notes)) |> 
  mutate(notes = if_else(phoneme == "t͡ʃ",
                         "Barnaby has a comment that sometimes there is more of a gap between t and ' so he sometimes transcribes T and sometimes t' (analysing the latter as [t] + glottal stop) - Crowley argues that there are no consonant clusters, so all instances of t' have been changed to T.",
                         notes)) |> 
  mutate(notes = if_else(phoneme == "ɛ",
                         "Many of the phonological treatments suggest that [e] and [ɛ] are allophones. Hence, perhaps in the modern version these would just be 'e' as well?",
                         notes)) |> 
  mutate(notes = if_else(phoneme == "o",
                         "Kähler makes a distinction between open and close o in the same way as open/close e. Since [o] and [e] do not occur all that often and there is a neutralisation when nasalised some argue that they are allophones. 'o' is used for both [o] and [ɔ] in the dictionary and texts.  Note that this leads to an asymmetry between e/ɛ (which are distinguished in the grammar and the pre-1975 texts) and o/ɔ, which are also distinguished in the grammar and the pre-1975 texts, but are not distinguished in our standard transcription.",
                         notes)) |> 
  mutate(notes = if_else(phoneme == "VV",
                         "sequence of two of the same vowel",
                         notes)) |> 
  mutate(notes = if_else(phoneme == "ə",
                         "Yoder assumes [ɘ] is the phonetic realisation of /ə/",
                         notes)) |> 
  mutate(remark = if_else(source == "Yoder (2011)" & phoneme %in% c("t͡ʃ", "d͡ʒ", "f", "ɲ"),
                          "present in Kahler, but not mentioned in Yoder",
                          remark)) |> 
  mutate(remark = if_else(source == "Nothofer (1992)" & phoneme %in% c("l", "t͡ʃ", "d͡ʒ", "f", "ɲ"),
                          "present in Kahler, but not mentioned in Nothofer",
                          remark)) |> 
  mutate(remark = if_else(commons == "'" & source %in% c("Enggano grammar (1940)", 
                                                         "Texts (1957-64)", 
                                                         "Book of Enggano texts (1975)", 
                                                         "Sumaryana transcription of 1975 texts", 
                                                         "Burleigh transcription of texts 1957-64",
                                                         "Common transcription all texts",
                                                         "Nothofer (1992)",
                                                         "Contemporary Enggano"),
                          "glottal stop",
                          remark)) |> 
  mutate(remark = if_else(grapheme == "-" & source %in% c("Brouwer <1855", 
                                                         "vd Straten & S. 1855", 
                                                         "Walland 1864", 
                                                         "Oudemans 1879"),
                          "hypen",
                          remark)) |> 
  mutate(source = str_replace(source, "Pieters 1891", "Pieters (1891")) |> 
  mutate(remark = if_else(grapheme == "k²",
                          "The \"◌²\" stands for reduplication",
                          remark)) |> 
  mutate(notes = if_else(source == "Helfrich (1916)",
                         str_c(notes, " ; what do umlauts mean? Helfrich is not consistent in the verhalen/texts - for example he alternates between e and è for /e/, he introduces symbols like a and o with accent ",
                               sep = ""),
                         notes)) |> 
  mutate(notes = if_else(source == "Helfrich (1916)",
                         str_remove(notes, "^\\s\\;\\s"),
                         notes)) |> 
  mutate(remark = if_else(source == "Burleigh transcription" & grapheme %in% stri_trans_nfc(c("Î", "Ê", "â", "î", "ô", "û")),
                          "̂  marks nasalisation",
                          remark)) |> 
  mutate(remark = if_else(source == "Burleigh transcription of texts 1957-64" & phoneme == "d",
                          "d' originally transcribed as simply \"d\", but changed on later pass to differentiate from d",
                          remark))

# googledrive::drive_create(name = "Enggano_Transcription_LongFormat",
#                           path = "https://drive.google.com/drive/u/0/folders/1vtNiXO6DSKzDcgK-Uh_cbQvnKxtiaOIr",
#                           type = "spreadsheet")
# Created Drive file:
#   • Enggano_Transcription_LongFormat <id: 14dqSBmov1j5mlIhpbKp53BIh8iNVvFgJjWNi8ZcOgo8>
#   With MIME type:
#   • application/vnd.google-apps.spreadsheet
# googlesheets4::sheet_write(trx, ss = "14dqSBmov1j5mlIhpbKp53BIh8iNVvFgJjWNi8ZcOgo8", sheet = "Sheet1")

# turn multybites character into a single one (esp. for those with diacritics)
stringi::stri_trans_nfc("Kã́hlēr")

## test if the above conversion is the same with the unified character (the right of `==`)
stringi::stri_trans_nfc("Kã́hlēr") == "Kã́hlēr"
# [1] TRUE

## the following produces FALSE as expected
"Kã́hlēr"==stringi::stri_trans_nfc("Kã́hlēr")
