library(tidyverse)
library(readxl)
library(stringi)

# orthpath <- "/Users/Primahadi/Library/CloudStorage/GoogleDrive-primahadi_wijaya@unud.ac.id/.shortcut-targets-by-id/1MO3Q9KZIODxlfPRyjLTtDUZuVkecFBp6/Enggano/data/Enggano-transcriptions.xlsx"

# read_xlsx(orthpath, sheet = "consonants", range = "A1:W20") |> 
#   write_rds("data/consonants.rds")

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
  mutate(grapheme = replace(grapheme, source == "Brouwer <1855" & commons == "c" & grapheme == "dj", "tj"))


# read_xlsx(orthpath, sheet = "vowels", range = "A1:V19") |>
#   write_rds("data/vowels.rds")

vowels <- read_rds("data/vowels.rds") |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\\n", " ; "))) |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " "))) |> 
  (\(x) x[1:17, -22])() |> 
  pivot_longer(cols = -c(phoneme, `Common transcription for all texts`), names_to = "source", values_to = "grapheme") |> 
  filter(str_detect(grapheme, "^(?i)not?", negate = TRUE)) |> 
  mutate(remarks = NA,
         remarks = str_extract(grapheme, "(?<=;\\s)\\/ī\\/ is indicated.+$|^\\/ī\\/ is indicated.+")) |> 
  mutate(grapheme = str_replace(grapheme, "^?\\/ī\\/ is indicated.+", ""),
         grapheme = str_replace(grapheme, "\\s\\;\\s$", ""),
         remarks = if_else(str_detect(grapheme, "\\(only ā\\)") & phoneme == "VV",
                           str_extract(grapheme, "\\(only ā\\)"),
                           remarks),
         grapheme = str_replace(grapheme, "\\(only ā\\)", ""),
         remarks = if_else(str_detect(grapheme, "^sometimes") & phoneme == "VV",
                           grapheme,
                           remarks)) |> 
  mutate(grapheme = str_remove(grapheme, "^sometimes adds.+$"),
         remarks = if_else(source == "Helfrich (1916)" & str_detect(grapheme, "(\\(but not |\\(this often corresponds)"),
                           str_extract(grapheme, "\\(.+$"),
                           remarks),
         grapheme = if_else(source == "Helfrich (1916)" & str_detect(grapheme, "(\\(but not |\\(this often corresponds)"),
                            str_remove_all(grapheme, "\\(.+$"),
                            grapheme),
         
         remarks = if_else(source == "Texts (1957-64)" & str_detect(grapheme, "\\(sometimes"),
                           str_extract(grapheme, "\\(.+$"),
                           remarks),
         grapheme = if_else(source == "Texts (1957-64)" & str_detect(grapheme, "\\(sometimes"),
                            str_remove_all(grapheme, "\\(.+$"),
                            grapheme)) |> 
  mutate(remarks = if_else(str_detect(grapheme, "possibly"),
                           str_extract(grapheme, "possibly"),
                           remarks),
         grapheme = if_else(str_detect(grapheme, "possibly"),
                            str_remove_all(grapheme, "(\\(possibly\\s|\\))"),
                            grapheme)) |> 
  mutate(remarks = if_else(str_detect(grapheme, "allophone"),
                           grapheme,
                           remarks),
         grapheme = if_else(str_detect(grapheme, "allophone"),
                            "e",
                            grapheme),
         
         remarks = if_else(str_detect(grapheme, "tilde"),
                           str_extract(grapheme, "\\[dot.+$"),
                           remarks),
         grapheme = if_else(str_detect(grapheme, "tilde"),
                            str_remove(grapheme, "\\s\\[dot.+$"),
                            grapheme)) |> 
  mutate(grapheme = str_trim(grapheme, side = "both")) |> 
  mutate(phoneme = replace(phoneme, remarks == "[dot and tilde]", "ə̃")) |> 
  mutate(phoneme = str_split(phoneme, "\\/")) |> 
  unnest_longer(phoneme) |> 
  mutate(grapheme = str_split(grapheme, "\\/")) |> 
  unnest_longer(grapheme) |> 
  mutate(phoneme = replace(phoneme, grapheme == "õ", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ė̃̃", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ɘ̃", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ə̃", "ə̃"),
         phoneme = replace(phoneme, grapheme == "ė̃", "ə̃")) |> 
  rename(commons = `Common transcription for all texts`)

# Helfrich's (1916) `oö` in common orthography becomes "o'o"

# tribble(
#   ~phoneme, ~source, ~grapheme,
#   "ī", "Brouwer <1855", "ie",
#   "ī", "vd Straten & S. 1855", "ie",
#   "ī", "Walland 1864", "ie"
# )

# read_xlsx("/Users/Primahadi/Downloads/Enggano-transcriptions.xlsx", sheet = 3, range = "A1:V24") |>
#   write_rds("data/diphthong.rds")

diphthong <- read_rds("data/diphthong.rds") |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\\n", " ; "))) |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " "))) |> 
  (\(x) x[1:20, -22])() |> 
  pivot_longer(cols = -c(phoneme, `Common transcription for all texts`), names_to = "source", values_to = "grapheme") |> 
  filter(str_detect(grapheme, "^(?i)not?", negate = TRUE)) |> 
  mutate(grapheme = str_replace(grapheme, "_y", "")) |> 
  mutate(grapheme = str_trim(grapheme, side = "both")) |> 
  mutate(graphemes = str_split(grapheme, "\\/")) |> 
  unnest_longer(graphemes) |> 
  rename(commons = `Common transcription for all texts`)


# turn multybites character into a single one (esp. for those with diacritics)
stringi::stri_trans_nfc("Kã́hlēr")

## test if the above conversion is the same with the unified character (the right of `==`)
stringi::stri_trans_nfc("Kã́hlēr") == "Kã́hlēr"
# [1] TRUE

## the following produces FALSE as expected
"Kã́hlēr"==stringi::stri_trans_nfc("Kã́hlēr")
