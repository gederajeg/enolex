library(qlcData)

source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")

# for Pak Cok (18 March 2024)
# eno_etym_long_mini8 |> 
#   mutate(across(where(is.character), ~replace_na(., ""))) |> 
#   write_tsv("data/dummy_for_pak_cok.tsv")

read_prof <- function(filepath = NULL) {
  df <- read.table(filepath,
                   sep = "\t",
                   quote = "",
                   header = TRUE,
                   fill = TRUE,
                   colClasses = "character",
                   comment.char = "")
  return(df)
}

phoneme_map <- function(profile_df, phoneme_df) {
  prof_df <- profile_df |> 
    mutate(Replacement = str_replace_all(Replacement,
                                         "ː",
                                         ":"))
  phon_df <- phoneme_df |> 
    select(Replacement = commons,
           Phoneme = phoneme) |> 
    distinct()
  
  tb1 <- prof_df |> 
    left_join(phon_df, by = join_by(Replacement)) |> 
    mutate(across(where(is.character), ~replace_na(., "")))
  
  tb1 <- tb1 |> 
    mutate(Phoneme = if_else(Phoneme == "" & str_detect(Replacement, "^([[:punct:]]|\\s)$"),
                             Replacement,
                             Phoneme),
           Phoneme = if_else(Phoneme == "" & str_detect(Replacement, "^([aiueo])\\1$"),
                             str_replace_all(Replacement, "^([aiueo])\\1$", "\\1ː"),
                             Phoneme))
  return(tb1)
}

phoneme_tokenise <- function(str, orth_prof, rgx = FALSE, ordr = NULL) {
  
  tb <- qlcData::tokenize(str,
                          profile = orth_prof,
                          transliterate = "Phoneme",
                          ordering = ordr,
                          normalize = "NFC",
                          method = "global",
                          sep.replace = "#",
                          regex = rgx)$strings |> 
    mutate(ortho_id = row_number()) |> 
    mutate(ipa = str_replace_all(transliterated, "\\s{1}", ""),
           ipa = str_replace_all(ipa, "\\#", " ")) |> 
    select(ortho_id, ipa_tokenised = transliterated, ipa)
  
  return(tb)
  
}

get_ortho_cols <- function(df) {
  
  return(select(df, words, originals, matches("^english"), tokenized, transliterated, commons, matches("^ipa$")))
  
}

# eno_etym_long_mini9 <- eno_etym_long_mini8 |> 
#   mutate(words = str_split(words, "\\s\\;\\s")) |> 
#   unnest_longer(words)

eno_etym_long_mini8 |> 
  count(year, EngganoLanguage, EngganoSource) |> 
  arrange(year) |> 
  as.data.frame()

eno_etym_long_mini8 <- eno_etym_long_mini8 |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("õ̲"), stri_trans_nfc("õ̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("o̲"), stri_trans_nfc("o̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("a̲"), stri_trans_nfc("a̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("e̲"), stri_trans_nfc("e̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("ẽ̲"), stri_trans_nfc("ẽ̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("o̲"), stri_trans_nfc("o̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("õ̲"), stri_trans_nfc("õ̠")),
                         words)) |>
  mutate(words = if_else(EngganoSource == "Capell 1982",
                         str_replace_all(words, stri_trans_nfc("u̲"), stri_trans_nfc("u̠")),
                         words))

# Brouwer < 1855 ====
brouwer <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Brouwer <1855") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for Brouwer < 1855 ====
# qlcData::write.profile(brouwer$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_01-brouwer1855_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
brw <- qlcData::tokenize(brouwer$words, 
                         profile = "ortho/_01-brouwer1855_profile-skeleton.tsv", 
                         file.out = "ortho/_01-brouwer1855",
                         method = "global", 
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
brw_str <- brw$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
brouwer <- brouwer |> 
  left_join(brw_str, by = join_by(ortho_id))

get_ortho_cols(brouwer)

### map the phonemic data to the skeleton profile ========
brw_prof <- read_prof("ortho/_01-brouwer1855_profile-skeleton.tsv")
brw_prof_phon <- phoneme_map(brw_prof, trx)
brw_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
# brw_prof_phon <- brw_prof_phon |> 
#   mutate(Phoneme = if_else(Phoneme == "" & Grapheme == "s",
#                            "ç",
#                            Phoneme))
brw_str_phon <- phoneme_tokenise(brouwer$words, 
                                 orth_prof = brw_prof_phon, 
                                 rgx = TRUE,
                                 ordr = NULL)
#### combined with the main data ====
brouwer <- brouwer |> 
  left_join(brw_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))
get_ortho_cols(brouwer)
##### save the tokenised and transliterated strings =====
brouwer |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_01-brouwer1855_strings-ipa.tsv")



# Boewang 1854 ====
boewang <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Boewang 1854") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for Boewang 1854 ====
# qlcData::write.profile(boewang$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_02-boewang1854_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
bwg <- qlcData::tokenize(boewang$words, 
                         profile = "ortho/_02-boewang1854_profile-skeleton.tsv", 
                         file.out = "ortho/_02-boewang1854",
                         method = "global", 
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#")

### tidying up the segmentised and transliterated table ====
bwg_str <- bwg$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
boewang <- boewang |> 
  left_join(bwg_str, by = join_by(ortho_id))

get_ortho_cols(boewang)

### map the phonemic data to the skeleton profile ========
bwg_prof <- read_prof("ortho/_02-boewang1854_profile-skeleton.tsv")
bwg_prof_phon <- phoneme_map(bwg_prof, trx)
bwg_prof_phon |> filter(Phoneme == "")
bwg_str_phon <- phoneme_tokenise(boewang$words, orth_prof = bwg_prof_phon)
#### combined with the main data ====
boewang <- boewang |> 
  left_join(bwg_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(boewang)
##### save the tokenised and transliterated strings =====
boewang |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_02-boewang1854_strings-ipa.tsv")

get_ortho_cols(boewang)



# Van Rosenberg 1855 ====
vrosenberg <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "v. Rosenberg 1855") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for v. Rosenberg 1855 ====
# qlcData::write.profile(vrosenberg$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_03-vRosenberg1855_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
vrosen <- qlcData::tokenize(vrosenberg$words, 
                         profile = "ortho/_03-vRosenberg1855_profile-skeleton.tsv", 
                         file.out = "ortho/_03-vRosenberg1855",
                         method = "global", 
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
vrosen_str <- vrosen$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
vrosenberg <- vrosenberg |> 
  left_join(vrosen_str, by = join_by(ortho_id))

get_ortho_cols(vrosenberg)

### map the phonemic data to the skeleton profile ========
vrosen_prof <- read_prof("ortho/_03-vRosenberg1855_profile-skeleton.tsv")
vrosen_prof_phon <- phoneme_map(vrosen_prof, trx)
vrosen_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
vrosen_prof_phon <- vrosen_prof_phon |>
  mutate(Phoneme = if_else(Phoneme == "" & Replacement %in% c("a'a", "a'o", "a'i"),
                           str_replace_all(Replacement, "\\'(.)$", "ʔ\\1"),
                           Phoneme),
         Phoneme = if_else(Left == "b" & Right == "h",
                           "ə",
                           Phoneme),
         Phoneme = replace(Phoneme, Replacement == "K" & Phoneme == "", "k"))
vrosen_str_phon <- phoneme_tokenise(vrosenberg$words, 
                                 orth_prof = vrosen_prof_phon, 
                                 rgx = TRUE,
                                 ordr = NULL)
vrosen_prof_phon |> filter(Phoneme == "")

#### combined with the main data ====
vrosenberg <- vrosenberg |> 
  left_join(vrosen_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))
get_ortho_cols(vrosenberg)
##### save the tokenised and transliterated strings =====
vrosenberg |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_03-vRosenberg1855_strings-ipa.tsv")








# Van de Straten & Severijn 1855 ====
vdstraten <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "vd Straten & S. 1855") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for Van de Straten & Severijn 1855 ====
# qlcData::write.profile(vdstraten$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_04-vdStraten1855_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
vds <- qlcData::tokenize(vdstraten$words, 
                            profile = "ortho/_04-vdStraten1855_profile-skeleton.tsv", 
                            file.out = "ortho/_04-vdStraten1855",
                            method = "global", 
                            transliterate = "Replacement", 
                            ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                            normalize = "NFC", 
                            sep.replace = "#")

### tidying up the segmentised and transliterated table ====
vds_str <- vds$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
vdstraten <- vdstraten |> 
  left_join(vds_str, by = join_by(ortho_id))

get_ortho_cols(vdstraten)

### map the phonemic data to the skeleton profile ========
vdstraten_prof <- read_prof("ortho/_04-vdStraten1855_profile-skeleton.tsv")
vdstraten_prof_phon <- phoneme_map(vdstraten_prof, trx)
vdstraten_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
vdstraten_prof_phon <- vdstraten_prof_phon |>
  mutate(Phoneme = replace(Phoneme,
                           Phoneme == "" & Grapheme == "nj",
                           "ɲ")) |> 
  mutate(Phoneme = replace(Phoneme,
                           Phoneme == "" & Grapheme == "g",
                           "k"))
vdstraten_str_phon <- phoneme_tokenise(vdstraten$words, 
                                    orth_prof = vdstraten_prof_phon, 
                                    rgx = FALSE,
                                    ordr = NULL)

#### combined with the main data ====
vdstraten <- vdstraten |> 
  left_join(vdstraten_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))
get_ortho_cols(vdstraten)
##### save the tokenised and transliterated strings =====
vdstraten |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_04-vdStraten1855_strings-ipa.tsv")





# Walland 1864 ====
walland <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Walland 1864") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for Walland 1864 ====
# qlcData::write.profile(walland$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_05-walland1864_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
wlnd <- qlcData::tokenize(walland$words, 
                         profile = "ortho/_05-walland1864_profile-skeleton.tsv", 
                         file.out = "ortho/_05-walland1864",
                         method = "global", 
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
wlnd_str <- wlnd$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
walland <- walland |> 
  left_join(wlnd_str, by = join_by(ortho_id))

get_ortho_cols(walland)

### map the phonemic data to the skeleton profile ========
walland_prof <- read_prof("ortho/_05-walland1864_profile-skeleton.tsv")
walland_prof_phon <- phoneme_map(walland_prof, trx)
walland_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
walland_prof_phon <- walland_prof_phon |>
  mutate(Phoneme = replace(Phoneme,
                           Phoneme == "" & Grapheme == "nj",
                           "ɲ"))
walland_prof_phon <- walland_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Grapheme == "L",
                           "l"))
walland_prof_phon <- walland_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Grapheme == "s",
                           "ç"))
walland_prof_phon <- walland_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Grapheme == "v",
                           "w"))
walland_str_phon <- phoneme_tokenise(walland$words, 
                                       orth_prof = walland_prof_phon, 
                                       rgx = TRUE,
                                       ordr = NULL)

#### combined with the main data ====
walland <- walland |> 
  left_join(walland_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(walland)

##### save the tokenised and transliterated strings =====
walland |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_05-walland1864_strings-ipa.tsv")







# Francis 1870 ====
francis <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Francis 1870") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for Francis 1870 ====
# qlcData::write.profile(francis$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_06-francis1870_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
frc <- qlcData::tokenize(francis$words, 
                         profile = "ortho/_06-francis1870_profile-skeleton.tsv", 
                         file.out = "ortho/_06-francis1870",
                         method = "global", 
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
frc_str <- frc$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
francis <- francis |> 
  left_join(frc_str, by = join_by(ortho_id))

get_ortho_cols(francis)


### map the phonemic data to the skeleton profile ========
francis_prof <- read_prof("ortho/_06-francis1870_profile-skeleton.tsv")
francis_prof_phon <- phoneme_map(francis_prof, trx)
francis_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
francis_prof_phon <- francis_prof_phon |>
  mutate(Phoneme = replace(Phoneme,
                           Phoneme == "" & Grapheme == "nj",
                           "ɲ"))
francis_prof_phon <- francis_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "o'o",
                           "oʔo"))
francis_prof_phon <- francis_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "a'a",
                           "aʔa"))
francis_str_phon <- phoneme_tokenise(francis$words, 
                                     orth_prof = francis_prof_phon, 
                                     rgx = TRUE,
                                     ordr = NULL)

#### combined with the main data ====
francis <- francis |> 
  left_join(francis_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(francis)

##### save the tokenised and transliterated strings =====
francis |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_06-francis1870_strings-ipa.tsv")





# Oudemans 1879 ====
oudemans <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Oudemans 1879") |> 
  mutate(ortho_id = row_number())
## create a skeleton profile for "Oudemans 1879" ====
# qlcData::write.profile(oudemans$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_07-oudemans1879_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
odm <- qlcData::tokenize(oudemans$words, 
                         profile = "ortho/_07-oudemans1879_profile-skeleton.tsv", 
                         file.out = "ortho/_07-oudemans1879",
                         method = "global", 
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
odm_str <- odm$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
oudemans <- oudemans |> 
  left_join(odm_str, by = join_by(ortho_id))

get_ortho_cols(oudemans)


### map the phonemic data to the skeleton profile ========
oudemans_prof <- read_prof("ortho/_07-oudemans1879_profile-skeleton.tsv")
oudemans_prof_phon <- phoneme_map(oudemans_prof, trx)
oudemans_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
oudemans_prof_phon <- oudemans_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "o'o",
                           "oʔo"))
oudemans_prof_phon <- oudemans_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "a'u",
                           "aʔu"))
oudemans_prof_phon <- oudemans_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "i'e",
                           "iʔe"))
oudemans_str_phon <- phoneme_tokenise(oudemans$words, 
                                     orth_prof = oudemans_prof_phon, 
                                     rgx = TRUE,
                                     ordr = NULL)

#### combined with the main data ====
oudemans <- oudemans |> 
  left_join(oudemans_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(oudemans)

##### save the tokenised and transliterated strings =====
oudemans |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_07-oudemans1879_strings-ipa.tsv")






# Helfrich 1888 (NEED RECHECK WHEN DANIEL HAS FINISHED) ====
helfrich1888 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Helfrich 1888") |> 
  mutate(ortho_id = row_number())
## check trema for glottal stop in Helfrich 1888 ====
helfrich1888 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
## create a skeleton profile for "Helfrich 1888" ====
# qlcData::write.profile(helfrich1888$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_08-helfrich1888_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
h88 <- qlcData::tokenize(helfrich1888$words, 
                         profile = "ortho/_08-helfrich1888_profile-skeleton.tsv", 
                         file.out = "ortho/_08-helfrich1888",
                         method = "global",
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
h88_str <- h88$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
helfrich1888 <- helfrich1888 |> 
  left_join(h88_str, by = join_by(ortho_id))

get_ortho_cols(helfrich1888)









# Helfrich & Pieters 1891 ====
helfrich_pieters1891 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Helfrich & Pieters 1891") |> 
  mutate(ortho_id = row_number())

## check trema for glottal stop in Helfrich & Pieters 1891 ====
helfrich_pieters1891 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
### there is only "aï", but not ï preceded by other vowels
helfrich_pieters1891 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words) |> filter(str_detect(words, stri_trans_nfc("(aï)")))
helfrich_pieters1891 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words) |> filter(str_detect(words, stri_trans_nfc("([iueo]ï)")))

## create a skeleton profile for "Helfrich & Pieters 1891" ====
# qlcData::write.profile(helfrich_pieters1891$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_09-helfrich_pieters1891_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
hp1891 <- qlcData::tokenize(helfrich_pieters1891$words, 
                         profile = "ortho/_09-helfrich_pieters1891_profile-skeleton.tsv", 
                         file.out = "ortho/_09-helfrich_pieters1891",
                         method = "global",
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
hp1891_str <- hp1891$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
helfrich_pieters1891 <- helfrich_pieters1891 |> 
  left_join(hp1891_str, by = join_by(ortho_id))

get_ortho_cols(helfrich_pieters1891)

### map the phonemic data to the skeleton profile ========
helfrich_pieters_prof <- read_prof("ortho/_09-helfrich_pieters1891_profile-skeleton.tsv")
helfrich_pieters_prof_phon <- phoneme_map(helfrich_pieters_prof, trx)
helfrich_pieters_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
helfrich_pieters_prof_phon <- helfrich_pieters_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "'o",
                           "ʔo"))
helfrich_pieters_prof_phon <- helfrich_pieters_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ė",
                           "ə"))
helfrich_pieters_prof_phon <- helfrich_pieters_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ŋ",
                           "ŋ"))
helfrich_pieters_prof_phon <- helfrich_pieters_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ñ",
                           "ɲ"))
helfrich_pieters_prof_phon <- helfrich_pieters_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ï",
                           "u̇"))
helfrich_pieters_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
helfrich_pieters_str_phon <- phoneme_tokenise(helfrich_pieters1891$words, 
                                      orth_prof = helfrich_pieters_prof_phon, 
                                      rgx = TRUE,
                                      ordr = NULL)

#### combined with the main data ====
helfrich_pieters1891 <- helfrich_pieters1891 |> 
  left_join(helfrich_pieters_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(helfrich_pieters1891)

##### save the tokenised and transliterated strings =====
helfrich_pieters1891 |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_09-helfrich_pieters1891_strings-ipa.tsv")




# modigliani 1894 ====
modi1894 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Modigliani 1894") |> 
  mutate(ortho_id = row_number(),
         words = str_replace(words, "ſ", "f"))
## check trema for glottal stop in Modigliani 1894 ====
modi1894 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
## create a skeleton profile for "Modigliani 1894" ====
# qlcData::write.profile(modi1894$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_10-modi1894_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
mod1894 <- qlcData::tokenize(modi1894$words, 
                         profile = "ortho/_10-modi1894_profile-skeleton.tsv", 
                         file.out = "ortho/_10-modi1894",
                         method = "global",
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
mod1894_str <- mod1894$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
modi1894 <- modi1894 |> 
  left_join(mod1894_str, by = join_by(ortho_id))

get_ortho_cols(modi1894)


### map the phonemic data to the skeleton profile ========
modi1894_prof <- read_prof("ortho/_10-modi1894_profile-skeleton.tsv")
modi1894_prof_phon <- phoneme_map(modi1894_prof, trx)
modi1894_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
modi1894_prof_phon <- modi1894_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ė",
                           "ə"))
modi1894_prof_phon <- modi1894_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ñ",
                           "ɲ"))
modi1894_prof_phon <- modi1894_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "C",
                           "k"))
modi1894_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
modi1894_str_phon <- phoneme_tokenise(modi1894$words, 
                                              orth_prof = modi1894_prof_phon, 
                                              rgx = TRUE,
                                              ordr = NULL)

#### combined with the main data ====
modi1894 <- modi1894 |> 
  left_join(modi1894_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(modi1894)

##### save the tokenised and transliterated strings =====
modi1894 |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_10-modi1894_strings-ipa.tsv")








# V. D. Noord (1895; Stokhof 1987 Holle List) =====
hollelist <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Stockhof 1987") |> 
  mutate(ortho_id = row_number())
## check trema for glottal stop in V. d. Noord (1895; Stokhof 1987 Holle List) ====
hollelist |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
hollelist |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[āīūēō]|[āīūēō].)"))) |>
  select(words)
## create a skeleton profile for "V. D. Noord (1895)" ====
# qlcData::write.profile(hollelist$words, normalize = "NFC", editing = TRUE, info = FALSE,
                       # file.out = "ortho/_11-stockhof1987_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
holle <- qlcData::tokenize(hollelist$words, 
                             profile = "ortho/_11-stockhof1987_profile-skeleton.tsv", 
                             file.out = "ortho/_11-stockhof1987",
                             method = "global",
                             transliterate = "Replacement", 
                             ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                             normalize = "NFC", 
                             sep.replace = "#",
                             regex = TRUE)

### tidying up the segmentised and transliterated table ====
holle_str <- holle$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
hollelist <- hollelist |> 
  left_join(holle_str, by = join_by(ortho_id))

get_ortho_cols(hollelist)

### map the phonemic data to the skeleton profile ========
hollelist_prof <- read_prof("ortho/_11-stockhof1987_profile-skeleton.tsv")
hollelist_prof_phon <- phoneme_map(hollelist_prof, trx)
hollelist_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "a'a",
                           "aʔa"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "OO",
                           "ɔː"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement  %in%  c("ă", "ă"),
                           "ᾰ"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement  %in%  c("ä", "ä"),
                           "ʔa"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "å",
                           "å"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ė",
                           "ə"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ñ",
                           "ɲ"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "š",
                           "ʃ"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "g",
                           "g"))
hollelist_prof_phon <- hollelist_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "s",
                           "s"))
hollelist_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
hollelist_str_phon <- phoneme_tokenise(hollelist$words, 
                                      orth_prof = hollelist_prof_phon, 
                                      rgx = TRUE,
                                      ordr = NULL)

#### combined with the main data ====
hollelist <- hollelist |> 
  left_join(hollelist_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(hollelist)

##### save the tokenised and transliterated strings =====
hollelist |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_11-stockhof1987_strings-ipa.tsv")











# Helfrich 1916 (NEED RECHECK WHEN DANIEL HAS FINISHED) ====
helfrich1916 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Helfrich 1916") |> 
  mutate(ortho_id = row_number())
## check trema for glottal stop in Helfrich 1916 ====
helfrich1916 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
## create a skeleton profile for "Helfrich 1916" ====
# qlcData::write.profile(helfrich1916$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_12-helfrich1916_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
h1916 <- qlcData::tokenize(helfrich1916$words, 
                         profile = "ortho/_12-helfrich1916_profile-skeleton.tsv", 
                         file.out = "ortho/_12-helfrich1916",
                         method = "global",
                         transliterate = "Replacement", 
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#",
                         regex = TRUE)

### tidying up the segmentised and transliterated table ====
h1916_str <- h1916$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
helfrich1916 <- helfrich1916 |> 
  left_join(h1916_str, by = join_by(ortho_id))

get_ortho_cols(helfrich1916)






# Amran et al. 1979 ====
amran1979 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Amran et al. 1979") |> 
  mutate(ortho_id = row_number())
## check trema for glottal stop in Amran et al. 1979 ====
amran1979 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
## create a skeleton profile for "Amran et al. 1979" ====
# qlcData::write.profile(amran1979$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                       file.out = "ortho/_13-amran1979_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
am1979 <- qlcData::tokenize(amran1979$words, 
                           profile = "ortho/_13-amran1979_profile-skeleton.tsv", 
                           file.out = "ortho/_13-amran1979",
                           method = "global",
                           transliterate = "Replacement", 
                           ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                           normalize = "NFC", 
                           sep.replace = "#",
                           regex = TRUE)

### tidying up the segmentised and transliterated table ====
am1979_str <- am1979$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
amran1979 <- amran1979 |> 
  left_join(am1979_str, by = join_by(ortho_id))

get_ortho_cols(amran1979)


### map the phonemic data to the skeleton profile ========
amran1979_prof <- read_prof("ortho/_13-amran1979_profile-skeleton.tsv")
amran1979_prof_phon <- phoneme_map(amran1979_prof, trx)
amran1979_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
amran1979_prof_phon <- amran1979_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ng",
                           "ŋ"))
amran1979_prof_phon <- amran1979_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "g",
                           "g"))
amran1979_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
amran1979_str_phon <- phoneme_tokenise(amran1979$words, 
                                       orth_prof = amran1979_prof_phon, 
                                       rgx = TRUE,
                                       ordr = NULL)

#### combined with the main data ====
amran1979 <- amran1979 |> 
  left_join(amran1979_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(amran1979)

##### save the tokenised and transliterated strings =====
amran1979 |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_13-amran1979_strings-ipa.tsv")














# Capell (1982) ====
capell1982 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == "Capell 1982") |> 
  mutate(ortho_id = row_number())
## check trema for glottal stop in Capell 1982 ====
capell1982 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
## create a skeleton profile for "Capell 1982" ====
# qlcData::write.profile(capell1982$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_14-capell1982_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
cp82 <- qlcData::tokenize(capell1982$words, 
                           profile = "ortho/_14-capell1982_profile-skeleton.tsv", 
                           file.out = "ortho/_14-capell1982",
                           method = "global",
                           transliterate = "Replacement", 
                           ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                           normalize = "NFC",
                           sep.replace = "#",
                           regex = FALSE)
cp82$errors
cp82$strings |> filter(str_detect(originals, stri_trans_nfc("o̠")))
cp82$strings |> filter(str_detect(originals, stri_trans_nfc("ẽ̠")))
cp82$missing

### tidying up the segmentised and transliterated table ====
cp82_str <- cp82$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
capell1982 <- capell1982 |> 
  left_join(cp82_str, by = join_by(ortho_id))

get_ortho_cols(capell1982)

### map the phonemic data to the skeleton profile ========
capell_prof <- read_prof("ortho/_14-capell1982_profile-skeleton.tsv")
capell_prof_phon <- phoneme_map(capell_prof, trx)
capell_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
capell_prof_phon <- capell_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("ẽ"),
                           "ẽ")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "e̲", "e")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "o̲", "o")) |>
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "ė", "ə")) |>
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "u̲", "u"))
capell_prof_phon <- capell_prof_phon |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("a̲"),
                           "a")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("̲e"),
                           "e")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("ẽ̲"),
                           "ẽ"))|> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("ẽ̠"),
                           "͏ɛ̃"))|> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "ẽ" & Replacement == "ẽ", "ɛ̃")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("ė"),
                           "ə")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("í"),
                           "i")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("ö"),
                           "ɨ")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("̲o"),
                           "o")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("õ̲"),
                           "õ")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == stri_trans_nfc("̲u"),
                           "u")) |> 
  mutate(Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "OO", "ɔː"),
         Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "õõ", "ɔ̃ː"),
         Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "õ", "ɔ̃"),
         Phoneme = replace(Phoneme, Phoneme == "" & Replacement == "í", "i"))
capell_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
capell_str_phon <- phoneme_tokenise(capell1982$words, 
                                      orth_prof = capell_prof_phon, 
                                      rgx = FALSE,
                                      ordr = NULL)

#### combined with the main data ====
capell1982 <- capell1982 |> 
  left_join(capell_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(capell1982)

##### save the tokenised and transliterated strings =====
capell1982 |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_14-capell1982_strings-ipa.tsv")




# Kahler (1987) ====
kahler1987 <- eno_etym_long_mini8 |> 
  filter(EngganoSource == stri_trans_nfc("Kähler 1987")) |> 
  mutate(ortho_id = row_number())
## check trema for glottal stop in Kahler 1987 ====
kahler1987 |> 
  filter(str_detect(words, stringi::stri_trans_nfc("(.[äïëöü]|[äïëöü].)"))) |>
  select(words)
## create a skeleton profile for "Kahler 1987" ====
# qlcData::write.profile(kahler1987$words, normalize = "NFC", editing = TRUE, info = FALSE,
#                        file.out = "ortho/_15-kahler1987_profile-skeleton.tsv")
### segmentise and transliterate after editing the skeleton profile ====
k87 <- qlcData::tokenize(kahler1987$words, 
                          profile = "ortho/_15-kahler1987_profile-skeleton.tsv", 
                          file.out = "ortho/_15-kahler1987",
                          method = "global",
                          transliterate = "Replacement", 
                          ordering = c("context", "size"), # cf. Moran & Cysouw (2018: 112-114)
                          normalize = "NFC",
                          sep.replace = "#",
                          regex = TRUE)
k87$errors
k87$strings |> filter(str_detect(originals, stri_trans_nfc("o̠")))
k87$strings |> filter(str_detect(originals, stri_trans_nfc("ẽ̠")))
k87$missing

### tidying up the segmentised and transliterated table ====
k87_str <- k87$strings |> 
  as_tibble() |> 
  mutate(ortho_id = row_number()) |> 
  mutate(commons = str_replace_all(transliterated, "\\s{1}", ""),
         commons = str_replace_all(commons, "\\#", " ")) |> 
  select(ortho_id, everything())

### combine with the main data ====
kahler1987 <- kahler1987 |> 
  left_join(k87_str, by = join_by(ortho_id))

get_ortho_cols(kahler1987)


### map the phonemic data to the skeleton profile ========
kahler1987_prof <- read_prof("ortho/_15-kahler1987_profile-skeleton.tsv")
kahler1987_prof_phon <- phoneme_map(kahler1987_prof, trx)
kahler1987_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme

# kahler1987_prof_phon <- kahler1987_prof_phon |> 
#   mutate(Phoneme = if_else(Phoneme == "" & str_detect(Replacement, "\\:"), str_replace(Replacement, "\\:", ""), Phoneme)) |> 
#   mutate(Phoneme = if_else(Phoneme == "" & str_detect(Replacement, "^(.)(.)\\1\\2"), str_c(str_extract(Replacement, "^.."), "ː", sep= ""), Phoneme)) |> 
#   mutate(Phoneme = if_else(Phoneme == "" & str_detect(Grapheme, stri_trans_nfc("ã̄")), ))
# 
# kahler1987_prof_phon |> filter(Phoneme == "")

# # special case for Kähler, I saved out the profile of the phoneme to be manually edited (below is the code to save it)
# # kahler1987_prof_phon |> write_tsv("ortho/_15-kahler1987_profile-skeleton-ipa.tsv")

# load the hand-edited IPA transliteration profile of Kahler (1987)
kahler1987_prof_phon <- read.table(file = "ortho/_15-kahler1987_profile-skeleton-ipa.tsv", 
                                   sep = "\t", header = TRUE, quote = "",
                                   encoding = "UTF-8") |> 
  as_tibble()

kahler1987_prof_phon <- kahler1987_prof_phon |>
  mutate(across(all_of(colnames(kahler1987_prof_phon)), 
                ~replace(., is.na(.), "")))

kahler1987_prof_phon |> filter(Phoneme == "") # check grapheme that has not Phoneme
kahler1987_str_phon <- phoneme_tokenise(kahler1987$words, 
                                       orth_prof = kahler1987_prof_phon, 
                                       rgx = TRUE,
                                       ordr = NULL)
kahler1987_str_phon |> as_tibble()
#### combined with the main data ====
kahler1987a <- kahler1987 |> 
  left_join(kahler1987_str_phon, by = join_by(ortho_id)) |> 
  mutate(across(matches("^(commons|transliterated)$"), ~str_replace_all(., "ː", ":")))

get_ortho_cols(kahler1987a)

##### save the tokenised and transliterated strings =====
kahler1987a |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_15-kahler1987_strings-ipa.tsv")
