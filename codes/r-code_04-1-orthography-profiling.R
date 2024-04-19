library(qlcData)

source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")

# for Pak Cok (18 March 2024)
eno_etym_long_mini8 |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  write_tsv("data/dummy_for_pak_cok.tsv")

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

# eno_etym_long_mini9 <- eno_etym_long_mini8 |> 
#   mutate(words = str_split(words, "\\s\\;\\s")) |> 
#   unnest_longer(words)

eno_etym_long_mini8 |> 
  count(year, EngganoLanguage, EngganoSource) |> 
  arrange(year) |> 
  as.data.frame()

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
                         ordering = "context", # cf. Moran & Cysouw (2018: 112-114)
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
                                 ordr = "context")
#### combined with the main data ====
brouwer <- brouwer |> 
  left_join(brw_str_phon, by = join_by(ortho_id))
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

### map the phonemic data to the skeleton profile ========
bwg_prof <- read_prof("ortho/_02-boewang1854_profile-skeleton.tsv")
bwg_prof_phon <- phoneme_map(bwg_prof, trx)
bwg_prof_phon |> filter(Phoneme == "")
bwg_str_phon <- phoneme_tokenise(boewang$words, orth_prof = bwg_prof_phon)
#### combined with the main data ====
boewang <- boewang |> 
  left_join(bwg_str_phon, by = join_by(ortho_id))
##### save the tokenised and transliterated strings =====
boewang |> 
  select(words, commons, commons_tokenised = transliterated, ipa, ipa_tokenised) |> 
  write_tsv("ortho/_02-boewang1854_strings-ipa.tsv")



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
                         ordering = "context", # cf. Moran & Cysouw (2018: 112-114)
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
                                 ordr = "context")
#### combined with the main data ====
vrosenberg <- vrosenberg |> 
  left_join(vrosen_str_phon, by = join_by(ortho_id))
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
                         sep.replace = "#")

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
                         regex = FALSE)

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
                         regex = FALSE)

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
