library(qlcData)

source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")

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
                         ordering = NULL, # cf. Moran & Cysouw (2018: 112-114)
                         normalize = "NFC", 
                         sep.replace = "#")

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
                         sep.replace = "#")

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
