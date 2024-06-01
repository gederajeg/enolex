# remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE)
# library(rcldf)
library(tidyverse)
library(common)

source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")

# acd <- rcldf::cldf("C:/Users/GRajeg/OneDrive - Nexus365/Documents/cldf/acd/cldf/cldf-metadata.json")
# acd <- rcldf::cldf("/Users/Primahadi/Documents/cldf_project/lexibank-acd-e39c642/cldf/cldf-metadata.json")
# write_rds(acd, "data/acd.rds")

acd <- read_rds("data/acd.rds")

summary(acd)
names(acd$tables)
acd$tables$protoforms.csv
acd$tables$FormTable
acd$tables$CognatesetTable

form_tb <- acd$tables$FormTable
language_tb <- acd$tables$LanguageTable
param_tb <- acd$tables$ParameterTable
cognate_tb <- acd$tables$CognateTable
cogset_tb <- acd$tables$CognatesetTable
contrib_tb <- acd$tables$ContributionTable
protoform_tb <- acd$tables$protoforms.csv
loan_tb <- acd$tables$loansets.csv
borrowing_tb <- acd$tables$BorrowingTable

# Keys between form_tb and cognate_tb is ID (in form_tb) and Form_ID (in cognate_tb)
# Keys between cogset_tb and cognate_tb is ID (cogset_tb) and Cognateset_ID (cognate_tb)
# Keys between language_tb and form_tb is ID (language_tb) and Language_ID (form_tb)
# The Reconstruction_ID column in the cognate_tb table matches with the ID column in the protoform_tb table
# The Cognateset_ID column in the cognate_tb table matches with the Cognateset_ID column in the protoform_tb table
# The cogset_tb table provides the protoform of the words
## For instance, in the EnoLEX spreadsheet, to search for the PMP etymon "*abut / *Ramut" in the ACD, these etymos need to be searched in the Form column of the cogset_tb
## cogset_tb |> filter(str_detect(Form, "^\\*abut"))
## Then, pull the relevant ID in the cogset_tb to be combined with the ACD clld version URL

cognateset_url_base <- "https://acd.clld.org/cognatesets/"
paste0(cognateset_url_base, "25576")
paste0(cognateset_url_base, "29851")


# get the distinct values of the etymology table
proto_distinct <- eno_etym_long_proto_df |> 
  filter(if_any(matches("_etymon"), ~!is.na(.))) |> 
  select(id, matches("^(PAN|PMP|Etymological)")) |> 
  distinct() |> 
  mutate(across(where(is.character), ~replace_na(., "")))

proto_distinct |> 
  filter(if_any(matches("etymon"), ~grepl("təlu", ., perl = TRUE)))

# proto_pan <- proto_distinct |> 
#   select(id, PAN_etymon, PAN_gloss, PAN_source, Etymological_source)
# proto_pmp <- proto_distinct |> 
#   select(id, PMP_etymon, PMP_gloss, PMP_source, Etymological_source)

# try to work from the gloss: get the gloss/parameter ID and then retrieve the form_ID
## before this, create wide table using rcldf::as.cldf.wide(acd, "FormTable")


CognateTable <- rcldf::as.cldf.wide(acd, "CognateTable")
FormTable <- rcldf::as.cldf.wide(acd, "FormTable")
CognateSetTable <- rcldf::as.cldf.wide(acd, "CognatesetTable")
CognateTable |> 
  colnames() |> 
  (\(x) str_subset(x, "^Form"))()

get_cognateset <- function(tb, rgx) {
  tb1 <- tb |> 
    filter(if_any(matches("^Form\\."), ~str_detect(., rgx)))
  return(tb1)
}

get_subproto <- function(tb, rgx, is_proto = NULL) {
  tb1 <- tb |> 
    filter(if_any(matches("(^Form|^Value)"), ~str_detect(., rgx)))
  if (is_proto == TRUE) {
    tb2 <- filter(tb1, if_any(matches("(^is_proto$|^is_proto\\.FormTable$)"), function(x) x == TRUE))
    return(tb2)
  } else {
    return(tb1)
  }
}

# CognateTable |> 
#   get_cognateset("RuqaNay")



# IGNORE THE ABOVE ======

library(tidyverse)

source("codes/r-code_01-lexdb-pre-processing.R")
source("codes/r-code_02-orthography.R")

cognateset_url_base <- "https://acd.clld.org/cognatesets/"

acd <- read_rds("data/acd.rds")

CognateTable <- rcldf::as.cldf.wide(acd, "CognateTable")
FormTable <- rcldf::as.cldf.wide(acd, "FormTable")
CognateSetTable <- rcldf::as.cldf.wide(acd, "CognatesetTable")

form_tb <- acd$tables$FormTable
language_tb <- acd$tables$LanguageTable
param_tb <- acd$tables$ParameterTable
cognate_tb <- acd$tables$CognateTable
cogset_tb <- acd$tables$CognatesetTable
contrib_tb <- acd$tables$ContributionTable
protoform_tb <- acd$tables$protoforms.csv
loan_tb <- acd$tables$loansets.csv
borrowing_tb <- acd$tables$BorrowingTable

# get the distinct values of the etymology table
proto_distinct <- eno_etym_long_proto_df |> 
  filter(if_any(matches("_etymon"), ~!is.na(.))) |> 
  select(id, matches("^(PAN|PMP|Etymological)")) |> 
  distinct() |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(PAN_url = PAN_etymon,
         PMP_url = PMP_etymon)

get_cognateset <- function(tb, rgx) {
  tb1 <- tb |> 
    filter(if_any(matches("^Form\\."), ~str_detect(., rgx)))
  return(tb1)
}

get_subproto <- function(tb, rgx) {
  tb1 <- tb |> 
    filter(if_any(matches("(^Form|^Value)"), ~str_detect(., rgx)))
  return(tb1)
}

get_id_cognateset <- function(tb, rgx) {
  return(unique(get_cognateset(tb, rgx)$Cognateset_ID.CognateTable))
}

get_url <- function(cols, rgx, url_base, url_part) {
  str_replace_all(cols, rgx, str_c('<a href="', url_base, url_part, '" target="_blank">\\1</a>', sep = ""))
}

# gloss_rgx <- "guts"
get_form_from_gloss <- function(gloss_rgx) {
  tb <- param_tb |> 
    filter(str_detect(Name, gloss_rgx))
  param_rgx <- tb |> 
    pull(ID) |> 
    (\(x) paste(x, collapse = "|"))() |> 
    (\(x) paste("(", x, ")", sep = ""))()
  form_tb |> 
    filter(str_detect(ID, param_rgx)) |> 
    select(ID, Language_ID, Value, Form, is_proto)
  
}

# acd_form <- "telu"
pan_url_create <- function(acd_form) {
  
  cognateset_url_base <- "https://acd.clld.org/cognatesets/"
  rgx <- paste("^\\*", acd_form, "$", sep = "")
  forms <- form_tb |> 
    filter(str_detect(Value, rgx)) |> 
    filter(is_proto) |> 
    filter(str_detect(ID, "^protoform\\-[0-9]+$"))
  forms
  proform <- protoform_tb |> 
    filter(Form_ID %in% forms$ID)
  # proform
  pan_url <- paste(cognateset_url_base, proform$Cognateset_ID, sep = "")
  # pan_url
  return(pan_url)
  
}
# enolex_form <- "\\(i\\)sa"
get_pan_url <- function(cols, enolex_form, pan_url = NULL) {
  
  rgx_to_replace <- paste("(\\b\\\\*?", enolex_form, "\\b)", sep = "")
  rgx_replacement <- paste('<a href="', pan_url, '" target="_blank">\\1</a>', sep = "")
  formsurl <- str_replace_all(cols, rgx_to_replace, rgx_replacement)
  return(formsurl)
  
}


# acd_form <- "mata"
pmp_url_create <- function(acd_form) {
  
  (rgx <- paste("^\\*", acd_form, "$", sep = ""))
  forms <- form_tb |> 
    filter(str_detect(Value, rgx)) |> 
    filter(is_proto) |> 
    filter(str_detect(ID, "^protoform\\-[0-9]+$"))
  proform <- protoform_tb |> 
    filter(Form_ID %in% forms$ID)
  # proform
  pmp_url <- paste(cognateset_url_base, proform$Cognateset_ID, "#s-", proform$ID, sep = "")
  return(pmp_url)
  
} # this function can be used to create url of the sub-proto-form (for example, the form "C<in>aqi" (https://acd.clld.org/cognatesets/25881#s-2006) is a PAN-labelled reconstruction of sub-proto-form of the main/super proto form "*Caqi" (https://acd.clld.org/cognatesets/25881))

pan_etymon <- proto_distinct$PAN_etymon
pmp_etymon <- proto_distinct$PMP_etymon

proto_distinct1 <- proto_distinct |> 
  mutate(PAN_url = get_url(PAN_url, "(\\b\\\\*ma\\-RuqaNay\\b)", cognateset_url_base, get_id_cognateset(CognateTable, '^\\*RuqaNay$')),
         PMP_url = str_replace_all(PMP_url, "(\\b\\\\*maRuqanay\\b)", '<a href="https://acd.clld.org/cognatesets/28052#s-4939" target="_blank">\\1</a>'),
         PMP_url = str_replace_all(PMP_url, "(\\b\\\\*baRani\\b)", '<a href="https://acd.clld.org/cognatesets/25144#s-657" target="_blank">\\1</a>')) |> 
  mutate(PAN_url = get_url(PAN_url, "(\\b\\\\*Sika\\b)", cognateset_url_base, get_id_cognateset(CognateTable, '^\\*Sika-$')),
         PMP_url = str_replace_all(PMP_url, "(\\\\*?\\(i\\)ka\\b)", '<a href="https://acd.clld.org/cognatesets/26451#s-11621" target="_blank">\\1</a>')) |> 
  mutate(across(matches("url"), ~get_url(., "(\\b\\\\*?lima\\b)", cognateset_url_base, get_id_cognateset(CognateTable, '^\\*lima$')))) |> 
  mutate(PAN_url = get_url(PAN_url, "(\\b\\\\*Səpat\\b)", cognateset_url_base, get_id_cognateset(CognateTable, '^\\*Sepat$'))) |> 
  mutate(PMP_url = replace(PMP_url, PMP_url == "*kapət", '*<a href="https://acd.clld.org/cognatesets/30436" target="_blank">kapət</a>')) |> 
  mutate(across(matches("url"), ~get_url(., "(\\b\\\\*qaqay\\b)", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*qaqay$")))) |> 
  mutate(across(matches("url"), ~get_url(., "(\\bwaqay\\b)", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*waqay$")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "maCa", pan_url_create("maCa")),
         PMP_url = get_pan_url(PMP_url, "mata", pmp_url_create("mata"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "duSa", pan_url_create("duSa")),
         PMP_url = get_pan_url(PMP_url, "duha", pmp_url_create("duha"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "amax", pan_url_create("amax")),
         PMP_url = get_pan_url(PMP_url, stri_trans_nfc("ama\\s\\/\\s\\*ama\\-ŋ"), pmp_url_create(stri_trans_nfc("amá\\-ŋ")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "Rumaq", pan_url_create("Rumaq")),
         PMP_url = get_pan_url(PMP_url, "Rumaq", pmp_url_create("Rumaq"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., stri_trans_nfc("təlu"), pan_url_create("telu")))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, stri_trans_nfc("əpat"), pmp_url_create("epat"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "C\\<in\\>aqi", pmp_url_create("C\\<in\\>aqi")),
         PMP_url = get_pan_url(PMP_url, "t\\<in\\>aqi", pmp_url_create("t\\<in\\>aqi"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "k\\-ita", pmp_url_create("k\\-ita"))) |>   # PMP "kita" is not explicitly attested in ACD under PMP
  mutate(PAN_url = get_pan_url(PAN_url, "um\\-a\\(R\\)i", pmp_url_create("um\\-ai")),
         PMP_url = get_pan_url(PMP_url, "um\\-aRi", pmp_url_create("um\\-aRi"))) |> # PMP "um-aRi" is not explicitly listed in ACD under "*ai3" but in "*aRi"! (other than mentioned in the note)
  mutate(PAN_url = get_pan_url(PAN_url, "kuliC", pan_url_create("kuliC")),
         PMP_url = get_pan_url(PMP_url, "kulit", pmp_url_create("kulit")[1])) |> 
  mutate(across(matches("url"), ~get_pan_url(., stri_trans_nfc("tuqəd"), pan_url_create("tuqed")))) |> 
  mutate(across(matches("url"), ~str_replace_all(., "(?<=\\*)(\\(i\\)sa\\b)", str_c('<a href="', pan_url_create("isa₁"), '" target="_blank">\\1</a>', sep = "")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "k\\<aN\\>uSkuS", pmp_url_create("k\\<aN\\>uSkuS")),
         PMP_url = get_pan_url(PMP_url, "k\\<an\\>uhkuh", pmp_url_create("k\\<an\\>uhkuh"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "a\\(R\\)i", pan_url_create("aRi"))) |> 
  mutate(across(matches("url"), ~str_replace_all(., "(\\(a[Nn]ak i\\) qaRta)", str_c('<a href="', pmp_url_create("anak i qaRta"), '" target="_blank">\\1</a>', sep = "")))) |> 
  mutate(across(matches("url"), ~str_replace_all(., stri_trans_nfc("(\\(b\\)əli|\\(b\\/w\\)əli)"), '<a href="https://acd.clld.org/cognatesets/25306#s-909" target="_blank">\\1</a>'))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "kawil", pan_url_create("kawil₁")),
         PMP_url = str_replace_all(PMP_url, "(kawil\\(ən\\))", str_c('<a href="', pan_url_create("kawil₁"), '" target="_blank">\\1</a>', sep = ""))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "dapaR", pan_url_create("dapaR")),
         PMP_url = get_pan_url(PMP_url, "lapad", pan_url_create("lapad")),
         PMP_url = str_replace_all(PMP_url, "\\b(buku\\(h\\))", str_c('<a href="', pan_url_create("bukuh"), '" target="_blank">\\1</a>', sep = "")),
         PMP_url = get_pan_url(PMP_url, "buŋkul", pan_url_create("buŋkul"))) |> 
  mutate(PMP_url = str_replace_all(PMP_url, "(?<=\\*)(bahu\\(q\\))", str_c('<a href="', pan_url_create("bahuq"), '" target="_blank">\\1</a>', sep = ""))) |> 
  mutate(PMP_url = get_url(PMP_url, "(\\bzəlaq\\b)", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*zelaq$")),
         PMP_url = get_url(PMP_url, "(\\bdilat\\b)", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*dilat$")),
         PMP_url = get_pan_url(PMP_url, "dilaq.", pmp_url_create("dilaq.")[str_detect(pmp_url_create("dilaq."), "7130$")]),
         PAN_url = get_url(PAN_url, "(?<=\\*)(dilaq)\\b", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*dilaq.$"))) |> 
  mutate(PMP_url = get_url(PMP_url, "(?<=^\\*)(\\(i\\-\\)sai)\\b", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*sai$"))) |> 
  mutate(PMP_url = get_url(PMP_url, "(?<=^\\*)(pia\\(n\\))", cognateset_url_base, get_id_cognateset(CognateTable, "^\\*pian$"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "Suaji", pan_url_create("Suaji")),
         PMP_url = str_replace_all(PMP_url, "(?<=\\*)(huaji)\\(\\-n\\/(ŋ)\\)", "\\1 / \\1-\\2"),
         PMP_url = get_pan_url(PMP_url, "huaji", pmp_url_create("huaji")),
         PMP_url = get_pan_url(PMP_url, "huaji\\-ŋ", pmp_url_create("huaji\\-ŋ"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "aNak", pan_url_create("aNak")),
         PMP_url = get_pan_url(PMP_url, "anak", pmp_url_create("anak")),
         PMP_url = get_pan_url(PMP_url, "qalejaw", pan_url_create("qalejaw"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "tiup", pan_url_create("tiup")),
         PAN_url = get_pan_url(PAN_url, "Caliŋa", pan_url_create("Caliŋa")),
         PMP_url = get_pan_url(PMP_url, "taliŋa", pmp_url_create("taliŋa")[1]),
         PMP_url = get_pan_url(PMP_url, "wahiR", pan_url_create("wahiR")),
         PAN_url = get_pan_url(PAN_url, "Sapuy", pan_url_create("Sapuy")),
         PMP_url = get_pan_url(PMP_url, "hapuy", pmp_url_create("hapuy"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "aku", pan_url_create("aku"))),
         PAN_url = get_pan_url(PAN_url, "quluh", pan_url_create("quluh")),
         PMP_url = get_pan_url(PMP_url, "qulu", pmp_url_create("qulu")[str_detect(pmp_url_create("qulu"), "4674")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "bəli", pan_url_create("beli")[1]),
         PMP_url = get_pan_url(PMP_url, "bəli", pmp_url_create("beli")[str_detect(pmp_url_create("beli"), "910")])) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "bulu", pan_url_create("bulu.")[1])) |> 
  mutate(across(matches("url"), ~get_pan_url(., "takut", pan_url_create("takut")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "sa-puluq", pmp_url_create("sa-puluq")),
         PMP_url = get_pan_url(PMP_url, "sa-ŋa-puluq", pmp_url_create("sa-ŋa-puluq"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "Rabiqi", pan_url_create("Rabiqi")[1]),
         PMP_url = get_pan_url(PMP_url, "Rabiqi", pmp_url_create("Rabiqi")[str_detect(pmp_url_create("Rabiqi"), "9029")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "sijəm", pan_url_create("sijem")),
         PMP_url = get_pan_url(PMP_url, "sejem", pmp_url_create("sejem")),
         PMP_url = get_pan_url(PMP_url, "sijəm", pmp_url_create("sijem"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "batux", pan_url_create("batux")),
         PMP_url = get_pan_url(PMP_url, "batu", pmp_url_create("batu"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "sauq", pan_url_create("sauq₂"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "ian", pan_url_create("ian₂")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "Cəbiq", pan_url_create("Cebiq")),
         PMP_url = get_pan_url(PMP_url, "təbiq", pmp_url_create("tebiq"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "susu", pan_url_create("susu₁")))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "təka", pan_url_create("teka₁")),
         PMP_url = get_pan_url(PMP_url, "qituŋ", pan_url_create("qituŋ")),
         PMP_url = get_pan_url(PMP_url, "kapak", pan_url_create("kapak")),
         PMP_url = get_pan_url(PMP_url, "qitəm", pan_url_create("qitem")),
         PMP_url = get_pan_url(PMP_url, "səjəb", pan_url_create("zeket")),
         PMP_url = get_pan_url(PMP_url, "dalij", pan_url_create("dalij")),
         PMP_url = get_pan_url(PMP_url, "daliŋ", pan_url_create("daliŋ"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "dakəp", pan_url_create("dakep")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "ma\\-aCay", pmp_url_create("ma\\-aCay")),
         PMP_url = get_pan_url(PMP_url, "m\\-atay", pmp_url_create("m\\-atay"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "libuR", pan_url_create("libuR")),
         PMP_url = get_pan_url(PMP_url, "puqun", pan_url_create("puqun")),
         PMP_url = get_pan_url(PMP_url, "sisir", pan_url_create("sisir")),
         PMP_url = get_pan_url(PMP_url, "timbaŋ", pan_url_create("timbaŋ")),
         PMP_url = get_pan_url(PMP_url, "iluR", pan_url_create("iluR₁"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "sələm", pan_url_create("selem₁"))),
         across(matches("url"), ~get_pan_url(., "bituqən", pan_url_create("bituqen")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "qətut", pan_url_create("qetut")[1]),
         PMP_url = get_pan_url(PMP_url, "qətut", pmp_url_create("qetut")[str_detect(pmp_url_create("qetut"), "4498$")]),
         PMP_url = get_pan_url(PMP_url, "qutut", pan_url_create("qutut"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "baqbaq", pan_url_create("baqbaq")),
         PMP_url = get_pan_url(PMP_url, "beqbeq", pan_url_create("beqbeq")),
         PMP_url = get_pan_url(PMP_url, "pagər", pan_url_create("pager")),
         PMP_url = get_pan_url(PMP_url, "kima", pan_url_create("kima"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "tuktuk", pan_url_create("tuktuk₃"))),
         across(matches("url"), ~get_pan_url(., "aŋay", pan_url_create("aŋay")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "buaq", pan_url_create("buaq")[1]),
         PMP_url = get_pan_url(PMP_url, "buaq", pmp_url_create("buaq")[str_detect(pmp_url_create("buaq"), "1331$")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "biRaq", pan_url_create("biRaq₁")[1]),
         PMP_url = get_pan_url(PMP_url, "biRaq", pmp_url_create("biRaq₁")[str_detect(pmp_url_create("biRaq₁"), "1219$")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "bəRay", pan_url_create("beRay")[1]),
         PMP_url = get_pan_url(PMP_url, "bəRay", pmp_url_create("beRay")[str_detect(pmp_url_create("beRay"), "\\-958$")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "Rusuk", pan_url_create("Rusuk")),
         PMP_url = get_pan_url(PMP_url, "Rusuk", pmp_url_create("Rusuk"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "si ia", pmp_url_create("si ia₁")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "dəŋəR", pan_url_create("deŋeR")[1]),
         PMP_url = get_pan_url(PMP_url, "dəŋəR", pmp_url_create("deŋeR")[2])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "kaka", pan_url_create("kaka₂")[1]),
         PMP_url = get_pan_url(PMP_url, "kaka", pmp_url_create("kaka₂")[2])) |> 
  mutate(across(matches("url"), ~get_pan_url(., "bayu", pan_url_create("bayu")))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "ma\\-qətaq", pmp_url_create("ma\\-qetaq")[str_detect(pmp_url_create("ma\\-qetaq"), "4488$")])),
         PAN_url = get_pan_url(PAN_url, "k\\-ami", pmp_url_create("k\\-ami")),
         PMP_url = get_pan_url(PMP_url, "k\\-ami", pmp_url_create("kam\\-ami"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "ikuR", pan_url_create("ikuR")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "timuR", pan_url_create("timuR")[1]),
         PMP_url = get_pan_url(PMP_url, "timuR", pmp_url_create("timuR")[str_detect(pmp_url_create("timuR"), "7785$")])) |> 
  mutate(across(matches("url"), ~get_pan_url(., "qəbəl", pan_url_create("qebel")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "duRi", pan_url_create("duRi")[1]),
         PMP_url = get_pan_url(PMP_url, "duRi", pmp_url_create("duRi")[str_detect(pmp_url_create("duRi"), "6826$")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "peñu", pan_url_create(stri_trans_nfc("peñu"))[1]),
         PMP_url = get_pan_url(PMP_url, "peñu", pmp_url_create(stri_trans_nfc("peñu"))[2])) |> 
  mutate(across(matches("url"), ~get_pan_url(., "baRəq", pan_url_create("baReq")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "buRuk", pan_url_create("buRuk")),
         PMP_url = get_pan_url(PMP_url, "buRuk", pmp_url_create("ma\\-buRuk"))) |> 
  mutate(across(matches("url"), ~get_pan_url(., "i", pan_url_create("i₂")))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "taqi", pmp_url_create("taqi")),
         PAN_url = get_pan_url(PAN_url, "Caqi", pan_url_create("Caqi"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "pijax", pan_url_create("pijax")),
         PMP_url = get_pan_url(PMP_url, "pija", pan_url_create("pijax"))) |> # PMP 'pija' is not available in ACD thus linked to the overall PAN *pijax
  mutate(PAN_url = get_pan_url(PAN_url, "buNuq", pan_url_create("buNuq₂")),
         PMP_url = get_pan_url(PMP_url, "bunuq", pmp_url_create("bunuq₂"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "kaSu", pan_url_create("kaSu")),
         PMP_url = get_pan_url(PMP_url, "kahu", pmp_url_create("kahu"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "huRaC", pan_url_create("huRaC")),
         PMP_url = get_pan_url(PMP_url, "uRat", pmp_url_create("uRat")[1])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "kuCux", pan_url_create("kuCux")),
         PMP_url = get_pan_url(PMP_url, "kutu", pmp_url_create("kutu\U2081"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "qaCi", pan_url_create("qaCi")),
         PMP_url = get_pan_url(PMP_url, "qəti", pan_url_create("qeti[\U2081]")),
         PMP_url = get_pan_url(PMP_url, "kəti", pan_url_create("keti"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "wanaN", pan_url_create("wanaN")),
         PMP_url = get_pan_url(PMP_url, "wanan", pmp_url_create("wanan")[str_detect(pmp_url_create("wanan"), "9474$")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "naNaq", pan_url_create("naNaq")),
         PMP_url = get_pan_url(PMP_url, "nanaq", pmp_url_create("nanaq"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "ujuŋ", pan_url_create("ujuŋ")),
         PMP_url = get_pan_url(PMP_url, "ijuŋ", pan_url_create("ijuŋ"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "abut", pan_url_create("abut[\U2080-\U2089]")[str_detect(pan_url_create("abut[\U2080-\U2089]"), "24769$")]),
         PMP_url = get_pan_url(PMP_url, "Ramut", pan_url_create("Ramut[\U2080-\U2089]")[str_detect(pan_url_create("Ramut[\U2080-\U2089]"), "30879$")])) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "CaliS", pan_url_create("CaliS")),
         PMP_url = get_pan_url(PMP_url, "talih", pmp_url_create("talih"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "putun", pan_url_create("putun[\U2082]")),
         PMP_url = get_pan_url(PMP_url, "putul", pmp_url_create("putul"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "CəbuS", pan_url_create("CebuS")),
         PAN_url = get_pan_url(PAN_url, "təbuS", pan_url_create("tebuS")),
         PMP_url = get_pan_url(PMP_url, "təbuh", pmp_url_create("tebuh[\U2081]"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "quSeNap", pan_url_create("quSeNap")),
         PMP_url = get_pan_url(PMP_url, "ənap", pan_url_create("enap"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "haRezan", pan_url_create("haRezan"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "-nu", pan_url_create("-nu[\U2081]")),
         PMP_url = get_pan_url(PMP_url, "a\\-nu", pmp_url_create("a\\-nu"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "RabuC", pan_url_create("RabuC")),
         PMP_url = get_pan_url(PMP_url, "Rabut", pmp_url_create("Rabut"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "bubuŋ-an", pmp_url_create("bubuŋ-an")),
         PMP_url = get_pan_url(PMP_url, "hasaŋ", pan_url_create("hasaŋ")),
         PMP_url = get_pan_url(PMP_url, "pagəl", pan_url_create("pagel")),
         PMP_url = get_pan_url(PMP_url, "umpu", pan_url_create("umpu")),
         PMP_url = get_pan_url(PMP_url, "bətəŋ", pan_url_create("beteŋ\U2082")),
         PMP_url = get_pan_url(PMP_url, "timbul", pan_url_create("timbul")),
         PMP_url = get_pan_url(PMP_url, "dutdut", pan_url_create("dutdut")),
         PMP_url = get_pan_url(PMP_url, "zutzut", pan_url_create("zutzut")),
         PMP_url = get_pan_url(PMP_url, "duniq", pan_url_create("duniq")),
         PMP_url = get_pan_url(PMP_url, "suluq", pan_url_create("suluq")),
         PMP_url = get_pan_url(PMP_url, "kua", pan_url_create("kua\U2082")),
         PMP_url = get_pan_url(PMP_url, "rəbuŋ", pan_url_create("rebuŋ")),
         PMP_url = get_pan_url(PMP_url, "butaq", pan_url_create("butaq\U2082")),
         PMP_url = get_pan_url(PMP_url, "huab", pan_url_create("huab\U2081")),
         PMP_url = get_pan_url(PMP_url, "buluŋ", pan_url_create("buluŋ\U2082")),
         PMP_url = get_pan_url(PMP_url, "baRəqaŋ", pan_url_create("baReqaŋ"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "bəRŋi", pan_url_create("beRŋi")),
         PMP_url = get_pan_url(PMP_url, "quzuŋ", pan_url_create("quzuŋ")),
         PMP_url = get_pan_url(PMP_url, "paqit", pmp_url_create("paqit\U2082")),
         PMP_url = get_pan_url(PMP_url, "si ida", pmp_url_create("si ida")),
         PMP_url = get_pan_url(PMP_url, "ma\\-kapal", pmp_url_create("ma\\-kapal")[str_detect(pmp_url_create("ma\\-kapal"), "26671")])) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "busbus", pan_url_create("busbus"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "taŋan", pan_url_create("taŋan")[1]),
         PMP_url = get_pan_url(PMP_url, "tələn", pan_url_create("telen")),
         PMP_url = get_pan_url(PMP_url, "suja", pan_url_create("suja")),
         PMP_url = get_pan_url(PMP_url, "tirtir", pan_url_create("tirtir\U2081"))) |> 
  mutate(PMP_url = get_pan_url(PMP_url, "banua", pan_url_create("banua")),
         PMP_url = get_pan_url(PMP_url, "gəlaŋ", pan_url_create("gelaŋ")),
         PMP_url = get_pan_url(PMP_url, "qabatiR", pan_url_create("qabatiR")),
         PMP_url = get_pan_url(PMP_url, "siuk", pan_url_create("siuk")),
         PMP_url = get_pan_url(PMP_url, "sihul", pan_url_create("sihul")),
         PMP_url = get_pan_url(PMP_url, "nabuq", pan_url_create("nabuq")),
         PMP_url = get_pan_url(PMP_url, "dabuq", pan_url_create("dabuq")),
         PMP_url = get_pan_url(PMP_url, "labuq\U2081", pan_url_create("labuq\U2081"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "nia", pmp_url_create("ni\\-ia")),
         PMP_url = get_pan_url(PMP_url, "n\\-ia", pmp_url_create("ni\\-ia")),
         PAN_url = str_replace_all(PAN_url, "(?<=^\\*)(\\-nu)$", str_c('<a href="', pan_url_create("\\-nu\U2081"), '" target="_blank">\\1</a>', sep = "")),
         PMP_url = str_replace_all(PMP_url, "(?<=^\\*)(\\=mu)$", str_c('<a href="', pan_url_create("\\-mu"), '" target="_blank">\\1</a>', sep = "")),
         PMP_url = str_replace_all(PMP_url, "(?<=^\\*)(\\=ku)$", str_c('<a href="', pan_url_create("\\-ku"), '" target="_blank">\\1</a>', sep = "")))

### note for myself
### if Daniel provided a PAN reconstruction form and this form is not available in the main/super proto-form table (CognateTable),
### then it is likely that it appears as PAN sub-proto-form (therefore, use the pmp_url_create() rather than pan_url_create()).
### the example is reconstruction for 'we' (PAN k-ita that appears not in the main/super proto-form page but as sub-proto-form [https://acd.clld.org/cognatesets/26557#s-11768])


## Checking (run all of these)
proto_distinct1 |> filter(if_any(matches("url"), ~str_detect(., 'sets\\/"')))
proto_distinct1 |> filter(if_any(matches("url"), ~str_detect(., 'sets\\/\\#s\\-"')))
proto_distinct1 |> 
  select(id, matches("url|gloss|PAN_source|PMP_source")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE)))
proto_distinct1 |> 
  select(id, matches("url|gloss|PAN_source|PMP_source")) |> 
  filter(if_all(matches("url"), ~str_detect(., "https", negate = TRUE)))
proto_distinct1 |> 
  select(id, matches("url|gloss")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  filter(if_any(matches("url"), ~str_detect(., "\\+")))
proto_distinct1 |> 
  select(id, matches("etymon|url|gloss")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  filter(if_any(matches("etymon"), ~str_detect(., "\\+"))) |> 
  select(matches("url"), matches("gloss"))
proto_distinct1 |> 
  select(id, matches("etymon|url|gloss")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |>
  filter(if_any(matches("url"), ~str_detect(., "\\<")))
proto_distinct1 |> 
  select(id, matches("PAN_source|PMP_source|url|gloss")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  filter(if_any(matches("url"), ~str_detect(., "\\(")))
proto_distinct1 |> 
  select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "WMP|POC", negate = FALSE)))
proto_distinct1 |> 
  select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  filter(if_any(matches("url"), ~str_detect(., "[^\\<\\+\\>]+")))
proto_distinct1 |> 
  select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = FALSE)))
### PMP
proto_distinct1 |> 
  select(id, matches("etymon|PMP_url|PMP_gloss|PMP_source")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  select(id, matches("PMP_(url|etymon|gloss|source)|PAN_etymon")) |> 
  filter(PMP_etymon != "?" & PMP_etymon != "" & PAN_etymon %in% c("?", "") & str_detect(PMP_source, "ACD"))

proto_distinct1 |> 
  select(id, matches("etymon|PMP_url|PMP_gloss|PMP_source")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  select(id, matches("PMP_(url|etymon|gloss|source)|PAN_etymon")) |> 
  filter(PMP_etymon != "?", PMP_etymon != "", PAN_etymon != "?", PAN_etymon != "")









proto_distinct1 |> 
  select(id, matches("etymon|PMP_url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = FALSE))) |> 
  select(matches("PMP_(url|etymon)"))
proto_distinct1 |> 
  select(id, matches("etymon|PMP_url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  select(matches("PMP_(url|etymon)")) |> 
  filter(PMP_etymon != "?" & PMP_etymon != "")
### PAN
proto_distinct1 |> 
  select(id, matches("etymon|PAN_url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = FALSE))) |> 
  select(matches("PAN_(url|etymon)"))
proto_distinct1 |> 
  select(id, matches("etymon|PAN_url|gloss|PAN_source")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = TRUE))) |> 
  select(id, matches("PAN_(url|etymon|gloss|source)")) |> 
  filter(PAN_etymon != "?" & PAN_etymon != "" & str_detect(PAN_etymon, "\\?", TRUE))


#### testing with reactable
proto_distinct1 |> 
  # select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", negate = FALSE))) -> x
library(reactable)
eno_etym_long_mini8 |> 
  filter(id  %in% x$id) |> left_join(x) |> 
  mutate(across(matches("url"), ~str_replace_all(., "\\<in\\>", "&lt;in&gt;"))) |>
  reactable(filterable = TRUE,
            columns = list(PMP_url = colDef(html = TRUE),
                           PAN_url = colDef(html = TRUE)))
