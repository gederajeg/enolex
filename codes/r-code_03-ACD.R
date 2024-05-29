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
  distinct()

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

# get the distinct values of the etymology table
proto_distinct <- eno_etym_long_proto_df |> 
  filter(if_any(matches("_etymon"), ~!is.na(.))) |> 
  select(id, matches("^(PAN|PMP|Etymological)")) |> 
  distinct() |> 
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
  str_replace_all(cols, rgx, str_c("[\\1](", url_base, url_part, ")", sep = ""))
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

get_pan_url <- function(cols, enolex_form, pan_url = NULL) {
  
  rgx_to_replace <- paste0("(\\b\\\\*?", enolex_form, "\\b)")
  rgx_replacement <- paste0("[\\1](", pan_url, ")")
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
  
}

proto_distinct1 <- proto_distinct |> 
  mutate(PAN_url = get_url(PAN_url, 
                           "(\\b\\\\*ma\\-RuqaNay\\b)", 
                           cognateset_url_base, 
                           get_id_cognateset(CognateTable, '^\\*RuqaNay$')),
         PMP_url = str_replace_all(PMP_url, "(\\b\\\\*maRuqanay\\b)", "[\\1](https://acd.clld.org/cognatesets/28052#s-4939)"),
         PMP_url = str_replace_all(PMP_url, "(\\b\\\\*baRani\\b)", "[\\1](https://acd.clld.org/cognatesets/25144#s-657)")) |> 
  mutate(PAN_url = get_url(PAN_url, 
                           "(\\b\\\\*Sika\\b)", 
                           cognateset_url_base, 
                           get_id_cognateset(CognateTable, '^\\*Sika-$')),
         PMP_url = str_replace_all(PMP_url, "(\\\\*?\\(i\\)ka\\b)", "[\\1](https://acd.clld.org/cognatesets/26451#s-11621)")) |> 
  mutate(across(matches("url"), ~get_url(., 
                                         "(\\b\\\\*?lima\\b)", 
                                         cognateset_url_base, 
                                         get_id_cognateset(CognateTable, '^\\*lima$')))) |> 
  mutate(PAN_url = get_url(PAN_url, 
                           "(\\b\\\\*Səpat\\b)", 
                           cognateset_url_base, 
                           get_id_cognateset(CognateTable, '^\\*Sepat$'))) |> 
  mutate(PMP_url = replace(PMP_url, PMP_url == "*kapət", "*[kapət](https://acd.clld.org/cognatesets/30436)")) |> 
  mutate(across(matches("url"), ~get_url(.,
                                         "(\\b\\\\*qaqay\\b)",
                                         cognateset_url_base,
                                         get_id_cognateset(CognateTable, "^\\*qaqay$")))) |> 
  mutate(across(matches("url"), ~get_url(.,
                                         "(\\bwaqay\\b)",
                                         cognateset_url_base,
                                         get_id_cognateset(CognateTable, "^\\*waqay$")))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "maCa", pan_url_create("maCa")),
         PMP_url = get_pan_url(PMP_url, "mata", pmp_url_create("mata"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "duSa", pan_url_create("duSa")),
         PMP_url = get_pan_url(PMP_url, "duha", pmp_url_create("duha"))) |> 
  mutate(PAN_url = get_pan_url(PAN_url, "amax", pan_url_create("amax")),
         PMP_url = get_pan_url(PMP_url, stri_trans_nfc("ama\\s\\/\\s\\*ama\\-ŋ"), pmp_url_create(stri_trans_nfc("amá\\-ŋ"))))


## Checking
proto_distinct1 |> 
  select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", TRUE)))
### PMP
proto_distinct1 |> 
  select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", FALSE))) |> 
  select(matches("PMP_url"))
### PAN
proto_distinct1 |> 
  select(id, matches("etymon|url")) |> 
  filter(if_any(matches("url"), ~str_detect(., "https", FALSE))) |> 
  select(matches("PAN_url"))
