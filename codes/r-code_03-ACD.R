# remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE)
library(rcldf)
library(tidyverse)
library(common)

acd <- cldf("C:/Users/GRajeg/OneDrive - Nexus365/Documents/cldf/acd/cldf/cldf-metadata.json")
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

# Keys between form_tb and cognate_tb is ID (form_tb) and Form_ID (cognate_tb)
# Keys between cogset_tb and cognate_tb is ID (cogset_tb) and Cognateset_ID (cognate_tb)
# Keys between language_tb and form_tb is ID (language_tb) and Language_ID (form_tb)
# Reconstruction_ID in the cognate_tb table matches with the ID in the protoform_tb
# Cognateset_ID in the cognate_tb table matches with the Cognateset_ID in the protoform_tb
# The cogset_tb provides the protoform of the words
## For instance, in the EnoLEX spreadsheet, to search for the PMP etymon "*abut / *Ramut" in the ACD, these etymos need to be searched in the Form column of the cogset_tb
## cogset_tb |> filter(str_detect(Form, "^\\*abut"))
## Then, pull the relevant ID in the cogset_tb to be combined with the ACD clld version URL

cognateset_url_base <- "https://acd.clld.org/cognatesets/"
paste0(cognateset_url_base, "25576")
paste0(cognateset_url_base, "29851")
