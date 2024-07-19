library(tidyverse)

x1 <- ReadBib("sources.bib")
cites <- vector(mode = "character", length = length(x1))
for (i in 1:length(x1)) {
  
  cites[i] <- str_replace(str_replace_all(str_replace_all(format(x1[[i]]), "\\\n", " "), "\\}", ""), "^\\[1\\]\\s", "")
  
}

bib2df::bib2df("sources.bib") |> 
  mutate(across(where(is.list), ~map(., paste, collapse = " ; "))) |> 
  mutate(across(where(is.list), ~map(., str_replace_all, "[{}]", ""))) |> 
  unnest_longer(col = c(AUTHOR, EDITOR)) |> 
  mutate(across(matches("TITLE"), ~str_replace_all(., "[{}]", ""))) |> 
  mutate(URL = if_else(is.na(URL) & !is.na(DOI),
                       str_c("https://doi.org/", DOI, sep = ""),
                       URL)) |> 
  mutate(CITATION = cites) |> 
  write_tsv("sources.tsv")

bib2df::bib2df("sources.bib") |> 
  mutate(across(where(is.list), ~map(., paste, collapse = " ; "))) |> 
  mutate(across(where(is.list), ~map(., str_replace_all, "[{}]", ""))) |> 
  unnest_longer(col = c(AUTHOR, EDITOR)) |> 
  mutate(across(matches("TITLE"), ~str_replace_all(., "[{}]", ""))) |> 
  mutate(URL = if_else(is.na(URL) & !is.na(DOI),
                       str_c("https://doi.org/", DOI, sep = ""),
                       URL)) |> 
  mutate(CITATION = cites) |> 
  write_rds("sources.rds")
