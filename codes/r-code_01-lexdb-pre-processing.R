library(tidyverse)

# get the URL for the Google sheet and load the data (periodically run codes in line 4, 7-11, and 14 when there is update from the master Google Spreadsheet)
# source("codes/r-code_00-lexdb-source-rawfile.R")

# save into .txt file to track using git
# eno_etym0 <- eno_etym
# colnames(eno_etym0) <- str_replace_all(colnames(eno_etym0), "\\\n", " ") |>
#   str_replace_all("\\s\\(", "__") |>
#   str_replace_all("\\)$", "")
# write_tsv(x = eno_etym0, file = "../enolex-raw-to-track/enolex-db-orig.tsv")

# save into .rds file
# write_rds(x = eno_etym, file = "data/eno_etym.rds")

# load the data
eno_etym <- read_rds("data/eno_etym.rds")

eno_etym1 <- eno_etym |> 
  
  ## exclude the contemporary column for now
  select(-`Contemporary Enggano root`) |> 
  
  rename(`Enggano Elicitation (Aron 2019)` = `Pak Aron Elicitation 2019`,
         semantic_field = `Semantic field`,
         indonesian_gloss = `Indonesian gloss`,
         english_gloss = `English gloss`)

## remove the Concepticon (and other irrelevant) columns first
eno_etym2 <- eno_etym1 |> 
  select(-Concepticon_ID, 
         -Concepticon_URL, 
         -`Number of Attestations`, 
         -`Enggano root (Kähler 1987)`)

## fix the column names
colnames(eno_etym2) <- str_replace_all(colnames(eno_etym2), "\\\n", " ") |> 
  str_replace_all("\\s\\(", "__") |> 
  str_replace_all("\\)$", "")

# hidden_rows <- c(2, 3, 8, 12, 15, 17:19, 24:25, 30:31, 34:35, 37, 39, 41, 43, 46:47, 49:50, 52:54, 56:59, 61:63, 65:66, 69, 71, 73, 76:77, 79:81, 83:84, 86, 88:91, 93, 96:98, 102, 104, 106:107)-1
# 
# eno_etym <- eno_etym[-hidden_rows, ]

# Add cognate ID
eno_etym2$id <- 1:nrow(eno_etym2)

# eno_etym_long <- eno_etym |> 
#   pivot_longer(cols = contains("Enggano"),
#                names_to = "EngganoSourceOriginal",
#                values_to = "words") |>  
#   separate(EngganoSourceOriginal, 
#            into = c("EngganoLanguage", "EngganoSource"), 
#            sep = "__", 
#            remove = FALSE)
# 
# eno_etym_long1 <- eno_etym_long |>
#   filter(!is.na(words)) |> 
#   select(-EngganoSourceOriginal) |> 
#   mutate(words = str_replace_all(words, "(\\n|, )", " ; "),
#          id = row_number(words),
#          year = if_else(str_detect(EngganoLanguage, "1895"), 
#                         "1895",
#                         ""),
#          year = if_else(str_detect(EngganoSource, "<1855"),
#                         "<1855",
#                         year),
#          year = if_else(year == "", 
#                         str_extract(EngganoSource, "[0-9]{4}$"), 
#                         year),
#          year = if_else(str_detect(EngganoSource, " ms\\."),
#                         "ms.",
#                         year),
#          year = factor(year, levels = c("1854", "<1855", "1855", "1864", "1870", "1879", "1887", "1891", "1894", "1895", "1916", "1982", "ms.", "1987", "2011", "2019", "2023"))) |>
#   select(year, words, indonesian_gloss, english_gloss, semantic_field, EngganoLanguage, EngganoSource, Remarks, id, everything()) |> 
#   arrange(indonesian_gloss, english_gloss, year)
# 
# eno_etym_long_mini <- eno_etym_long1 |> 
#   select(-matches("^P[AM]"), -`Etymological source`, -`Remark on etymology`)
# 
# eno_etym_long_proto_df <- eno_etym_long1 |> 
#   select(id, words, matches("^P[AM]"), `Etymological source`, `Remark on etymology`)
# 
# remarks_split <- eno_etym_long_mini |> 
#   mutate(Remarks = str_replace_all(Remarks, "\\, (?=note[0-9])", "_")) |>
#   mutate(Remarks = str_replace_all(Remarks, "\\, meai(?=ng[0-9])", "_meani")) |> 
#   mutate(Remarks2 = str_split(Remarks, "_")) |> 
#   unnest_longer(Remarks2, values_to = "Remarks2") |> 
#   select(-Remarks) |> 
#   distinct()

# Editing the content/typo of the remarks
eno_etym3 <- eno_etym2 |> 
  mutate(Remarks = if_else(str_detect(Remarks, "'brother'_unmarried_cf"), str_replace(Remarks, "('brother')_(unmarried)(_cf)", "\\1_note: \\2\\3"), Remarks)) |> 
  mutate(Remarks = str_replace_all(Remarks, "', meaning1979", "_meaning1979")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\, (?=note[0-9])", "_")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\n(?=note)", "")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\\\(?=note)", "")) |> 
  mutate(Remarks = str_replace_all(Remarks, "note\\s1887", "note1887")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\bnote\\s+sure\\b", "not sure")) |> 
  mutate(Remarks = str_replace_all(Remarks, "meaing1894", "meaning1894")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\, meai(?=ng[0-9])", "_meani")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\s+(1894)(meaning)", "\\2\\1")) |> 
  mutate(Remarks = str_replace_all(Remarks, "^(meaing|meaing|maning)(?=[0-9])", "meaning")) |> 
  mutate(Remarks = str_replace_all(Remarks, "\\,\\s+2011\\&2019meaning", "_meaning2011&2019")) |> 
  mutate(Remarks = str_replace_all(Remarks, 
                                   "meaning1855vdS 'outside' meaning1870 'outside, low'", 
                                   "meaning1855vdS 'outside'_meaning1870 'outside, low'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Modigliani BI ayo?", "note1894: Bahasa Indonesia \"ayo\"?"),
         Remarks = replace(Remarks, Remarks == "Mod. 'mengiris kayu'", "note1894: 'mengiris kayu'"),
         Remarks = str_replace_all(Remarks, "(note)\\&(?=1987)", "\\1")) |> 
  mutate(Remarks = str_replace_all(Remarks, "(?<=^note2011)\\ː", ":")) |> 
  mutate(Remarks = str_replace_all(Remarks, "Kähler and 2023 'baby'", "meaning1987K&2023 'baby'")) |> 
  mutate(Remarks = replace(Remarks, str_detect(Remarks, "Kasim loanword\nKähler, Yoder lit. 'frame of thunder'"), "note1987: loanword_note1987K&2011: lit 'frame of thunder'")) |>
  mutate(Remarks = replace(Remarks, str_detect(Remarks, "Kähler lit. 'hair of house'"), "note1987K: lit. 'hair of house'")) |>
  mutate(Remarks = replace(Remarks, str_detect(Remarks,"^Kähler archaic$"), "note1987K: archaic")) |>
  mutate(Remarks = replace(Remarks, str_detect(Remarks, "^Kähler lit. 'for piercing'$"), "note1987K: lit 'for piercing'")) |>
  mutate(Remarks = replace(Remarks, str_detect(Remarks, "^lit. 'killer', Kähler archaic$"), "note: lit. 'killer'_note1987K: archaic")) |> 
  mutate(Remarks = replace(Remarks, str_detect(Remarks, "^note1864: loanword Buginese, Kähler archaic$"), "note1864: loanword Buginese_note1987K: archaic")) |> 
  mutate(Remarks = replace(Remarks, str_detect(Remarks, "^Kähler < e-hoo u-uba ‘inside house'$"), "note1987K: from e-hoo u-uba 'inside house'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Kähler cf. water", "meaning1987K 'water'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Kähler loanword", "note1987K: loanword")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Kähler lit. 'for cutting'", "note1987K: lit. 'for cutting'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Kähler lit. 'for pounding'", "note1987K: lit. 'for pounding'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "lit. \"which is to be burned\" (Kähler 1975:VI)", "note1987K: lit. 'which is to be burned' (Kähler 1975:VI)")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "lit. 'that which is to be fished' (Kähler 1975:VI, Kähler 1987:10)", "note1987K: lit. 'that which is to be fished' (Kähler 1975:VI, Kähler 1987:10)")) |> 
  mutate(Remarks = str_replace(Remarks, "Kähler lit\\. 'for sharpening", "note1987K: lit. 'for sharpening'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. lit. 'not like'", "note1894: lit. 'not like'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. 'sentire/merasa sakit'", "note1894: 'sentire/merasa sakit'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. 'rancore/hati kecil'", "note1894: 'rancore/hati kecil'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. 'speranza/harap'", "note1894: 'speranza/harap'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. also 'mercy'", "note1894: also 'mercy'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. 'basta!'", "note1894: 'basta!'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. 'pacco di tabacco/lempeng'", "note1894: 'pacco di tabacco/lempeng'")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Mod. unclear (he has appassito/burus)", "note1894: unclear (he has 'appassito/burus')")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "nasi, Kähler loanword", "note1987K: loanword")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "Kasim, Yoder loanword", "note1987&2011: loanword")) |> 
  mutate(Remarks = replace(Remarks, Remarks == "lit. 4+4, Yoder 'lay + ?'", "note: lit. 4+4_note2011: 'lay + ?'"))

eno_etym4 <- eno_etym3 |> 
  
  ## split the Remarks column by "_" separator
  mutate(Remarks2 = str_split(Remarks, "_")) |> 
  
  ## unnest into long table
  unnest_longer(Remarks2, values_to = "Remarks2") |> 
  select(-Remarks) |> 
  distinct()

# Turn into long, vertical table
eno_etym_long <- eno_etym4 |>
  pivot_longer(cols = contains("Enggano"),
               names_to = "EngganoSourceOriginal",
               values_to = "words")
eno_etym_long <- eno_etym_long |> 
  # separate(EngganoSourceOriginal,
  #          into = c("EngganoLanguage", "EngganoSource"),
  #          sep = "__",
  #          remove = FALSE)
  separate_wider_delim(cols = EngganoSourceOriginal,
                       delim = "__",
                       names = c("EngganoLanguage", "EngganoSource"),
                       cols_remove = FALSE)

eno_etym_long1 <- eno_etym_long |>
  filter(!is.na(words)) |>
  select(-EngganoSourceOriginal) |>
  
  # to handle Brouwer early 1850s where Daniel changed the column name
  mutate(EngganoSource = str_replace(EngganoSource, "early 1850s$", "<1855")) |> 
  
  mutate(words = str_replace_all(words, "(\\n|, )", " ; "),
         year = if_else(str_detect(EngganoLanguage, "1895"),
                        "1895",
                        ""),
         year = if_else(str_detect(EngganoSource, "(<1855|\\bearly\\s1850s\\b)"),
                        "<1855",
                        year),
         year = if_else(year == "",
                        str_extract(EngganoSource, "[0-9]{4}$"),
                        year),
         year = if_else(str_detect(EngganoSource, " ms\\."),
                        "ms.",
                        year),
         year = factor(year, levels = c("<1855", "1854", "1855", "1864", "1870", "1878", 
                                        "1879", "1888", "1891", "1894", "1895", 
                                        "1916", "1979", "1982", "ms.", "1987", "2011", 
                                        "2019", "2023"))) |>
  select(year, words, indonesian_gloss, english_gloss, semantic_field, 
         EngganoLanguage, EngganoSource, Remarks2, id, everything()) |>
  arrange(id, indonesian_gloss, english_gloss, year)

year_source_df <- tribble(~EngganoSource, ~year_id, ~year,
                          "Kähler 1987", "1987K", "1987",
                          "Yoder 2011", "2011", "2011",
                          "Oudemans 1879", "1879", "1879",
                          "Helfrich & Pieters 1891", "1891", "1891",
                          "Modigliani 1894", "1894", "1894",
                          "Stockhof 1987", "1895", "1895",
                          "Aron 2019", "2019", "2019",
                          "Helfrich 1888", "1888", "1888",
                          "Walland 1864", "1864", "1864",
                          "Amran et al. 1979", "1979", "1979",
                          "Capell 1982", "1982", "1982",
                          "Kasim et al. 1987", "1987", "1987",
                          "Nothofer 1986 ms.", "ms.", "ms.",
                          "Zakaria et al. 2023", "2023", "2023",
                          "v. Rosenberg 1855", "1855vR", "1855",
                          "v. Rosenberg 1878", "1878", "1878",
                          "Brouwer <1855", "<1855", "<1855",
                          "vd Straten & S. 1855", "1855vdS", "1855",
                          "Francis 1870", "1870", "1870",
                          "Helfrich 1916", "1916", "1916",
                          "Boewang 1854", "1854", "1854") |> 
  mutate(year = factor(year, 
                       levels = c("<1855", "1854", "1855", "1864", "1870", "1878",
                                  "1879", "1888", "1891", "1894", "1895", 
                                  "1916", "1979", "1982", "ms.", "1987", "2011", 
                                  "2019", "2023"))) |> 
  arrange(year)

eno_etym_long_mini <- eno_etym_long1 |>
  select(-matches("^P[AM]"), -`Etymological source`, -`Remark on etymology`) |> 
  left_join(select(year_source_df, -year))

eno_etym_long_proto_df <- eno_etym_long1 |>
  select(id, year, words, indonesian_gloss, english_gloss, EngganoLanguage, 
         EngganoSource, 
         matches("^P[AM]"), `Etymological source`, `Remark on etymology`) |> 
  rename(PAN_etymon = `PAN etymon`,
         PAN_gloss = `PAN gloss`,
         PAN_source = `PAN source`,
         PMP_etymon = `PMP etymon`,
         PMP_gloss = `PMP gloss`,
         PMP_source = `PMP source`,
         Etymological_source = `Etymological source`,
         Remark = `Remark on etymology`)

remarks_split <- eno_etym_long_mini |> 
  select(Remarks2, id) |> 
  distinct() |> 
  mutate(Remarks3 = if_else(str_detect(Remarks2, "meaning[<0-9A-Za-z]+?\\&"),
                            str_replace_all(Remarks2, "\\&", "_meaning"),
                            Remarks2),
         Remarks3 = if_else(str_detect(Remarks2, "note[<0-9A-Za-z]+?\\&"),
                            str_replace_all(Remarks3, "\\&", "_note"),
                            Remarks3)) |> 
  filter(if_all(where(is.character), ~!is.na(.))) |> 
  mutate(rm4 = Remarks3)

remarks_split1 <- remarks_split |> 
  select(id, rm4) |> 
  mutate(rm4 = if_else(str_detect(rm4, "meaning[0-9_:<A-Za-z]+\\b"), 
                       str_replace_all(rm4, "(meaning[0-9_:<A-Za-z]+\\b)", "<m>\\1</m>"), 
                       rm4),
         rm4 = if_else(str_detect(rm4, "note[<_:ː0-9A-Za-z]+\\b"),
                       str_replace_all(rm4, "(note[<_:0-9A-Za-z]+\\b)", "<n>\\1</n>"),
                       rm4),
         rm4 = if_else(str_detect(rm4, "^(?<!\\>)note"),
                       str_replace_all(rm4, "^((note)(?=\\:)|(note)(?=\\ː))", "<n>\\1</n>"),
                       rm4),
         tagged = if_else(str_detect(rm4, "^\\<."), TRUE, FALSE))
rm_tagged <- remarks_split1 |> 
  filter(tagged) |> 
  separate(rm4, 
           into = c("marker", "content"), 
           sep = "(\\<\\/.\\>)", 
           remove = TRUE) |> 
  mutate(marker = str_replace(marker, "^\\<.\\>", "")) |> 
  mutate(marker = str_split(marker, "_")) |> 
  unnest_longer(marker) |> 
  mutate(types = if_else(str_detect(marker, "^note"), "note", NA),
         types = if_else(str_detect(marker, "^meaning"), "meaning", types)) |> 
  mutate(marker = str_replace(marker, "^(note|meaning)", "")) |> 
  rename(year = marker) |> 
  mutate(content = str_replace(content, "^\\:?\\s+", ""),
         content = if_else(types == "meaning",
                           str_replace_all(content, "(^'|'$)", ""),
                           content)) |> 
  mutate(year = replace(year, year %in% c("1895K", "1897K"), "1987K"),
         year = replace(year, year %in% c("1855"), "1855vR")) |> 
  rename(year_id = year)

rm_tagged
rm_untagged <- remarks_split1 |> 
  filter(!tagged)

# join the "meaning" content with non-empty year_id with the main database
rm_tagged_non_empty_year_meaning <- rm_tagged |> 
  filter(year_id != "", types == "meaning")
rm_tagged_non_empty_year_notes <- rm_tagged |> 
  filter(year_id != "", types == "note") |> 
  rename(notes = content) |> 
  select(-types, -tagged) |> 
  group_by(id, year_id) |> 
  mutate(isi = paste(notes, collapse = " ; ")) |> 
  select(-notes) |> 
  rename(notes = isi) |> 
  distinct()

eno_etym_long_mini1 <- eno_etym_long_mini |> 
  select(-Remarks2) |>
  distinct() |> # Remarks2 col. inflates the rows/creates duplicates, hence `distinct()`
  
  # join the `meaning` remark by cognate ID and year_ID
  left_join(rm_tagged_non_empty_year_meaning |> 
              select(-tagged, -types), 
            by = join_by(id, year_id)) |> 
  mutate(english_new = if_else(!is.na(content), 
                               content, 
                               english_gloss)) |> 
  select(-content) |> 
  
  # count the number of forms to ease the integration of notes that contain info on what each word form means
  mutate(n_word = str_count(words, "[^;,]+")) |> 
  
  left_join(rm_tagged_non_empty_year_notes, by = join_by(id, year_id))
eno_etym_long_mini1

# fix the code for "literal meaning" into just "lit."
eno_etym_long_mini2 <- eno_etym_long_mini1 |> 
  mutate(notes = replace(notes, notes == "literally 10x40+2x40+10", "lit. '10x40+2x40+10'")) |> 
  mutate(notes = str_replace(notes, "^(literal(ly)?( mean(s|ing))?|lit\\.\\smeaning)", "lit.")) |> 
  mutate(notes = str_replace(notes, "^literally", "lit. ")) |> 
  mutate(notes = str_replace(notes, "\\blit\\.\\:", "lit. ")) |> 
  mutate(notes = str_replace(notes, "(?<=leaf\\sof\\stree)'\\?", "?'")) |> 
  mutate(notes = str_replace(notes, "(?<=^lit\\.\\s)'I'm(?=\\s)", "'I am")) |> 
  mutate(notes = str_replace(notes, "(?<=^lit\\.\\s)(10x[?]x40[+]10)", "'\\1'")) # |> 
  # filter(str_detect(notes, "^lit\\.")) |> select(notes) |> as.data.frame()

eno_etym_long_mini2

eno_etym_long_mini3 <- eno_etym_long_mini2 |> 
  mutate(notesnew = if_else(str_detect(notes, "^second word means\\s"),
                            str_c('<w><m>', str_extract(words, "(?<=\\[).+?(?=\\])"), '</m> <def>', str_replace(notes, "^second word", ""), '</def></w>', sep = ""),
                            NA)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^first word means?\\s'[^']+?'\\,\\ssecond word means?\\s'[^']+'$") & 
                              str_detect(words, "\\;\\s[^; ]+$"),
                            str_c(
                              # first word
                              '<w><m>', 
                              str_extract(words, "^[^;]+?(?=\\s\\;)"), 
                              '</m> <def>', 
                              str_replace_all(notes, "(^first word |\\,\\ssecond word.+$)", ""),
                              '</def></w> ; ',
                              
                              # second word
                              '<w><m>',
                              str_extract(words, "(?<=\\;\\s).+$"),
                              '</m> <def>',
                              str_replace_all(notes, "^first .+second word (?=means?)", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^lit\\.\\s"),
                            notes,
                            notesnew)) |> 
  
  mutate(notesnew = if_else(str_detect(notes, "^first word mean.+?\\, second word mean.+?\\, third word mean.+?\\, fourth word mean.+?\\, fifth word mean"),
                            str_c(
                              # first word
                              '<w><m>',
                              str_extract(words, "^[^;]+?(?=\\s)"),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word |\\,\\ssecond word.+$)", ""),
                              "</def></w> ; ",
                              
                              # second word
                              '<w><m>',
                              str_replace(str_extract(words, "^[^;]+?\\s\\;\\s[^;]+?(?=\\s\\;)"), "^.+?\\;\\s", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word.+?second word |\\,\\sthird.+$)", ""),
                              "</def></w> ; ",
                              
                              # third word
                              '<w><m>',
                              str_replace_all(words, "(^[^;]+? ; [^;]+ ; | ; [^;]+ ; [^;]+$)", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word .+third word|\\,\\sfourth word.+$)", ""),
                              "</def></w> ; ",
                              
                              # fourth word
                              '<w><m>',
                              str_replace_all(words, "(^[^;]+?\\s\\;\\s[^;]+?\\s\\;\\s[^;]+?\\s\\;\\s|\\s\\;\\s[^;]+$)", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word .+fourth word|\\,\\sfifth word.+$)", ""), 
                              "</def></w> ; ",
                              
                              # fifth word
                              '<w><m>',
                              str_extract(words, "(?<=\\s\\;\\s)[^;]+?$"),
                              '</m> <def>',
                              str_replace(notes, "^first.+fifth word", ""), 
                              "</def></w>",
                              
                              sep = ""),
                            notesnew)) |> 
  
  mutate(notesnew = if_else(str_detect(notes, "^first word mean.+?\\, second word mean.+?\\, third word mean.+?\\, fourth word mean") & n_word == 4,
                            str_c(
                              # first word
                              '<w><m>',
                              str_extract(words, "^[^;]+?(?=\\s)"),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word |\\,\\ssecond word.+$)", ""),
                              "</def></w> ; ",
                              
                              # second word
                              '<w><m>',
                              str_replace(str_extract(words, "^[^;]+?\\s\\;\\s[^;]+?(?=\\s\\;)"), "^.+?\\;\\s", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word.+?second word |\\,\\sthird.+$)", ""),
                              "</def></w> ; ",
                              
                              # third word
                              '<w><m>',
                              str_replace_all(words, "(^[^;]+?\\s\\;\\s[^;]+?\\s\\;\\s|\\s\\;\\s[^;]+$)", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word .+third word|\\,\\sfourth word.+$)", ""),
                              "</def></w> ; ",
                              
                              # fourth word
                              '<w><m>',
                              str_extract(words, "(?<=\\s\\;\\s)[^;]+?$"),
                              '</m> <def>',
                              str_replace(notes, "^first.+fourth word", ""), 
                              "</def></w>",
                              sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element", negate = TRUE) & 
                              str_detect(notes, "^first\\s[w]") &
                              str_detect(words, "^[^;]+\\;[^;]+$"), # two-word combination
                            str_c(
                              # first word
                              '<w><m>',
                              str_extract(words, "^[^;]+?(?=\\s\\;)"),
                              '</m> <def>',
                              str_replace_all(notes, "(^first words?|\\,\\s(the\\s)?second\\s(word)?.+$)", ""),
                              "</def></w> ; ",
                              # second word
                              '<w><m>',
                              str_extract(words, "(?<=\\;\\s).+$"),
                              '</m> <def>',
                              str_replace_all(str_replace_all(notes, "(^first.+?(?=(the\\s)?second))", " "), "(^first.+$|\\b(the\\s)?second(\\sword)?)", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notes = if_else(str_detect(notes, "^first and second"),
                         str_replace_all(notes,
                                         "^(first) and (second) (word) (means?) ('[^']+?')",
                                         "\\1 \\3 \\4 \\5, \\2 \\3 \\4 \\5"),
                         notes)) |> 
  mutate(notes = if_else(str_detect(notes, "(\\bthird\\b) and (\\bfourth\\b)"),
                         str_replace_all(notes,
                                         "(third) and (fourth) (word) (means?) ('[^']+?')",
                                         "\\1 \\3 \\4 \\5, \\2 \\3 \\4 \\5"),
                         notes)) |> 
  mutate(notes = if_else(str_detect(notes, "\\bsecond and third word means?"),
                         str_replace_all(notes,
                                         "(\\bsecond) and (third) (word) (means?) ('[^']+?')",
                                         "\\1 \\3 \\4 \\5, \\2 \\3 \\4 \\5"),
                         notes)) |> 
  mutate(notes = if_else(str_detect(notes, "^(the )?(first) two (words?) (means?) ('[^']+?')"),
                         str_replace_all(notes,
                                         "^(the )?(first) two (words?) (means?) ('[^']+?')",
                                         "\\2 \\3 \\4 \\5, second \\3 \\4 \\5"),
                         notes)) |> 
  mutate(notesnew = if_else(n_word == 4,
                            str_c(
                              # first word
                              '<w><m>',
                              str_extract(words, "^[^;]+(?=\\;)"),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word |\\,\\ssecond word.+$)", ""),
                              "</def></w> ; ",
                              
                              # second word
                              '<w><m>',
                              str_replace(str_extract(words, "^[^;]+?\\s\\;\\s[^;]+?(?=\\s\\;)"), "^.+?\\;\\s", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word.+?second word |\\,\\sthird.+$)", ""),
                              "</def></w> ; ",
                              
                              # third word
                              '<w><m>',
                              str_replace_all(words, "(^[^;]+?\\s\\;\\s[^;]+?\\s\\;\\s|\\s\\;\\s[^;]+$)", ""),
                              '</m> <def>',
                              str_replace_all(notes, "(^first word .+third word|\\,\\sfourth word.+$)", ""),
                              "</def></w> ; ",
                              
                              # fourth word
                              '<w><m>',
                              str_extract(words, "(?<=\\s\\;\\s)[^;]+?$"),
                              '</m> <def>',
                              str_replace(notes, "^first.+fourth word", ""), 
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(n_word == 3 &
                              str_detect(notes, "element", negate = TRUE) &
                              str_detect(notes, "^first w"),
                            str_c(
                              # first word
                              "<w><m>",
                              str_extract(words, "^[^;]+(?=\\;)"),
                              "</m> <def>",
                              str_replace(str_extract(notes, "^first word.+(?=\\,\\ssecond wor)"), "^first words? ", ""),
                              "</def></w> ; ",
                              
                              # second word
                              "<w><m>",
                              str_extract(words, "(?<=\\s\\;\\s)[^;]+(?=\\;)"),
                              "</m> <def>",
                              str_replace(str_extract(notes, "second words? mea.+(?=\\,\\s(the\\s)?third wor)"), "second words? ", ""),
                              "</def></w> ; ",
                              
                              # third word
                              "<w><m>",
                              str_extract(words, "(?<=\\;\\s)[^;]+$"),
                              "</m> <def>",
                              str_replace(str_extract(notes, "(the\\s)?third words? means?.+$"), "(the\\s)?third words? ", ""),
                              "</def></w>",
                              
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == 1864 &
                              words == "oehouw-oenie [manu]" &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c("<w><m>", 
                                  str_extract(words, "\\[manu\\]"), 
                                  "</m> is <def>",
                                  str_extract(notes, "unclear$"),
                                  "</def></w>",
                                  sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == 1895 &
                              words == "kolè [amie amie]",
                            str_c("<w><m>",
                                  str_extract(words, "\\[amie amie\\]"),
                                  "</m> <def>",
                                  str_replace(notes, "^.+(?=means\\s\\'shoulder\\'$)", ""),
                                  "</def></w>",
                                  sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == "1855vdS" &
                              words == "[moko] doeboe" &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "^\\[moko\\]"),
                              "</m> <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w> ; ",
                              
                              # second element
                              "<w><m>",
                              str_extract(words, "doeboe$"),
                              "</m> <def>",
                              str_replace_all(notes, "^(the )?first element.+(the )?second element ", ""),
                              "</def></w>",
                              sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == "1864" &
                              words == "[oebah-]dobah" &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "^\\[oebah\\-\\]"),
                              "</m> <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w> ; ",
                              
                              # second element
                              "<w><m>",
                              str_extract(words, "dobah$"),
                              "</m> <def>",
                              str_replace_all(notes, "^(the )?first element.+(the )?second element ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == "1895" &
                              words == "dŏboe[kabandiăk]" &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "dŏboe"),
                              "</m> <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w> ; ",
                              
                              # second element
                              "<w><m>",
                              str_extract(words, "\\[kabandiăk\\]"),
                              "</m> is <def>",
                              str_replace_all(notes, "^(the )?first element.+(the )?second element ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id %in% c("<1855", "1855vdS") &
                              words %in% c("moko[-dobo]", "moko [doeboe]") &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "^moko"),
                              "</m> <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w> ; ",
                              
                              # second element
                              "<w><m>",
                              str_extract(words, "\\[\\-?doe?boe?\\]"),
                              "</m> <def>",
                              str_replace_all(notes, "^(the )?first element.+(the )?second element ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == "1895" &
                              words == "[oeahaub]oepèĕ" &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "^\\[.+?\\]"),
                              "</m> is <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id %in% c("<1855", "1855vdS") &
                              words %in% c("[koa-]pidjoe", "[koe] pidjoe") &
                              str_detect(notes, "element") &
                              n_word == 1,
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "^\\[ko[ae]\\-?\\]"),
                              "</m> <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w> ; ",
                              
                              # second element
                              "<w><m>",
                              str_extract(words, "pidjoe"),
                              "</m> <def>",
                              str_replace_all(notes, "^(the )?first element.+(the )?second element ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(n_word == 1 &
                              year_id == "1891" &
                              str_detect(notes, "element") &
                              words == "[èdödöo]eijo",
                            str_c(
                              # first element
                              "<w><m>",
                              str_extract(words, "^\\[.+?\\]"),
                              "</m> is <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(n_word == 1 &
                              year_id == "1895" &
                              str_detect(notes, "element") &
                              words == "èoba kamakŏ",
                            str_c(# first element
                              "<w><m>",
                              str_extract(words, "^[^ ]+(?=\\s)"),
                              "</m> <def>",
                              str_replace_all(notes, "(\\,\\s(the )?second element.+|^(the )?first element )", ""),
                              "</def></w>",
                              sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^first mean") &
                              n_word == 2,
                            str_c(
                              # first word
                              "<w><m>",
                              str_extract(words, "^[^;]+(?=\\;)"),
                              "</m> <def>",
                              str_replace_all(notes, "(^first (?=means? \\')|(?<=\\')\\, second (words? )means? .+$)", ""),
                              "</def></w> ; ",
                              
                              # second word
                              "<w><m>",
                              str_extract(words, "(?<=\\;\\s)[^;]+$"),
                              "</m> <def>",
                              str_replace_all(notes, "^first .+second words?", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") &
                              n_word == 2 &
                              str_detect(notes, "(^first word means.+second word means)"),
                            str_c(
                              # first word
                              "<w><m>",
                              str_extract(words, "^[^;]+(?=\\s\\;)"),
                              "</m> <def>",
                              str_replace_all(notes, "(^first word\\s(?=means)|(?<=\\')\\,\\ssecond word mean.+)", ""),
                              "</def></w> ; ",
                              
                              # second word
                              "<w><m>",
                              str_extract(words, "(?<=\\;\\s)[^;]+$"),
                              "</m> <def>",
                              str_extract(notes, "(?<=second word )means\\s'[^']+?'"),
                              "</def></w>",
                              
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") &
                              n_word == 2 & 
                              words == "[moko-]dobo ; [panoekoe-am]dobo",
                            str_c(notesnew,
                                  
                                  # first element
                                  "__<w element='1'><m>",
                                  str_extract(words, "^\\[moko\\-\\]"),
                                  "</m> in \"",
                                  str_extract(words, "^\\[moko\\-\\]dobo"),
                                  "\" <def>",
                                  str_replace_all(notes, "(^first.+first element of the first word |\\,\\sthe second element of the first.+$)", ""),
                                  "</def></w>",
                                  
                                  # second element
                                  "__<w element='2'><m>",
                                  str_extract(words, "(?<=^\\[moko\\-\\])(dobo)"),
                                  "</m> in \"",
                                  str_extract(words, "^\\[moko\\-\\]dobo"),
                                  "\" <def>",
                                  str_replace(notes, "(^first.+first element of the first word.+second element of the first word )", ""),
                                  "</def></w>",
                                  sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") &
                              n_word == 2 &
                              words == "oebah ; oebah[-dobah]",
                            str_c(notesnew,
                                  
                                  # second element
                                  "__<w element='2'><m>",
                                  str_extract(words, "\\[\\-dobah\\]"),
                                  "</m> in \"",
                                  str_extract(words, "\\boebah\\[\\-dobah\\]$"),
                                  "\" <def>",
                                  str_replace(notes, "(^first word.+second element of second word )", ""),
                                  "</def></w>",
                                  sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") &
                              n_word == 2 &
                              words == "djoeba[-beri-berri] ; joeba",
                            str_c(notesnew,
                                  
                                  # second element
                                  "__<w element='2'><m>",
                                  str_extract(words, "\\[\\-beri\\-berri\\]"),
                                  "</m> in \"",
                                  str_extract(words, "\\bdjoeba\\[\\-beri\\-berri\\]"),
                                  "\" <def>",
                                  str_replace_all(notes, "(^first word.+second element in first word |but\\scf.+$)", ""),
                                  "</def></w> <re><w>", # <re> element is related entry in TEI lexicographic attribute
                                  str_replace(notes, "^first.+(?=but)", ""),
                                  "</w></re>",
                                  sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") &
                              n_word == 2 &
                              words == "mŏkŏ ; mŏkŏ [èfōka]",
                            str_c(
                              str_replace(notes, "second element.+$", ""),
                              "<w element='2'><m>",
                              str_extract(words, "\\[èfōka\\]"),
                              "</m> in \"",
                              str_replace(words, "^[^;]+;\\s", ""),
                              "\" <def>",
                              str_replace(notes, "^meaning of second element.+in second word", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") & is.na(notesnew) & n_word == 3 & words == "[dŏboe]kabandiăk ; kahĕdiak ; baēdiăk",
                            str_c(
                              "<w element='1'><m>",
                              str_extract(words, "^\\[[^;]+?\\](?=kabandi)"),
                              "</m> in \"",
                              str_extract(words, "^[^;]+?(?=\\s\\;)"),
                              "\" <def>",
                              str_replace(notes, "^first element.+(?=means)", ""),
                              "</def></w>"
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^loanword\\s(?!or\\s)") &
                              str_detect(notes, "loanword\\s(?![A-Z])"),
                            str_c("<etym type='loanword'>", str_replace(notes, "^loanword ", ""), "</etym>", sep = ""),
                            notesnew),
         notesnew = if_else(str_detect(notes, "(^loanword[?]?$|^loanword or continuation[?]?)"),
                            str_c("<etym type='", notes, "'/>", sep = ""),
                            notesnew)) |> 
  mutate(notesnew = if_else(year_id == "1888" & 
                              str_detect(notes, "belongs? to Hel"), 
                            str_c("\"", words, "\"", str_replace(notes, "^[Tt]his word(?=\\sbelongs?)", ""), sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "element") & 
                              n_word == 2 &
                              words == "èŏkŏ(k) ; ŏkia [èpiko(k)]",
                            str_c(notesnew,
                                  # second element in second word
                                  "__<w element='2'><m>",
                                  str_replace(words, "^.+ŏkia ", ""),
                                  "</m> in \"",
                                  str_replace(words, "^[^;]+\\;\\s", ""),
                                  "\" <def>",
                                  str_replace(notes, "(^first word.+second element in second word )", ""),
                                  "</def></w>",
                                  sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "stepson"), 
                            str_c(notesnew, 
                                  "__<w element='1'><m>", 
                                  str_extract(words, "(?<=\\;\\s)\\[.+?\\]"), 
                                  "</m> in \"", 
                                  str_extract(words, "(?<=\\;\\s)\\[.+?\\].+$"), 
                                  "\" <def>means 'I'</def></w>", 
                                  sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(words, "\\[maenu\\]"), 
                            str_c(notesnew, 
                                  "__<w element='1'><m>", 
                                  str_extract(words, "\\[maenu\\]"), 
                                  "</m> in \"", 
                                  str_extract(words, "(?<=\\;\\s).+cahora$"), 
                                  "\" <def>unclear</def></w>", 
                                  sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(n_word == 2 & 
                              str_detect(notes, "element") & 
                              str_detect(words, "kahara"), 
                            str_c(notesnew, 
                                  "__<w element='1'><m>", 
                                  str_extract(words, "^\\[kahara\\]"), 
                                  "</m> in \"[kahara] koewo\" <def>unclear</def></w>" , 
                                  sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(n_word == 2 &
                              str_detect(notes, "^(?i)the first word mean"),
                            str_c(
                              
                              # first word
                              '<w><m>', 
                              str_extract(words, "^[^;]+?(?=\\s\\;)"), 
                              '</m> <def>', 
                              str_replace_all(notes, "(^(?i)the first word |\\,\\sthe second word.+$)", ""),
                              '</def></w> ; ',
                              
                              # second word
                              '<w><m>',
                              str_extract(words, "(?<=\\;\\s).+$"),
                              '</m> <def>',
                              str_replace_all(notes, "(^(?i)the first .+second word (?=means?)|\\.\\sThe meaning of the second.+)", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(n_word == 2 & 
                              str_detect(notes, "meaning of .+part"),
                            str_c(notesnew, 
                                  "__<w element='2'><m>", 
                                  str_replace_all(words, "(^.+\\;\\s|\\[.+\\]\\s?)", ""), 
                                  "</m> in \"", 
                                  str_replace(words, "^.+\\;\\s", ""), 
                                  "\" <def>", 
                                  str_replace(notes, "^.+second part.+second word ", ""), 
                                  "</def></w>", 
                                  sep = ""),
                            notesnew))

eno_etym_long_mini4 <- eno_etym_long_mini3 |> 
  mutate(notesnew = if_else(str_detect(notes, "brave"), 
                            str_c("<w><m>", 
                                  str_extract(words, "(?<=\\;\\s).+$"), 
                                  "</m> <note>", 
                                  str_extract(notes, "is listed under 'brave'"), 
                                  "</note> <def>", 
                                  str_replace(notes, "^the second.+brave.+and ", ""), 
                                  "</def></w>",
                                  sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = replace(notesnew, 
                            notes == "'ho' means 'already'", 
                            "<w><m>ho</m> <def>means 'already'</def></w>")) |> 
  mutate(notesnew = if_else(str_detect(notes, "syllable"),
                            str_c("<w><m>",
                                  str_extract(words, "(?<=\\[).+?(?=\\])"),
                                  "</m> <def>",
                                  str_replace(notes, "^.+syllable ", ""),
                                  "</def></w>",
                                  sep = ""
                                  ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "reduplicated") &
                              words == "noekie noekie",
                            str_c(
                              "<w><m>",
                              words,
                              "</m> <def>",
                              str_replace_all(notes, "(^redup.+?form\\s|\\,\\sunredu.+)", ""),
                              "</def></w> ; ",
                              "<w><m>",
                              str_replace(words, "^[^ ]+\\s", ""),
                              "</m> <def>",
                              str_replace(notes, "^redu.+unreduplicated form ", ""),
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^(archaic|'five\\sperson')$"),
                            notes,
                            notesnew)) |> 
  mutate(notesnew = if_else(notes %in% c("the meaning is 'make peace' but it literally means 'do good'", "imperative", "intransitive", "6 a.m.", "polite word", "'2x person +10?'"),
                            notes,
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "kahafie ; kahapie ; [wa]kafie",
                            str_c(
                              "<w><m>",
                              str_extract(words, "^[^;]+?(?=\\s\\;)"),
                              "</m> and <m>",
                              str_extract(words, "(?<=\\;\\s)([^;]+?)(?=\\s\\;)"),
                              "</m> <note>",
                              str_replace_all(notes, "(^(the )?first two words |\\,\\sthe third word.+)", ""),
                              "</note></w> ; <w><m>",
                              str_extract(words, "(?<=\\;\\s)[^;]+$"),
                              "</m> <def>",
                              str_replace(notes, "^.+the third word ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = str_replace(notesnew, "\\s\\;\\s\\<w\\>\\<m\\>foeroeroe\\<\\/m\\>\\s\\<def\\>.+$", "")) |> 
  mutate(notesnew = if_else(str_detect(notes, "^first word unexplained"),
                            str_c(
                              '<w><m>',
                              str_extract(words, "(?<=\\()[^)]+?(?=\\))"),
                              "</m> in \"",
                              words,
                              '" <def>',
                              str_extract(notes, "unexplained"),
                              "</def>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "[uuaha] mena ; manà", 
                            "<w><m>[uuaha] mena</m> <def>means 'bring'</def></w>__<w element=2><m>mena</m> in \"[uuaha] mena\" <def>means 'take'</def></w>__<w element=1><m>uuaha</m> in \"[uuaha] mena\" <def>means 'I am going'</def></w>__<w element=2><m>mena</m> in \"[uuaha] mena\" <def>is a conjugated form of the root \"na-\"</def></w>", 
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "first.+conservative") & n_word > 1, 
                            str_c("<w><m>", 
                                  str_replace(words, "\\s\\;.+", ""), 
                                  "</m> <note>", 
                                  str_extract(notes, "(?<=word\\s)must be.+$"), 
                                  "</note></w>", sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(notes == "meaning of second part unclear, perhaps 'woman'?", 
                            str_replace(notes, "second part", 
                                        str_c("<w><m>", str_replace(words, "^[^ ]+?\\s", ""), "</m></w>", sep = "")), 
                            notesnew)) |> 
  mutate(notesnew = if_else(notes == "second element unclear, maybe meaning 'forest'?", 
                            str_c("<w element='2'><m>", 
                                  str_extract(words, "kaoeè"),
                                  "</m> and <m>", 
                                  str_extract(words, "kiŏha"),
                                  "</m> <def>", 
                                  str_replace(notes, "^second element ", ""), 
                                  "</def></w>", 
                                  sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "these forms are somewhat unclear but must belong to the same root"),
                            str_c(
                              str_replace(notes, "(?<=\\;).+", " <w element=1><m>"),
                              str_replace_all(words, "(^.+(?=\\[)|(?<=\\]).+$)", ""),
                              "</m> in \"",
                              str_replace(words, "^.+(?=\\[)", ""),
                              "\" ",
                              str_replace(notes, "^.+third word ", "<def>"),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "mino [afo]",
                            str_replace(notes,
                                        "the first part",
                                        str_replace(str_replace(words, "\\s\\[.+$", ""), "(^.+)", "<w element=1><m>\\1</m>")),
                            notesnew),
         notesnew = if_else(words == "mino [afo]",
                            str_replace(notesnew, "('finger')", "\\<def>\\1</def></w>"),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "second element means 'male/female'") & words == "kahao [ehuda/ẽmãnĩ]",
                            str_c(
                              "<w element=2><m>",
                              str_replace(words, "^.+(?=\\[)", ""),
                              "</m> <def>",
                              str_replace(notes, "^second element ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "[oeaba]ōdie",
                            str_c(
                              "<w element=1><m>",
                              str_replace(words, "(?<=\\]).+$", ""),
                              "</m> <def>",
                              str_replace(notes, "^first part ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |>
  mutate(notesnew = if_else(str_detect(notes, "^(?i)this (word|form) literally"),
                            str_c(
                              "<w><m>",
                              words,
                              "</m> <def>",
                              str_replace(notes, "^(?i)this (word|form) ", ""),
                              "</def></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = replace(notesnew, words  %in%  c("naa(ha)fè", "", "èhaĕ"), NA)) |> 
  mutate(notesnew = if_else(str_detect(notes, "first part unclear$"),
                            str_c(
                              "<w element='1'><m>",
                              str_extract(words, "(?<=^\\[)[^]]+?(?=\\])"), 
                              "</m> in \"", 
                              words, 
                              "\" <def>", 
                              str_extract(notes, "unclear$"), "</def></w>", 
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^meaning 'head'"),
                            str_c(
                              "<w><m>",
                              words,
                              "</m> <def>",
                              str_extract(notes, "^meaning 'head'"),
                              "</def> <note>",
                              str_replace(notes, "^meaning 'head', ", ""),
                              "</note></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^second word is missing its second part"),
                            str_c(
                              "<w><m>",
                              str_replace(words, "^.+?\\;\\s", ""),
                              "</m> <note>",
                              str_replace(notes, "^second word ", ""),
                              "</note></w>",
                              sep = ""
                            ),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^first part perhaps corresponds to") & year_id=="1894", 
                            str_c("<w><m>", 
                                  str_replace(words, "(?<=\\])chi$", ""), 
                                  "</m> ", 
                                  str_replace_all(notes, "(^first part |(?<=\\bto\\s).+cf.+$)", ""), 
                                  "<link target='#", str_replace_all(notes, "(^first part perhaps.+to\\s|\\,\\s.+$)", ""), 
                                  "'>", str_replace_all(notes, "(^first part perhaps.+to\\s|\\,\\s.+$)", ""), "</link> <re><w>", 
                                  str_replace(notes, "^first part.+(?=\\bcf\\.)", ""), 
                                  "</w></re>", 
                                  sep = ""
                                  ), 
                            notesnew))

eno_etym_long_mini5 <- eno_etym_long_mini4 |> 
  mutate(notesnew = if_else(notes == "cf. sea",
                            str_replace(notes, "\\b(sea)\\b", "<re><w>\\1</w></re>"),
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "behoba abie obie", 
                            str_c("<w><m>", 
                                  str_extract(notes, "^\"behoba\""), 
                                  "</m> ", 
                                  str_extract(notes, "perhaps means"), 
                                  " <def>", 
                                  str_extract(notes, "'has come'"), 
                                  "</def></w> ; <w><m>", 
                                  str_extract(notes, "\"abie obie\""), 
                                  "</m> means <def>", 
                                  str_extract(notes, "'fire'"), 
                                  "</def></w>", 
                                  sep = ''), 
                            notesnew)) |> 
  mutate(notesnew = if_else(notes == "compound of 'belly + ?'",
                            "compound of 'belly + ?' ; <re><w>belly</w></re>",
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "kahapudu",
                            str_replace(notes, "(?<=^obsolete variant of )(.+$)", "<re><w>\\1</w></re>"),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^Kähler also glosses e-poko"),
                            str_replace(notes, "(?<=also\\sglosses\\s)(e\\-poko)", "<re><w>\\1</w></re>"),
                            notesnew)) |> 
  mutate(notesnew = if_else(words == "kakènèbaka ; kakèbara", 
                            str_c('"', 
                                  str_extract(words, "(?<=\\s\\;\\s).+$"), 
                                  '" ', 
                                  str_extract(notes, "is perhaps a mishearing of "), 
                                  '"', 
                                  str_extract(words, "^[^ ;]+(?=\\s\\;)"), '"', sep = ""), 
                            notesnew)) |> 
  mutate(notesnew = if_else(notes %in% c(#"the second word is perhaps a mishearing of first word", 
                                         #"first word means 'high', second word means 'long'",
                                         "clipping of second part?",
                                         "clipping of previous word with genitive"#,
                                         #"clipping of second word, i.e. 'skin of ...'",
                                         #"clipping of second part for 'eye'"
                                         ),
                            "PENDING",
                            notesnew)) |> 
  mutate(notesnew = if_else(notes == "first word means 'high', second word means 'long'" & year == "1895",
                            "PENDING",
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "likely a derivative of ara 'child'"), 
                            str_replace(notes, 
                                        "(?<=of\\s)(ara) ('child')", 
                                        "<re><w>\\1</w> <def>\\2</def></re>"
                                        ), 
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "likely a derivative of ara 'child'"), 
                            str_replace(notesnew, 
                                        "(?<=corresponding to Kähler )([^']+?)\\s(\\'[^']+\\'$)", 
                                        "<w>\\1</w> <def>\\2</def></re>"
                                        ), 
                            notesnew)) |>  
  mutate(notesnew = if_else(str_detect(notes, "likely a derivative of ara 'child'"), 
                            str_replace(notesnew, 
                                        "(?<=corresponding to )([^ ]+?)(?=\\s)", 
                                        "<re><ref>\\1</ref>"), 
                            notesnew)) |>
  mutate(notesnew = if_else(str_detect(notes, "kaiya 'will come'"), 
                            str_replace(notes, "(Kähler) has (kaiya) ('will come')", 
                                        "<re><ref>\\1</ref> <w>\\2</w> <def>\\3</def></re>"), 
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "This word only appears in compounds such as 'farmer' and 'sow'"), 
                            str_replace_all(notes, 
                                            "\\'\\b(farmer|sow)\\b\\'", 
                                            "<link target='#\\1'>\\1</link>"), 
                            notesnew)) |> 
  mutate(notesnew = if_else(notes == "listed under 'give' in the wordlist, but translated as 'fruit' in the examples",
                            str_replace_all(notes, "\\'\\b(give|fruit)\\b\\'", "<link target='#\\1'>\\1</link>"),
                            notesnew)) |> 
  mutate(notesnew = if_else(notes == "Modigliani says this term is used by the Malay people",
                            str_replace(notes, "\\bthis term\\b", words),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^inferred from rooster and hen$"),
                            str_replace_all(notes, "\\b(rooster|hen)\\b", "<link target='#\\1'>\\1</link>"),
                            notesnew)) |> 
  mutate(notesnew = if_else(str_detect(notes, "^Yoder form means"),
                            replace(notes, str_detect(notes, "^Yoder form means"),
                                    "<w><m>harub</m> <def>means 'your possession'</def></w> and relates to <re><ref>Kähler</ref> <w>hadu</w></re> and <re><ref>contemporary Enggano</ref> <w><m>hear</m> which becomes <m>haru'</m> <def>'my possession'</def></w> and <w><m>harub</m> <def>'your possession'</def></w></re> and possible connection to <re><w>a-nu</w></re>"),
                            notesnew)) |> 
  
  # change the notes into NA for "èhaĕ" and "naa(ha)fè" because this is a single word in the different dialect of Enggano for the same year of 1891
  mutate(notes = replace(notes, words %in% c("èhaĕ", "naa(ha)fè") & year == "1891" & EngganoLanguage == "Enggano Kèfoe", NA))

eno_etym_long_mini6 <- eno_etym_long_mini5 |> 
  mutate(notesnew = if_else(!is.na(notes) & is.na(notesnew),
                            notes,
                            notesnew))

# combine the general note (for a given row [i.e., ID]) without specific year
eno_etym_long_mini7 <- eno_etym_long_mini6 |> 
  left_join(rm_tagged |> 
              filter(year_id == "") |> 
              select(id, note_by_id = content))


# processing the un-tagged remarks (remarks without year notes)
rm_untagged1 <- rm_untagged |> 
  
  # identify which ID appears twice (due to the earlier splitting of the notes)
  group_by(id) |> 
  
  # combine these multiple IDs into a single note
  mutate(rm5 = str_c(rm4, collapse = "_;_")) |> 
  select(id, rm5, tagged) |> 
  distinct() |> 
  mutate(rm5 = str_replace(rm5, "^(literally|literal meaning)", "lit."))

# save the untagged note as .csv for further editing before being merged with the database
# rm_untagged1 |> write_csv2(file = "data/rm_untagged1.csv")

# edit the untagged notes
rm_untagged2 <- rm_untagged1 |> 
  mutate(notesnew = rm5,
         notesnew = str_replace(notesnew, "\\, cf", " ; cf"),
         notesnew = str_replace_all(notesnew, "_(\\;)_", " \\1 "),
         notesnew = str_replace(notesnew, "cf\\. van Rosenberg Kepoe Taigoeka 'big island'", "<re><ref>van Rosenberg</ref> <w><m>Kepoe-taïgoeka</m> <def>'big island'</def></w></re>"),
         notesnew = str_replace(notesnew, "\\sout\\/side$", "outside"),
         notesnew = str_replace_all(notesnew, "(\\s\\&\\s|\\band\\b)", " , "),
         notesnew = replace(notesnew, rm5 == "cf. Edwards (2015:68) for split of /o/", "<re><ref>Edwards (2015:68)</ref> <note>for split of /o/</note></re>"),
         notesnew = replace(notesnew, rm5 == "cf. Edwards (2015:68) for split of /o/, Yoder says this is a loanword", "<re><ref>Edwards (2015:68)</ref> <note>for split of /o/</note></re> <re><ref>Yoder (2011)</ref> <note>says this is a loanword</note></re>"),
         notesnew = replace(notesnew, rm5 == "cf. ter Keurs (2002)", "<re><ref>ter Keurs (2002)</ref></re>"),
         notesnew = replace(notesnew, rm5 == "cf. BI jorok", "<re><lang>BI</lang> <def>'jorok'</def></re>"),
         notesnew = replace(notesnew, rm5 == "cf. Clercq 1909:359 'bladscheede van Areca Catechu'", "<re><ref>Clercq 1909:359</ref> <def>'bladscheede van Areca Catechu'<def></re>")) |> 
  rename(note_untagged = notesnew)

eno_etym_long_mini8 <- eno_etym_long_mini7 |> 
  left_join(rm_untagged2 |> select(id, note_untagged)) |> 
  rename(note_year = notesnew,
         note_id = note_by_id, # note_id column is for general note applying for the whole row (hence note_by_id)
         note_etc = note_untagged) |>
  mutate(note_year = str_replace_all(note_year, "<re><w>cf. by</w></re>", "<re>cf. <link target='#by'>by</link></re>"),
         note_year = str_replace_all(note_year, "<re><w>but cf. boat</w></re>", "but <re>cf. <link target='#boat'>boat</link></re>"),
         note_year = str_replace_all(note_year, "cf\\. dog", "<re>cf. <link target='#dog'>dog</link></re>"),
         note_year = str_replace_all(note_year, "\\bcf\\. <re><w>sea</w></re>", "<re>cf. <link target='#sea'>sea</link></re>")) |> 
  mutate(entry_id = row_number()) |>  # add entry_id for unique ID of rows in the whole table 
  
  # Fixing anomaly in Capell (1982)
  mutate(english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "ai ; kabebai" &
                                 english_new == "ear",
                               "come"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "baka" &
                                 english_new == "fat (n.)",
                               "eye"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "fufuri" &
                                 english_new == "fire",
                               "tailbone"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "hela" &
                                 english_new == "fly",
                               "flesh"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "qapo" &
                                 english_new == "head",
                               "finger"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "ulu ; udu" &
                                 english_new == "hear",
                               "head"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, "\\(ki\\)dehoi") &
                                 english_new == "hot",
                               "hear"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, "^kaidəhaudə$") &
                                 english_new == "house",
                               "hot"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, stringi::stri_trans_nfc("pūnũ")) &
                                 english_new == "lie down",
                               "leaf"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, stringi::stri_trans_nfc("hũhũ")) &
                                 english_new == "man",
                               "owl"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "mani" &
                                 english_new == "many",
                               "man/male"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "koh(oi)" &
                                 english_new == "mouth",
                               "mountain"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, "^kaa") &
                                 english_new == "name",
                               "mouth"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, "\\bkakak\\b") &
                                 english_new == "way",
                               "person"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "niə" &
                                 english_new == "throat",
                               "name"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "okahaqe" &
                                 english_new == "new",
                               "throat"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == stringi::stri_trans_nfc("nōninə") &
                                 english_new == "night",
                               "correct"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == stringi::stri_trans_nfc("põõ") &
                                 english_new == "not",
                               "night"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "fanu" &
                                 english_new == "one",
                               "nose"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "kaik" &
                                 english_new == "red",
                               "one"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 words == "uli" &
                                 english_new == "sit",
                               "skin"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, "\\beheku\\b") &
                                 english_new == "sleep",
                               "sit"),
         english_new = replace(english_new,
                               EngganoSource == "Capell 1982" &
                                 str_detect(words, "\\balouhu\\b") &
                                 english_new == "small",
                               "sky"))


# Dummy untuk Pak Cok
# eno_etym_long_mini5 |> 
#   left_join(eno_etym_long_proto_df |> distinct()) |> 
#   select(cognate_id = id, year, words, indonesian = indonesian_gloss, english = english_new, semantic_field, source = EngganoSource, notes = notesnew, matches("^(PAN|PMP|Remark on)")) |> 
#   write_tsv("data/dummy_for_pak_cok.tsv")

test_df <- eno_etym_long_mini6 |> filter(!is.na(notes), notes != "PENDING") |> select(id, year_id, words, english_new, n_word, notes, notesnew)
test_df |> 
  filter(!is.na(notes), is.na(notesnew)) |> 
  select(year_id, words, n_word,notes) |> 
  arrange(year_id) |> 
  print(n=Inf)
