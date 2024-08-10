library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(colourvalues)

# Data Preparation =====
year_arranged <- c("< 1855", "1854", "1855", "1864", "1870", "1878", "1879", 
                   "1888", "1891", "1894", "1916", "1979", "1982", "1987", 
                   "2011", "2019", "2022")
bibs <- read_rds("sources.rds") |> 
  filter(BIBTEXKEY != "NothoferMS") |> 
  mutate(YEAR = replace(YEAR, YEAR == "n.d.", "< 1855")) |> 
  mutate(YEAR = factor(YEAR, levels = year_arranged)) |> 
  arrange(YEAR, AUTHOR) |> 
  mutate(Sources = str_replace(Sources, "et al\\. 2023", "et al. 2022")) |> 
  mutate(Sources = replace(Sources, Sources == "vd Straten & S. 1855", "vd Straaten & Severijn 1855"))

## Read in the main data
elx <- read_rds("enolex.rds") |> 
  mutate(Sources = replace(Sources, Sources == "vd Straten & S. 1855", "vd Straaten & Severijn 1855")) |> 
  mutate(Etymology_Source = str_replace_all(Etymology_Source, "^(Lafeber)(1922)",
                                        "\\1 \\2"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "\\, ",
                                            "; "),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(\\bACD\\b)\\s([0-9]+)",
                                            "\\1 <a href='https://acd.clld.org/cognatesets/\\2' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Edwards )(2015)",
                                            "\\1<a href='https://openresearch-repository.anu.edu.au/server/api/core/bitstreams/5bd92bd2-ff85-4e76-92c0-8f52593a4654/content' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(1987)",
                                            "<a href='https://books.google.co.id/books/about/Enggano_deutsches_Wörterbuch.html?id=OEsOAAAAYAAJ&redir_esc=y' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Smith )(2020)",
                                            "\\1<a href='https://www.austronesianist.com/_files/ugd/fb0c2e_c7954bbefb464344a104aa45fecc6d24.pdf' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Nothofer )(1986)",
                                            "\\1<a href='https://openresearch-repository.anu.edu.au/server/api/core/bitstreams/749ab386-9a3e-49d8-bd0e-7929cec4c069/content' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Nothofer )(1994)",
                                            "\\1<a href='https://doi.org/10.1515/9783110883091.389' target='_blank'>\\2</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(?<=Zorc )(1995)",
                                            "<a href='https://zorc.net/RDZorc/publications/093=GlossaryOfAustronesianReconstructions[ACD].pdf' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(1975)",
                                            "<a href='https://search.worldcat.org/title/Texte-von-der-Insel-Enggano-:-(Berichte-uber-eine-untergehende-Kultur)/oclc/2333004' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(1940)",
                                            "<a href='https://glottolog.org/resource/reference/id/38922' target='_blank'>\\1</a>"),
         Etymology_Source = str_replace_all(Etymology_Source,
                                            "(Mahdi )(1988)",
                                            "\\1<a href='https://books.google.co.id/books/about/Morphophonologische_Besonderheiten_und_h.html?id=RWMOAAAAYAAJ&redir_esc=y' target='_blank'>\\2</a>"))

## Dialect by sources
dialect_info <- elx |> 
  select(Sources, Doculect) |> 
  distinct() |> 
  rename(Dialect_Info = Doculect) |> 
  mutate(Dialect_Info = replace(Dialect_Info,
                                Sources %in% c("Zakaria et al. 2022",
                                               "Aron 2019"),
                                "Enggano Meok"),
         Dialect_Info = replace(Dialect_Info,
                                Dialect_Info == "Enggano",
                                "Enggano (unspec.)")) |> 
  group_by(Sources) |> 
  mutate(Dialect_Info = str_c(Dialect_Info, collapse = " ; ")) |> 
  ungroup() |> 
  distinct()

## count the forms by sources
form_count <- elx |> 
  select(Sources, Original_Form) |> 
  group_by(Sources) |> 
  summarise(Count_of_Original_Form = n_distinct(Original_Form))

### join the count and dialect info with bibs
bibs <- bibs |> 
  left_join(form_count) |> 
  left_join(dialect_info)

## Prepare the choice for the English concept
elx_eng <- sort(unique(elx$English), decreasing = FALSE)
sem_choices_eng <- c("(none)", elx_eng)

## Prepare the choice for the Sources
bib_choices <- c("(none)", bibs$Sources)

bibs1 <- select(bibs,
                # -Sources,
                -BIBTEXKEY,
                -YEAR,
                -URL) |> 
  mutate(CITATION = str_replace_all(CITATION, "(\\s)_", "\\1<em>"), 
         CITATION = str_replace_all(CITATION, "_(\\s|[[:punct:]])", "</em>\\1"),
         CITATION = if_else(str_detect(CITATION, "\\<https"),
                            str_replace_all(CITATION, "\\<(https[^>]+?)\\>", 
                                            "<a href='\\1' target='_blank'>URL</a>"),
                            CITATION)) |> 
  rename(YEAR = YEAR_URL) |> 
  select(Year = YEAR, Sources, Form_Count = Count_of_Original_Form, Dialect_Info, Citation = CITATION)

english_gloss <- selectizeInput(inputId = "English_Gloss", 
                                options = list(dropdownParent = "body"),
                                label = "Concepts",
                                choices = NULL, 
                                selected = NULL
)

js_enter_key <- '
$(document).on("keyup", function(e) {
  if((e.keyCode == 13)){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});'

# js_enter_key <- '$(document).keyup(function(event) {
#     if ($("#site_search").is(":focus") && (event.key == "Enter")) {
#        $("#goButton").click();
#     }
# });'

# js_enter_key <- '
#   $(document).ready(function() {
#     $(window).keydown(function(event){
#       if(event.keyCode == 13) {
#         event.preventDefault();
#         return false;
#       }
#     });
#   });
#   '

# one_search_all <- textInput("searchbar", label = "Search", placeholder = "Type & press Enter")

link_enolex_github <- tags$a(shiny::icon("github"), "GitHub", 
                             href="https://github.com/engganolang/enolex", 
                             target="_blank")

link_enggano_web <- tags$a(shiny::icon("globe", lib = "glyphicon"), "Enggano webpage", 
                           href="https://enggano.ling-phil.ox.ac.uk/", 
                           target="_blank")

bibs <- selectizeInput(inputId = "References",
                       label = "Sources",
                       choices = NULL, 
                       selected = NULL)

# Cards =====
## MAIN page ====
cards <- list(
  background_image = 
    card(card_image("estuary.JPG", border_radius = "none", height = "255px"),
         card_footer("The estuary towards the Indian Ocean from the Bak Blau lake, Enggano Island",
                     class = "fs-6; fw-lighter; blockquote-footer; border-0"),
         class = "border-0")
  ,
  citation = card(class = "border-0",
                  card_body(div(h2("How to cite EnoLEX")),
                            div(p("Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Pramartha, Erik Zoebel, Charlotte Hemmings, I Wayan Arka, Mary Dalrymple (2024).", em("EnoLEX: A Diachronic Lexical Database for the Enggano Language."), "Available online at", a("https://enggano.shinyapps.io/enolex/", href='https://enggano.shinyapps.io/enolex/', target='_blank')), style="font-size: 0.9em"),
                            div(p("Rajeg, Gede Primahadi Wijaya, Daniel Krauße, and Cokorda Rai Adi Pramartha (2024).", a("EnoLEX: A Diachronic Lexical Database for the Enggano language", href='https://enggano.ling-phil.ox.ac.uk/static/papers/EnoLEX%20-%20A%20Diachronic%20Lexical%20Database%20for%20the%20Enggano%20language%20[Preprint].pdf', target="_blank"), ". In", em("Proceedings of AsiaLex 2024 (The Asian Association for Lexicography 2024 Hybrid Conference)."), "Toyo University, Tokyo: Japan."), style="font-size: 0.9em")
                  )),
  enolex_description = card(card_body(h1(strong("EnoLEX: A diachronic lexical database for the Enggano language")),
                                      tags$figure(img(src = "file-oxweb-logo.gif", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"), 
                                                  img(src = "file-lingphil.png", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"),
                                                  img(src = "file-ahrc.png", align = "left", width = 280, style = "margin-right: 5px; margin-top: 10px", display = "inline-block")),
                                      tags$figcaption(em(a("This research", href="https://enggano.ling-phil.ox.ac.uk", target="_blank"), "is funded by the Arts and Humanities Research Council (AHRC) Grant ID ", a("AH/S011064/1", href="https://gtr.ukri.org/projects?ref=AH%2FS011064%2F1", target="_blank"), " and ", a("AH/W007290/1", href="https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1", target="_blank"), ".")),
                                      h2("Overview"),
                                      p("EnoLEX collates lexical data from", actionLink("SourcesTabLink", "legacy materials and contemporary fieldwork data"), "about the Enggano language, ranging from simple/short and extensive word lists, anthropological and ethnographic writings, a dictionary, thesis, and contemporary Enggano data. The materials span over 150 years from the middle of the 19th century up to the present. With expert cognate-judgement, EnoLEX offers historical development of word forms expressing a certain concept/meaning."),
                                      
                                      h2("How to get started"),
                                      p("The first option is that users can go to the", actionLink("CognatesTabLink", "Search"), "tab and then, from the left-hand side sidebar, select the concept to filter forms expressing that concept and how they develop across periods."),
                                      p("The second option is a gloabal search by entering any search term (e.g., Indonesian translation, Enggano form, English, etc.) in the search field below. Then, the app will filter from the database any observation whose columns contain the typed value."),
                                      # tags$input(type = "search", id = "site_search", name = "q", placeholder = "Type and Enter"),
                                      tags$script(js_enter_key),
                                      textInput(inputId = "site_search", label = "Search", placeholder = "Type and Enter"),
                                      
                                      h2("Licensing"),
                                      HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://enggano.shinyapps.io/enolex/"><em>EnoLEX</em></a> edited by <span property="cc:attributionName">Daniel Krauße, Gede Primahadi W. Rajeg, Cokorda Pramartha, Erik Zoebel, Charlotte Hemmings, I Wayan Arka, and Mary Dalrymple</span> is licensed under <a href="https://creativecommons.org/licenses/by-nc/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative Commons Attribution-NonCommercial 4.0 International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""></a></p>')
                                      ),
    
                            textOutput("overview")
  ) #,
  #cognate_output <- card(
  #  full_screen = T,
  #  card_header("Cognate groups"),
  #  layout_sidebar(sidebar = english_gloss,
  #                 dataTableOutput("cognatesOut"))
  #)
)

## COGNATES page ====
cognate_cards <- list(
  cognate_table = card(
    layout_sidebar(sidebar = sidebar(english_gloss,
                                     open = list(mobile = "always-above"), 
                                     width = 200),
                  div(DT::DTOutput(outputId = "cognatesOut"), 
                      style = "font-size: 96%")
                   ),
    fill = TRUE,
    height = 700),
  concept_card = card(
    uiOutput(outputId = "ConceptEnglishIndonesian"),
    min_height = 100,
    fill = TRUE,
    id = "ConceptTranslation",
    class = "bg-secondary"
  ),
  reconstruction = card(
    card_title("Reconstruction info"),
    uiOutput(outputId = "PMP_PAN_reconstruction"),
    min_height = 100,
    fill = TRUE,
    id = "PMP_PAN_card",
    class = "bg-light"
  )

)

ui <- page_navbar(
  id = "tabs",
  fillable = TRUE,
  # footer = "<div>",
  theme = bs_theme(
    version = bslib::version_default(),
    bootswatch = "cosmo",
    bg = "#f9f8f5", 
    fg = "#002147", 
    primary = "#193658",
    # secondary="#003947",
    secondary = "#E4F0EF",
    "link-hover-color" = "#be0f34",
    "link-color" = "#3277ae"
    # source from Oxford colour parameters: https://www.ox.ac.uk/public-affairs/style-guide/digital-style-guide
  ),
  # collapsible = TRUE,
  underline = TRUE,
  nav_panel(title = "Main",
            
            layout_columns(
            
              cards[["enolex_description"]],
              
              layout_columns(
                
                cards[["citation"]],
                cards[["background_image"]],
                col_widths = c(12, 12)
                
              )
              
            )
  ),
  nav_panel(title = "Search",
            
            layout_columns(
              
              layout_columns(
                
                cognate_cards[["concept_card"]],
                cognate_cards[["reconstruction"]],
                col_widths = c(5, 7)
                
              ),
              
              cognate_cards[["cognate_table"]],
              col_widths = c(12, 12)
            )
            
  ),
  nav_panel(title = "Sources",
            div(DT::DTOutput(outputId = "enolex_materials"), 
                style = "font-size: 96%")
            # dataTableOutput("enolex_materials")
            ),
  nav_menu(title = "Links",
           nav_item(link_enolex_github),
           nav_item(link_enggano_web)),
  nav_panel_hidden(value = "Global Search Results",
                     div(DT::DTOutput(outputId = "global_search_table"), 
                         style = "font-size: 96%"))
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, inputId = "English_Gloss", choices = sem_choices_eng, server = TRUE)
  
  updateSelectizeInput(session, inputId = "References", choices = bib_choices, server = TRUE)
  
  ### reactive output for Global Search =====
  globalsearch_tb <- eventReactive(input[["keyPressed"]],
                                
                                {
                                  search_pattern <- str_c("\\b", input$site_search, "\\b", sep = "")
                                  tb <- elx |> 
                                    filter(if_any(where(is.character), ~str_detect(., search_pattern))) |> 
                                    select(Cognate_ID, Year, Sources, Original_Form, Standardised_Orthography = Orthography,
                                           Phonemic_Transcription = IPA)
                                    # select(where(function(x) any(grepl(search_pattern, x, perl = TRUE))))
                                    
                                    cog_id_colouring <- unique(tb$Cognate_ID)
                                  
                                  DT::datatable(tb,
                                                escape = FALSE,
                                                selection = "single",
                                                options = list(paging = FALSE,
                                                               scrollY = "500px",
                                                               scrollX = TRUE,
                                                               autoWidth = TRUE,
                                                               columnDefs = list(list(className = "dt-center",
                                                                                      targets = c(1, 2)),
                                                                                 list(width = "50px",
                                                                                      targets = "Cognate_ID"),
                                                                                 list(width = "50px",
                                                                                      targets = "Year"))),
                                                filter = "top",
                                                style = "bootstrap4",
                                                class = list(stripe = FALSE)) |>
                                    formatStyle("Cognate_ID",
                                                backgroundColor = styleEqual(cog_id_colouring,
                                                                             colour_values(factor(cog_id_colouring),
                                                                                           palette = "rdylbu",
                                                                                           alpha = 65)))
                                  
                                  
                                }
                                
                                )
  observeEvent(globalsearch_tb(),
               {
                 req(nrow(globalsearch_tb()$x$data) > 0)
                 output$global_search_table <- DT::renderDT(globalsearch_tb())
                 nav_select("tabs", "Global Search Results")
                 updateTextInput(session = session, inputId = "site_search", value = "")
              
                 # output$table_out <- renderTable(subset(df, grepl(input$searchbar, Species)))
                 # updateNavbarPage(session = session, inputId = "my_panel",
                 #                  selected = "data")
                 
               })
  
  ### reactive output for COGNATE Table ====
  notes <- reactive(
    
    {
      
      if (input$English_Gloss != "(none)") {
        
        tb_note <- elx |> 
          filter(English %in% input$English_Gloss) |> 
          select(Cognate_ID,
                 Year,
                 Sources,
                 Original_Form,
                 English_Original,
                 Note_for_Cognate,
                 Note_for_Year) |> 
          # filter(if_any(matches("English_Original|^Note_"), ~!is.na(.))) |> 
          mutate(across(where(is.character), ~replace_na(., "-"))) |> 
          mutate(across(matches("^Note_for"), ~str_replace_all(., 
                                                               "(?<!\\s)'",
                                                               "’"))) |> 
          mutate(across(matches("^Note_for"), ~str_replace_all(., 
                                                               "(?<=\\s)'",
                                                               "‘"))) |> 
          mutate(across(matches("^Note_for"), ~str_replace_all(., 
                                                               "(?<!\\s)\"",
                                                               "”"))) |> 
          mutate(across(matches("^Note_for"), ~str_replace_all(., 
                                                               "(?<=\\s)\"",
                                                               "“"))) |> 
          mutate(across(matches("Note_for"), ~str_replace_all(.,
                                                              "__",
                                                              " ; "))) |> 
          mutate(across(matches("Note_for"), ~str_replace_all(.,
                                                              "\\s{2,}",
                                                              " "))) |> 
          mutate(English_Original = if_else(English_Original != "-",
                                            str_c("<p><strong>Original gloss</strong>: ‘", English_Original, "’</p>", sep = ""),
                                            str_c("<p><strong>Original gloss</strong>: ", English_Original, "</p>", sep = ""))) |> 
          mutate(Note_for_Cognate = str_c("<p>Note for <strong>Cognate ID ", Cognate_ID, "</strong>: ", Note_for_Cognate, "</p>", sep = ""),
                 Note_for_Year = str_c("<p>Note for <strong>", Sources, "</strong>: ", Note_for_Year, "</p>", sep = "")) |> 
          mutate(notes_all = str_c(English_Original, Note_for_Cognate, Note_for_Year, sep = " ")) |> 
          select(notes_all)
        
      } else {
        
        
        
      }
      
    }
    
  )
  
  shinyInput <- function(FUN, len, rows_to_add, id, ...) {
    
    inputs <- character(len)
    
    for (i in seq_len(len)) {
      
      if (i %in% rows_to_add) {
        
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
        
      } else {
        
        next
        
      }
      
      
    }
    
    inputs
  }
  
  english_selected <- reactive(
    
    { if (input$English_Gloss != "(none)") {
      
      tb <- elx |> 
        filter(English %in% input$English_Gloss) |> 
        select(Cognate_ID, Year, Sources, Original_Form, Standardised_Orthography = Orthography,
               Phonemic_Transcription = IPA) # |> 
        # select(where(~!all(is.na(.))))
      
      for_checking_notes <- elx |>
        filter(English %in% input$English_Gloss) |>
        select(English_Original, Note_for_Year, Note_for_Cognate)
      
      for_checking_vector <- c(any(!is.na(for_checking_notes$English_Original)),
                               any(!is.na(for_checking_notes$Note_for_Year)),
                               any(!is.na(for_checking_notes$Note_for_Cognate)))
      
      rows_to_add <- which(!is.na(for_checking_notes$English_Original) | 
                             !is.na(for_checking_notes$Note_for_Year) | 
                             !is.na(for_checking_notes$Note_for_Cognate))
      
      if (any(length(rows_to_add) > 0)) {
        
        tb <- tb |> 
          mutate(Details = shinyInput(actionButton, nrow(tb), rows_to_add = rows_to_add, 'button_', label = "More",
                                      onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'))
        
      }
      
      
      cog_id_colouring <- unique(tb$Cognate_ID)
      
      DT::datatable(tb,
                    escape = FALSE,
                    selection = "single",
                    options = list(paging = FALSE,
                                   scrollY = "500px",
                                   scrollX = TRUE,
                                   autoWidth = TRUE,
                                   columnDefs = list(list(className = "dt-center",
                                                          targets = c(1, 2)),
                                                     list(width = "50px",
                                                          targets = "Cognate_ID"),
                                                     list(width = "50px",
                                                          targets = "Year"))),
                    filter = "top",
                    style = "bootstrap4",
                    class = list(stripe = FALSE)) |>
        formatStyle("Cognate_ID",
                    backgroundColor = styleEqual(cog_id_colouring,
                                                 colour_values(factor(cog_id_colouring),
                                                               palette = "rdylbu",
                                                               alpha = 65)))
      
    } else {
      
      elx |> 
        filter(English %in% "sadsakdasklaskcmasl") |> 
        DT::datatable()
      
    }
      
    }
  )
  
  output$cognatesOut <- renderDT(
    {
      req(input$English_Gloss != "(none)" & !is.null(input$English_Gloss) & input$English_Gloss != "")
      english_selected()
    }
  )
  
  observeEvent(input$select_button, {
    
    rownum <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    notes_to_show <- HTML(pull(notes()[rownum, ], notes_all))
    
    if (is.null(rownum) || rownum == '') {} else{
      
      showModal(modalDialog(
        notes_to_show,
        title = paste0("Note(s) for row: ", rownum),
        size = "m",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  materials_table <- reactive(
    {
      DT::datatable(bibs1,
                    escape = FALSE,
                    selection = "single",
                    # rownames = FALSE,
                    options = list(paging = FALSE,
                                   scrollY = "500px",
                                   scrollX = TRUE,
                                   # autoWidth = TRUE,
                                   columnDefs = list(list(className = "dt-center",
                                                          targets = c(1, 3)),
                                                     list(width = "50px",
                                                          targets = "Year"),
                                                     list(width = "60px",
                                                          targets = "Form_Count"),
                                                     list(width = "170px",
                                                          targets = "Sources"))),
                    filter = "top",
                    style = "bootstrap4",
                    class = list(stripe = FALSE))
    }
  )
  
  output$enolex_materials <- renderDT(materials_table())
  
  
  ### reactive output for CONCEPT Translation ====
  concept_idn_translation <- reactive(
    
    {
      
      eng <- str_to_sentence(unique(pull(filter(elx, English %in% input$English_Gloss), English)))
      
      if (length(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))) > 1) {
        
        # italics Indonesian gloss
        # idn <- str_c("‘<em>", str_c(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "</em>’", sep = "")
        
        # non-italics
        idn <- str_c("‘", str_c(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "’", sep = "")
        
        idn_orth <- elx |> 
          filter(English %in% input$English_Gloss) |> 
          select(Sources, Indonesian, Standardised_Orthography = Orthography) |> 
          distinct()
        
        idn_notes <- idn_orth |> 
          group_by(Indonesian, Sources) |> 
          mutate(forms = str_c("<strong>", Standardised_Orthography = Orthography, "</strong>", sep = "")) |> 
          mutate(forms = str_c(forms, collapse = ", ")) |> 
          group_by(Sources, Indonesian) |> 
          mutate(forms = str_c(forms, " (", Sources, ")", sep = "")) |> 
          ungroup() |> 
          select(forms, Indonesian) |> 
          distinct() |> 
          group_by(Indonesian) |> 
          mutate(forms = str_c(forms, collapse = "; ")) |> 
          mutate(to_print = str_c("‘", Indonesian, "’ is the gloss for: ", forms, sep = "")) |> 
          ungroup() |> 
          pull(to_print) |> 
          unique()
        
        idn_notes <- str_c(str_c("<p>", idn_notes, "</p>", sep =""), collapse = "")
        
      } else {
        
        # italics Indonesian gloss
        # idn <- str_c("‘<em>", str_c(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "</em>’", sep = "")
        
        # non-italics
        idn <- str_c("‘", str_c(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "’", sep = "")
        
      }
      
      concepticon <- unique(pull(filter(elx, English %in% input$English_Gloss), Concepticon_Gloss))
      
      if (!is.na(concepticon) & length(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))) == 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3><p>Corresponding concept set in Concepticon: ", concepticon, "</p>", sep = ""))  
        
      } else if (!is.na(concepticon) & length(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))) > 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3><p>Corresponding concept set in Concepticon: ", concepticon, "</p><p>", idn_notes, "</p>", sep = ""))
        
      } else if (is.na(concepticon) & length(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))) == 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3>", sep = ""))
        
      } else if (is.na(concepticon) & length(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))) > 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3>", "<p>", idn_notes, "</p>", sep = ""))
      }
      
    }
    
  )
  
  output$ConceptEnglishIndonesian <- renderUI(
    
    {
      
      if(all(req(input$English_Gloss) != "(none)" & !is.null(req(input$English_Gloss))))
      renderUI(concept_idn_translation())
      
    }
    
  )
  
  ### reactive output for PROTO-RECONSTRUCTION ====
  proto_reconstruction <- reactive(
    
    {
      
      if(all(req(input$English_Gloss) != "(none)" & !is.null(req(input$English_Gloss)))) {
        
        
        recx <- elx |> 
          filter(English %in% input$English_Gloss) |> 
          select(Cognate_ID, matches("PMP|PAN|Etymology_Source")) |> 
          filter(if_any(matches("PMP|PAN"), ~!is.na(.))) |> 
          distinct() |> 
          mutate(across(matches("PAN|PMP|Etymology"), ~replace_na(., "-"))) |> 
          mutate(across(matches("PAN|PMP|Etymology"), ~replace(., . %in% c("?", "??", "???"), "?"))) |> 
          mutate(reconstr = str_c("<p><code>Cognate_ID</code> ", 
                                  Cognate_ID, 
                                  " - <strong>PAN</strong>: ", 
                                  PAN_Etymon, 
                                  " ‘", 
                                  PAN_English, 
                                  "’ (Source: ", PAN_Source, ")", 
                                  "; <strong>PMP</strong>: ", 
                                  PMP_Etymon, 
                                  " ‘", 
                                  PMP_English, 
                                  "’ (Source: ", 
                                  PMP_Source, 
                                  ") (<strong>Etymology Source</strong>: ",
                                  Etymology_Source,
                                  ")</p>",
                                  sep = "")
                 ) |> 
          mutate(reconstr = str_c(reconstr, collapse = " ")) |> 
          pull(reconstr) |> 
          unique()
        
        recx <- HTML(recx)
        
      }
        
      
    }
    
  )
  
  output$PMP_PAN_reconstruction <- renderUI(
    
    {
      
      if(all(req(input$English_Gloss) != "(none)" & !is.null(req(input$English_Gloss)) & req(proto_reconstruction() != "") & !is.na(proto_reconstruction())))
      renderUI(proto_reconstruction())
      
    }
    
  )
  
  # the following code run the clicking on Search hyperlink the main panel/page
  observeEvent(input$CognatesTabLink, {
    updateTabsetPanel(session = session, "tabs", "Search")
  })
  
  # the following code run the clicking on Sources hyperlink the main panel/page
  observeEvent(input$SourcesTabLink, {
    updateTabsetPanel(session = session, "tabs", "Sources")
  })
  
}

shinyApp(ui, server)