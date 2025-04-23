# EnoLEX programmatic R codes for the Shiny app available online at https://enggano.shinyapps.io/enolex/
# Gede Primahadi W. Rajeg (2024) (CC-BY-NC-SA 4.0 International)
# University of Oxford & Udayana University

library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(colourvalues)
library(RSQLite)
library(DBI)
library(glue)
# library(dbplyr)

## IMPORTANT:
### regularly check, update, and re-run (when there is an update) codes in `r-code_07-enolex-app-data-preparation`
### before running this app.R since pre-processing and preparation of data
### were done in that `...07...` script.

# Data Preparation =====

## Connect to the EnoLEX sqlite ====
enolex_db <- dbConnect(SQLite(), "enolex.sqlite")
enolex <- tbl(enolex_db, "enolex")
enolex_glb <- tbl(enolex_db, "enolex_glb") |> 
  rename(Concepticon = Concepticon_Gloss)
RSQLite::initRegExp(enolex_db)
RSQLite::initExtension(enolex_db, "regexp")

## to be used in the global search so that the searching
## does not include url
reconst_concept_df <- enolex |> 
  select(ID, PAN_Etymon, PMP_Etymon, Etymology_Source, 
         Concepticon = Concepticon_Gloss)

## Read in the main data for Sources ====
### from output data generated in script `r-code_07-...`
# bibs <- read_rds("sources.rds")

## Read in the main data for EnoLEX ====
### from output data generated in script `r-code_07-...`
# enolex <- read_rds("enolex.rds")

## Dialect by sources ======
### from output data generated in script `r-code_07-...`
# dialect_info <- read_rds("dialect_info.rds")

## Count the forms by sources =====
# form_count <- enolex |> 
#   select(Sources, Original_Form) |> 
#   group_by(Sources) |> 
#   summarise(Count_of_Original_Form = n_distinct(Original_Form))

## Prepare the choice for the English concept =====
enolex_eng <- tbl(enolex_db, "enolex") |> 
  pull(English) |> 
  unique() |> 
  sort(decreasing = FALSE)
# enolex_eng <- sort(unique(enolex$English), decreasing = FALSE)
sem_choices_eng <- c("(none)", enolex_eng)

## Prepare the choice for the Sources =====
# bib_choices <- c("(none)", bibs$Sources)

english_gloss <- selectizeInput(inputId = "English_Gloss", 
                                options = list(dropdownParent = "body",
                                               closeAfterSelect = TRUE),
                                label = "Concepts",
                                choices = NULL, 
                                selected = "(none)"
)

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

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

# one_search_all <- textInput("searchbar", label = "Search", 
#                             placeholder = "Type & press Enter")

link_enolex_github <- tags$a(shiny::icon("github"), "EnoLEX app source", 
                             href="https://github.com/engganolang/enolex", 
                             target="_blank")

link_enggano_web <- tags$a(shiny::icon("globe", lib = "glyphicon"), 
                           "Enggano webpage", 
                           href="https://enggano.ling-phil.ox.ac.uk/", 
                           target="_blank")

link_contemporary_enggano <- tags$a(shiny::icon("globe", lib = "glyphicon"), 
                                    "Contemporary Enggano Dictionary",
                                    href="https://portal.sds.ox.ac.uk/projects/Contemporary_Enggano_Dictionary/238013",
                                    target="_blank")

link_kahler <- tags$a(shiny::icon("globe", lib = "glyphicon"), 
                      "Digitised Enggano-German dictionary",
                      href="https://portal.sds.ox.ac.uk/projects/Retro-digitisation_of_the_Enggano-German_Dictionary/237998",
                      target="_blank")

link_holle_list <- tags$a(shiny::icon("globe", lib = "glyphicon"), 
                          "Holle List (the New Basic List)",
                          href="https://doi.org/10.25446/oxford.23205173.v6",
                          target="_blank")

# bibs <- selectizeInput(inputId = "References",
#                        label = "Sources",
#                        choices = NULL, 
#                        selected = NULL)

# Cards =====
## MAIN page ====
cards <- list(
  background_image = 
    card(card_image("estuary_new.JPG", 
                    # height = "330px",
                    border_radius = "none"),
         card_body(fill = FALSE,
                   p("The estuary towards the Indian Ocean from the Bak Blau lake, Enggano Island",
                     class = "fw-light text-muted")),
         class = "border-0")
  ,
  citation = card(#class = "border-0",
    min_height = "275px",
    card_body(div(h2("How to cite EnoLEX")),
              div(p("Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Pramartha, Erik Zobel, Bernd Nothofer, Charlotte Hemmings, Sarah Ogilvie, I Wayan Arka, Mary Dalrymple (2024).", em("EnoLEX: A Diachronic Lexical Database for the Enggano Language."), "Online resource. University of Oxford.", a("http://dx.doi.org/10.25446/oxford.28282946.v1", href = "http://dx.doi.org/10.25446/oxford.28282946.v1", target = "_blank"), ". Available online at", a("https://enggano.shinyapps.io/enolex/", href = 'https://enggano.shinyapps.io/enolex/', target = '_blank')), style="font-size: 0.9em"),
              div(p("Rajeg, Gede Primahadi Wijaya, Daniel Krauße, Cokorda Pramartha, Erik Zobel, Bernd Nothofer, Charlotte Hemmings, Sarah Ogilvie, I Wayan Arka, Mary Dalrymple (2024).", em("R codes and curated dataset for “EnoLEX: A Diachronic Lexical Database for the Enggano Language”."), "(Version 0.0.1) [Computer software]. Available at", a('https://github.com/engganolang/enolex', href = 'https://github.com/engganolang/enolex', target = '_blank')), style="font-size: 0.9em"),
              div(p("Rajeg, Gede Primahadi Wijaya, Daniel Krauße, and Cokorda Rai Adi Pramartha (2024).", a("EnoLEX: A Diachronic Lexical Database for the Enggano language", href='https://doi.org/10.25446/oxford.27013864.v1', target="_blank"), ". In", em("Proceedings of AsiaLex 2024 (The Asian Association for Lexicography 2024 Hybrid Conference)."), "Toyo University, Tokyo: Japan."), style="font-size: 0.9em")
    )),
  enolex_description = card(card_body(h1(strong("EnoLEX: A diachronic lexical database for the Enggano language")),
                                      tags$figure(img(src = "file-oxweb-logo.gif", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"), 
                                                  img(src = "file-lingphil.png", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"),
                                                  img(src = "file-ahrc.png", align = "left", width = 280, style = "margin-right: 5px; margin-top: 10px", display = "inline-block")),
                                      tags$figcaption(em("The", a("Enggano research", href="https://enggano.ling-phil.ox.ac.uk", target="_blank"), "is funded by the Arts and Humanities Research Council (AHRC) Grant IDs ", a("AH/S011064/1", href="https://gtr.ukri.org/projects?ref=AH%2FS011064%2F1", target="_blank"), " and ", a("AH/W007290/1", href="https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1", target="_blank"), ".")),
                                      h2("Overview"),
                                      p("EnoLEX collates lexical data from", actionLink("SourcesTabLink", HTML("<strong>legacy materials and contemporary fieldwork data</strong>")), "about the Enggano language, ranging from simple/short and extensive word lists, anthropological and ethnographic writings, a dictionary, thesis, and contemporary Enggano data. The materials span over 150 years from the middle of the 19th century up to the present. With expert cognate-judgement, EnoLEX offers historical development of word forms expressing a certain concept/meaning."),
                                      
                                      h2("How to get started"),
                                      p("The first option is the", actionLink("CognatesTabLink", HTML("<strong>Concept Search</strong>")), "tab and then, from the left-hand side sidebar, select the concept to filter forms expressing that concept and how they develop across periods."),
                                      p("The second option is the", actionLink("GlobalSearch", HTML("<strong>Global Search</strong>")), "tab to entering any search term (e.g., Indonesian translation, Enggano form, English, etc.) in the search box there. Then, the app will filter from the database any observation whose columns contain the typed value."),
                                      p("The third option is browsing individual word list per author/source. This can be done via clicking an author's name under the", HTML("<code>Sources</code>"), "column within the (i)", actionLink("SourcesTabLink1", HTML("<strong>Sources</strong>")), "tab and (ii) the", actionLink("CognatesTabLink1", HTML("<strong>Concept Search</strong>")), "tab."),
                                      # tags$input(type = "search", id = "site_search", name = "q", placeholder = "Type and Enter"),
                                      # tags$script(js_enter_key),
                                      # tags$script(jscode),
                                      # textInput(inputId = "site_search", label = "Search", placeholder = "Type and Enter"),
                                      
                                      # fluidRow(
                                      #   column(11, tagAppendAttributes(textInput("site_search", label = " ", width = "60%", placeholder = "Type"),
                                      #                                  `data-proxy-click` = "btn")),
                                      #   column(1, div( style = "margin-top: 24px; margin-left: -210px", actionButton("btn", "Search")))),
                                      
                                      h2("Licensing"),
                                      HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://enggano.shinyapps.io/enolex/">EnoLEX</a> online database edited by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://github.com/engganolang/enolex">Daniel Krauße, Gede Primahadi W. Rajeg, Cokorda Pramartha, Erik Zobel, Bernd Nothofer, Charlotte Hemmings, Sarah Ogilvie, I Wayan Arka, and Mary Dalrymple</a> is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a></p>')
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
  
  ## 1 - Cognate table output
  cognate_table = card(id = "cognate_table",
                       
                       ### 1.1 - concept selection
                       layout_sidebar(sidebar = sidebar(english_gloss,
                                                        open = list(mobile = "always-above"), 
                                                        width = 200),
                                      
                                      ### 1.2 - table panel
                                      div(DT::DTOutput(outputId = "cognatesOut"), 
                                          style = "font-size: 96%")
                       ),
                       fill = TRUE,
                       height = 700),
  
  ## 2 - Concept(icon) card (top left)
  concept_card = card(
    uiOutput(outputId = "ConceptEnglishIndonesian"),
    min_height = 100,
    fill = TRUE,
    id = "ConceptTranslation",
    class = "bg-secondary"
  ),
  
  ## 3 - Reconstruction info card (top right)
  reconstruction = card(
    card_title("Reconstruction info"),
    uiOutput(outputId = "PMP_PAN_reconstruction"),
    min_height = 100,
    fill = TRUE,
    id = "PMP_PAN_card",
    class = "bg-light"
  )
  
)

## GLOBAL search page ====
full_db_page <- list(
  db_table = card(
    layout_sidebar(
      sidebar = sidebar(textInput("global_search", 
                                  label = "Database search", width = 150),
                        radioButtons(inputId = "pattern_matching_options",
                                     label = "Search using:",
                                     choiceNames = list(
                                       HTML("<a href='https://www.regular-expressions.info/' target='_blank'>Regular Expression</a>"),
                                       "Exact Match",
                                       "Partial Match"
                                     ),
                                     choiceValues = list(
                                       "regex",
                                       "exact_match",
                                       "partial_match"
                                     ),
                                     selected = "exact_match"),
                        open = list(mobile = "always-above"),
                        width = 200),
      div(DT::DTOutput(outputId = "global_search_output"),
          style = "font-size: 95%")
    )
  )
)

# UI configuration =====
ui <- function(request) {
  page_navbar(
    
    id = "tabs",
    fillable = FALSE,
    # title = "EnoLEX",
    window_title = "EnoLEX",
    # footer = "<div>",
    theme = bs_theme(
      version = bslib::version_default(),
      bootswatch = "cosmo",
      base_font = font_google("Noto Serif", local = FALSE),
      heading_font = font_link(family = "Noto Serif", href = "https://fonts.googleapis.com/css2?family=Noto+Serif:ital,wght@0,900;1,900&display=swap"),
      font_scale = .90,
      bg = "#f9f8f5", 
      fg = "#002147", 
      primary = "#193658",
      # secondary="#003947",
      secondary = "#E4F0EF",
      "link-hover-color" = "#be0f34",
      "link-color" = "#3277ae"
      # source from Oxford colour parameters: https://communications.web.ox.ac.uk/communications-resources/visual-identity/identity-guidelines/colours
    ),
    # collapsible = TRUE,
    underline = TRUE,
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "ox_brand1_rev.png")),
    nav_panel(title = "Home",
              
              layout_columns(
                
                cards[["enolex_description"]],
                
                layout_columns(
                  
                  cards[["citation"]],
                  cards[["background_image"]],
                  col_widths = c(12, 12)
                  
                )
                
              )
    ),
    nav_panel(title = "Concept Search",
              
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
    nav_panel(title = "Global Search",
              layout_columns(
                full_db_page[["db_table"]]
              )),
    nav_panel(title = "Sources",
              div(DT::DTOutput(outputId = "enolex_materials"), 
                  style = "font-size: 96%")
              # dataTableOutput("enolex_materials")
    ),
    nav_menu(title = "Links",
             nav_item(link_enggano_web),
             nav_item(link_enolex_github),
             nav_item(link_contemporary_enggano),
             nav_item(link_kahler),
             nav_item(link_holle_list)) #,
    # nav_panel(value = "search_panel",
    #           textInput("global_search",
    #                     label = NULL,
    #                     value = "Search"))
  )

}
# ui <- page_navbar(
    


# SERVER configuration =====
server <- function(input, output, session) {
  
  
  setBookmarkExclude(names = c("pattern_matching_options"))
  updateSelectizeInput(session, inputId = "English_Gloss", choices = sem_choices_eng, server = TRUE)
  
  # updateSelectizeInput(session, inputId = "References", choices = bib_choices, server = TRUE)
  
  # input_english_gloss <- reactive({
  #   input$English_Gloss
  # })
  
  ### reactive output for COGNATE Table ====
  
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
  
  notes <- reactive({

    req(input$English_Gloss != "(none)")
    
    enolex |>
      filter(glue::glue_sql(str_c("English = '",
                                  !!input$English_Gloss,
                                  "'",
                                  sep = ""))) |>
      select(Cognate_ID,
             Year,
             Sources,
             Original_Form, 
             Standardised_Orthography = Orthography,
             Phonemic_Transcription = IPA,
             English_Original,
             Note_for_Cognate,
             Note_for_Year) |>
      # filter(if_any(matches("English_Original|^Note_"), ~!is.na(.))) |>
      mutate(across(matches("(English_Original|Note_for_Cognate|Note_for_Year)"),
                    \(x) if_else(is.na(x), "-", x))) |>
      distinct() |> 
      collect() |>
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
  }

  )
  
  english_selected <- reactive(
    
    {
      req(input$English_Gloss != "(none)")
      
      tb <- enolex |>
        filter(glue::glue_sql(str_c("English = '", 
                                    !!input$English_Gloss, 
                                    "'", 
                                    sep = ""))) |>
        select(Cognate_ID, 
               Year, 
               Sources, 
               Original_Form, 
               Standardised_Orthography = Orthography,
               Phonemic_Transcription = IPA,
               English_Original, 
               Note_for_Year, 
               Note_for_Cognate) |>
        distinct() |> 
        collect()
      
      for_checking_vector <- c(any(!is.na(tb$English_Original)),
                               any(!is.na(tb$Note_for_Year)),
                               any(!is.na(tb$Note_for_Cognate)))
      
      rows_to_add <- which(!is.na(tb$English_Original) |
                             !is.na(tb$Note_for_Year) |
                             !is.na(tb$Note_for_Cognate))
      
      if (any(length(rows_to_add) > 0)) {
        
        tb <- tb |>
          mutate(Details = shinyInput(actionButton, 
                                      nrow(tb), 
                                      rows_to_add = rows_to_add, 
                                      'button_', 
                                      label = "More",
                                      onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})')) |> 
          relocate(Details, 
                   .before = Year)
        
      }
      
      cog_id_colouring <- unique(tb$Cognate_ID)
      
      tb_out_concept <- tb |> 
        select(-English_Original,
               -Note_for_Year,
               -Note_for_Cognate)
      rm(tb)
      selectable_column <- which(colnames(tb_out_concept) %in% 
                                   c("Sources", "Details"))
      tb_out_concept |> 
        DT::datatable(escape = FALSE,
                      selection = "none",
                      options = list(paging = FALSE,
                                     scrollY = "500px",
                                     scrollX = TRUE,
                                     scrollCollapse = TRUE,
                                     # autoWidth = TRUE,
                                     language = list(searchPlaceholder = "Search"),
                                     columnDefs = list(list(className = "dt-center",
                                                            targets = c(1, 2)),
                                                       list(width = "50px",
                                                            targets = "Cognate_ID"),
                                                       list(width = "50px",
                                                            targets = "Year"))),
                      filter = "top",
                      # callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search" )'),
                      style = "bootstrap4",
                      class = list(stripe = FALSE)) |>
        formatStyle("Cognate_ID",
                    backgroundColor = styleEqual(cog_id_colouring,
                                                 colour_values(factor(cog_id_colouring),
                                                               palette = "rdylbu",
                                                               alpha = 65))) |> 
        formatStyle("Sources",
                    color = "#3277ae",
                    cursor = "pointer")
      
    }
    
  )
  
  observe({
    
    req(input$English_Gloss != "(none)" & 
          !is.null(input$English_Gloss) & 
          input$English_Gloss != "")
    output$cognatesOut <- renderDT({english_selected()})
    
  })
  
  observeEvent(input$select_button, {
    
    rownum <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    notes_to_show <- HTML(pull(notes()[rownum, ], notes_all))
    
    if (is.null(rownum) || rownum == '') {} else{
      
      showModal(
        modalDialog(
          notes_to_show,
          title = paste0("Note(s) for row: ", rownum),
          size = "m",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
    
  })
  
  global_search <- reactive({
    
    if (input$pattern_matching_options == "regex") {
      
      glb <- enolex_glb |> 
        collect() |> 
        filter(if_any(where(is.character), 
                      ~str_detect(., regex(input$global_search, 
                                           ignore_case = FALSE)))) |> 
        select(where(function(x) any(!is.na(x)))) |> 
        mutate(across(where(is.character), ~gsub(
          paste(c("(", input$global_search, ")"), collapse = ""),
          "<span style='background-color:#D4CDF4;'>\\1</span>",
          .,
          TRUE,
          TRUE
        ))) |> 
        select(!matches("(PAN_Etymon|PMP_Etymon|Etymology_Source|Concepticon(_Gloss)?)"))
      
      reconst_concept_df_filtered <- reconst_concept_df |> 
        filter(ID %in% !!glb$ID) |> 
        collect()
      glb <- glb |> 
        left_join(reconst_concept_df_filtered)
      
      
    } else if (input$pattern_matching_options == "exact_match") {
      
      glb <- enolex_glb |> 
        collect() |> 
        filter(if_any(where(is.character), 
                      ~str_detect(., 
                                  regex(str_c("\\b", 
                                              input$global_search, 
                                              "\\b", 
                                              sep = ""), 
                                        ignore_case = FALSE)))) |> 
        select(where(function(x) any(!is.na(x)))) |> 
        mutate(across(where(is.character), ~gsub(
          paste(c("\\b(", input$global_search, ")\\b"), collapse = ""),
          "<span style='background-color:#65E5AE;'>\\1</span>",
          .,
          TRUE,
          TRUE
        ))) |> 
        select(!matches("(PAN_Etymon|PMP_Etymon|Etymology_Source|Concepticon(_Gloss)?)"))
      
      reconst_concept_df_filtered <- reconst_concept_df |> 
        filter(ID %in% !!glb$ID) |> 
        collect()
      glb <- glb |> 
        left_join(reconst_concept_df_filtered)
      
    } else if (input$pattern_matching_options == "partial_match") {
      
      glb <- enolex_glb |> 
        collect() |> 
        filter(if_any(where(is.character), 
                      ~str_detect(string = ., 
                                  pattern = fixed(input$global_search)))) |> 
        select(where(function(x) any(!is.na(x)))) |> 
        mutate(across(where(is.character), ~gsub(
          paste(c("(", input$global_search, ")"), collapse = ""),
          "<span style='background-color:#F7EF66;'>\\1</span>",
          .,
          TRUE,
          TRUE
        ))) |> 
        select(!matches("(PAN_Etymon|PMP_Etymon|Etymology_Source|Concepticon(_Gloss)?)"))
      
      reconst_concept_df_filtered <- reconst_concept_df |> 
        filter(ID %in% !!glb$ID) |> 
        collect()
      glb <- glb |> 
        left_join(reconst_concept_df_filtered)
      
    }
    
    cog_id_colouring <- unique(glb$Cognate_ID)
    
    glb |> 
      rename(Standardised_Orthography = Orthography) |> 
      # select(-ID, -Semantic_Field) |> 
      datatable(escape = FALSE,
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
    
  })
  
  output$global_search_output <- DT::renderDT({
    req(input$global_search)
    global_search()
  })
  
  materials_table <- reactive(
    {
      tbl(enolex_db, "bibs1") |> 
        collect() |>
        DT::datatable(escape = FALSE,
                      selection = "none",
                      # rownames = FALSE,
                      options = list(paging = FALSE,
                                     scrollCollapse = TRUE,
                                     scrollY = "500px",
                                     scrollX = TRUE,
                                     language = list(searchPlaceholder = "Search"),
                                     #autoWidth = TRUE,
                                     columnDefs = list(list(className = "dt-center",
                                                            targets = c(1:4)),
                                                       list(width = "45px",
                                                            targets = c("Published", "Collected")),
                                                       list(width = "55px",
                                                            targets = "Form_Count"),
                                                       list(width = "80px",
                                                            targets = "Dialect"),
                                                       list(width = "80px",
                                                            targets = "Place"),
                                                       list(width = "170px",
                                                            targets = "Sources"))),
                      filter = "top",
                      # callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search" )'),
                      style = "bootstrap4",
                      class = list(stripe = FALSE)) |> 
        formatStyle("Sources",
                    color = "#3277ae",
                    cursor = "pointer")
    }
  )
  
  output$enolex_materials <- renderDT(materials_table())
  
  
  ### reactive output for CONCEPT Translation ====
  concept_idn_translation <- reactive(
    
    {
      
      eng <- str_to_sentence(unique(pull(filter(enolex, 
                                                glue::glue_sql(str_c("English = '", 
                                                                     !!input$English_Gloss, 
                                                                     "'", 
                                                                     sep = ""))), 
                                         English)))
      
      if (length(unique(pull(filter(enolex, 
                                    glue::glue_sql(str_c("English = '", 
                                                         !!input$English_Gloss, 
                                                         "'", 
                                                         sep = ""))), 
                             Indonesian))) > 1) {
        
        # italics Indonesian gloss
        # idn <- str_c("‘<em>", str_c(unique(pull(filter(enolex, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "</em>’", sep = "")
        
        # non-italics
        idn <- str_c("‘", str_c(unique(pull(filter(enolex, 
                                                   glue::glue_sql(str_c("English = '", 
                                                                        !!input$English_Gloss, 
                                                                        "'", 
                                                                        sep = ""))), 
                                            Indonesian)), 
                                collapse = ", "), 
                     "’", 
                     sep = "")
        
        idn_orth <- enolex |> 
          filter(glue::glue_sql(str_c("English = '", 
                                      !!input$English_Gloss, 
                                      "'", 
                                      sep = ""))) |> 
          select(Sources, Indonesian, Standardised_Orthography = Orthography) |> 
          distinct() |> 
          collect()
        
        idn_notes <- idn_orth |> 
          group_by(Indonesian, Sources) |> 
          mutate(forms = str_c("<strong>", Standardised_Orthography, "</strong>", sep = "")) |> 
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
        
        idn_notes <- str_c(str_c("<p>", idn_notes, "</p>", sep =""), 
                           collapse = "")
        
      } else {
        
        # italics Indonesian gloss
        # idn <- str_c("‘<em>", str_c(unique(pull(filter(enolex, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "</em>’", sep = "")
        
        # non-italics
        idn <- str_c("‘", str_c(unique(pull(filter(enolex, 
                                                   glue::glue_sql(
                                                     str_c("English = '", 
                                                           !!input$English_Gloss, 
                                                           "'", 
                                                           sep = ""))
        ), 
        Indonesian)), 
        collapse = ", "), 
        "’", 
        sep = "")
        
      }
      
      concepticon <- unique(pull(filter(enolex, glue::glue_sql(str_c("English = '", 
                                                                     !!input$English_Gloss, 
                                                                     "'", 
                                                                     sep = ""))
      ), 
      Concepticon_Gloss))
      
      if (!is.na(concepticon) & length(unique(pull(filter(enolex, 
                                                          glue::glue_sql(str_c("English = '", 
                                                                               !!input$English_Gloss, 
                                                                               "'", 
                                                                               sep = ""))), 
                                                   Indonesian))) == 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3><p>Corresponding concept set in Concepticon: ", concepticon, "</p>", sep = ""))  
        
      } else if (!is.na(concepticon) & length(unique(pull(filter(enolex, 
                                                                 glue::glue_sql(
                                                                   str_c("English = '", 
                                                                         !!input$English_Gloss, 
                                                                         "'", 
                                                                         sep = ""))), 
                                                          Indonesian))) > 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3><p>Corresponding concept set in Concepticon: ", concepticon, "</p><p>", idn_notes, "</p>", sep = ""))
        
      } else if (is.na(concepticon) &  length(unique(pull(filter(enolex, 
                                                                 glue::glue_sql(
                                                                   str_c("English = '", 
                                                                         !!input$English_Gloss, 
                                                                         "'", 
                                                                         sep = ""))), 
                                                          Indonesian))) == 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3>", sep = ""))
        
      } else if (is.na(concepticon) & length(unique(pull(filter(enolex, 
                                                                glue::glue_sql(
                                                                  str_c("English = '", 
                                                                        !!input$English_Gloss, 
                                                                        "'", 
                                                                        sep = ""))), 
                                                         Indonesian))) > 1) {
        
        HTML("<h3>", str_c(eng, " ", idn, "</h3>", "<p>", idn_notes, "</p>", sep = ""))
      }
      
    }
    
  )
  
  output$ConceptEnglishIndonesian <- renderUI(
    
    {
      
      if(all(req(input$English_Gloss) != "(none)" & 
            !is.null(req(input$English_Gloss))
      )
      )
        renderUI(concept_idn_translation())
      
    }
    
  )
  
  ### reactive output for PROTO-RECONSTRUCTION ====
  proto_reconstruction <- reactive(
    
    {
      
      if(all(req(input$English_Gloss) != "(none)" & !is.null(req(input$English_Gloss)))) {
        
        
        recx <- enolex |> 
          filter(glue::glue_sql(str_c("English = '", 
                                      !!input$English_Gloss, 
                                      "'", 
                                      sep = ""))) |> 
          select(Cognate_ID, matches("PMP|PAN|Etymology_Source")) |> 
          filter(if_any(matches("PMP|PAN"), ~!is.na(.))) |> 
          distinct() |> 
          collect() |> 
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
      
      if(all(req(input$English_Gloss) != "(none)" & 
             !is.null(req(input$English_Gloss)) & 
             req(proto_reconstruction() != "") & 
             !is.na(proto_reconstruction())))
        renderUI(proto_reconstruction())
      
    }
    
  )
  
  # the following code run the clicking on Search hyperlink the main panel/page
  observeEvent(input$CognatesTabLink, {
    updateTabsetPanel(session = session, inputId = "tabs", 
                      selected = "Concept Search")
  })
  
  observeEvent(input$CognatesTabLink1, {
    updateTabsetPanel(session = session, inputId = "tabs", 
                      selected = "Concept Search")
  })
  
  observeEvent(input$GlobalSearch, {
    updateTabsetPanel(session = session, inputId = "tabs", 
                      selected = "Global Search")
  })
  
  # the following code run the clicking on Sources hyperlink the main panel/page
  observeEvent(input$SourcesTabLink, {
    updateTabsetPanel(session = session, inputId = "tabs", 
                      selected = "Sources")
  })
  
  # the following code run the clicking on Sources hyperlink the main panel/page
  observeEvent(input$SourcesTabLink1, {
    updateTabsetPanel(session = session, inputId = "tabs", 
                      selected = "Sources")
  })
  
  # observer to close individual by source word list
  ## inspiration: https://stackoverflow.com/a/58593130
  observeEvent(input$tabs, {
    if (input$tabs != "Word List by Source") {
      nav_remove("tabs", 
                 target = "Word List by Source")
    }
  })
  
  # observer in the Sources for revealing individual list by source
  observeEvent(input$enolex_materials_cell_clicked, {
    
    source_info <- input$enolex_materials_cell_clicked
    
    if (!is.null(source_info$value) && source_info$col == 3) {
      indiv_tb <- reactive({
        enolex |> 
          filter(glue::glue_sql(str_c("Sources = '",
                                      source_info$value,
                                      "'",
                                      sep = ""))) |> 
          select(Cognate_ID, Original_Form, 
                 Standardised_Orthography = Orthography,
                 Phonemic_Transcription = IPA, 
                 Concepticon = Concepticon_Gloss) |> 
          arrange(Original_Form) |> 
          collect() |> 
          distinct() |> 
          DT::datatable(escape = FALSE,
                        options = list(scrollY = "500px",
                                       scrollX = TRUE,
                                       scrollCollapse = TRUE,
                                       # pageLength = 20,
                                       # autoWidth = TRUE,
                                       language = list(searchPlaceholder = "Search"),
                                       columnDefs = list(list(className = "dt-center",
                                                              targets = c(1, 2)),
                                                         list(width = "50px",
                                                              targets = "Cognate_ID")),
                                       layout = list(bottom = "paging",
                                                     bottomEnd = NULL)),
                        filter = "top",
                        # callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search" )'),
                        style = "bootstrap4",
                        class = list(stripe = FALSE))
      })
      nav_insert(id = "tabs",
                 target = "Sources",
                 position = "after",
                 select = TRUE,
                 nav_panel("Word List by Source",
                           tags$h1(str_c("Word list from ‘",
                                         source_info$value,
                                         "’",
                                         sep = "")),
                           tags$br(),
                           p(HTML(pull(filter(tbl(enolex_db, "bibs1"),
                                              glue::glue_sql(str_c("Sources = '",
                                                                   source_info$value,
                                                                   "'",
                                                                   sep = ""))), Citation))),
                           tags$br(),
                           card_body(DT::DTOutput(outputId = "individualWlist"))
                 )
      )
      output$individualWlist <- DT::renderDataTable({
        indiv_tb()})
    } else {
      
      return()
      
    }
  }
  )
  
  # observer in the Concept for revealing individual list by source
  observeEvent(input$cognatesOut_cell_clicked, {
    
    source_info_concept <- input$cognatesOut_cell_clicked
    
    if (!is.null(source_info_concept$value) &&
        
        # check if the clicked value is only part of the Sources vector
        source_info_concept$value %in% pull(enolex, Sources)) {
      
      indiv_tb <- reactive({
        enolex |> 
          filter(glue::glue_sql(str_c("Sources = '",
                                      source_info_concept$value,
                                      "'",
                                      sep = ""))) |> 
          select(Cognate_ID, Original_Form, 
                 Standardised_Orthography = Orthography,
                 Phonemic_Transcription = IPA, 
                 Concepticon = Concepticon_Gloss) |> 
          arrange(Original_Form) |> 
          collect() |> 
          distinct() |> 
          DT::datatable(escape = FALSE,
                        options = list(scrollY = "500px",
                                       scrollX = TRUE,
                                       scrollCollapse = TRUE,
                                       # pageLength = 20,
                                       # autoWidth = TRUE,
                                       language = list(searchPlaceholder = "Search"),
                                       columnDefs = list(list(className = "dt-center",
                                                              targets = c(1, 2)),
                                                         list(width = "50px",
                                                              targets = "Cognate_ID")),
                                       layout = list(bottom = "paging",
                                                     bottomEnd = NULL)),
                        filter = "top",
                        # callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search" )'),
                        style = "bootstrap4",
                        class = list(stripe = FALSE))
      })
      nav_insert(id = "tabs",
                 target = "Sources",
                 position = "after",
                 select = TRUE,
                 nav_panel("Word List by Source",
                           tags$h1(str_c("Word list from ‘",
                                         source_info_concept$value,
                                         "’",
                                         sep = "")),
                           tags$br(),
                           p(HTML(pull(filter(tbl(enolex_db, "bibs1"),
                                              glue::glue_sql(str_c("Sources = '",
                                                                   source_info_concept$value,
                                                                   "'",
                                                                   sep = ""))), Citation))),
                           tags$br(),
                           card_body(DT::DTOutput(outputId = "individualWlist"))
                 )
      )
      
      output$individualWlist <- DT::renderDataTable({
        indiv_tb()})
      
    } else {
      
      return()
      
    }
    
  }
  )
  
  # bookmarking; source: https://stackoverflow.com/a/71807248
  observeEvent(getQueryString(session)$page, {
    currentQueryString <- getQueryString(session)$page # alternative: parseQueryString(session$clientData$url_search)$page
    if(is.null(input$tabs) || !is.null(currentQueryString) && currentQueryString != input$tabs){
      freezeReactiveValue(input, "tabs")
      updateNavbarPage(session, "tabs", selected = currentQueryString)
    }
  }, priority = 1)
  
  observeEvent(input$tabs, {
    currentQueryString <- getQueryString(session)$page # alternative: parseQueryString(session$clientData$url_search)$page
    pushQueryString <- paste0("?page=", input$tabs)
    if(is.null(currentQueryString) || currentQueryString != input$tabs){
      freezeReactiveValue(input, "tabs")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  
  observe({
    if(input$English_Gloss != "(none)" & input$tabs == "Concept Search") {
      currentQueryString <- getQueryString(session)$page
      pushQueryString <- paste0("#", input$English_Gloss)
      updateQueryString(pushQueryString, mode = "push", session)
    }
  
    
  })
  
  
  
  onRestored(function(state) {
    # This works, because it doesn't use the inputMessageQueue. Should it use a
    # queue that's flushed on flushOutput?
    # showNotification('xxxx')
    
    # This doesn't, because it uses the inputMessageQueue
    updateSelectizeInput(session, "English_Gloss",
                         choices = state$input$English_Gloss, # <- this works to restore the app state by copying the URL
                         selected = state$input$English_Gloss) # <- this works to restore the app state by copying the URL
  })
  
  
  
  # observeEvent(input$English_Gloss, {
  #   currentQueryString <- getQueryString(session)$page
  #   pushQueryString <- paste0(currentQueryString, "?=", input$English_Gloss)
  #   if(is.null(currentQueryString) || currentQueryString != input$English_Gloss){
  #     # freezeReactiveValue(input, "English_Gloss")
  #     updateQueryString(pushQueryString, mode = "push", session)
  #   }
  # })
  
  # # Automatically bookmark every time an input changes
  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # # Update the query string
  # onBookmarked(updateQueryString())
  
}

onStop(function() {
  dbDisconnect(enolex_db)
})

shinyApp(ui, server, enableBookmarking = "url")