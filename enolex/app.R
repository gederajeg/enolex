library(shiny)
library(reactable)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(colourvalues)

# Preparation =====
year_arranged <- c("< 1855", "1854", "1855", "1864", "1870", "1878", "1879", "1888", "1891", "1894", "1916", "1979", "1982", "1987", "2011", "2019", "2022")
bibs <- read_rds("sources.rds") |> 
  filter(BIBTEXKEY != "NothoferMS") |> 
  mutate(YEAR = replace(YEAR, YEAR == "n.d.", "< 1855")) |> 
  mutate(YEAR = factor(YEAR, levels = year_arranged)) |> 
  arrange(YEAR, AUTHOR)
elx <- read_rds("enolex.rds")

elx_eng <- sort(unique(elx$English), decreasing = FALSE)
sem_choices_eng <- c("(none)", elx_eng)
bib_choices <- c("(none)", bibs$Sources)

bibs1 <- select(bibs,
                -Sources,
                -BIBTEXKEY,
                -YEAR,
                -URL) |> 
  mutate(CITATION = str_replace_all(CITATION, "(\\s)_", "\\1<em>"), 
         CITATION = str_replace_all(CITATION, "_(\\s|[[:punct:]])", "</em>\\1"),
         CITATION = if_else(str_detect(CITATION, "\\<https"),
                            str_replace_all(CITATION, "\\<(https[^>]+?)\\>", "<a href='\\1' target='_blank'>URL</a>"),
                            CITATION)) |> 
  rename(YEAR = YEAR_URL) |> 
  select(YEAR, CITATION)

english_gloss <- selectizeInput(inputId = "English_Gloss", 
                                options = list(dropdownParent = "body"),
                                label = "Concepts",
                                choices = NULL, 
                                selected = NULL
)

link_enolex_github <- tags$a(shiny::icon("github"), "GitHub", href="https://github.com/engganolang/enolex", target="_blank")
link_enggano_web <- tags$a(shiny::icon("globe", lib = "glyphicon"), "Enggano webpage", href="https://enggano.ling-phil.ox.ac.uk/", target="_blank")
bibs <- selectizeInput(inputId = "References",
                       label = "Sources",
                       choices = NULL, 
                       selected = NULL)

cards <- list(
  background_image = 
    
    card(card_image("estuary.JPG", border_radius = "none"),
         card_footer("The estuary towards the Indian Ocean from the Bak Blau lake, Enggano Island",
                     class = "fs-6; fw-lighter; blockquote-footer; border-0"),
         class = "border-0")
  ,
  citation = card(class = "border-0",
                  card_body(div(h5("How to cite EnoLEX")),
                            div(p("Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Pramartha, Erik Zoebel, Charlotte Hemmings, I Wayan Arka, Mary Dalrymple (2024).", em("EnoLEX: A Diachronic Lexical Database for the Enggano language.")), style="font-size: 0.875em"),
                            #div(h5("Please also cite the following paper:")),
                            div(p("Rajeg, Gede Primahadi Wijaya, Daniel Krauße, and Cokorda Rai Adi Pramartha (2024).", a("EnoLEX: A Diachronic Lexical Database for the Enggano language", href='https://enggano.ling-phil.ox.ac.uk/static/papers/EnoLEX%20-%20A%20Diachronic%20Lexical%20Database%20for%20the%20Enggano%20language%20[Preprint].pdf', target="_blank"), ". In", em("Proceedings of AsiaLex 2024 (The Asian Association for Lexicography 2024 Hybrid Conference)."), "Toyo University, Tokyo: Japan."), style="font-size: 0.875em")
                  )),
  enolex_description = card(card_body(h3(strong("EnoLEX: A diachronic lexical database of the Enggano language")),
                                      h4("Overview"),
                                      p("EnoLEX collates lexical data from", actionLink("SourcesTabLink", "legacy materials and contemporary fieldwork data"), "about the Enggano language, ranging from simple/short and extensive word lists, anthropological and ethnographic writings, a dictionary, thesis, and contemporary Enggano data. The materials span over 150 years from the middle of the 19th century up to the present. With expert cognate-judgement, EnoLEX offers historical development of word forms expressing a certain concept/meaning."),
                                      
                                      h4("How to get started"),
                                      p("Users can go to the", actionLink("CognatesTabLink", "Cognates"), "tab and then, from the left-hand side sidebar, select the concept to filter forms expressing that concept and how they develop across periods."),
                                      
                                      h4("Licensing"),
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

cognate_cards <- list(
  cognate_table = card(
    # card_header("Cognate groups"),
    layout_sidebar(sidebar = sidebar(english_gloss, open = list(mobile = "always-above")),
                   div(DT::DTOutput("cognatesOut"), style = "font-size:90%")),
    height = 600),
  concept_card = card(
    htmlOutput("ConceptEnglishIndonesian"),
    min_height = 110,
    fill = TRUE,
  )

)

ui <- page_navbar(
  id = "tabs",
  theme = bs_theme(
    version = bslib::version_default(),
    bootswatch = "cosmo",
    bg = "#f9f8f5", 
    fg = "#002147", 
    #primary = "#3277ae",
    primary = "#193658",
    secondary="#003947",
    "link-hover-color" = "#be0f34",
    "link-color" = "#3277ae"
    # source from Oxford colour parameters: https://www.ox.ac.uk/public-affairs/style-guide/digital-style-guide
  ),
  # collapsible = TRUE,
  underline = TRUE,
  nav_panel(title = "Main",
            
            
            layout_columns(
              cards[["enolex_description"]],
              # overview,
              layout_columns(
                cards[["citation"]],
                cards[["background_image"]],
                col_widths = c(12, 12)
              )
              
            )
            
            
            # layout_columns(cards[["enolex_description"]], 
            #                cards[["citation"]],
            #                cards[["background_image"]],
            #                col_widths = c(5, 7, 12),
            #                row_heights = c(5, 6))
            # )
  ),
  nav_panel(title = "Cognates",
            
            cognate_cards[["concept_card"]],
            cognate_cards[["cognate_table"]]
            # layout_columns(cognate_cards[["concept_card"]],
            #                cognate_cards[["cognate_table"]],
            #                col_widths = c(12, 12))
            # layout_sidebar(sidebar = english_gloss,
            #                dataTableOutput("cognatesOut"))
  ),
  nav_panel(title = "Sources",
            dataTableOutput("enolex_materials")),
  nav_menu(title = "Links",
           nav_item(link_enolex_github),
           nav_item(link_enggano_web))
  # nav_panel(title = "Cognates", 
  #           p("To be filled with cognacy info.")),
  # nav_panel_hidden(value = "Cognates"),
  # fillable_mobile = TRUE #,
  # title =  "EnoLEX"
  # sidebar = sidebar(
  #   title = "Global Controls",
  #   english_gloss,
  #   bibs
  # )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, inputId = "English_Gloss", choices = sem_choices_eng, server = TRUE)
  updateSelectizeInput(session, inputId = "References", choices = bib_choices, server = TRUE)
  
  english_selected <- reactive(
    {
      if(input$English_Gloss != "(none)") {
        tb <- select(filter(elx, English %in% input$English_Gloss), Cognate_ID, Year, Sources, Original_Form, Orthography, IPA, English_Original, PMP_Etymon, PAN_Etymon)
        tb <- tb |> 
          mutate(English_Original = replace_na(English_Original, ""))
        
        cog_id_colouring <- unique(tb$Cognate_ID)
        
        DT::datatable(tb, escape = FALSE, options = list(paging = FALSE,
                                                         scrollY = "500px",
                                                         scrollX = TRUE,
                                                         autoWidth = TRUE,
                                                         columnDefs = list(list(className = "dt-center",
                                                                                targets = 1))), 
                      filter = "top", 
                      style = "bootstrap4",
                      class = list(stripe = FALSE)) |> 
          # mutate(Cognate_ID = factor(Cognate_ID)) |> 
          formatStyle("Cognate_ID",
                      backgroundColor = styleEqual(cog_id_colouring,
                                                   colour_values(factor(cog_id_colouring), palette = "viridis", alpha = 65)))
      } else {
        # DT::datatable(select(filter(elx, Cognate_ID %in% sample(unique(elx$Cognate_ID), size = 1)), Cognate_ID, Year, Original_Form, Orthography, IPA, Indonesian, English, PMP_Etymon, PAN_Etymon), escape = FALSE)
        tb <- select(filter(elx, English == "dsadnfjsadnfkladfnasdlk"), Cognate_ID, Year, Sources, Original_Form, Orthography, IPA, PMP_Etymon, PAN_Etymon)
        DT::datatable(tb, escape = FALSE)
      }
    }
  )
  
  materials_table <- reactive({
    DT::datatable(bibs1, escape = FALSE, options = list(paging = FALSE))
  })
  output$enolex_materials <- renderDT(materials_table())
  
  output$cognatesOut <- renderDT(english_selected())
  

    
    output$ConceptEnglishIndonesian <- renderUI(
    {
      req(input$English_Gloss != "(none)")
        
        eng <- str_to_sentence(unique(pull(filter(elx, English %in% input$English_Gloss), English)))
        # idn_interim <- unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))
        
        if (length(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian))) > 1) {
          
          idn <- str_c("‘<em>", str_c(unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian)), collapse = ", "), "</em>’", sep = "")
          
          idn_orth <- elx |> 
            filter(English %in% input$English_Gloss) |> 
            select(Sources, Indonesian, Orthography) |> 
            distinct()
          idn_notes <- idn_orth |> 
            group_by(Indonesian, Sources) |> 
            mutate(forms = str_c("<em>", Orthography, "</em>", sep = "")) |> 
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
          
          idn <- str_c("‘<em>", unique(pull(filter(elx, English %in% input$English_Gloss), Indonesian)), "</em>’", sep = "")
          
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
  
  
  # the following code run the clicking on Cognates hyperlink the main panel/page
  observeEvent(input$CognatesTabLink, {
    updateTabsetPanel(session = session, "tabs", "Cognates")
  })
  
  # the following code run the clicking on Sources hyperlink the main panel/page
  observeEvent(input$SourcesTabLink, {
    updateTabsetPanel(session = session, "tabs", "Sources")
  })
  
  # observeEvent(english_selected(), {nav_show("tabs", "Cognates", select = TRUE)})
  
  # gg_plot <- reactive({
  #   ggplot(penguins) +
  #     geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
  #     theme_bw(base_size = 16) +
  #     theme(axis.title = element_blank())
  # })
  
  #output$bill_length <- renderPlot(gg_plot() + aes(bill_length_mm))
  # output$bill_depth <- renderPlot(gg_plot() + aes(bill_depth_mm))
  # output$overview <- renderText({input$oveview})
}

shinyApp(ui, server)