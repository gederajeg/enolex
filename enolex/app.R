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
                                label = "Concepts",
                                choices = NULL, 
                                selected = NULL
)
bibs <- selectizeInput(inputId = "References",
                       label = "Sources",
                       choices = NULL, 
                       selected = NULL)

cards <- list(
  background_image = card(
    full_screen = T,
    card_image(file = "estuary.JPG",
               fill = TRUE,
               class = "border-0"),
    card_footer("The estuary towards the Indian Ocean from the Bak Blau lake, Enggano Island",
                class = "fs-6; fst-italic")
  ),
  citation = card(
    full_screen = T,
    card_body(card_title(div(h4("How to cite EnoLEX"), style="font-family:georgia")),
              div(p(class = "text-muted", "Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Pramartha, Erik Zoebel, Charlotte Hemmings, I Wayan Arka, Mary Dalrymple (2024).", em("EnoLEX: A Diachronic Lexical Database for the Enggano language.")), style="font-family:georgia"),
              div(h4(class = "text-muted", "Please also cite the following paper:"), style="font-family:georgia"),
              div(p(class = "text-muted", "Rajeg, Gede Primahadi Wijaya, Daniel Krauße, and Cokorda Rai Adi Pramartha (2024).", a("EnoLEX: A Diachronic Lexical Database for the Enggano language", href='https://enggano.ling-phil.ox.ac.uk/static/papers/EnoLEX%20-%20A%20Diachronic%20Lexical%20Database%20for%20the%20Enggano%20language%20[Preprint].pdf', target="_blank"), ". In", em("Proceedings of AsiaLex 2024 (The Asian Association for Lexicography 2024 Hybrid Conference)."), "Toyo University, Tokyo: Japan."), style="font-family:georgia")
    )),
  enolex_description = card(
    full_screen = T,
    card_body(card_title(h3("Overview")),
              p("EnoLEX collates lexical data from", actionLink("SourcesTabLink", "legacy materials and contemporary fieldwork data"), "about the Enggano language, ranging from simple/short and extensive word lists, anthropological and ethnographic writings, a dictionary, thesis, and contemporary Enggano data. The materials span over 150 years from the middle of the 19th century up to the present. With expert cognate-judgement, EnoLEX offers historical development of word forms expressing a certain concept/meaning."),
              h4("How to get started"),
              p("Users can go to the", actionLink("CognatesTabLink", "Cognates"), "tab and then, from the left-hand side sidebar, select the concept to filter forms expressing that concept and how they develop across periods.")),
    textOutput("overview")
  ) #,
  #cognate_output <- card(
  #  full_screen = T,
  #  card_header("Cognate groups"),
  #  layout_sidebar(sidebar = english_gloss,
  #                 dataTableOutput("cognatesOut"))
  #)
)

cognate_cards <- list(cognate_table = card(
  full_screen = T,
  card_header("Cognate groups"),
  layout_sidebar(sidebar = english_gloss,
                 div(dataTableOutput("cognatesOut"), style = "font-size:90%"))
),
background_image = card(
  full_screen = T,
  card_image(file = "estuary.JPG",
             fill = TRUE,
             class = "border-0",
             width = "60%"),
  card_footer("The estuary towards the Indian Ocean from the Bak Blau lake, Enggano Island",
              class = "fs-6; fst-italic")
)
)

ui <- page_navbar(
  id = "tabs",
  theme = bs_theme(
    version = bslib::version_default(),
    bootswatch = "cosmo",
    base_font = font_google("Fira Sans")
  ),
  fillable = "Cognates",
  underline = TRUE,
  nav_panel(title = "Main", 
            layout_columns(cards[["background_image"]], 
                           cards[["citation"]], 
                           cards[["enolex_description"]],
                           col_widths = c(7, 5, 12),
                           row_heights = c(5, 6))),
  nav_panel(title = "Cognates",
            layout_columns(cognate_cards[[1]])
            # layout_sidebar(sidebar = english_gloss,
            #                dataTableOutput("cognatesOut"))
  ),
  nav_panel(title = "Sources",
            dataTableOutput("enolex_materials")),
  # nav_panel(title = "Cognates", 
  #           p("To be filled with cognacy info.")),
  # nav_panel_hidden(value = "Cognates"),
  fillable_mobile = TRUE,
  title = "EnoLEX: A Diachronic Lexical Database of the Enggano Language"
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
        tb <- select(filter(elx, English %in% input$English_Gloss), Cognate_ID, Year, Original_Form, Orthography, IPA, Indonesian, English, PMP_Etymon, PAN_Etymon)
        # tb <- mutate(tb,
        #              Cognate_ID = factor(Cognate_ID, levels = unique(tb$Cognate_ID)))
        # cogtb_id <- unique(tb$Cognate_ID)
        # cogtb_id_col <- brewer.pal(n = length(cogtb_id), name = "Paired")
        DT::datatable(tb, escape = FALSE, options = list(paging = FALSE,
                                                         autoWidth = TRUE,
                                                         columnDefs = list(list(className = "dt-center",
                                                                                targets = 1))), 
                      filter = "top", 
                      style = "bootstrap4") |> 
          # mutate(Cognate_ID = factor(Cognate_ID)) |> 
          formatStyle("Cognate_ID",
                      backgroundColor = styleEqual(unique(tb$Cognate_ID),
                                                   colour_values(unique(tb$Cognate_ID), palette = "viridis", alpha = 65)))
      } else {
        # DT::datatable(select(filter(elx, Cognate_ID %in% sample(unique(elx$Cognate_ID), size = 1)), Cognate_ID, Year, Original_Form, Orthography, IPA, Indonesian, English, PMP_Etymon, PAN_Etymon), escape = FALSE)
        tb <- select(filter(elx, English == "dsadnfjsadnfkladfnasdlk"), Cognate_ID, Year, Original_Form, Orthography, IPA, Indonesian, English, PMP_Etymon, PAN_Etymon)
        DT::datatable(tb, escape = FALSE)
      }
    }
  )
  
  materials_table <- reactive({
    DT::datatable(bibs1, escape = FALSE, options = list(paging = FALSE))
  })
  output$enolex_materials <- renderDT(materials_table())
  
  output$cognatesOut <- renderDT(english_selected())
  
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