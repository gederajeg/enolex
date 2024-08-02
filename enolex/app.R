library(shiny)
library(reactable)
library(tidyverse)

# Preparation =====
sources_all <- read_rds("sources.rds")
enolex <- read_rds("enolex.rds")

# count the number of forms in each source =====
enolex_sources_count <- enolex |> 
  group_by(Year, BIBTEXKEY, AUTHOR, TITLE) |> 
  summarise(number_of_forms = n_distinct(Orthography))
enolex_sources_count

# count the number of cognates per meaning =====
## English =====
cognate_header_eng <- enolex |> 
  group_by(Cognate_ID) |> 
  slice_sample(n = 1) |> 
  select(Cognate_ID, Orthography, English)

enolex_cognates_eng <- enolex |> 
  group_by(English, Cognate_ID) |> 
  summarise(number_of_form = n_distinct(Orthography), .groups = "drop") |> 
  arrange(desc(number_of_form)) |> 
  rename(Number_of_word_per_Cognate_ID = number_of_form) |> 
  left_join(cognate_header_eng)

enolex_cognates_eng |> 
  filter(Number_of_word_per_Cognate_ID >= 10) |> 
  as.data.frame()

## Indonesian =====
cognate_header_idn <- enolex |> 
  group_by(Cognate_ID) |> 
  slice_sample(n = 1) |> 
  select(Cognate_ID, Orthography, Indonesian)

enolex_cognates_idn <- enolex |> 
  group_by(Indonesian, Cognate_ID) |> 
  summarise(number_of_form = n_distinct(Orthography), .groups = "drop") |> 
  arrange(desc(number_of_form)) |> 
  rename(Number_of_word_per_Cognate_ID = number_of_form) |> 
  left_join(cognate_header_idn)

enolex_cognates_idn |> 
  filter(Number_of_word_per_Cognate_ID >= 10) |> 
  as.data.frame()

# enolex concepts ====
enolexsem <- enolex |> 
  select(Indonesian, English, Concepticon_Gloss, Semantic_Field) |> 
  distinct() |> 
  mutate(Parameter_ID = paste(row_number(), "-", English, "-", Indonesian, sep = ""))

# enolex form ====
enolexform <- enolex |> 
  select(Cognate_ID, Year, Original_Form, Orthography, IPA, Indonesian, English, Concepticon = Concepticon_Gloss, English_Original, Doculect, Sources = BIBTEXKEY, Note_for_Year, Note_for_Cognate, PAN_Etymon, PAN_English, PMP_Etymon, PMP_English)
enolexform

sem_choices_eng <- c("ALL", enolexsem$English)
sem_choices_idn <- c("ALL", enolexsem$Indonesian)


ui <- fluidPage(
  
  #theme = bslib::bs_theme(bootswatch = "united"),
  
  titlePanel(title = div(HTML("<b>EnoLEX</b>: <em>A Diachronic Lexical Database for the Enggano Language</em>")), windowTitle = "EnoLEX"),
  
  sidebarLayout(
    
    sidebarPanel(width = 2,
      selectizeInput(inputId = "English", label = "Meaning (Eng)", choices = NULL)
      # selectizeInput(inputId = "Indonesian", label = "Makna (Idn)", choices = NULL)
    ),
    mainPanel(
      width = 9,
      h2("Beta version"),
      p(div(HTML("Krauße, Daniel, Gede Primahadi Wijaya Rajeg, Cokorda Pramartha, Erik Zoebel, Charlotte Hemmings, I Wayan Arka, Mary Dalrymple (2024). <em>EnoLEX: A Diachronic Lexical Database for the Enggano language.</em>"))),
      reactableOutput(outputId = "enoform_eng")
      # reactableOutput(outputId = "enoform_idn")
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "English", choices = sem_choices_eng, server = TRUE)
  # updateSelectizeInput(session, "Indonesian", choices = sem_choices_idn, server = TRUE)
  tb <- reactive({ if(input$English !="ALL") {
    
    select(filter(enolexform, English %in% input$English), Cognate_ID, Year, Orthography, IPA, Indonesian, English, Concepticon, English_Original, Doculect, Sources, matches("(PAN|PMP|Note)"))
    
  }  else {
    
    select(enolexform, Cognate_ID, Year, Orthography, IPA, Indonesian, English, Concepticon, English_Original, Doculect, Sources, matches("(PAN|PMP|Note)"))
    
  }})
  
  # tb_idn <- reactive({ if (input$Indonesian !="All") subset(enolexform, Indonesian %in% input$Indonesian) else enolexform})
  
  output$enoform_eng <- renderReactable({
    
    reactable(tb(), 
              # details = function(index) {
              #   
              #   moreinfo <- distinct(select(filter(enolexform, Cognate_ID == enolexform$Cognate_ID[index]), Note_for_Year, Note_for_Cognate, PAN_Etymon, PAN_English, PMP_Etymon, PMP_English))
              #   htmltools::div(style = "padding: 1rem",
              #                  reactable(moreinfo, outlined = TRUE,
              #                            columns = list(PAN_Etymon = colDef(html = TRUE),
              #                                           PMP_Etymon = colDef(html = TRUE)))
              #   )
              # },
              defaultColDef = colDef(minWidth = 140), 
              searchable = TRUE, 
              filterable = TRUE, 
              wrap = TRUE, 
              highlight = TRUE, 
              bordered = TRUE,
              columns = list(
                PAN_Etymon = colDef(html = TRUE),
                Concepticon = colDef(html = TRUE, minWidth = 160),
                PMP_Etymon = colDef(html = TRUE),
                Sources = colDef(minWidth = 160),
                Cognate_ID = colDef(minWidth = 95, align = "left"),
                Year = colDef(minWidth = 70)))
    
  })
  
  # output$enoform_idn <- renderReactable({
  #   
  #   reactable(tb_idn(), defaultColDef = colDef(filterable = TRUE, minWidth = 140), columns = list(
  #     PAN_Etymon = colDef(html = TRUE),
  #     PMP_Etymon = colDef(html = TRUE)))
  #   
  # })
  
  
}

shinyApp(ui, server)