library(shiny)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)

# guess_import <- function(input_filename) {
#   extension <- str_extract(input_filename, "\\.[A-z]+")

#   if (extension == ".xlsx" | extension == ".xls") {
#     read_excel(input_filename)
#   } else if (extension == ".csv") {
#     read_csv(input_filename)
#   } else if (extension == ".dta") {
#     read_dta(input_filename)
#   } else if (extension == ".sav") {
#     read_spss(input_filename)
#   } else if (extension == ".sas") {
#     read_sas(input_filename)
#   } else {
#     return(NULL)
#   }
# }


viz_vocab <- read_csv("data/visual_vocabulary.csv")

ui <- fluidPage(

   # Application title
   titlePanel("tablol", windowTitle = "uninvent the wheel."),
   includeCSS("www/simplex.css"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
          # cascading menu system prompts relationship and then chart
          uiOutput("select_relationship"),
          textOutput("desc_text"),
          br(),
          uiOutput("select_chart"),

         fileInput("infile", label = "upload your data (in csv format)"),

         uiOutput("x_variable"),
         uiOutput("y_variable"),
         uiOutput("z_variable"),

         actionButton("go", "Go!", icon = icon("area-chart"))
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("graph")
      )
   ),
  hr(),
  a("Inspired by the FT Visual Vocabulary.",
      href="https://github.com/ft-interactive/chart-doctor/raw/master/visual-vocabulary/Visual-vocabulary.pdf")
)

server <- function(input, output, session) {

# Relationship selectors ------------------------------------------------------
  output$select_relationship <- renderUI({
    selectInput("relationship_type", "what kind of relationship are you depicting?",
                choices = unique(viz_vocab$relationship_type))
  })

  output$desc_text <- renderText({
    req(input$relationship_type)

    relationship_description <- viz_vocab %>%
      filter(relationship_type == input$relationship_type) %>%
      select(description) %>% unique(.) %>% as.character(.)

    (relationship_description)
  })

  output$select_chart <- renderUI({

    req(input$relationship_type)

    available_charts <- viz_vocab %>%
      filter(relationship_type == input$relationship_type) %>%
      select(chart_type)

    selectInput("chart_type", "which chart would you like to use?",
                choices = available_charts)
  })
# Ingest file -----------------------------------------------------------------
  graph_data <- eventReactive(input$infile, {
    read_csv(input$infile$datapath)
  })

# Variable selectors ----------------------------------------------------------
  output$x_variable <- renderUI({

    selectizeInput("x",
      "select your x variable:",
      choices = names(graph_data()),
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })

  output$y_variable <- renderUI({

    selectizeInput("y",
      "add your y variable:",
      choices = names(graph_data()),
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })

  output$z_variable <- renderUI({

    selectizeInput("z",
      "add your z variable:",
      choices = names(graph_data()),
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })

# Graphs ----------------------------------------------------------------

  which_geom <- reactive({
    switch(input$chart_type,
      "histogram" = geom_histogram(),
      "boxplot" = geom_boxplot(),
      "violin plot" = geom_violin(),
      "population pyramid" = geom_bar(),
      "dot strip plot" = geom_point(),
      "barcode plot" = geom_raster(),
      "cumulative curve" = stat_ecdf()
    )
  })

  output$graph <- renderPlot({


  })
}

# Run the application
shinyApp(ui = ui, server = server)
