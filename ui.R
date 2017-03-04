# user interface --------------------------------------------------------------
library(shiny)

shinyUI(
  fluidPage(
  # Application title
    titlePanel("tablol", windowTitle = "uninvent the wheel."),
    includeCSS("www/simplex.css"),
    fluidRow(  
    # Sidebar with a slider input for number of bins
      column(3,
       wellPanel(
         selectInput("chart_type", "which chart would you like to use?",
          choices = c("bar", "line", "scatter")
          ),

         fileInput("infile", label = "upload your data (in csv format)"),

         uiOutput("x_variable"),
         uiOutput("y_variable"),
         uiOutput("z_variable")
         )
       ),
      column(6, 
       mainPanel(plotOutput("graph"))
       ),
      column(3, 
       wellPanel(
        h4("plot labels"),
        textInput("x_label", "x-axis label"),
        textInput("y_label", "y-axis label"),
        textInput("source_label", "source label",
          placeholder = "Source: GAO analysis...")
        ) 
       )
      )
    )
  )