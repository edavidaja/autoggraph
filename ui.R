# user interface --------------------------------------------------------------
library(shiny)

shinyUI(
  fluidPage(
  # Application title
    titlePanel("tablol", windowTitle = "uninvent the wheel."),
    includeCSS("www/simplex.css"),
    fluidRow(  
      column(3,
       wellPanel(
         selectInput("chart_type", "which chart would you like to use?",
          choices = list(
            `select a chart type` = "",
            `one variable` = c("bar", "histogram", "density"),
            `two variable` = c("line", "step", "scatterplot")
            )
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
        ),
       ## conditional panels with plot specific options ------------------------
       wellPanel(
        h4("plot-specific options:"),
        ### scatterplots ------------------------------------------------------
        conditionalPanel("input.chart_type == 'scatterplot'",
          p("smoother?")
          ),
        ### 
        conditionalPanel("input.chart_type == 'bar'",
          p("horizontal?")
          ),
        conditionalPanel("input.chart_type == 'line'",
          p("linetypes?")
          )
        ),
       ## links to outputs of application -------------------------------------
        wellPanel(
          h4("export:"),
          downloadLink("raster_download", 
            "download raster files for use in drafts or presentations:"),
          br(),
          br(),
          downloadLink("vector_download", 
            "download vector files for further VCA customization:"),
          br(),
          br(),
          downloadLink("code_download", 
            "share the plot generating code with your data analyst:")
        ) 
       )
      )
    )
  )