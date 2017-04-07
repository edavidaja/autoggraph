# user interface --------------------------------------------------------------
library(shiny)

shinyUI(
  navbarPage("tablol", id = "which_panel",
    # instructions UI ---------------------------------------------------------
    tabPanel("instructions",
      includeMarkdown("www/instructions.Rmd")
      ),
    # plot UI -----------------------------------------------------------------
    tabPanel("plots",
    includeCSS("www/simplex.css"),
      fluidRow(  
        column(3,
         wellPanel(
           selectInput("chart_type", "which chart would you like to use?",
            choices = list(
              `select a chart type` = "",
              `univariate` = c("histogram", "density", "pie"),
              `multivariate` = c("bar", "line", "step", "scatterplot", "boxplot",
                "stacked bar", "clustered bar", "filled bar", "bubble chart"),
              `statistical` = c("pointrange", "error bar")
              )
            ),

           fileInput("infile", label = "upload your data (in excel or csv format)"),
           uiOutput("excel_sheet_selector"),
           uiOutput("variable_selector")
           ),
         wellPanel(
          h4("plot specifics"),
          uiOutput("plot_options")
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
          textInput("labels", "third variable labels, separated by commas",
            placeholder = "one, two, three"),
          textInput("source_label", "source label",
            placeholder = "Source: GAO analysis...")
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
  )