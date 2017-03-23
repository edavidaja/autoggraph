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
              # if we're not able to do something about 
              `select a chart type` = "",
              `one variable` = c("bar", "histogram", "density", "pie"),
              `two variable` = c("stacked bar", "clustered bar", "line", "step", "scatterplot", "boxplot"),
              `three variable` = c("stacked bar", "clustered bar", "scatterplot"),
              `four variable` = c("bubble chart", "")
              )
            ),

           fileInput("infile", label = "upload your data (in excel or csv format)"),
           uiOutput("excel_sheet_selector"),
           uiOutput("variable_selector")
           ),
         wellPanel(
          h4("plot-specific options:"),
          uiOutput("plot_options")
          )
         ),
        column(6, 
         mainPanel(plotOutput("graph"))
         ),
        column(3, 
         wellPanel(
          # TODO(): it may make sense to move this to a dynamic UI if we can
          # get number of variable inputs to depend on option group in chart
          # selector
          h4("plot labels"),
          textInput("x_label", "x-axis label"),
          textInput("y_label", "y-axis label"),
          textInput("z_label", "labels for third variable",
            placeholder = "'one', 'two', 'three'"),
          textInput("w_label", "labels for fourth variable",
            placeholder = "'one', 'two', 'three'"),
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