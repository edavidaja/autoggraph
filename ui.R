# user interface --------------------------------------------------------------
library(shiny)

shinyUI(


  navbarPage("autoggraph", id = "which_panel",
    # instructions UI ---------------------------------------------------------
    tabPanel("instructions",
     tags$head(
      tags$link(rel = "icon", type = "image/png", href = "favicon.png")
      ),
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
              `multivariate` = c("bar", "line", "step", "area",
                "scatterplot", "boxplot", "heatmap"),
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
          plotOutput("graph")
          ),
        column(3,
          uiOutput("plot_labels")
          )
        )
      )
    )
  )