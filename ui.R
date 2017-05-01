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
              `multivariate` = c("bar", "line", "step", "area", "scatterplot", "boxplot",
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
          plotOutput("graph")
          ),
        column(3, 
         wellPanel(
          h4("plot labels"),
          textInput("x_label", "x-axis label"),
          textInput("y_label", "y-axis label"),
          textInput("z_guide", "discrete variable name"),
          textInput("z_label", "discrete variable labels, separated by commas",
            placeholder = "one, two, three, ..."),
          textInput("w_guide", "continuous variable name"),
          textInput("w_label", "continuous variable labels, separated by commas",
            placeholder = "low, high"),
          textInput("source_label", "source label",
            placeholder = "Source: GAO analysis...")
          ),         
       ## links to outputs of application -------------------------------------
         wellPanel(
          div(style="display:inline-block", h4("export:")),
          div(style="display:inline-block", downloadButton(outputId = "bundle", label = "results"))
          ) 
         )
        )
      )
    )
  )