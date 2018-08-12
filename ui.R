library(shiny)
library(shinyjs)
library(rhandsontable)

function(request) {
  shinyUI(
    navbarPage("autoggraph",
      id = "which_panel",
      # reshape UI ---------------------------------------------------------
      tabPanel(
        "data",
        includeCSS("www/simplex.css"),
        useShinyjs(),
        tags$head(
          tags$link(rel = "icon", type = "image/png", href = "favicon.png")
        ),
        fluidRow(
          column(
            3,
            wellPanel(
              fileInput("infile", label = "upload your data:"),
              uiOutput("excel_sheet_selector"),
              uiOutput("delim_selector"),
              uiOutput("reshape_btns"),
              uiOutput("reshape_me"),
              uiOutput("reshape_options"),
              conditionalPanel(
                "input.reshape_variables == 'recode'",
                rHandsontableOutput("recode")
              ),
              uiOutput('date_format'),
              uiOutput("table_btn")
            )
          ),
          column(
            6,
            rHandsontableOutput("table")
          )
        )
      ),
      # plot UI -----------------------------------------------------------------
      tabPanel(
        "plots",
        fluidRow(
          column(
            3,
            wellPanel(
              h4("plot type"),
              selectInput("chart_type",
                label = NULL,
                choices = list(
                  `select a chart type` = "",
                  `general` = c("area", "bar", "boxplot", "density", "heatmap", "histogram", "line", "pie", "scatterplot", "step", "violin"),
                  `confidence intervals` = c("pointrange", "error bar")
                )
              ),
              uiOutput("variable_selector")
            ),
            uiOutput("plot_options")
          ),
          column(
            6,
            h4("plot preview:"),
            plotOutput("graph")
          ),
          column(
            3,
            uiOutput("plot_labels"),
            hidden(
              div(
                id = "fine_tuning_well",
                wellPanel(
                  h4("fine tuning"),
                  numericInput("offset_x", "adjust x-axis label position", value = 0, min = -.2, max = 1),
                  p("the x-axis label should be aliged with the leftmost data point"),
                  numericInput("offset_y", "adjust y-axis label position", value = 0, min = -.2, max = 1),
                  p("the y axis label should be aligned with the leftmost y-axis value"),
                  numericInput("export_height", "export height", value = 6.83, min = 4),
                  numericInput("export_width", "export width", value = 7, min = 5),
                  numericInput("offset_source", "adjust source line position", value = 0),
                  downloadButton("preview", "preview image", inline = TRUE),
                  downloadButton("proof", "download proof", inline = TRUE)
                )
              )
            )
          )
        )
      )
    )
  )
}
