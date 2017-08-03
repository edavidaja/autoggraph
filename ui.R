# user interface --------------------------------------------------------------
library(shiny)
library(shinyjs)

jsCode <- "
shinyjs.showFileModified = function() {
        var input, file;

        if (typeof window.FileReader !== 'function' &&
            typeof window.FileReader !== 'object') {
            write('The file API isnt supported on this browser yet.');
            return;
        }
        input = document.getElementById('infile');
        console.log(input.files[0].lastModifiedDate.toString());
        Shiny.onInputChange('infile_mtime', input.files[0].lastModifiedDate.toString());
    }
"
function(request) {
  shinyUI(
    navbarPage("autoggraph", id = "which_panel",
    # instructions UI ---------------------------------------------------------
      tabPanel("instructions",
       tags$head(
        tags$link(rel = "icon", type = "image/png", href = "favicon.png")
        ),
       includeHTML("www/ins.html")
       ),
    # plot UI -----------------------------------------------------------------
      tabPanel("plots",
        includeCSS("www/simplex.css"),
        fluidRow(  
          column(3,
           wellPanel(
            h4("plot type | data:"),
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
           uiOutput("plot_options")
           ),
          column(6,
            h4("plot preview:"),
            plotOutput("graph")
            ),
          column(3,
            uiOutput("plot_labels"),
            hidden(
              div(id = "fine_tuning_well",
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
              ),
            useShinyjs(),
            extendShinyjs(text = jsCode, functions = c("showFileModified")),
            hidden(
              textInput("infile_mtime", "infile_mtime")
              )
            )
          )
        )
      )
    )
  }
