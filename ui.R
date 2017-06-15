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
        Shiny.onInputChange('storage', input.files[0].lastModifiedDate.toString());
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
            useShinyjs(),
            extendShinyjs(text = jsCode),
            textInput("storage", "storage")
            )
          )
        )
      )
    )
  }
