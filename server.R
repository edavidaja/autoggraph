library(readr)
library(readxl)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(magrittr)
library(haven)
library(purrr)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(rlang)
library(lubridate)
library(glue)


# gao theme -------------------------------------------------------------------
theme_gao <- list(
  theme_minimal(),
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(size = 7, face = "bold"),
    plot.title = element_text(size = 7, face = "bold"),
    axis.title.x = element_text(hjust = 0, size = 7, face = "bold"),
    axis.text = element_text(size = 7, face = "bold")
  )
)

# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {
# set location of zip for export
# option is set here rather than in global because the sessions variable
# is only available within the scope of the server function
Sys.setenv(
  R_ZIPCMD = "/usr/bin/zip",
  RSTUDIO_PANDOC = "/usr/local/bin"
  )
source("import.R", local = TRUE)
source("data-manipulation.R", local = TRUE)

  # bookmarking state ----------------------------------------------------------
  # since the plot_opts id is randomly generated, onBookmark must be used in
  # order to save its state
  onBookmark(function(state) {
    plot_id <- plot_opts()
    state$values$id <- plot_id
    state$values$infile <- fil$infile
  })

  fil <- reactiveValues(infile = NULL)
  original_ops <- reactiveValues(id = NULL, loaded = FALSE, infile = NULL)
  original_data_ops <- reactiveValues(id = NULL, loaded = FALSE, infile = NULL)

  # app data store
  stored_data <- reactiveValues(data = NULL, orig_data = NULL, types = NULL, ext = NULL)

  onRestore(function(state) {
    original_ops$id <- state$values$id
    original_ops$infile <- state$values$infile
  })

  # Ingest file -----------------------------------------------------------------

  # observe file input, sheet, and cell range inputs for changes and
  # read data based on file extension
  observeEvent(c(input$infile, input$which_sheet, input$cell_range, input$which_delim), {
    req(input$infile)
    stored_data$ext <- tolower(tools::file_ext(input$infile$name))

    if (stored_data$ext %in% valid_filetypes) {
      stored_data$orig_data <- stored_data$plot_data <- stored_data$data <- 
        switch(stored_data$ext, 
          "xls"      = ingest_xls(range = input$cell_range),
          "xlsx"     = ingest_xlsx(range = input$cell_range),
          "dta"      = read_dta(input$infile$datapath),
          "sas7bdat" = read_sas(input$infile$datapath),
          ingest_delim(delim = input$which_delim)
        )
      stored_data$variable_names <- names(stored_data$data)
      # putting types into a reactive
      stored_data$types <- get_data_types()
    } else {
      reset("infile")
      showModal(modalDialog(
        title = "invalid data uploaded.",
        "Upload files in excel, sas, stata, or comma-delimited format.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  base_plot <- reactive({
    ggplot(data = stored_data$data)
  })
  # Variable selectors ----------------------------------------------------------
  # variable selectors are rendered once graph data is uploaded
  # conditional panels are used to display only the relevant input variables
  # for the selected plot type
  output$variable_selector <- renderUI({
    req(input$infile)

    list(
      conditionalPanel(
        condition = "input.chart_type != 'pie'",
        selectInput("x",
          "select your x variable:",
          choices = c("x variable" = "", names(stored_data$plot_data))
        )
      ),
      conditionalPanel(
        condition = "input.chart_type != 'density' & input.chart_type != 'histogram'",
        selectInput("y",
          "select your y variable:",
          choices = c("y variable" = "", names(stored_data$plot_data))
        )
      ),
      conditionalPanel(
        condition =
          "(input.z != '' | input.w != '' | input.y != '') &
            !(input.chart_type == 'line' | input.chart_type == 'density' |
            input.chart_type == 'histogram' | input.chart_type == 'step' |
            input.chart_type == 'area') &
            input.x != ''",
        selectInput("reorder_x",
          label = "sort the x-axis by:",
          choices = c("sort by" = "", names(stored_data$plot_data))
        )
      ),
      conditionalPanel(
        condition = "input.chart_type != 'heatmap'",
        selectInput("z",
          "add a grouping variable:",
          choices = c("grouping variable" = "", names(stored_data$plot_data))
        )
      ),
      conditionalPanel(
        condition = "input.z != ''",
        radioButtons("wrap",
          label = "display grouping with:",
          choices = c("colors", "facets"),
          selected = "colors",
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.wrap == 'facets'",
        radioButtons("free_facet",
          label = "allow facet axes to vary?",
          choices = c("no" = "fixed", "by x" = "free_x", "by y" = "free_y", "both" = "free"),
          selected = "fixed",
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.chart_type == 'heatmap' | input.chart_type == 'scatterplot'",
        selectInput("w",
          "add an additional continuous variable:",
          choices = c("continuous variable" = "", names(stored_data$plot_data))
        )
      ),
      conditionalPanel(
        condition = "input.z != '' | input.w != ''",
        selectInput("palette_selector",
          label = "select a color palette:",
          choices = c("classic", "qualitative", "sequential", "diverging"),
          selected = "classic"
        )
      ),
      actionButton("do_plot", "plot:", icon = icon("area-chart"))
    )
  })

  # plot specific options -----------------------------------------------------
  # plot specific options are assinged an id combining a random number with
  # the name of the option. This randomly generated number changes whenever
  # the selected plot type is changes, and ensures that options specific
  # to one plot do not persist across plot types (smoothers in particular)

  plot_opts <- eventReactive(input$chart_type, {
    if (!is.null(original_ops$id) & original_ops$loaded == FALSE) {
      original_ops$loaded <- TRUE
      original_ops$id
    } else {
      paste0("_", round(runif(1, 1, 100), 0))
    }
  })

  output$plot_options <- renderUI({
    req(input$chart_type, input$infile)

    switch(input$chart_type,
      "scatterplot" =
        list(
          wellPanel(
            h4("plot specifics"),
            sliderInput(
              inputId = paste0("scatter_option_alpha", plot_opts()),
              "point opacity",
              min = 0, max = 100, value = 100, step = 1, post = "%",
              ticks = FALSE
            ),
            checkboxInput(
              inputId = paste0("scatter_option_grid", plot_opts()),
              "show gridlines?"
            ),
            checkboxInput(
              inputId = paste0("scatter_option_smooth", plot_opts()),
              "add a smoother?"
            ),
            uiOutput("smoother_options")
          )
        ),
      "pointrange" =
        list(
          wellPanel(
            h4("plot specifics"),
            selectInput(
              inputId = paste0("pointrange_lower", plot_opts()),
              "lower bound",
              choices = c("lower bound" = "", names(stored_data$data))
            ),
            selectInput(
              inputId = paste0("pointrange_upper", plot_opts()),
              "upper bound",
              choices = c("upper bound" = "", names(stored_data$data))
            )
          )
        ),
      "error bar" =
        list(
          wellPanel(
            h4("plot specifics"),
            selectInput(
              inputId = paste0("errorbar_lower", plot_opts()),
              "lower bound",
              choices = c("lower bound" = "", names(stored_data$data))
            ),
            selectInput(
              inputId = paste0("errorbar_upper", plot_opts()),
              "upper bound",
              choices = c("upper bound" = "", names(stored_data$data))
            )
          )
        ),
      "pie" =
        wellPanel(
          h4("plot specifics"),
          a(
            p("no. pie charts are the worst."),
            href = "http://www.businessinsider.com/pie-charts-are-the-worst-2013-6"
          )
        ),
      "bar" =
        list(
          wellPanel(
            h4("plot specifics"),
            selectInput(
              inputId = paste0("bar_type", plot_opts()),
              "select a bar type",
              choices = c(
                "stacked" = "stack", "clustered" = "dodge", "filled" = "fill"
              )
            )
          )
        ),
      "histogram" =
        list(
          wellPanel(
            h4("plot specifics"),
            numericInput(
              inputId = paste0("hist_bins", plot_opts()),
              "number of bins", value = 30
            )
          )
        ),
      "violin" =
        list(
          wellPanel(
            h4("plot specifics"),
            sliderInput(
              inputId = paste0("violin_bandwidth", plot_opts()),
              "wiggle:",
              min = .1, max = 1, value = 1, ticks = FALSE
            ),
            selectInput(
              inputId = paste0("violin_scale", plot_opts()),
              "scale:",
              choices = c(
                "equal area" = "area",
                "equal width" = "width",
                "proportional" = "count"
              )
            )
          )
        )
    )
  })

  # options that should be available if any smoother is added to a scatterplot
  output$smoother_options <- renderUI({
    req(input[[paste0("scatter_option_smooth", plot_opts())]])

    list(
      radioButtons(
        inputId = paste0("scatter_option_smooth_group", plot_opts()),
        "smooth over:",
        selected = "overall",
        choices = c("overall", "groups"),
        inline = TRUE
      ),
      radioButtons(
        inputId = paste0("scatter_option_smooth_se", plot_opts()),
        "confidence interval?",
        choices = c("yes" = TRUE, "no" = FALSE),
        inline = TRUE
      ),
      sliderInput(
        inputId = paste0("scatter_options_smooth_span", plot_opts()),
        "wiggle", min = 0, max = 1, value = .7, step = .1,
        ticks = FALSE
      )
    )
  })

  which_palette <- reactive({
  
    print ('hi')
    # if a z variable is set, then the level count should be mapped to the
    # number of levels of the discrete variable; otherwise, a five class
    # palette is used
    if (input$z != "") {
      level_count <- nrow(unique(stored_data$data[input$z]))
    } else {
      level_count <- 5
    }
    
    print (level_count)

    switch(input$palette_selector,
      "classic" = {
        if (input$chart_type %in% c("bar", "boxplot")) {
          validate(
            need(level_count < 6, "The classic GAO palette for this graph type accepts at most 5 classes. Try 'qualitative' instead.")
          )
          c("#FFFFFF", "#99CCFF", "#044F91", "#409993", "#330033")
        } else {
          validate(
            need(level_count < 5, "The classic GAO palette for this graph type accepts at most 4 classes. Try 'qualitative' instead.")
          )
          c("#99CCFF", "#044F91", "#409993", "#330033")
        }
      },
      "qualitative" = {
        validate(
          need(level_count < 9, "The qualitative palette accepts at most 8 classes.")
        )
        brewer.pal(level_count, "Set2")
      },
      "sequential" = {
        validate(
          need(level_count < 10, "The sequential palette accepts at most 9 classes.")
        )
        brewer.pal(level_count, "Blues")
      },
      "diverging" = {
        validate(
          need(level_count < 12, "The diverging palette accepts at most 11 classes.")
        )
        brewer.pal(level_count, "RdYlBu")
      }
    )
  })

  # based on the selected chart type and variables selected within that chart type,
  # this observer sets the color palette to the option most likely to be
  # appropriate
  observeEvent({
    c(input$w, input$z)
  }, {
    req(input$chart_type, input$x)

    switch(input$chart_type,
      "scatterplot" = {
        if (input$w != "" & input$z != "") {
          updateSelectInput(session, "palette_selector", selected = "classic")
        } else if (input$w != "" & input$z == "") {
          updateSelectInput(session, "palette_selector", selected = "sequential")
        } else {
          updateSelectInput(session, "palette_selector", selected = "classic")
        }
      },
      "heatmap" = updateSelectInput(session, "palette_selector", selected = "diverging")
    )

    updateTextInput(session, "w_guide", value = input$w)
  })

  set_var_types <- reactive({
    
    # get factor orders
    list_factor_order <- list(input$factor_order_x, input$factor_order_y, input$factor_order_z)
    list_factor_order <- set_names(list_factor_order, c('x_var', 'y_var', 'z_var'))
    list_factor_order <- unlist(list_factor_order)
    # get variable inputs
    list_inputs <- list(input$x, input$y, input$z)
    list_inputs <- set_names(list_inputs, 'x_var', 'y_var', 'z_var')
    list_inputs <- unlist(list_inputs)
    
    print(bind_rows(list_inputs, list_factor_order))
    
    req(length(list_factor_order) > 0)
    req(length(list_inputs) > 0)

    fctr <- unique(gsub('_.*', '', names(list_factor_order)))
    var_to_use <- list_inputs[grepl(glue(fctr, '_var'), names(list_inputs))]


    mutation <- glue('if (class(stored_data$data[["{input_to_replace}"]]) %in% c("character", "factor")){{
    stored_data$data[["{input_to_replace}"]] <- factor(stored_data$data[["{input_to_replace}"]], levels = {new_levels})
                     }}',
                  input_to_replace = var_to_use,
                  new_levels = quo_text(as.character(list_factor_order)))
    write_to_log(mutation, checkinputs = F)
    eval(mutation %>% parse_expr())

    if (input$reorder_x != "") {
      
      validate(
        need(! class(stored_data$data[[input$reorder_x]]) %in% c("character", "factor"), "Select a numeric variable")
      )
      
      mutation <- glue('
      if (class(stored_data$data[["{input_to_replace}"]]) %in% c("character", "factor")) {{
        stored_data$data[["{input_to_replace}"]] <- (reorder(stored_data$data[["{input_to_replace}"]], stored_data$data[[input$reorder_x]]))
      }}else {{
        stored_data$data[["{input_to_replace}"]] <- as.numeric(reorder(stored_data$data[["{input_to_replace}"]], stored_data$data[["{order_by}"]]))
      }}',
      input_to_replace = input$x,
      order_by = input$reorder_x
      )
      
      print (mutation)

      if (class(stored_data$data[[input$x]]) %in% c("character", "factor")) {
        stored_data$data[[input$x]] <- (reorder(stored_data$data[[input$x]], stored_data$data[[input$reorder_x]]))
      } else {
        stored_data$data[[input$x]] <- as.numeric(reorder(stored_data$data[[input$x]], stored_data$data[[input$reorder_x]]))
      }
    }
    
    stored_data$data
  })

  # aesthetics ----------------------------------------------------------------

  base_aes <- reactive({
    # return aesthetics based on which combinations of
    # data input fields are selected
    # x only

    if (is.null(input$x) & is.null(input$z) & is.null(input$w) & is.null(input$z)) {
      aes()
    }

    else if (input$x != "" & input$y == "" & input$z == "" & input$w == "") {
      aes(x = stored_data$data[[input$x]])
    }

    # x and y
    else if (input$x != "" & input$y != "" & input$z == "" & input$w == "") {
      aes(x = stored_data$data[[input$x]], y = stored_data$data[[input$y]])
    }
    # x and z
    else if (input$x != "" & input$y == "" & input$z != "" & input$w == "") {
      aes(x = stored_data$data[[input$x]])
    }
    #  x, y and, z
    else if (input$x != "" & input$y != "" & input$z != "" & input$w == "") {
      aes(x = stored_data$data[[input$x]], y = stored_data$data[[input$y]])
    }
    # x, y, and w
    else if (input$x != "" & input$y != "" & input$z == "" & input$w != "") {
      aes(x = stored_data$data[[input$x]], y = stored_data$data[[input$y]])
    }
    # x, y, z, and w
    else if (input$x != "" & input$y != "" & input$z != "" & input$w != "") {
      aes(x = stored_data$data[[input$x]], y = stored_data$data[[input$y]])
    }
  })
  # geometries ----------------------------------------------------------------

  which_geom_xy <- reactive({

    # select geom based on selected chart type for the univariate or
    # two-variable case.
    switch(input$chart_type,
      "histogram" = {
        if (class(stored_data$data[[input$x]]) %in% c("character", "factor")) {
          stat_count(fill = "#0039A6")
        } else {
          geom_histogram(
            fill = "#0039A6",
            bins = input[[paste0("hist_bins", plot_opts())]]
          )
        }
      },
      "density" = geom_density(fill = "#0039A6"),
      "line" = geom_line(color = "#0039A6", size = 1.1),
      "step" = geom_step(color = "#0039A6"),
      "violin" = geom_violin(
        adjust = input[[paste0("violin_bandwidth", plot_opts())]],
        scale = input[[paste0("violin_scale", plot_opts())]],
        color = "#0039A6"
      ),
      "scatterplot" = geom_point(
        alpha = input[[paste0("scatter_option_alpha", plot_opts())]] / 100,
        color = "#0039A6"
      ),
      "bar" = {
        if (input$y == "") {
          geom_bar(position = "stack", fill = "#0039A6")
        } else {
          geom_bar(position = "stack", stat = "identity", fill = "#0039A6")
        }
      },
      "boxplot" = geom_boxplot(color = "#0039A6"),
      "pointrange" = {
        validate(
          need(input[[paste0("pointrange_lower", plot_opts())]] != "", "select a lower bound variable to continue"),
          need(input[[paste0("pointrange_upper", plot_opts())]] != "", "select an upper bound variable to continue")
        )
        geom_pointrange(
          aes_string(
            ymin = input[[paste0("pointrange_lower", plot_opts())]],
            ymax = input[[paste0("pointrange_upper", plot_opts())]]
          ),
          color = "#0039A6"
        )
      },
      "error bar" = {
        validate(
          need(input[[paste0("errorbar_lower", plot_opts())]] != "", "select a lower bound variable to continue"),
          need(input[[paste0("errorbar_upper", plot_opts())]] != "", "select an upper bound variable to continue")
        )
        geom_errorbar(
          aes_string(
            ymin = input[[paste0("errorbar_lower", plot_opts())]],
            ymax = input[[paste0("errorbar_upper", plot_opts())]]
          ),
          color = "#0039A6"
        )
      }
    )
  })

  which_geom_z <- reactive({
    switch(input$chart_type,
      "histogram" = {
        if (class(stored_data$data[[input$x]]) %in% c("character", "factor")) {
          stat_count(
            aes(fill = factor(stored_data$data[[input$z]]))
          )
        } else {
          geom_histogram(
            aes(
              fill = factor(stored_data$data[[input$z]])
            ),
            bins = input[[paste0("hist_bins", plot_opts())]]
          )
        }
      },
      "density" = geom_density(
        aes(
          color = factor(stored_data$data[[input$z]]),
          linetype = factor(stored_data$data[[input$z]])
        ),
        size = 1.1
      ),
      "line" = geom_line(
        aes(
          color = factor(stored_data$data[[input$z]]),
          linetype = factor(stored_data$data[[input$z]])
        ),
        size = 1.1
      ),
      "step" = geom_step(
        aes(
          color = factor(stored_data$data[[input$z]]),
          linetype = factor(stored_data$data[[input$z]])
        ),
        size = 1.1
      ),
      "boxplot" = geom_boxplot(
        aes(
          fill = factor(stored_data$data[[input$z]])
        ),
        color = "black"
      ),
      "violin" = geom_violin(
        aes(
          color = factor(stored_data$data[[input$z]])
        ),
        adjust = input[[paste0("violin_bandwidth", plot_opts())]],
        scale = input[[paste0("violin_scale", plot_opts())]],
      ),
      "scatterplot" = geom_point(
        aes(
          color = factor(stored_data$data[[input$z]]),
          shape = factor(stored_data$data[[input$z]])
        ),
        size = 2,
        alpha = input[[paste0("scatter_option_alpha", plot_opts())]] / 100
      ),
      "bar" = {
        if (input$y == "") {
          geom_bar(
            aes(
              fill = factor(stored_data$data[[input$z]])
            ),
            position = input[[paste0("bar_type", plot_opts())]],
            color = "black"
          )
        } else {
          geom_bar(
            aes(
              fill = factor(stored_data$data[[input$z]])
            ),
            position = input[[paste0("bar_type", plot_opts())]],
            stat = "identity",
            color = "black"
          )
        }
      },
      "pointrange" = {
        validate(
          need(input[[paste0("pointrange_lower", plot_opts())]] != "", "select a lower bound variable to continue"),
          need(input[[paste0("pointrange_upper", plot_opts())]] != "", "select an upper bound variable to continue")
        )
        geom_pointrange(
          aes_string(
            ymin = stored_data$data[[input[[paste0("pointrange_lower", plot_opts())]]]],
            ymax = stored_data$data[[input[[paste0("pointrange_upper", plot_opts())]]]],
            color = factor(stored_data$data[[input$z]])
          )
        )
      },
      "error bar" = {
        validate(
          need(input[[paste0("errorbar_lower", plot_opts())]] != "", "select a lower bound variable to continue"),
          need(input[[paste0("errorbar_upper", plot_opts())]] != "", "select an upper bound variable to continue")
        )
        geom_errorbar(
          aes_string(
            ymin = stored_data$data[[input[[paste0("pointrange_lower", plot_opts())]]]],
            ymax = stored_data$data[[input[[paste0("pointrange_upper", plot_opts())]]]],
            color = factor(stored_data$data[[input$z]])
          )
        )
      },
      "area" = {
        validate(
          need(input$z != "", "area charts require a grouping variable")
        )
        list(
          geom_area(
            aes(
              fill = factor(stored_data$data[[input$z]])
            ),
            alpha = .1
          ),
          geom_line(
            aes(
              color = factor(stored_data$data[[input$z]]),
              linetype = factor(stored_data$data[[input$z]])
            ),
            size = 1.1,
            position = "stack"
          )
        )
      }
    )
  })

  which_geom_w <- reactive({
    switch(input$chart_type,
      "scatterplot" =
        geom_point(
          aes_string(color = input$w),
          alpha = input[[paste0("scatter_option_alpha", plot_opts())]] / 100
        ),
      "heatmap" = geom_tile(
        aes_string(fill = stored_data$data[[input$w]])
      )
    )
  })

  which_geom_w_z <- reactive({
    if (input$wrap == "facets") {
      geom_point(
        aes(color = stored_data$data[[input$w]]),
        alpha = input[[paste0("scatter_option_alpha", plot_opts())]] / 100
      )
    } else {
      geom_point(
        aes_string(
          size = stored_data$data[[input$w]],
          colour = factor(stored_data$data[[input$z]])
        ),
        alpha = input[[paste0("scatter_option_alpha", plot_opts())]] / 100
      )
    }
  })

  # plot labels -----------------------------------------------------------------
  output$plot_labels <- renderUI({
    req(input$infile)

    conditionalPanel(
      condition = "input.chart_type != '' & input.chart_type != 'pie'",
      wellPanel(
        h4("plot labels"),
        hr(),
        checkboxInput("flip_axes", "invert x and y axes"),
        textInput("x_label", "x-axis label"),
        hidden(
          radioButtons("x_val_format",
            label = "x-axis value format",
            choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE
          )
        ),
        hidden(
          textInput("x_breaks", "x-axis breaks", placeholder = "min, max, interval")
        ),
        uiOutput("drag_drop_x"),
        hr(),
        textInput("y_label", "y-axis label"),
        hidden(
          radioButtons("y_val_format",
            label = "y-axis value format",
            choices = c("none" = "", "dollar", "comma", "percent"),
            inline = TRUE
          )
        ),
        hidden(
          textInput("y_breaks", "y-axis breaks", placeholder = "min, max, interval")
        ),
        uiOutput("drag_drop_y"),
        conditionalPanel(
          condition = "input.z != ''",
          hr(),
          textInput("z_guide", "grouping variable name"),
          textInput("z_label", "grouping variable labels, separated by commas",
            placeholder = "group one, group two, group three, ..."
          ),
          uiOutput("drag_drop_z")
        ),
        conditionalPanel(
          condition = "input.chart_type == 'scatterplot'",
          textInput("smoother_label", "overall smoother label",
            placeholder = "smoothed y on x"
          )
        ),
        conditionalPanel(
          hr(),
          condition = "(input.chart_type == 'heatmap' | input.chart_type == 'scatterplot') &
        input.w != ''",
          textInput("w_guide", "continuous variable name"),
          textInput("w_label", "continuous variable labels, separated by commas",
            placeholder = "low, high"
          )
        ),
        hr(),
        textInput("source_label", "source label:",
          placeholder = "GAO analysis of..."
        ),
        textInput("report_number", "report number:",
          placeholder = "GAO-XX-XXX"
        ),
        h4("export:"),
        downloadButton(outputId = "bundle", label = "results", inline = TRUE),
        bookmarkButton(inline = TRUE),
        actionButton("fine_tuning", label = "fine tuning", icon = icon("sliders"), inline = TRUE)
      )
    )
  })


  output$drag_drop_x <- renderUI({
    req(class(stored_data$data[[input$x]]) %in% c("character", "factor"))
    choices <- levels(factor(stored_data$data[[input$x]]))
    selectizeInput("factor_order_x", "click and drag to reorder your x variable",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list("drag_drop"))
    )
  })

  output$drag_drop_y <- renderUI({
    req(class(stored_data$data[[input$y]]) %in% c("character", "factor"))

    choices <- levels(factor(stored_data$data[[input$y]]))

    selectizeInput(
      "factor_order_y", "click and drag to reorder your y variable",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list("drag_drop"))
    )
  })

  output$drag_drop_z <- renderUI({
    req(input$z)

    if (input$z_label != "") {
      choices <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
      levels(stored_data$data[[input$z]]) <- choices
    } else if (input$z != "") {
      choices <- levels(factor(stored_data$data[[input$z]]))
    } else {
      choices <- NULL
    }

    selectizeInput("factor_order_z",
      "click and drag to reorder your grouping variable:",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list("drag_drop"))
    )
  })

  observeEvent(input$x, {
    toggle("x_breaks",
      condition = (
        class(stored_data$data[[input$x]]) %in% c("double", "integer", "numeric", "Date"))
    )
    toggle("x_val_format",
      condition = (
        class(stored_data$data[[input$x]]) %in% c("double", "integer", "numeric"))
    )
    updateTextInput(session, "x_label", value = input$x)
  })

  observeEvent(input$y, {
    toggle("y_breaks",
      condition = (
        class(stored_data$data[[input$y]]) %in% c("double", "integer", "numeric"))
    )
    toggle("y_val_format",
      condition = (
        class(stored_data$data[[input$y]]) %in% c("double", "integer", "numeric"))
    )
  })

  # z is alawys a factor!
  observeEvent(input$z, {
    # req(input$z)
    updateTextInput(session, "z_guide", value = input$z)
    # stored_data$data[[input$z]] <- factor(stored_data$data[[input$z]])
  })

  observeEvent(input$fine_tuning, toggle("fine_tuning_well"))

  output$preview <- downloadHandler(
    filename = function() {
      paste("preview.png")
    },
    content = function(file) {
      ggsave(file,
        plot = graph_it(), device = "png",
        width = input$export_width, height = input$export_height
      )
    }
  )

  build_graph <- reactive({
    
    p <- base_plot() + base_aes() +
      labs(
        y = "", title = input$y,
        caption = paste("Source: ", input$source_label, " | ", input$report_number, sep = "")
      )
    # add geom function depending on selected variables
    # only x or x & y
    if (input$z == "" & input$w == "") {
      p <- p + which_geom_xy()
    }
    
    ## z and no w ---------------------------------------------------------------
    else if (input$z != "" & input$w == "") {
      if (input$wrap == "facets") {
        if (input$z_label == "") { # unlabeled grid
          p <- p + which_geom_xy() + facet_wrap(as.formula(paste("~", input$z)), scales = input$free_facet)
        } else { # grid with custom labels
          plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
          label_wrap <- function(variable, value) {
            unlist(strsplit(input$z_label, ",", fixed = TRUE))
          }
          p <- p + which_geom_xy() + facet_wrap(as.formula(paste("~", input$z)), labeller = label_wrap, scales = input$free_facet)
        }
      } else { # z mapped to color rather than grid
        # apply color or fill if no custom labels are set based on chart type
        if (input$z_label == "") { # default labels
          if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
            p <- p + scale_fill_manual(values = which_palette())
            p <- p + guides(fill = guide_legend(title.position = "top", ncol = 1))
          } else if (input$chart_type %in% c("density", "line", "step", "pointrange", "error bar")) {
            p <- p + scale_color_manual(values = which_palette())
            p <- p + guides(color = guide_legend(title.position = "top", ncol = 1))
          } else if (input$chart_type == "scatterplot") {
            p <- p + scale_color_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = which_palette()
            )
            p <- p + guides(
              color = guide_legend(order = 1, title.position = "top", ncol = 1, override.aes = list(alpha = 1, size = 3)),
              shape = guide_legend(order = 1, title.position = "top", ncol = 1),
              fill = guide_legend(order = 2)
            )
          } else if (input$chart_type == "area") {
            # todo(ajae): why is this a triple assignment?
            p <- p + scale_fill_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = which_palette()
            )
            p <- p + scale_linetype_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = c(1, 2, 3, 4, 5, 6)
            )
            p <- p + scale_color_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = which_palette()
            )
            p <- p + guides(
              fill = guide_legend(title.position = "top", ncol = 1),
              color = guide_legend(title.position = "top", ncol = 1),
              linetype = guide_legend(title.position = "top", ncol = 1)
            )
          }
        } else { # apply custom labels
          
          plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
          
          if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
            p <- p + scale_fill_manual(values = which_palette(), labels = plot_labels)
            p <- p + guides(fill = guide_legend(title.position = "top", ncol = 1))
          } else if (input$chart_type %in% c("pointrange", "error bar")) {
            p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
            p <- p + guides(color = guide_legend(title.position = "top", ncol = 1))
          } else if (input$chart_type %in% c("density", "line", "step")) {
            p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
            p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6), labels = plot_labels)
            p <- p + guides(color = guide_legend(title.position = "top", ncol = 1))
          } else if (input$chart_type == "scatterplot") {
            p <- p + scale_color_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = which_palette(), labels = plot_labels
            )
            p <- p + scale_shape_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = c(15, 16, 17, 18, 3, 8, 7), labels = plot_labels
            )
            p <- p + guides(
              color = guide_legend(input$z, order = 1, title.position = "top", ncol = 1, override.aes = list(alpha = 1, size = 3)),
              shape = guide_legend(input$z, order = 1, title.position = "top", ncol = 1),
              fill = guide_legend(order = 2)
            )
          } else if (input$chart_type == "area") {
            p <- p + scale_fill_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = which_palette(), labels = plot_labels
            )
            p <- p + scale_linetype_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = c(1, 2, 3, 4, 5, 6), labels = plot_labels
            )
            p <- p + scale_color_manual(
              name = ifelse(input$z_guide == "", input$z, input$z_guide),
              values = which_palette(), labels = plot_labels
            )
            p <- p + guides(
              fill = guide_legend(title.position = "top", ncol = 1),
              color = guide_legend(title.position = "top", ncol = 1),
              linetype = guide_legend(title.position = "top", ncol = 1)
            )
          }
        }
        p <- p + which_geom_z()
      }
      print("z fired")
    }
    ## w and no z ---------------------------------------------------------------
    else if (input$z == "" & input$w != "") {
      if (input$w_label == "") { # default labels
        if (input$chart_type == "scatterplot") {
          p <- p + scale_color_gradientn(colors = which_palette())
          p <- p + guides(
            color = guide_colorbar(
              order = 1,
              title.position = "top"
            ),
            fill = guide_legend(order = 2)
          )
        } else if (input$chart_type == "heatmap") {
          p <- p + scale_fill_gradientn(colors = which_palette())
        }
      } else { # custom w labels
        plot_labels <- unlist(strsplit(input$w_label, ",", fixed = TRUE))
        if (input$chart_type == "scatterplot") {
          p <- p + scale_color_gradientn(
            colors = which_palette(),
            breaks = c(
              min(stored_data$data[input$w], na.rm = TRUE),
              max(stored_data$data[input$w], na.rm = TRUE)
            ),
            labels = c(plot_labels[1], plot_labels[2])
          )
          p <- p + guides(
            color = guide_colorbar(
              order = 1,
              title.position = "top"
            ),
            fill = guide_legend(order = 2)
          )
        } else if (input$chart_type == "heatmap") {
          p <- p + scale_fill_gradientn(
            colors = which_palette(),
            breaks = c(
              min(stored_data$data[input$w], na.rm = TRUE),
              max(stored_data$data[input$w], na.rm = TRUE)
            ),
            labels = c(plot_labels[1], plot_labels[2])
          )
        }
      }
      p <- p + which_geom_w()
      print("w fired")
    }
    
    ## z and w ------------------------------------------------------------------
    else if (input$z != "" & input$w != "") {
      if (input$wrap == "facets") {
        if (input$z_label == "") { # grid, default labels
          p <- p + scale_color_gradientn(colors = which_palette())
          p <- p + facet_wrap(as.formula(paste("~", input$z)), scales = input$free_facet)
          p <- p + guides(
            color = guide_colorbar(order = 1, title.position = "top"),
            fill = guide_legend(order = 2)
          )
        } else { # grid, custom labels
          plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
          label_wrap <- function(variable, value) {
            unlist(strsplit(input$z_label, ",", fixed = TRUE))
          }
          p <- p + which_geom_xy() + facet_wrap(as.formula(paste("~", input$z)), labeller = label_wrap, scales = input$free_facet)
          p <- p + scale_color_gradientn(colors = which_palette())
          p <- p + guides(
            color = guide_colorbar(order = 1, title.position = "top"),
            fill = guide_legend(order = 2)
          )
        }
      } else { # z mapped to color
        if (input$z_label == "") { # default labels
          p <- p + scale_color_manual(values = which_palette())
          p <- p + guides(
            color = guide_legend(order = 1, title.position = "top"),
            size = guide_legend(order = 2, title.position = "top"),
            fill = guide_legend(order = 3)
          )
        } else { # custom labels
          plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
          p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
          p <- p + guides(
            color = guide_legend(order = 1, title.position = "top"),
            size = guide_legend(order = 2, title.position = "top"),
            fill = guide_legend(order = 3)
          )
        }
      }
      p <- p + which_geom_w_z()
    }
    
    ## apply smoother to scatter plot -------------------------------------------
    if (!is.null(input[[paste0("scatter_option_smooth", plot_opts())]])) {
      if (input[[paste0("scatter_option_smooth", plot_opts())]] == TRUE) {
        if (input[[paste0("scatter_option_smooth_group", plot_opts())]] == "groups") {
          p <- p + geom_smooth(
            method = "loess",
            aes(color = stored_data$data[[input$z]]),
            se = input[[paste0("scatter_option_smooth_se", plot_opts())]],
            span = input[[paste0("scatter_options_smooth_span", plot_opts())]],
            linetype = "dashed"
          )
        } else {
          p <- p + geom_smooth(
            method = "loess",
            aes_string(fill = quote(input$smoother_label)),
            span = input[[paste0("scatter_options_smooth_span", plot_opts())]],
            se = input[[paste0("scatter_option_smooth_se", plot_opts())]],
            color = "black",
            linetype = "dashed"
          ) +
            scale_fill_manual(values = "grey50")
        }
      }
    }
    
    ## custom axis and series labels --------------------------------------------
    if (input$flip_axes == FALSE) {
      if (input$x_label != "") {
        p <- p + labs(x = input$x_label) + coord_cartesian()
      }
      if (input$y_label != "") {
        p <- p + labs(y = "", title = input$y_label) + coord_cartesian()
      }
    } else { # flip x and y axes
      if (input$x_label != "") {
        p <- p + labs(y = input$x_label) + coord_flip()
      } else {
        p <- p + labs(y = input$y) + coord_flip()
      }
      if (input$y_label != "") {
        p <- p + labs(x = "", title = input$y_label) + coord_flip()
      } else {
        p <- p + labs(x = "", title = input$x) + coord_flip()
      }
    }
    
    if (input$z_guide != "" & input$w_guide == "") {
      if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
        p <- p + labs(fill = input$z_guide)
      } else if (input$chart_type %in% c("pointrange", "error bar")) {
        p <- p + labs(color = input$z_guide)
      } else if (input$chart_type %in% c("density", "line", "step", "area")) {
        p <- p + labs(color = input$z_guide, linetype = input$z_guide)
      } else if (input$chart_type == "scatterplot") {
        p <- p + labs(color = input$z_guide, shape = input$z_guide, fill = "")
      }
    } else if (input$w_guide != "" & input$z_guide == "") {
      if (input$chart_type == "scatterplot") {
        p <- p + labs(color = input$w_guide, fill = "")
      } else if (input$chart_type == "heatmap") {
        p <- p + labs(fill = input$w_guide)
      }
    } else if (input$w_guide != "" & input$z_guide != "") {
      p <- p + labs(size = input$w_guide, color = input$z_guide, fill = "")
    }
    
    if (input$x_breaks != "") {
      seq <- as.numeric(unlist(strsplit(input$x_breaks, ",", fixed = TRUE)))
      
      p <- p + scale_x_continuous(
        breaks = seq(from = seq[1], to = seq[2], by = seq[3])
      )
    }
    
    if (input$y_breaks != "") {
      seq <- as.numeric(unlist(strsplit(input$y_breaks, ",", fixed = TRUE)))
      
      p <- p + scale_y_continuous(
        breaks = seq(from = seq[1], to = seq[2], by = seq[3])
      )
    }
    
    if (input$x_val_format != "") {
      switch(input$x_val_format,
             "dollar" = p <- p + scale_x_continuous(labels = scales::dollar),
             "comma" = p <- p + scale_x_continuous(labels = scales::comma),
             "percent" = p <- p + scale_x_continuous(labels = scales::percent)
      )
    }
    if (input$y_val_format != "") {
      switch(input$y_val_format,
             "dollar" = p <- p + scale_y_continuous(labels = scales::dollar),
             "comma" = p <- p + scale_y_continuous(labels = scales::comma),
             "percent" = p <- p + scale_y_continuous(labels = scales::percent)
      )
    }
    
    # suppress gridlines except in case of cleveland plots
    if (!is.null(input[[paste0("scatter_option_grid", plot_opts())]])) {
      if (input[[paste0("scatter_option_grid", plot_opts())]] == FALSE) {
        p <- p + theme_gao + theme(panel.grid = element_blank())
      } else { # scatter_option_grid == TRUE
        p <- p + theme_gao
      }
    } else { # apply default GAO theme for all other plots
      p <- p + theme_gao + theme(panel.grid = element_blank())
    }
    
    # title position adjustments -----------------------------------------------
    if (input$offset_x != "") {
      p <- p + theme(axis.title.x = element_text(hjust = input$offset_x))
    }
    if (input$offset_y != "") {
      p <- p + theme(plot.title = element_text(hjust = input$offset_y))
    }
    if (input$offset_source != "") {
      p <- p + theme(plot.caption = element_text(hjust = input$offset_source))
    }
    p
    
  })
  # plot builder --------------------------------------------------------------
  graph_it <- reactive({
    
    validate(
      need(input$chart_type != "", "please select a chart type")
    )
    # rendering a plot
    req(input$chart_type, input$infile, input$do_plot)

    build_graph()
    # make sure this is updated!
    # generate base plot:

  })

  # observer assumes presence of label block on file upload
  observeEvent(c(input$reorder_x, input$flip_axes), {
    if (input$flip_axes == FALSE) {
      updateTextInput(session, "x_label", value = input$x)
      reset("y_label")
    } else {
      updateTextInput(session, "y_label", value = input$x)
      reset("x_label")
    }

    if (input$flip_axes == TRUE) {
      updateTextInput(session, "y_val_format", label = "x value format")
      updateTextInput(session, "x_val_format", label = "y value format")
    } else {
      updateTextInput(session, "x_val_format", label = "x value format")
      updateTextInput(session, "y_val_format", label = "y value format")
    }
  })

  ## render the plot ------------------------------------------------------------
  output$graph <- renderPlot({
    
    req(input$infile)
    req(input$do_plot)
    
    print ('i am in the output')
    # graph it 
    graph_it()
  })

  # Download zip file ------------------------------------------------------------
  output$bundle <- downloadHandler(
    contentType = "application/zip",
    filename = function() {
      paste("autoggraph-", input$chart_type, ".zip", sep = "")
    },
    content = function(file) {
      svg_out <- tempfile(pattern = "svg_", fileext = ".svg")
      png_out <- tempfile(pattern = "png_", fileext = ".png")
      rds_out <- tempfile(pattern = "plot_object_", fileext = ".rds")
      log_out <- tempfile(pattern = "log_", fileext = ".txt")

      ggsave(svg_out,
        width = input$export_width, height = input$export_height
      )

      ggsave(png_out,
        width = input$export_width, height = input$export_height,
        units = "in", dpi = 300
      )

      write_rds(graph_it(), rds_out, compress = "none")

      write_lines(
        paste(
          "generated by autoggraph v.", app_version, "\r\n",
          "generated on:", Sys.time(), "\r\n",
          "input file:", input$infile$name, "\r\n",
          "size:", input$infile$size, "\r\n",
          "md5:", digest::digest(file = input$infile$datapath, algo = "md5"),
          sep = " ", collapse = "\r\n"
        ),
        log_out
      )

      zip(
        zipfile = file,
        files = c(rds_out, log_out, png_out, svg_out)
      )
    }
  )

  # proof -----------------------------------------------------------------------
  output$proof <- downloadHandler(
    
    filename = "proof.html",
    content = function(file) {
      # Copy the proof file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempproof <- file.path(tempdir(), "proof.Rmd")
      file.copy("proof.Rmd", tempproof, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(data = stored_data$data, plot = graph_it())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempproof,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )

  observeEvent(input$do_plot, {
    
    
    stored_data$data <- set_var_types()
    # Update plot generating label and icon after initial plot is rendered
    updateActionButton(session, "do_plot",
      label = "update plot",
      icon = icon("refresh")
    )
  }, ignoreInit = TRUE)
})
