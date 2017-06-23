library(shiny)
library(readr)
library(readxl)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(shinyjs)

# gao theme -------------------------------------------------------------------
theme_gao <- list(
  theme_minimal(),
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(size = 7),
    plot.title = element_text(size = 7, face = "bold"),
    axis.title.x = element_text(hjust = 0, size = 7, face = "bold"),
    axis.text = element_text(size = 7, face = "bold"),
    panel.grid = element_blank()
    )
  )

# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {

  hide('infile_mtime')
  observeEvent(input$infile, {
    js$showFileModified()
    })

  # Ingest file -----------------------------------------------------------------
  output$excel_sheet_selector <- renderUI({

    req(input$infile)

    ext <- tools::file_ext(input$infile$name)
    if (ext %in% c("xls", "xlsx")) {

      file.rename(input$infile$datapath, paste(input$infile$datapath, ext, sep="."))
      selectInput("which_sheet", "select a worksheet:", 
        choices = excel_sheets(paste(input$infile$datapath, ext, sep="."))
        )
    }
  })

  graph_data <- reactive({

    req(input$infile$name)

    ext <- tools::file_ext(input$infile$name)
    if (ext %in% c("xls", "xlsx")) {
      file.rename(input$infile$datapath, paste(input$infile$datapath, ext, sep="."))
      req(input$which_sheet)
      read_excel(paste(input$infile$datapath, ext, sep="."), sheet = input$which_sheet)
    } else if (ext == "csv") {
      read_csv(input$infile$datapath)
    }

  })

    # Variable selectors ----------------------------------------------------------
  output$variable_selector <- renderUI({

    req(graph_data())

    list(
      selectInput("x",
       "select your x variable:",
       choices =  c("x variable" = "", names(graph_data()))
       ),

      selectInput("y",
       "select your y variable:",
       choices =  c("y variable" = "", names(graph_data()))
       ),

      selectInput("z",
       "add an additional discrete variable:",
       choices =  c("discrete variable" = "", names(graph_data()))
       ),

      selectInput("w",
       "add an additional continuous variable:",
       choices =  c("continuous variable" = "", names(graph_data()))
       ),
      conditionalPanel(condition = "input.z != '' | input.w != ''",
        selectInput("palette_selector", label = "select a color palette", 
          choices = c("classic", "qualitative", "sequential", "diverging")
          )
        ),
      actionButton("do_plot", "can i have your autoggraph?", icon = icon("area-chart"))
      )
  })

  # plot specific options block based on dynamic ui example -------------------
  # http://shiny.rstudio.com/gallery/dynamic-ui.html

  plot_opts <- eventReactive(input$chart_type, {
    print ("plot opts fired")
    as.character(paste0(round(runif(1, 1, 100), 0), "_"))
  })

  output$plot_options <- renderUI({

    req(input$chart_type)
    
    switch(input$chart_type,
      "scatterplot" = 
      list(
        wellPanel(
          h4("plot specifics"),
          sliderInput(
            inputId = paste0(plot_opts(), "scatter_option_alpha"), 
            "point transparency", 
            min = 0, max = 1, value = 1, step = .01,
            ticks = FALSE
            ),
          selectInput(
            inputId = paste0(plot_opts(), "scatter_option_smooth"),
            "add a smoother:", 
            choices = c("smoother" = "", "loess", "linear")
            ),
          uiOutput("loess_options")
          )
        ),
      "pointrange" = 
        list(
          wellPanel(
            h4("plot specifics"),
            selectInput(
              inputId = paste0(plot_opts(), "pointrange_lower"),
              "lower bound", 
              choices = c("lower bound" = "", names(graph_data()))
              ),
            selectInput(
              inputId = paste0(plot_opts(), "pointrange_upper"),
              "upper bound", 
              choices = c("upper bound" = "", names(graph_data()))
              )
            )
          ),
      "error bar" = 
        list(
          wellPanel(
            h4("plot specifics"),
            selectInput(
              inputId = paste0(plot_opts(), "errorbar_lower"),
              "lower bound", 
              choices = c("lower bound" = "", names(graph_data()))
              ),
            selectInput(
              inputId = paste0(plot_opts(), "errorbar_upper"),
              "upper bound", 
              choices = c("upper bound" = "", names(graph_data()))
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
            inputId = paste0(plot_opts(), "bar_type"),
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
          inputId = paste0(plot_opts(), "hist_bins"),
          "number of bins", value = 30
          )
        )
        )
      )
})

output$loess_options <- renderUI({

  switch(input[[paste0(plot_opts(), "scatter_option_smooth")]],
    "loess" =
      list(
        sliderInput(
          inputId = paste0(plot_opts(), "scatter_option_span"),
          "wiggle", min = 0, max = 1, value = .7, step = .1,
          ticks = FALSE
          ),
        radioButtons(
          inputId = paste0(plot_opts(), "scatter_option_se"),
          "confidence interval?",
          choices = c("yes" = TRUE, "no" = FALSE),
          inline = TRUE
          )
        )
      )
})

which_palette <- reactive({

  # if a z variable is set, then the level count should be mapped to the 
  # number of levels of the discrete variable; otherwise, a five class
  # palette is used
  if (input$z !=  "") {
    level_count <- nrow(unique(graph_data()[input$z]))
  } else {
    level_count <- 5
  }

  switch(input$palette_selector,
    "classic"     = c("#5EB6E4", "#0039A6", "#008B95", "#5E2750", "#BB9115"),
    "qualitative" = brewer.pal(level_count, "Set2"),
    "sequential"  = brewer.pal(level_count, "Blues"),
    "diverging"   = brewer.pal(level_count, "RdYlBu")
    )
})


  # aesthetics ----------------------------------------------------------------

base_aes <- reactive({

    # return aesthetics based on which combinations of  
    # data input fields are selected
    # x only
  if (input$x != "" & input$y == "" & input$z == "" & input$w == "") {
    aes_string(x = as.name(input$x))
  }
    # x and y
  else if (input$x != "" & input$y != "" & input$z == "" & input$w == "") {
    aes_string(x = as.name(input$x), y = as.name(input$y))
  }
    # x and z
  else if (input$x != "" & input$y == "" & input$z != "" & input$w == "") {
    aes_string(x = as.name(input$x))
  }
    #  x, y and, z
  else if (input$x != "" & input$y != "" & input$z != "" & input$w == "") {
    aes_string(x = as.name(input$x), y = as.name(input$y))
  } 
    # x, y, and w
  else if (input$x != "" & input$y != "" & input$z == "" & input$w != "") {
    aes_string(x = as.name(input$x), y = as.name(input$y))
  }
    # x, y, z, and w
  else if (input$x != "" & input$y != "" & input$z != "" & input$w != "") {
    aes_string(x = as.name(input$x), y = as.name(input$y))
  }
})

  # geometries ----------------------------------------------------------------

which_geom_xy <- reactive({

    # select geom based on selected chart type for the univariate or
    # two-variable case.    
  req(graph_data())

  switch(input$chart_type,
   "histogram" = {
     if (sapply(graph_data()[,input$x], class) %in% c("character", "factor")) {
       stat_count(fill = "#044F91")
     } else {
       geom_histogram(
        fill = "#044F91",
        bins = input[[paste0(plot_opts(), "hist_bins")]]
        )
     }
   },
   "density" = geom_density(fill = "#044F91"),
   "line" = geom_line(color = "#044F91", size = 1.1),
   "step" = geom_step(color = "#044F91"),
   "scatterplot" = geom_point(
    alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]], 
    color = "#044F91"
    ),
   "bar" = {
     if (input$y == "") {  
      geom_bar(position = "stack", fill = "#044F91")
    } else {
      geom_bar(position = "stack", stat = "identity", fill = "#044F91")
    } 
  },
  "boxplot" = geom_boxplot(color = "#044F91"),
  "pointrange" = geom_pointrange(
    aes_string(
      ymin = as.name(input[[paste0(plot_opts(), "pointrange_lower")]]),
      ymax = as.name(input[[paste0(plot_opts(), "pointrange_upper")]]) 
      ),
    color = "#044F91"
    ),
  "error bar" = geom_errorbar(
    aes_string(
      ymin = as.name(input[[paste0(plot_opts(), "errorbar_lower")]]),
      ymax = as.name(input[[paste0(plot_opts(), "errorbar_upper")]])
      ),
    color = "#044F91"
    )
  )
})

which_geom_z <- reactive({

  req(graph_data())

  switch(input$chart_type,
    "histogram" = {
      if (sapply(graph_data()[,input$x], class) %in% c("character", "factor")) {
        stat_count(
          aes_string(fill = as.name(input$z))
          )
      } else { 
        geom_histogram(
          aes_string(fill = as.name(input$z)),
          bins = input[[paste0(plot_opts(), "hist_bins")]]
          )
      }
    },
    "density" = geom_density(
      aes_string(color = input$z, linetype = input$z),
      size = 1.1
      ),
    "line" = geom_line(
      aes_string(color = input$z, linetype = input$z),
      size = 1.1
      ),
    "step" = geom_step(
      aes_string(color = input$z, linetype = input$z),
      size = 1.1
      ),
    "boxplot" = geom_boxplot(aes_string(fill = input$z)),
    "scatterplot" = geom_point(
      aes_string(color = input$z, shape = input$z),
      size = 2,
      alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]
      ),
    "bar" = { 
      if (input$y == "") {  
        geom_bar(
          aes_string(fill = input$z),
          position =  input[[paste0(plot_opts(), "bar_type")]])
      } else {
        geom_bar(
          aes_string(fill = input$z),
          position =  input[[paste0(plot_opts(), "bar_type")]],
          stat = "identity"
          )
      }
    },
    "pointrange" = geom_pointrange(
      aes_string(
        ymin  = as.name(input[[paste0(plot_opts(), "pointrange_lower")]]),
        ymax  = as.name(input[[paste0(plot_opts(), "pointrange_upper")]]),
        color = as.name(input$z) 
        )
      ),
    "error bar" = geom_errorbar(
      aes_string(
        ymin  = as.name(input[[paste0(plot_opts(), "errorbar_lower")]]),
        ymax  = as.name(input[[paste0(plot_opts(), "errorbar_upper")]]),
        color = as.name(input$z)
        )
      ),
    "area" = list(
      geom_area(
        aes_string(fill = input$z),
        alpha = .1
        ), 
      geom_line(
        aes_string(color = input$z, linetype = input$z),
        size= 1.1, position = "stack"
        )
      )
    )
})

which_geom_w <- reactive({

  req(graph_data())

  switch(input$chart_type,
    "scatterplot" =
    geom_point(
      aes_string(color = input$w),
      alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]
      ),
    "heatmap" = geom_tile(
      aes_string(fill = input$w)
      )
    )
})

which_geom_w_z <- reactive({

  req(graph_data())

  geom_point(
    aes_string(
      size = input$w,
      colour = input$z
      ),
    alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]    
    )
})

output$plot_labels <- renderUI({
  req(graph_data())

  wellPanel(
    h4("plot labels"),
    textInput("x_label", "x-axis label"),
    radioButtons("x_val_format", label = "x value format",
      choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE),
    textInput("y_label", "y-axis label"),
    radioButtons("y_val_format", label = "y value format",
      choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE),
    textInput("z_guide", "discrete variable name"),
    textInput("z_label", "discrete variable labels, separated by commas",
      placeholder = "one, two, three, ..."),
    textInput("w_guide", "continuous variable name"),
    textInput("w_label", "continuous variable labels, separated by commas",
      placeholder = "low, high"),
    textInput("source_label", "source label",
      placeholder = "Source: GAO analysis..."),
    h4("export:"),
    downloadButton(outputId = "bundle", label = "results")
    )
})

  # plot builder --------------------------------------------------------------
graph_it <- eventReactive(input$do_plot, {
    # require chart type, data to be loaded, 
    # and an x variable to be selected before
    # rendering a plot
  req(input$chart_type, graph_data(), input$x)

    # generate base plot:
  p <- ggplot(data = graph_data()) + base_aes() + labs(y = "", title = input$y)

    # add geom function depending on selected variables
    # only x or x & y
  if (input$z == "" & input$w == "") {
    p <- p + which_geom_xy()
    print ("xy fired")
  }

    # z and no w
  else if (input$z != "" & input$w == "") {
      # apply color or fill if no custom labels are set based on chart type
    if (input$z_label == "") {
      if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
        p <- p + scale_fill_manual(values = which_palette())    
      } else if (input$chart_type %in% c("density", "line", "step", "scatterplot", "pointrange", "error bar")) {
        p <- p + scale_color_manual(values = which_palette())
      } else if (input$chart_type == "area") {
        p <- p + scale_fill_manual(values = which_palette())    
        p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6))
        p <- p + scale_color_manual(values = which_palette())
      }
    } else {
      plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
      if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
        p <- p + scale_fill_manual(values = which_palette(), labels = plot_labels)
      } else if (input$chart_type %in% c("pointrange", "error bar")) {
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
      } else if (input$chart_type %in% c("density", "line", "step")) {
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
        p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6), labels = plot_labels)
      } else if (input$chart_type == "scatterplot") {
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
        p <- p + scale_shape_manual(values = c(15, 16, 17, 18, 3, 8, 7), labels = plot_labels)
      } else if (input$chart_type == "area") {
        p <- p + scale_fill_manual(values = which_palette(), labels = plot_labels)
        p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6), labels = plot_labels)
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
      }
    }
    p <- p + which_geom_z()
    print ("z fired")
  }

    # w and no z
  else if (input$z == "" & input$w != "") {

    if (input$w_label == "") {
      if (input$chart_type == "scatterplot") {  
        p <- p + scale_color_gradientn(colors = which_palette())
      } else if (input$chart_type == "heatmap") {
        p <- p + scale_fill_gradientn(colors = which_palette())
      }
    } else {
      plot_labels <- unlist(strsplit(input$w_label, ",", fixed = TRUE))
      if (input$chart_type == "scatterplot") {  
        p <- p + scale_color_gradientn(
          colors = which_palette(),
          breaks = c(
            min(graph_data()[input$w], na.rm = TRUE), 
            max(graph_data()[input$w], na.rm = TRUE)
            ),
          labels = c(plot_labels[1], plot_labels[2])
          )
      } else if (input$chart_type == "heatmap") {
        p <- p + scale_fill_gradientn(
          colors = which_palette(),
          breaks = c(
            min(graph_data()[input$w], na.rm = TRUE), 
            max(graph_data()[input$w], na.rm = TRUE)
            ),
          labels = c(plot_labels[1], plot_labels[2])
          )
      }
    }
    p <- p + which_geom_w()
    print ("w fired")
  }

    # z and w
  else if (input$z!= "" & input$w != "") {

    if (input$z_label == "") {
      p <- p + scale_color_manual(values = which_palette())    
    } else {
      plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
      p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)    
    }
    p <- p + which_geom_w_z()
    print ("w & z fired")
  }


    ## additional geom layers -------------------------------------------------
    ## apply smoother to scatter plot

  if (!is.null(input[[paste0(plot_opts(), "scatter_option_smooth")]])) {
    switch(input[[paste0(plot_opts(), "scatter_option_smooth")]],
      "loess" = 
      p <- p + geom_smooth(
        method = "loess", 
        span = input[[paste0(plot_opts(), "scatter_option_span")]], 
        se = input[[paste0(plot_opts(), "scatter_option_se")]]
        ),
      "linear" = 
      p <- p + geom_smooth(
        method = "lm"
        )
      )
  }
    ## custom labels ----------------------------------------------------------

  if (input$x_label != "") {
    p <- p + labs(x = input$x_label)
  }
  if (input$y_label != "") {
    p <- p + labs(y = "", title = input$y_label)
  }
  if (input$source_label != "") {
    p <- p + labs(caption = input$source_label)
  }
  if (input$z_guide != "" & input$w_guide == "") {
    if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
      p <- p + labs(fill = input$z_guide)
    } else if (input$chart_type %in% c("pointrange", "error bar")) {
      p <- p + labs(color = input$z_guide)
    } else if (input$chart_type %in% c("density", "line", "step", "area")) {
      p <- p + labs(color = input$z_guide, linetype = input$z_guide)
    } else if (input$chart_type == "scatterplot") {
      p <- p + labs(color =  input$z_guide, shape = input$z_guide)
    }
  } else if (input$w_guide != "" & input$z_guide == "") {
    if (input$chart_type == "scatterplot") {
      p <- p + labs(color = input$w_guide) 
    } else if (input$chart_type == "heatmap") {
      p <- p + labs(fill = input$w_guide)
    }     
  } else if (input$w_guide != "" & input$z_guide != "") {
    p <- p + labs(size = input$w_guide, color = input$z_guide)
  }
  if (input$x_val_format != "") {
    switch(input$x_val_format,
      "dollar"  = p <- p + scale_x_continuous(labels = scales::dollar),
      "comma"   = p <- p + scale_x_continuous(labels = scales::comma),
      "percent" = p <- p + scale_x_continuous(labels = scales::percent)
      )
  }
  if (input$y_val_format != "") {
    switch(input$y_val_format,
      "dollar"  = p <- p + scale_y_continuous(labels = scales::dollar),
      "comma"   = p <- p + scale_y_continuous(labels = scales::comma),
      "percent" = p <- p + scale_y_continuous(labels = scales::percent)
      )
  }
  p <- p + theme_gao
  p
})

output$graph <- renderPlot({
  graph_it()
})

   # Download file -------------------------------------------------------------
output$bundle <- downloadHandler(
  contentType = "application/zip",
  filename = function() {
    paste("autoggraph-", input$chart_type, ".zip", sep = "" ) 
  },
  content = function(file) {
    vector_out_large <- tempfile(pattern = "vector_large_", fileext = ".svg")
    vector_out_small <- tempfile(pattern = "vector_small_", fileext = ".svg")
    raster_out_large <- tempfile(pattern = "raster_large_", fileext = ".png")
    raster_out_small <- tempfile(pattern = "raster_small_", fileext = ".png")
    plotobj_out      <- tempfile(pattern = "plot_object_", fileext = ".rds")
    log_out          <- tempfile(pattern = "log_", fileext = ".txt")

    ggsave(vector_out_large, width = 7.58, height = 6.83)
    ggsave(vector_out_small, width = 5, height = 4.51)
    ggsave(raster_out_large, width = 7.58, height = 6.83, units = "in", dpi = 600)
    ggsave(raster_out_small, width = 5, height = 4.51, units = "in", dpi = 600)

    write_rds(graph_it(), plotobj_out, compress = "none")

    write_lines(
      paste(
        "generated by autoggraph v.", app_version, "\r\n",
        "generated on:", Sys.time(), "\r\n",
        "input file:", input$infile$name, "\r\n",
        "file last modified:", input$infile_mtime, "\r\n", 
        "size:", input$infile$size, "\r\n",
        sep = " ", collapse = "\r\n" 
        ),
      log_out
      )

    zip(
      zipfile = file,
      files = c(
        plotobj_out, log_out, 
        raster_out_large, raster_out_small,
        vector_out_large, vector_out_small
        )
      )
  } 
  )

observe({
  req(input$do_plot)

    # Update plot generating label and icon after initial plot is rendered
  updateActionButton(session, "do_plot",
    label = "update plot",
    icon = icon("refresh")
    )
})

})
