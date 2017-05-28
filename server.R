library(shiny)
library(readr)
library(readxl)
library(ggplot2)
library(stringr)
library(RColorBrewer)

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
    # ),
    )
  # guides(
  #   color = guide_legend(title.position = "top", ncol = 1),
  #   fill = guide_legend(title.position = "top", ncol = 1),
  #   size = guide_legend(title.position = "top", ncol = 1)
  #   )
  )

# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {

  plot_opts <- eventReactive(input$chart_type, {
    print ("plot opts fired")
    as.character(paste0(round(runif(1, 1, 100), 0), "_"))
  })

  # plot specific options block based on dynamic ui example -------------------
  # http://shiny.rstudio.com/gallery/dynamic-ui.html
  output$plot_options <- renderUI({

    req(input$chart_type)
    
    print(plot_opts())
    switch(input$chart_type,
      "scatterplot" = 
        list(
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
        sliderInput(
          inputId = paste0(plot_opts(), "scatter_option_span"),
          "wiggle", min = 0, max = 1, value = .7, step = .1,
          ticks = FALSE
          ),
        radioButtons(
          inputId = paste0(plot_opts(), "scatter_option_se"),
          "confidence interval?",
          choices = c("yes" = TRUE, "no" = FALSE)
          )
        ),
      "pointrange" = 
        list(
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
        ),
      "error bar" = 
        list(
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
        ),
      "pie" = 
       a(
        p("no. pie charts are the worst."), 
        href = "http://www.businessinsider.com/pie-charts-are-the-worst-2013-6"
        ),
      "bar" =
        selectInput(
          inputId = paste0(plot_opts(), "bar_type"),
          "select a bar type",
          choices = c(
            "stacked" = "stack", "clustered" = "dodge", "filled" = "fill"
            )
        ),
        "histogram" =
          numericInput(
            inputId = paste0(plot_opts(), "hist_bins"),
            "number of bins", value = 30
            )
      )
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
  
  # Download file -------------------------------------------------------------
  output$bundle <- downloadHandler(
    contentType = "application/zip",
    filename = function() {
      paste("autoggraph-", input$chart_type, ".zip", sep = "" ) 
    },
    content = function(file) {
      # TODO(ajae): restore vector output after installing svg lite
      # vector_out  <- tempfile(fileext = ".svg")
      raster_out  <- tempfile(fileext = ".png")
      plotobj_out <- tempfile(fileext = ".rds")
      log_out     <- tempfile(fileext = ".txt")

      # ggsave(vector_out, width = 7.58, height = 6.83)
      ggsave(raster_out, width = 7.58, height = 6.83)

      write_rds(graph_it(), plotobj_out, compress = "none")
      # TODO(ajae): write rds checksum into log file if needed
      
      write_lines(
        paste(
          "generated by autoggraph v.", version, "\r\n",
          "generated on:", Sys.time(), "\r\n",
          "input file:", input$infile$name, "\r\n",
          "size:", input$infile$size, "\r\n",
          sep = " ", collapse = "\r\n" 
          ),
        log_out
        )
      # zip(zipfile = file, files = c(vector_out, raster_out, plotobj_out, log_out))
      zip(zipfile = file, files = c(raster_out, plotobj_out, log_out))
    } 
    )

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

  which_aes <- reactive({

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
      aes_string(x = as.name(input$x), fill = as.name(input$z))
    }
    #  x, y and, z
    else if (input$x != "" & input$y != "" & input$z != "" & input$w == "") {
      aes_string(x = as.name(input$x), y = as.name(input$y), fill = as.name(input$z))
    } 
    # x, y, and w
    else if (input$x != "" & input$y != "" & input$z == "" & input$w != "") {
      aes_string(x = as.name(input$x), y = as.name(input$y), colour = as.name(input$w))
    }
    # x, y, z, and w
    else if (input$x != "" & input$y != "" & input$z != "" & input$w != "") {
      aes_string(x = as.name(input$x), y = as.name(input$y), colour = as.name(input$z))
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
          bins = input[[paste0(plot_opts(), "hist_bins")]])
       }
     },
     "density" = geom_density(fill = "#044F91"),
     "line" = geom_line(color = "#044F91"),
     "step" = geom_step(fill = "#044F91"),
     "scatterplot" = geom_point(
        alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]], 
        color = "#044F91"
        ),
     "bar" = {
     if (input$y == "") {  
          geom_bar(position = "stack", color = "#044F91", fill = "#044F91")
        } else {
          geom_bar(position =  "stack", stat = "identity", color = "#044F91", fill = "#044F91")
        } 
     },
     "boxplot" = geom_boxplot(color = "#044F91"),
     "pointrange" = geom_pointrange(
        aes_string(
          ymin = input[[paste0(plot_opts(), "pointrange_lower")]],
          ymax = input[[paste0(plot_opts(), "pointrange_upper")]] 
          )
      ),
     "error bar" = geom_errorbar(
        aes_string(
          ymin = input[[paste0(plot_opts(), "errorbar_lower")]],
          ymax = input[[paste0(plot_opts(), "errorbar_upper")]]
          )
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
      "density" = geom_density(aes_string(color = input$z)),
      "line" = geom_line(aes_string(color = input$z)),
      "step" = geom_step(aes_string(color = input$z)),
      "boxplot" = geom_boxplot(aes_string(fill = input$z)),
      "scatterplot" = geom_point(shape = 21, size = 2, color = "white",
        alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]),
      "bar" = {
        if (input$y == "") {  
          geom_bar(position =  input[[paste0(plot_opts(), "bar_type")]])
        } else {
          geom_bar(position =  input[[paste0(plot_opts(), "bar_type")]], stat = "identity")
        }
      },
    "pointrange" = geom_pointrange(
      aes_string(
        ymin  = input[[paste0(plot_opts(), "pointrange_lower")]],
        ymax  = input[[paste0(plot_opts(), "pointrange_upper")]],
        color = input$z 
        )
      ),
    "error bar" = geom_errorbar(
      aes_string(
        ymin  = input[[paste0(plot_opts(), "errorbar_lower")]],
        ymax  = input[[paste0(plot_opts(), "errorbar_upper")]],
        color = input$z
        )
      ),
    "area" = list(
      geom_area(alpha = .1), 
      geom_line(
        aes_string(color = input$z), size= 1.1, position = "stack")
      )
    )
  })

  which_geom_w <- reactive({

    req(graph_data())

    switch(input$chart_type,
      "scatterplot" =
        geom_point(
          aes_string(fill = input$w),
          shape = 21, 
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

  # plot builder --------------------------------------------------------------
  graph_it <- eventReactive(input$do_plot, {
    # require chart type, data to be loaded, 
    # and an x variable to be selected before
    # rendering a plot
    req(input$chart_type, graph_data(), input$x)
    
    # generate base plot:
    p <- ggplot(data = graph_data()) + which_aes() + labs(y = "", title = input$y)

    # add geom function depending on selected variables
    # only x or x & y
    if (input$z == "" & input$w == "") {
      p <- p + which_geom_xy()
      print ("xy fired")
    }

    # z and no w
    else if (input$z != "" & input$w == "") {
      if (input$z_label == "") {
        p <- p + scale_fill_manual(values = which_palette())    
        p <- p + scale_color_manual(values = which_palette())    
      } else {
        plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
        p <- p + scale_fill_manual(values = which_palette(), labels = plot_labels)    
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)    
      }
      p <- p + which_geom_z()
      print ("z fired")
    }
    
    # w and no z
    else if (input$z == "" & input$w != "") {
      if (input$w_label == "") {
        p <- p + scale_color_gradientn(colors = which_palette())
        p <- p + scale_fill_gradientn(colors = which_palette())
      } else {
        plot_labels <- unlist(strsplit(input$w_label, ",", fixed = TRUE))
        p <- p + scale_color_gradientn(
          colors = which_palette(),
          breaks = c(
            min(graph_data()[input$w], na.rm = TRUE), 
            max(graph_data()[input$w], na.rm = TRUE)
            ),
          labels = c(plot_labels[1], plot_labels[2])
          )
        p <- p + scale_fill_gradientn(
          colors = which_palette(),
          breaks = c(
            min(graph_data()[input$w], na.rm = TRUE), 
            max(graph_data()[input$w], na.rm = TRUE)
            ),
          labels = c(plot_labels[1], plot_labels[2])
          )
      }
      p <- p + which_geom_w()
      print ("w fired")
    }

    # z and w
    else if (input$z!= "" & input$w != "") {

      level_count <- nrow(unique(graph_data()[input$z]))
      if (input$z_label == "") {
        p <- p + scale_fill_manual(values = which_palette())    
        p <- p + scale_color_manual(values = which_palette())    
      } else {
        plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
        p <- p + scale_fill_manual(values = which_palette(), labels = plot_labels)    
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
        p <- p + geom_smooth(method = "lm")
        )
    }
    ## custom labels ----------------------------------------------------------
    ## Skip the custom label block until the labels have been rendered 
    if (input$do_plot > 1) {

      if (input$x_label != "") {
        p <- p + labs(x = input$x_label)
      }
      if (input$y_label != "") {
        p <- p + labs(y = "", title = input$y_label)
      }
      if (input$source_label != "") {
        p <- p + labs(caption = input$source_label)
      }
      if (input$z_guide != "") {
        p <- p + labs(color = input$z_guide) 
        p <- p + labs(fill = input$z_guide)
      }
      if (input$w_guide != "") {
        p <- p + labs(size = input$w_guide) 
        p <- p + labs(color = input$w_guide) 
        p <- p + labs(fill = input$w_guide)
      }
    }
    p <- p + theme_gao
    
    p

  })

  output$graph <- renderPlot({
    graph_it()
  })

})
