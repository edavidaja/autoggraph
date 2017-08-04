library(readr)
library(readxl)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(magrittr)

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
    axis.text = element_text(size = 7, face = "bold"),
    panel.grid = element_blank()
    )
  )

# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {
  output$landing_page <- renderUI({
    if(session$clientData$url_hostname == "shiny.eseclab.gov") {
      # set location of zip for export
      # option is set here rather than in global because the sessions variable
      # is only available within the scope of the server function
      Sys.setenv(R_ZIPCMD="/usr/bin/zip")
      includeHTML("www/ins-deploy.html")
    } else {
      includeHTML("www/ins.html")
    }
  })

  observeEvent(input$infile, {
    js$showFileModified()
    })

  # bookmarking stuff ----------------------------------------------------------
  onBookmark(function(state) {
    plot_id <- plot_opts()
    state$values$id <- plot_id
    print (fil$infile)
    state$values$infile <- fil$infile
  })
  
  fil <- reactiveValues(infile = NULL)  
  original_ops <- reactiveValues(id = NULL, loaded = FALSE, infile = NULL)
 
  onRestore(function(state) {
    original_ops$id <- state$values$id
    original_ops$infile <- state$values$infile
    print (original_ops$infile)
  })
  
  # Ingest file -----------------------------------------------------------------
  output$excel_sheet_selector <- renderUI({

    req(input$infile)

    ext <- tools::file_ext(input$infile$name)
    if (ext %in% c("xls", "xlsx")) {
      if (! is.null(original_ops$infile)) {
        print (original_ops$infile)
        selectInput("which_sheet", "select a worksheet:", 
                    choices = excel_sheets(paste(original_ops$infile$datapath, ext, sep=".")))             
      } else {
        file.rename(input$infile$datapath, paste(input$infile$datapath, ext, sep="."))
        fil$infile <- input$infile
        selectInput("which_sheet", "select a worksheet:", 
          choices = excel_sheets(paste(input$infile$datapath, ext, sep=".")))          
      }
    }
  })

  graph_data <- reactive({

    req(input$infile$name)

    ext <- tools::file_ext(input$infile$name)
    if (ext %in% c("xls", "xlsx")) {
      
      req(input$which_sheet)
      if (!is.null(original_ops$infile)) {
        temp <- read_excel(
          paste(original_ops$infile$datapath, ext, sep="."),
          sheet = input$which_sheet)
        names(temp) %<>% make.names(., unique = TRUE)
        temp
      } else {
        file.rename(input$infile$datapath, paste(input$infile$datapath, ext, sep="."))        
        temp <- read_excel(
          paste(input$infile$datapath, ext, sep="."),
          sheet = input$which_sheet)
        names(temp) %<>% make.names(., unique = TRUE)
        temp
      }
    } else if (ext == "csv") {
      temp <- read_csv(input$infile$datapath)
      names(temp) %<>% make.names(., unique = TRUE)
      temp
    }

  })

    # Variable selectors ----------------------------------------------------------
  output$variable_selector <- renderUI({

    req(graph_data(), input$chart_type, input$chart_type != "pie")

    list(
      selectInput("x",
       "select your x variable:",
       choices =  c("x variable" = "", names(graph_data()))
       ),
      conditionalPanel(
        condition = "input.chart_type != 'density' & input.chart_type != 'histogram'", 
        selectInput("y",
          "select your y variable:",
          choices =  c("y variable" = "", names(graph_data()))
          )
        ),
     conditionalPanel(
        condition = 
          "(input.z != '' | input.w != '' | input.y != '') &
          !(input.chart_type == 'line' | input.chart_type == 'density' |
          input.chart_type == 'histogram' | input.chart_type == 'step' |
          input.chart_type == 'area') &
          input.x != ''",
        selectInput("reorder_x", label = "reorder your x axis", 
          choices = c("order by" = "", names(graph_data()))
        )
      ),
      conditionalPanel(
        condition = "input.chart_type != 'heatmap'",
        selectInput("z",
          "add an additional discrete variable:",
          choices =  c("discrete variable" = "", names(graph_data()))
          )
        ),
      conditionalPanel(
        condition = "input.z != ''",
        radioButtons("wrap", label = "group your variables by", 
          choices = c("color", "grid"),
          selected = "color",
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.chart_type == 'heatmap' | input.chart_type == 'scatterplot'",
        selectInput("w",
          "add an additional continuous variable:",
          choices =  c("continuous variable" = "", names(graph_data()))
          )
        ),
      conditionalPanel(
        condition = "input.z != '' | input.w != ''",
        selectInput("palette_selector", label = "select a color palette", 
          choices = c("classic", "qualitative", "sequential", "diverging"),
          selected = "classic"
          )
        ),
      actionButton("do_plot", "can i have your autoggraph?", icon = icon("area-chart"))
      )
  })

  # plot specific options block based on dynamic ui example -------------------
  # http://shiny.rstudio.com/gallery/dynamic-ui.html

  plot_opts <- eventReactive(input$chart_type, {
    print ("plot opts fired")
    if(! is.null(original_ops$id) & original_ops$loaded == FALSE) {
      original_ops$loaded <- TRUE
      original_ops$id
    } else {
      as.character(paste0(round(runif(1, 1, 100), 0), "_"))
    }
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
          uiOutput("smoother_options"),
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

output$smoother_options <- renderUI({
  
  req(input[[paste0(plot_opts(), "scatter_option_smooth")]])
  list(
    radioButtons(
      inputId = paste0(plot_opts(), "scatter_option_smooth_group"),
      "smooth over:",
      selected = "overall",
      choices = c("overall", "groups"),
      inline = TRUE
    ),
    radioButtons(
      inputId  = paste0(plot_opts(), "scatter_option_smooth_se"),
      "confidence interval?",
      choices = c("yes" = TRUE, "no" = FALSE),
      inline = TRUE
      )
    )
})

output$loess_options <- renderUI({

  req(input[[paste0(plot_opts(), "scatter_option_smooth")]] == "loess")

  sliderInput(
    inputId = paste0(plot_opts(), "scatter_option_loess_span"),
    "wiggle", min = 0, max = 1, value = .7, step = .1,
    ticks = FALSE
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

  # check whether the number of classes exceeds
  validate(
    need(level_count < 10, "you have selected a variable with too many classes"
      )
    )

  switch(input$palette_selector,
    "classic" = {
      if (input$chart_type %in% c("bar", "boxplot")) {
        c("#FFFFFF", "#5EB6E4", "#0039A6", "#008B95", "#5E2750")
      } else {
        c("#5EB6E4", "#0039A6", "#008B95", "#5E2750")
      }
    },
    "qualitative" = brewer.pal(level_count, "Set2"),
    "sequential"  = brewer.pal(level_count, "Blues"),
    "diverging"   = brewer.pal(level_count, "RdYlBu")
    )

})

# based on the selected chart type and variables selected within that chart type,
# this observer sets the color palette to the option most likely to be
# appropriate
observeEvent({c(input$w, input$z)}, {
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
})


  # aesthetics ----------------------------------------------------------------

base_aes <- reactive({

    # return aesthetics based on which combinations of  
    # data input fields are selected
    # x only
  if (input$x != "" & input$y == "" & input$z == "" & input$w == "") {
    aes_string(x = input$x)
  }
    # x and y
  else if (input$x != "" & input$y != "" & input$z == "" & input$w == "") {
    aes_string(x = input$x, y = input$y)
    if (input$reorder_x != '') {
      aes_string(
        x = paste0("reorder(",  input$x,", ", input$reorder_x, ")"),
        y =  input$y
        )
    } else {
      aes_string(x = input$x, y = input$y)
    }
  }
    # x and z
  else if (input$x != "" & input$y == "" & input$z != "" & input$w == "") {
    aes_string(x = input$x)
  }
    #  x, y and, z
  else if (input$x != "" & input$y != "" & input$z != "" & input$w == "") {
    aes_string(x = input$x, y = input$y)
    if (input$reorder_x != '') {
      aes_string(
        x = paste0("reorder(",  input$x,", ", input$reorder_x, ")"),
        y =  input$y
        )
    } else {
      aes_string(x = input$x, y = input$y)
    }
  } 
    # x, y, and w
  else if (input$x != "" & input$y != "" & input$z == "" & input$w != "") {
    aes_string(x = input$x, y = input$y)
    if (input$reorder_x != '') {
      aes_string(
        x = paste0("reorder(",  input$x,", ", input$reorder_x, ")"),
        y =  input$y
        )
    } else {
      aes_string(x = input$x, y = input$y)
    }
  }
    # x, y, z, and w
  else if (input$x != "" & input$y != "" & input$z != "" & input$w != "") {
    aes_string(x = input$x, y = input$y)
    if (input$reorder_x != '') {
      aes_string(
        x = paste0("reorder(",  input$x,", ", input$reorder_x, ")"),
        y =  input$y
        )
    } else {
      aes_string(x = input$x, y = input$y)
    }
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
      ymin = input[[paste0(plot_opts(), "pointrange_lower")]],
      ymax = input[[paste0(plot_opts(), "pointrange_upper")]] 
      ),
    color = "#044F91"
    ),
  "error bar" = geom_errorbar(
    aes_string(
      ymin = input[[paste0(plot_opts(), "errorbar_lower")]],
      ymax = input[[paste0(plot_opts(), "errorbar_upper")]]
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
          aes_string(
            fill = paste("factor(", input$z, ")")
            )
          )
      } else { 
        geom_histogram(
          aes_string(
            fill = paste("factor(", input$z, ")")
            ),
          bins = input[[paste0(plot_opts(), "hist_bins")]]
          )
      }
    },
    "density" = geom_density(
      aes_string(
        color    = paste("factor(", input$z, ")"),
        linetype = paste("factor(", input$z, ")")
        ),
      size = 1.1
      ),
    "line" = geom_line(
      aes_string(
        color    = paste("factor(", input$z, ")"),
        linetype = paste("factor(", input$z, ")")
        ),
      size = 1.1
      ),
    "step" = geom_step(
      aes_string(
        color    = paste("factor(", input$z, ")"),
        linetype = paste("factor(", input$z, ")")
        ),
      size = 1.1
      ),
    "boxplot" = geom_boxplot(
      aes_string(
        fill = paste("factor(", input$z, ")")
        ),
      color = "black"
      ),
    "scatterplot" = geom_point(
      aes_string(
        color = paste("factor(", input$z, ")"),
        shape = paste("factor(", input$z, ")")
        ),
      size = 2,
      alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]
      ),
    "bar" = { 
      if (input$y == "") {  
        geom_bar(
          aes_string(fill = paste("factor(", input$z, ")")),
          position =  input[[paste0(plot_opts(), "bar_type")]],
          color = "black"
          )
      } else {
        geom_bar(
          aes_string(fill = paste("factor(", input$z, ")")),
          position =  input[[paste0(plot_opts(), "bar_type")]],
          stat = "identity",
          color = "black"
          )
      }
    },
    "pointrange" = geom_pointrange(
      aes_string(
        ymin  = input[[paste0(plot_opts(), "pointrange_lower")]],
        ymax  = input[[paste0(plot_opts(), "pointrange_upper")]],
        color = paste("factor(", input$z, ")")
        )
      ),
    "error bar" = geom_errorbar(
      aes_string(
        ymin  = input[[paste0(plot_opts(), "errorbar_lower")]],
        ymax  = input[[paste0(plot_opts(), "errorbar_upper")]],
        color = paste("factor(", input$z, ")")
        )
      ),
    "area" = list(
      geom_area(
        aes_string(
          fill = paste("factor(", input$z, ")")
          ),
        alpha = .1
        ), 
      geom_line(
        aes_string(
          color    = paste("factor(", input$z, ")"),
          linetype = paste("factor(", input$z, ")")
          ),
        size = 1.1,
        position = "stack"
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
  
  if (input$wrap == "grid") {
    geom_point(
      aes_string(
        color = input$w
      ),
      alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]    
    )     
  } else {
    geom_point(
      aes_string(
        size = input$w,
        colour = input$z
      ),
      alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]    
    )    
  }

})

output$plot_labels <- renderUI({
  req(graph_data(), input$chart_type, input$chart_type != "pie")

  wellPanel(
    h4("plot labels"),
    textInput("x_label", "x-axis label"),
    hidden(
      radioButtons("x_val_format", label = "x value format",
        choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE)
      ),
    textInput("y_label", "y-axis label"),
    hidden(
      radioButtons("y_val_format", label = "y value format",
        choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE)
      ),
    conditionalPanel(condition = "input.z != ''",
      textInput("z_guide", "discrete variable name"),
      textInput("z_label", "discrete variable labels, separated by commas",
        placeholder = "one, two, three, ...")
      ),
    conditionalPanel(
      condition = "input.chart_type == 'scatterplot'",
      textInput("smoother_label", "overall smoother label",
        placeholder = "smoothed y on x")
      ),
    conditionalPanel(
      condition = "(input.chart_type == 'heatmap' | input.chart_type == 'scatterplot') &
      input.w != ''",
      textInput("w_guide", "continuous variable name"),
      textInput("w_label", "continuous variable labels, separated by commas",
        placeholder = "low, high")
      ),
    textInput("source_label", "source label",
      placeholder = "Source: GAO analysis..."),
    h4("export:"),
    downloadButton(outputId = "bundle", label = "results", inline = TRUE),
    bookmarkButton(inline = TRUE),
    actionButton("fine_tuning", label = "fine tuning", icon = icon("sliders"), inline = TRUE)
    )
})
  # attempting to use the obvious test for numericness does not work here
  observeEvent(input$x, {
    toggle("x_val_format",
      condition = (
        class(graph_data()[[input$x]])) %in% c("double", "integer", "numeric")
        )
  })

  observeEvent(input$y, {
    toggle("y_val_format",
      condition = (
        class(graph_data()[[input$y]])) %in% c("double", "integer", "numeric")
        )
  })

  observeEvent(input$fine_tuning, toggle("fine_tuning_well"))

  output$preview <- downloadHandler(
  filename = function() {
    paste("preview.png") 
  },
  content = function(file) {
    ggsave(file, plot = graph_it(), device = "png",
      width = input$export_width, height = input$export_height)
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
    
    if (input$wrap == "grid") {
      if (input$z_label == "") {
        p <- p + which_geom_xy() + facet_wrap(as.formula(paste("~", input$z)))
      } else {
        plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
        label_wrap <- function(variable, value) {
          unlist(strsplit(input$z_label, ",", fixed = TRUE))
        }  
        p <- p + which_geom_xy() + facet_wrap(as.formula(paste("~", input$z)), labeller = label_wrap)
      }
    } else {
      # apply color or fill if no custom labels are set based on chart type
      if (input$z_label == "") {
        if (input$chart_type %in% c("histogram", "boxplot", "bar")) {
          p <- p + scale_fill_manual(values = which_palette())
          p <- p + guides(fill = guide_legend(title.position = "top", ncol = 1))
        } else if (input$chart_type %in% c("density", "line", "step", "pointrange", "error bar")) {
          p <- p + scale_color_manual(values = which_palette())
          p <- p + guides(color = guide_legend(title.position = "top", ncol = 1 ))
        } else if (input$chart_type == "scatterplot") {
          p <- p + scale_color_manual(values = which_palette())
          p <- p + guides(
          color = guide_legend(
            input$z,
            order = 1,
            title.position = "top",
            ncol = 1,
            override.aes = list(alpha = 1, size = 3)
            ),
          shape = guide_legend(
            input$z,
            order = 1,
            title.position = "top",
            ncol = 1
            ),
          linetype = guide_legend(order = 2)
          )
        } else if (input$chart_type == "area") {
          p <- p + scale_fill_manual(values = which_palette())    
          p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6))
          p <- p + scale_color_manual(values = which_palette())
          p <- p + guides(fill = guide_legend(title.position = "top", ncol = 1))
        }
      } else {
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
          p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
          p <- p + scale_shape_manual(values = c(15, 16, 17, 18, 3, 8, 7), labels = plot_labels)
          p <- p + guides(
            color = guide_legend(input$z,
              order = 1,
              title.position = "top",
              ncol = 1,
              override.aes = list(alpha = 1, size = 3)
              ),
            shape = guide_legend(input$z,
              order = 1,
              title.position = "top",
              ncol = 1
              ),
            linetype = guide_legend(order = 2)
            )
        } else if (input$chart_type == "area") {
          p <- p + scale_fill_manual(values = which_palette(), labels = plot_labels)
          p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6), labels = plot_labels)
          p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
          p <- p + guides(
            fill     = guide_legend(input$z, title.position = "top", ncol = 1),
            color    = guide_legend(input$z, title.position = "top", ncol = 1),
            linetype = guide_legend(input$z, title.position = "top", ncol = 1)
            )
        }
      }
      p <- p + which_geom_z()
    }
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
    if (input$wrap == "grid") {
      
      if (input$z_label == "") {
        p <- p + facet_wrap(as.formula(paste("~", input$z)))
      } else {
        plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
        label_wrap <- function(variable, value) {
          unlist(strsplit(input$z_label, ",", fixed = TRUE))
        }  
        p <- p + which_geom_xy() + facet_wrap(as.formula(paste("~", input$z)), labeller = label_wrap)
      }
    } else {
      if (input$z_label == "") {
        p <- p + scale_color_manual(values = which_palette())    
      } else {
        plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)    
      }
    }
    p <- p + which_geom_w_z()
    print ("w & z fired")   
}


    ## additional geom layers -------------------------------------------------
    ## apply smoother to scatter plot

  if (!is.null(input[[paste0(plot_opts(), "scatter_option_smooth")]])) {
    
    switch(input[[paste0(plot_opts(), "scatter_option_smooth")]],
      "loess" = {
        if (input[[paste0(plot_opts(), "scatter_option_smooth_group")]] == 'groups') {
          p <- p + geom_smooth(
            method = "loess",
            span = input[[paste0(plot_opts(), "scatter_option_loess_span")]],
            se = input[[paste0(plot_opts(), "scatter_option_smooth_se")]],
            aes_string(color = paste("factor(", input$z, ")"))
          )
        } else {
          p <- p + geom_smooth(
            method = "loess", 
            span = input[[paste0(plot_opts(), "scatter_option_loess_span")]], 
            se = input[[paste0(plot_opts(), "scatter_option_smooth_se")]],
            aes_string(linetype = quote(input$smoother_label))
          )            
        }
        },
      "linear" = {
        if (input[[paste0(plot_opts(), "scatter_option_smooth_group")]] == 'groups') {
          p <- p + geom_smooth(
            method = "lm",
            aes_string(color = paste("factor(", input$z, ")"))
            )
        } else {
          p <- p + geom_smooth(
            method = "lm",
            aes_string(linetype = quote(input$smoother_label))
            )
        }
      }
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
      p <- p + labs(color =  input$z_guide, shape = input$z_guide, linetype = "")
    }
  } else if (input$w_guide != "" & input$z_guide == "") {
    if (input$chart_type == "scatterplot") {
      p <- p + labs(color = input$w_guide, linetype = "") 
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
  
  # after you set the theme, look for any offsets
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

# Using paste() results in "factor()" appearing in the z variable by default
# this observer sets the value of the z-guide to the name of the variable
# selected in Z once a variable is selected
observeEvent(input$z, {
  updateTextInput(session, "z_guide", value = input$z) 
  })

observeEvent(input$reorder_x, {
  updateTextInput(session, "x_label", value = input$x) 
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
    vector_out       <- tempfile(pattern = "vector_", fileext = ".svg")
    raster_out       <- tempfile(pattern = "raster_", fileext = ".png")
    plotobj_out      <- tempfile(pattern = "plot_object_", fileext = ".rds")
    log_out          <- tempfile(pattern = "log_", fileext = ".txt")

    ggsave(vector_out, width = input$export_width, height = input$export_height)
    ggsave(raster_out, width = input$export_width, height = input$export_height,
      units = "in", dpi = 600
      )

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
      files = c(plotobj_out, log_out, raster_out, vector_out)
      ) 
  })

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
      params <- list(data = graph_data(), plot = graph_it())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempproof, output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )

observeEvent(input$do_plot, {
  
  req(input$do_plot)
  
  # Update plot generating label and icon after initial plot is rendered
  updateActionButton(session, "do_plot",
    label = "update plot",
    icon = icon("refresh")
    )
  })

})
