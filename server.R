library(readr)
library(readxl)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(magrittr)
library(extrafont)
library(dplyr)
library(tidyr)


# gao theme -------------------------------------------------------------------
theme_gao <- list(
  theme_minimal(),
  theme(
    text = element_text(family = "Liberation Sans"),
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

  output$landing_page <- renderUI({
    if(session$clientData$url_hostname == "shiny.eseclab.gov") {
      # set location of zip for export
      # option is set here rather than in global because the sessions variable
      # is only available within the scope of the server function
      Sys.setenv(R_ZIPCMD="/usr/bin/zip")
      Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin")
      includeHTML("www/ins-deploy.html")
    } else {
      includeHTML("www/ins.html")
    }
  })

  # the custom javascript function for recovering the modification time of the
  # uploaded file is executed whenever a file is uploaded
  observeEvent(input$infile, {
    js$showFileModified()
  })

  # bookmarking state ----------------------------------------------------------
  # since the plot_opts id is randomly generated, onBookmark must be used in
  # order to save its state 
  onBookmark(function(state) {
    plot_id             <- plot_opts()
    state$values$id     <- plot_id
    state$values$infile <- fil$infile
  })
  
  fil          <- reactiveValues(infile = NULL)
  original_ops <- reactiveValues(id = NULL, loaded = FALSE, infile = NULL)
  
  # filer commands
  filters <- reactiveValues(ids = NULL, vars = NULL, ops = NULL, condition = NULL)
  
  
  # here is the data that we are going to work with
  stored_data <- reactiveValues(data = NULL, orig_data = NULL)
  
  
  onRestore(function(state) {
    original_ops$id     <- state$values$id
    original_ops$infile <- state$values$infile
  })
  
  # Ingest file -----------------------------------------------------------------
  # a bug in readxl makes it unable to read files directly from the fileInput()
  # so all calls to readxl functions require the use of paste() to append
  # the proper extension to them
  output$excel_sheet_selector <- renderUI({
    
    req(input$infile)
    
    ext <- tools::file_ext(input$infile$name)
    if (ext %in% c("xls", "xlsx")) {
      selectInput("which_sheet", "select a worksheet:", choices = excel_sheets(input$infile$datapath))
      }
  })

  # make.names() is used to coerce all column names to valid R names after using
  # read_csv() or read_excel()
  graph_data <- reactive({
    
    req(input$infile$name)

    ext <- tools::file_ext(input$infile$name)
    if (ext == "xls") {
      req(input$which_sheet)
      temp <- read_xlsx(input$infile$datapath, sheet = input$which_sheet)
      names(temp) %<>% make.names(., unique = TRUE)
      temp
    } else if (ext == "xlsx") {
      req(input$which_sheet)
      temp <- read_xlsx(input$infile$datapath, sheet = input$which_sheet)
      names(temp) %<>% make.names(., unique = TRUE)
      temp
    } else if (ext == "csv") {
      temp <- read_csv(input$infile$datapath)
      names(temp) %<>% make.names(., unique = TRUE)
      stored_data$data <- temp
      stored_data$orig_data <- temp
    }
    stored_data
    
  })
  
  # Variable selectors ----------------------------------------------------------
  # variable selectors are rendered once graph data is uploaded
  # conditional panels are used to display only the relevant input variables
  # for the selected plot type
  output$variable_selector <- renderUI({
    
    req(graph_data()$orig_data)
    
    isolate({

    list(
      conditionalPanel(
        condition = "input.chart_type != '' & input.chart_type != 'pie'",
        selectInput("x",
         "select your x variable:",
         choices =  c("x variable" = "", names(stored_data$data))
         )
        ),
        conditionalPanel(
          condition = "input.chart_type != 'density' & input.chart_type != 'histogram'", 
          selectInput("y",
            "select your y variable:",
            choices =  c("y variable" = "", names(stored_data$data))
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
            choices = c("order by" = "", names(stored_data$data))
          )
        ),
        conditionalPanel(
          condition = "input.chart_type != 'heatmap'",
          selectInput("z",
            "add an additional discrete variable:",
            choices =  c("discrete variable" = "", names(stored_data$data))
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
          condition = "input.wrap == 'grid'",
          radioButtons("free_facet", label = "allow axes to vary",
            choices = c("no" = "fixed", "by x" = "free_x", "by y" = "free_y", "both" = "free"),
            selected = "fixed",
            inline = TRUE
            )
          ),
        conditionalPanel(
          condition = "input.chart_type == 'heatmap' | input.chart_type == 'scatterplot'",
          selectInput("w",
            "add an additional continuous variable:",
            choices =  c("continuous variable" = "", names(stored_data$data))
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
  })

  # plot specific options -----------------------------------------------------
  # plot specific options are assinged an id combining a random number with
  # the name of the option. This randomly generated number changes whenever
  # the selected plot type is changes, and ensures that options specific
  # to one plot do not persist across plot types (smoothers in particular)

  plot_opts <- eventReactive(input$chart_type, {
    
    print ('plot opts fired')

    if(!is.null(original_ops$id) & original_ops$loaded == FALSE) {
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
          checkboxInput(
            inputId = paste0(plot_opts(), "scatter_option_grid"),
            "show gridlines (useful for cleveland dot plots)"
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
              choices = c("lower bound" = "", names(stored_data$data))
              ),
            selectInput(
              inputId = paste0(plot_opts(), "pointrange_upper"),
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
              inputId = paste0(plot_opts(), "errorbar_lower"),
              "lower bound", 
              choices = c("lower bound" = "", names(stored_data$data))
              ),
            selectInput(
              inputId = paste0(plot_opts(), "errorbar_upper"),
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

# options that should be available if any smoother is added to a scatterplot
# TODO(ajae): passing the option to remove the confidence interval to
# method "lm" causes the smoother not to render at all
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

# ideally this span parameter would be a conditional panel hidden in overall
# smoother options, but mapping conditions to dynamically determined id
# names is difficult

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
    level_count <- nrow(unique(stored_data$data[input$z]))
  } else {
    level_count <- 5
  }

  switch(input$palette_selector,
    "classic" = {
      if (input$chart_type %in% c("bar", "boxplot")) {
        validate(
          need(level_count < 6, "The classic GAO palette for this graph type accepts at most 5 classes. Try 'qualitative' instead."
          )
        )
        c("#FFFFFF", "#99CCFF", "#044F91", "#409993", "#330033")
      } else {
        validate(
          need(level_count < 5, "The classic GAO palette for this graph type accepts at most 4 classes. Try 'qualitative' instead."
            )
          )
        c("#99CCFF", "#044F91", "#409993", "#330033")
      }
    },
    "qualitative" = {
      validate(
        need(level_count < 9, "The qualitative palette accepts at most 8 classes."
          )
        )
      brewer.pal(level_count, "Set2")
    },
    "sequential" = {
      validate(
        need(level_count < 10, "The sequential palette accepts at most 9 classes."
          )
        )
      brewer.pal(level_count, "Blues")
    },
    "diverging" = {
      validate(
        need(level_count < 12, "The diverging palette accepts at most 11 classes."
          )
        )
      brewer.pal(level_count, "RdYlBu")
    }
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
    if (input$type_variable != ''){
      
      if (input$type_variable == 'factor'){
        stored_data$data[[input$x]] <- as.factor(as.character(stored_data$data[[input$x]]))
      }
      else if (input$type_variable == 'numeric'){
        stored_data$data[[input$x]] <- as.numeric(as.character(stored_data$data[[input$x]]))
      }
    }
    
    if (input$type_variable_y != ''){
      
      if (input$type_variable_y == 'factor'){
        stored_data$data[[input$y]] <- as.factor(as.character(stored_data$data[[input$y]]))
      }
      else if (input$type_variable_y == 'numeric'){
        stored_data$data[[input$y]] <- as.numeric(as.character(stored_data$data[[input$y]]))
      }
    }

    print (input$type_variable)
    
    if (! is.null(input$factor_order_x)){
      
      if (sapply(stored_data$data[,input$x], class) %in% c("character", "factor")){
        stored_data$data[[input$x]] <- factor(stored_data$data[[input$x]], levels = input$factor_order_x)
      }
    }
    
    if (!is.null(input$factor_order_y)){
      
      if (sapply(stored_data$data[,input$y], class) %in% c("character", "factor")){
        stored_data$data[[input$y]] <- factor(stored_data$data[[input$y]], levels = input$factor_order_y)
      }
    }
    
    if (!is.null(input$factor_order_z)){
      
      if (sapply(stored_data$data[,input$z], class) %in% c("character", "factor")){
        stored_data$data[[input$z]] <- factor(stored_data$data[[input$z]], levels = input$factor_order_z)
      }
    }
    

    
    if (input$reorder_x != ""){
      
      stored_data$data[[input$x]] <- reorder(stored_data$data[[input$x]], stored_data$data[[input$reorder_x]])
    }    

    if (input$x != "" & input$y == "" & input$z == "" & input$w == "") {
      
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
      # 
        aes(x = stored_data$data[[input$x]], y = stored_data$data[[input$y]])
    }
    


  })
  
  # geometries ----------------------------------------------------------------
  
  which_geom_xy <- reactive({
    
    # select geom based on selected chart type for the univariate or
    # two-variable case.    
  req(stored_data$data)

  print( input[[paste0(plot_opts(), "pointrange_lower")]])
  switch(input$chart_type,
   "histogram" = {
     if (sapply(stored_data$data[,input$x], class) %in% c("character", "factor")) {
       stat_count(fill = "#0039A6")
     } else {
       geom_histogram(
        fill = "#0039A6",
        bins = input[[paste0(plot_opts(), "hist_bins")]]
        )
     }
   },
   "density" = geom_density(fill = "#0039A6"),
   "line" = geom_line(color = "#0039A6", size = 1.1),
   "step" = geom_step(color = "#0039A6"),
   "scatterplot" = geom_point(
      alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]],
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
  "pointrange" = geom_pointrange(
    aes_string(
      ymin = input[[paste0(plot_opts(), "pointrange_lower")]],
      ymax = input[[paste0(plot_opts(), "pointrange_upper")]] 
      ),
    color = "#0039A6"
    ),
  "error bar" = geom_errorbar(
    aes_string(
      ymin = input[[paste0(plot_opts(), "errorbar_lower")]],
      ymax = input[[paste0(plot_opts(), "errorbar_upper")]]
      ),
    color = "#0039A6"
    )
  )
})

  which_geom_z <- reactive({
    
    req(stored_data$data)
    
    switch(input$chart_type,
       "histogram" = {
         if (sapply(stored_data$data[,input$z], class) %in% c("character", "factor")) {
           stat_count(
             aes(fill = stored_data$data[[input$z]])
           )
         } else { 
           geom_histogram(
            aes(fill = stored_data$data[[input$z]]),
            bins = input[[paste0(plot_opts(), "hist_bins")]]
           )
         }
       },
       "density" = geom_density(
         aes(
           color    = stored_data$data[[input$z]],
           linetype = stored_data$data[[input$z]]
         ),
         size = 1.1
       ),
       "line" = geom_line(
         aes(
           color    = stored_data$data[[input$z]],
           linetype = stored_data$data[[input$z]]
         ),
         size = 1.1
       ),
       "step" = geom_step(
         aes(
           color    = stored_data$data[[input$z]],
           linetype = stored_data$data[[input$z]]
         ),
         size = 1.1
       ),
       "boxplot" = geom_boxplot(
         aes(
           fill = stored_data$data[[input$z]]
         ),
         color = "black"
       ),
       "scatterplot" = geom_point(
         aes(
           color = stored_data$data[[input$z]],
           shape = stored_data$data[[input$z]]
         ),
         size = 2,
         alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]
       ),
       "bar" = {
         if (input$y == "") {  
          geom_bar(
            aes(
              fill = stored_data$data[[input$z]],
              position =  input[[paste0(plot_opts(), "bar_type")]],
              color = "black"
              )
           )
         } else {
           geom_bar(
             aes(
               fill = stored_data$data[[input$z]],
               position =  input[[paste0(plot_opts(), "bar_type")]],
               stat = "identity",
               color = "black"
             )
           )
         }
       },
       "pointrange" = geom_pointrange(
         aes_string(
           ymin  = stored_data$data[[input[[paste0(plot_opts(), "pointrange_lower")]]]],
           ymax  = stored_data$data[[input[[paste0(plot_opts(), "pointrange_upper")]]]],
           color = stored_data$data[[input$z]]
         )
       ),
       "error bar" = geom_errorbar(
         aes_string(
           ymin  = stored_data$data[[input[[paste0(plot_opts(), "pointrange_lower")]]]],
           ymax  = stored_data$data[[input[[paste0(plot_opts(), "pointrange_upper")]]]],
           color = stored_data$data[[input$z]]
         )
       ),
       "area" = list(
         geom_area(
           aes(
             fill = stored_data$data[[input$z]]
           ),
           alpha = .1
         ), 
         geom_line(
           aes_string(
             color    = stored_data$data[[input$z]],
             linetype = stored_data$data[[input$z]]
           ),
           size = 1.1,
           position = "stack"
         )
       )
    )
  })
  
  which_geom_w <- reactive({
    
    req(stored_data$data)
    
    switch(input$chart_type,
	   "scatterplot" =
	     geom_point(
	       aes_string(color = input$w),
	       alpha = input[[paste0(plot_opts(), "scatter_option_alpha")]]
	     ),
	   "heatmap" = geom_tile(
	     aes_string(fill = stored_data$data[[input$w]])
	   )
    )
  })

which_geom_w_z <- reactive({

  req(stored_data$data)
  
  if (input$wrap == "grid") {
    geom_point(
      aes(
        color = stored_data$data[[input$w]]
      ),
      alpha = stored_data$data[[input[[paste0(plot_opts(), "scatter_option_alpha")]]]]    
    )     
  } else {
    geom_point(
      aes_string(
        size = stored_data$data[[input$w]],
        colour = stored_data$data[[input$z]]
      ),
      alpha = stored_data$data[[input[[paste0(plot_opts(), "scatter_option_alpha")]]]]    
    )    
  }

})  



# plot labels ----------------------------------------------------------------- 
output$plot_labels <- renderUI({
  

  req(stored_data$data)
  

  
  conditionalPanel(
    condition = "input.chart_type != '' & input.chart_type != 'pie'",
    wellPanel(
      h4("plot labels"),
      textInput("x_label", "x-axis label"),
      hidden(
        radioButtons("x_val_format", label = "x value format",
          choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE)
        ),
        radioButtons("type_variable", label = "change the type of variable",
                     choices = c(
                       "keep as is" = "", "factor", "numeric"
                     ), inline = TRUE),
      hidden(
        textInput("x_breaks", "break after every nth point", "")
      ),
      uiOutput("drag_drop_x"),
      textInput("y_label", "y-axis label"),
      hidden(
        radioButtons("y_val_format", label = "y value format",
          choices = c("none" = "", "dollar", "comma", "percent"), inline = TRUE)
        ),
      radioButtons("type_variable_y", label = "change the type of variable",
                   choices = c(
                     "keep as is" = "", "factor", "numeric"
                   ), inline = TRUE),
      hidden(
        textInput("y_breaks", "break after every nth point", "")
      ),
      uiOutput("drag_drop_y"),
      # TODO(ajae): consider whether it is worth restricting the conditions under
      # which the option to flip axes appears
      checkboxInput("flip_axes", "reverse x and y axes"),
      conditionalPanel(condition = "input.z != ''",
        textInput("z_guide", "discrete variable name"),
        textInput("z_label", "discrete variable labels, separated by commas",
          placeholder = "one, two, three, ..."),
        uiOutput("drag_drop_z")
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
        placeholder = "GAO analysis..."),
      textInput("report_number", "report number",
        placeholder = "GAO-XX-XXX"),
      h4("export:"),
      downloadButton(outputId = "bundle", label = "results", inline = TRUE),
      bookmarkButton(inline = TRUE),
      actionButton("fine_tuning", label = "fine tuning", icon = icon("sliders"), inline = TRUE)
      )
    )
})




output$drag_drop_x <- renderUI({
  
  req(input$x)
  req(sapply(stored_data$data[,input$x], class) %in% c("character", "factor"))
  
    choices <-  levels(unique(as.factor(stored_data$data[[input$x]])))

    selectizeInput("factor_order_x", "click and drag to reorder your x variable",
      choices =  choices,
      selected =  choices,
      multiple = TRUE, 
      options = list(plugins = list("drag_drop"))
      
      )
  

})

output$drag_drop_y <- renderUI({
  
  req(input$y)
  req(sapply(stored_data$data[,input$y], class) %in% c("character", "factor"))
  
  choices <-  levels(unique(as.factor(stored_data$data[[input$y]])))
  
  selectizeInput("factor_order_y", "click and drag to reorder your x variable",
                 choices =  choices,
                 selected =  choices,
                 multiple = TRUE, 
                 options = list(plugins = list("drag_drop"))
                 
  )
  
  
})
  
output$drag_drop_z <- renderUI({
    
    req(input$z)
    
    
    if (input$z_label != "") {
      choices <-  unlist(strsplit(input$z_label, ",", fixed = TRUE))
      levels(stored_data$data[[input$z]]) <- choices
    } else {
      choices <- levels(unique(as.factor(stored_data$data[[input$z]])))
    }

    selectizeInput("factor_order_z", "click and drag to reorder your discrete variable:",
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
  })

  observeEvent(input$y, {
    
    toggle("y_breaks",
           condition = (
             class(stored_data$data[[input$y]]) %in% c("double", "integer", "numeric"))
    ) 
  })
  

  # attempting to use the obvious test for numericness does not work here
  # only show the value formatters for x and y if the variables are numeric
  observeEvent(input$x, {

  toggle("x_val_format",
      condition = (
        class(stored_data$data[[input$x]]) %in% c("double", "integer", "numeric"))
        ) 
    })


  
  observeEvent(input$y, {
    toggle("y_val_format",
      condition = (
        class(stored_data$data[[input$y]]) %in% c("double", "integer", "numeric"))
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
    req(input$chart_type, stored_data$data)
    

  
    # generate base plot:
  p <- ggplot(data = stored_data$data) +
    base_aes() +
    labs(y = "", title = input$y,
      caption = paste("Source: ", input$source_label, " | ", input$report_number, sep=""))

    # add geom function depending on selected variables
    # only x or x & y
  if (input$z == "" & input$w == "") {
    p <- p + which_geom_xy()
  }

  ## z and no w ---------------------------------------------------------------
  else if (input$z != "" & input$w == "") {
    
    if (input$wrap == "grid") {
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
          p <- p + guides(color = guide_legend(title.position = "top", ncol = 1 ))
        } else if (input$chart_type == "scatterplot") {
          p <- p + scale_color_manual(values = which_palette())
          p <- p + guides(
            color = guide_legend(input$z, order = 1, title.position = "top", ncol = 1, override.aes = list(alpha = 1, size = 3) ),
            shape = guide_legend(input$z, order = 1, title.position = "top", ncol = 1 ),
            fill  = guide_legend(order = 2)
            )
        } else if (input$chart_type == "area") {
          p <- p + scale_fill_manual(values = which_palette())    
          p <- p + scale_linetype_manual(values = c(1, 2, 3, 4, 5, 6))
          p <- p + scale_color_manual(values = which_palette())
          p <- p + guides(
            fill     = guide_legend(input$z, title.position = "top", ncol = 1),
            color    = guide_legend(input$z, title.position = "top", ncol = 1),
            linetype = guide_legend(input$z, title.position = "top", ncol = 1)
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
          p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
          p <- p + scale_shape_manual(values = c(15, 16, 17, 18, 3, 8, 7), labels = plot_labels)
          p <- p + guides(
            color = guide_legend(input$z, order = 1, title.position = "top", ncol = 1,
              override.aes = list(alpha = 1, size = 3)),
            shape = guide_legend(input$z, order = 1, title.position = "top", ncol = 1 ),
            fill  = guide_legend(order = 2)
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
    print ("w fired")
  }

  ## z and w ------------------------------------------------------------------
  else if (input$z!= "" & input$w != "") {
    if (input$wrap == "grid") {
      if (input$z_label == "") { # grid, default labels
        p <- p + scale_color_gradientn(colors = which_palette())
        p <- p + facet_wrap(as.formula(paste("~", input$z)), scales = input$free_facet)
        p <- p + guides(
          color = guide_colorbar(order = 1, title.position = "top"),
          fill  = guide_legend(order = 2)
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
          fill  = guide_legend(order = 2)
          )
      }
    } else { # z mapped to color
      if (input$z_label == "") { # default labels
        p <- p + scale_color_manual(values = which_palette())
        p <- p + guides(
          color = guide_legend(order = 1, title.position = "top"),
          size  = guide_legend(order = 2, title.position = "top"),
          fill  = guide_legend(order = 3)
          )
      } else { # custom labels
        plot_labels <- unlist(strsplit(input$z_label, ",", fixed = TRUE))
        p <- p + scale_color_manual(values = which_palette(), labels = plot_labels)
        p <- p + guides(
          color = guide_legend(order = 1, title.position = "top"),
          size  = guide_legend(order = 2, title.position = "top"),
          fill  = guide_legend(order = 3)
          )
      }
    }
    p <- p + which_geom_w_z()
    print ("w & z fired")
    }

  ## apply smoother to scatter plot -------------------------------------------
  if (!is.null(input[[paste0(plot_opts(), "scatter_option_smooth")]])) {
    # TO DO YOU CAN JUST CHANGE THIS BACK
    switch(input[[paste0(plot_opts(), "scatter_option_smooth")]],
      "loess" = {
        if (input[[paste0(plot_opts(), "scatter_option_smooth_group")]] == 'groups') {
          p <- p + geom_smooth(
            method = "loess",
            aes(color =  stored_data$data[[input$z]]),          
            span = input[[paste0(plot_opts(), "scatter_option_loess_span")]],           
            se = input[[paste0(plot_opts(), "scatter_option_smooth_se")]],
            linetype = "dashed"
          )
        } else {
          p <- p + geom_smooth(
            method = "loess", 
            aes_string(fill = quote(input$smoother_label)),
            span = input[[paste0(plot_opts(), "scatter_option_loess_span")]], 
            se = input[[paste0(plot_opts(), "scatter_option_smooth_se")]],
            color = "black",
            linetype = "dashed"
            ) + 
            scale_fill_manual(values = "grey50")
        }
        },
      "linear" = {
        if (input[[paste0(plot_opts(), "scatter_option_smooth_group")]] == 'groups') {
          p <- p + geom_smooth(
            method = "lm",
            aes(color =  stored_data$data[[input$z]]),
            linetype = "dashed"
            )
        } else {
          p <- p + geom_smooth(
            method = "lm",
            aes_string(fill = quote(input$smoother_label)),
            color = "black",
            linetype = "dashed"
            ) + 
            scale_fill_manual(values = "grey50")
        }
      }
    )
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
      p <- p + labs(color =  input$z_guide, shape = input$z_guide, fill = "")
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
  
  if (input$x_breaks != ''){
    seq <- as.numeric(unlist(strsplit(input$x_breaks, ",", fixed = TRUE)))
    
    p <- p + scale_x_continuous(breaks = seq(from = seq[1],
                                             to = seq[2],
                                             by = seq[3]))      
  }
  
  if (input$y_breaks != ''){
    seq <- as.numeric(unlist(strsplit(input$y_breaks, ",", fixed = TRUE)))
    
    p <- p + scale_y_continuous(breaks = seq(from = seq[1],
                                             to = seq[2],
                                             by = seq[3]))      
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

  # suppress gridlines except in case of cleveland plots
  if (!is.null(input[[paste0(plot_opts(), "scatter_option_grid")]])) { 
    if (input[[paste0(plot_opts(), "scatter_option_grid")]] == FALSE) {
      p <- p + theme_gao + theme(panel.grid = element_blank())
    } else { # scatter_option_grid == TRUE
      p <- p + theme_gao
    }
  } else { # apply default GAO theme for all other plots
    p <- p + theme_gao + theme(panel.grid = element_blank())
  }
  
  # titile position adjustments -----------------------------------------------
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

# since several function calls are constructed with paste(), observers are used
# to set the label field to the name of the selected variable, which obscures
# the call to "factor()" or the like in the plot assembly
observeEvent(input$z, {
  updateTextInput(session, "z_guide", value = input$z) 
  })

observeEvent(input$w, {
  updateTextInput(session, "w_guide", value = input$w) 
  })

observeEvent(c(input$reorder_x, input$flip_axes), {
  if (input$flip_axes == FALSE) {
    updateTextInput(session, "x_label", value = input$x)
    reset("y_label")
  } else {
    updateTextInput(session, "y_label", value = input$x)
    reset("x_label")
  }
  })

observeEvent(input$flip_axes, {
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
  graph_it()
  })

# Download zip file ------------------------------------------------------------
output$bundle <- downloadHandler(
  contentType = "application/zip",
  filename = function() {
    paste("autoggraph-", input$chart_type, ".zip", sep = "" ) 
  },
  content = function(file) {

    tif_out <- tempfile(pattern = "tif_", fileext = ".tiff")
    svg_out <- tempfile(pattern = "svg_", fileext = ".svg")
    png_out <- tempfile(pattern = "png_", fileext = ".png")
    rds_out <- tempfile(pattern = "plot_object_", fileext = ".rds")
    log_out <- tempfile(pattern = "log_", fileext = ".txt")

    ggsave(svg_out, width = input$export_width, height = input$export_height,
      system_fonts = list(sans = "Liberation Sans"))
    ggsave(tif_out, width = input$export_width, height = input$export_height,
      units = "in", dpi = 300
      )
    ggsave(png_out, width = input$export_width, height = input$export_height,
      units = "in", dpi = 300
      )

    write_rds(graph_it(), rds_out, compress = "none")

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
      files = c(rds_out, log_out, png_out, svg_out, tif_out)
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
      params <- list(data = stored_data$data, plot = graph_it())

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
