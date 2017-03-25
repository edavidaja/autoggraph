library(shiny)
library(readr)
library(readxl)
library(ggplot2)
library(stringr)
# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # output$third_variable <- renderUI({
  #   req(input$z)
  #   
  #   labels <-  unlist(unique(graph_data()[,input$z]))
  #   lapply(1:length(labels), function(i) {
  #     textInput(as.character(labels[i]), paste0(as.character(labels[i]), " factor label"))
  #   })
  #   
  # })
  # plot specific options block based on dynamic ui example -------------------
  # http://shiny.rstudio.com/gallery/dynamic-ui.html
  output$plot_options <- renderUI({

    req(input$chart_type)
    
    plot_opts <<- paste0(round(runif(1, 1, 100), 0), '_')
    switch(input$chart_type,
      "scatterplot" = 
      list(
        selectInput(inputId = paste0(plot_opts, "scatter_option_smooth"), "add a smoother:", choices = c("smoother" = '', "loess", "linear")),
        sliderInput(inputId = paste0(plot_opts, "scatter_option_span"), "wiggle", min = 0, max = 1, value = .7, step = .1, ticks = FALSE),
        checkboxInput(inputId = paste0(plot_opts, "scatter_option_se"), "confidence interval?", value = TRUE)
        ),
      "pointrange" = 
      list(
        selectInput(inputId = paste0(plot_opts, "pointrange_option_lower"), "lower bound:", 
          choices = c('lower bound variable' = '', names(graph_data()))),
        selectInput(inputId = paste0(plot_opts, "pointrange_option_upper"), "upper bound:", 
          choices = c('upper bound variable' = '', names(graph_data())))
        ),
      "error bar" = 
      list(
        selectInput(inputId = paste0(plot_opts, "stat_option_lower"), "lower bound:", 
          choices = c('lower bound variable' = '', names(graph_data()))),
        selectInput(inputId = paste0(plot_opts, "stat_option_upper"), "upper bound:", 
          choices = c('upper bound variable' = '', names(graph_data())))
        ),
      "pie" = 
      a(p("no. pie charts are the worst."), 
        href = "http://www.businessinsider.com/pie-charts-are-the-worst-2013-6"
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
       "add a third variable:",
       choices =  c("third variable (e.g. color)" = "", names(graph_data()))
       ),

      selectInput("w",
       "add a fourth variable:",
       choices =  c("fourth variable (e.g. point size)" = "", names(graph_data()))
       )
      )
  })


  # Graphs ----------------------------------------------------------------

  which_smoother <- reactive({

    switch(input[[paste0(plot_opts, 'scatter_option_smooth')]],
      'loess' = geom_smooth(method = 'loess', span = input[[paste0(plot_opts, 'scatter_option_span')]], se = input[[paste0(plot_opts, 'scatter_option_se')]]),
      'linear' = geom_smooth(method = 'lm')
      )
  })


  which_aes <- reactive({

    if (input$x != '' & input$y == '' & input$z == '')
    {
      aes_string(x=as.name(input$x))
    }
    # x and z
    else if (input$x != '' & input$y == '' & input$z != '')
    {
      aes_string(x=as.name(input$x), fill = as.name(input$z))
    }    
    # x and y
    else if (input$x != '' & input$y != '' & input$z == '')
    {
      aes_string(x=as.name(input$x), y=as.name(input$y))
    }
    else if (input$x != '' & input$y != '' & input$z != '')
    {
      # gotta set out scatterplot to differentiate between fills and colors
      if (input$chart_type == 'scatterplot')
      {
        aes_string(x=as.name(input$x), y=as.name(input$y), colour=as.name(input$z))  
      } else {
        aes_string(x=as.name(input$x), y=as.name(input$y), fill=as.name(input$z))
      } 
    }
  })

  which_geom <- reactive({

    print (sapply(graph_data()[,input$x], class))
    
    req(graph_data())
    
    switch(input$chart_type,
     'histogram' = {
       if (sapply(graph_data()[,input$x], class) %in% c("character", "factor")) {
         print ('ok')
         stat_count(color = '#044F91', fill = '#044F91')
       } else { 
         geom_histogram(color = '#044F91', fill = '#044F91')
       }
     },
     'density' = geom_density(fill = '#044F91'),
     'step' = geom_step(fill = '#044F91'),
     'scatterplot' = geom_point(fill = '#044F91'),
     'bar' = geom_bar(position = 'dodge', stat = "identity", fill = '#044F91'),
     'pointrange' = geom_pointrange(
       aes_string(
         ymin = input[[paste0(plot_opts, 'pointrange_option_lower')]],
         ymax = input[[paste0(plot_opts, 'pointrange_option_upper')]] 
         ), fill = '#044F91'),
     'error bar' = geom_errorbar(
      aes_string(
        ymin = input[[paste0(plot_opts, 'stat_option_lower')]],
        ymax = input[[paste0(plot_opts, 'stat_option_upper')]]
        )
      )
     )
  })
  # added an extra which_geom function for z vars to override the default color
  # changed the these function from switch to if to handle some extra logic
  which_geom_z <- reactive({

    print (sapply(graph_data()[,input$x], class))

    req(graph_data())

    switch(input$chart_type,
      'bar' = geom_bar(position = 'dodge', stat = "identity", fill = '#044F91'),
      'histogram' = {
        if (sapply(graph_data()[,input$x], class) %in% c("character", "factor")) {
          print ('ok')
          stat_count(color = '#044F91', fill = '#044F91')
        } else { 
          geom_histogram(color = '#044F91', fill = '#044F91')
        }
      },
      'density' = geom_density(fill = '#044F91'),
      'column' = geom_col(),
      'line' = geom_line(),
      'step' = geom_step(fill = '#044F91'),
      'boxplot' = geom_boxplot(),
      'scatterplot' = geom_point(fill = '#044F91'),
      'stacked bar' = geom_bar(stat = "identity", position = "stack"),
      'clustered bar' = geom_bar(stat = "identity", position = "dodge"), 
      'pointrange' = geom_pointrange(
        aes_string(
          ymin = input[[paste0(plot_opts, 'pointrange_option_lower')]],
          ymax = input[[paste0(plot_opts, 'pointrange_option_upper')]] 
          ), fill = '#044F91'),
      'error bar' = geom_errorbar(
        aes_string(
          ymin = input[[paste0(plot_opts, 'stat_option_lower')]],
          ymax = input[[paste0(plot_opts, 'stat_option_upper')]]
          ), fill = '#044F91'),
      'hex bins' = geom_hex(fill = '#044F91'),
      'heatmap' = geom_tile(),
      'contour' = geom_contour()
      )
  })



  output$graph <- renderPlot({

    req(input$chart_type, input$x)
    if (! is.null(graph_data()))
    {
      # custom theme ... I think there's a different v of ggplot2 on VDI; plot.caption isn't implemented yet so
      # I commented out for now
      gao_theme <-  theme_minimal() + theme(
        plot.caption = element_text(hjust = 0, size = 6),
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        axis.title.x = element_text(hjust = 0, size = 7, face = "bold"),
        axis.text = element_text(size = 7, face = "bold"),
        panel.grid = element_blank()
        )

      # gao custome palette
      gao_palette <- c('#99CCFF', '#3F9993', '#044F91', '#330033')

      if (input$x != '')
      {

        p <- ggplot(data = graph_data()) + which_aes() + ylab("")

        if (input$z == '')
        {
          p <- p + which_geom()
        }

        else if (input$z != '')
        {
          level_count <- nrow(unique(graph_data()[input$z]))
          # gotta set out scatterplot to differentiate between fills and colors
          if (input$chart_type == 'scatterplot' | input$chart_type == 'step' | input$chart_type == 'line')
          {
            if (input$labels == '')
            {
              p <- p + scale_color_manual(values = gao_palette[1:level_count])        
            }
            else
            {
              plot_labels <- unlist(strsplit(input$labels, ',', fixed = TRUE))
              print(plot_labels)
              p <- p + scale_color_manual(values = gao_palette[1:level_count], labels = plot_labels)       
            }
          }
          else
          {
            if (input$labels == '')
            {
              p <- p + scale_fill_manual(values = gao_palette[1:level_count])    
            }
            else
            {
              plot_labels <- unlist(strsplit(input$labels, ',', fixed = TRUE))
              p <- p + scale_fill_manual(values = gao_palette[1:level_count], labels = plot_labels)    
            }
          }
          p <- p + which_geom_z()
        }

        if (! is.null(input[[paste0(plot_opts, 'scatter_option_smooth')]]))
        {
          if (input[[paste0(plot_opts, 'scatter_option_smooth')]] == 'linear' | input[[paste0(plot_opts, 'scatter_option_smooth')]] == 'loess')
          {
            p <- p + which_smoother()
          }          
        }

        if (input$x_label != '')
        {
          p <- p + xlab(input$x_label)
        }
        if (input$y_label != '')
        {
          p <- p + labs(y = "", title = input$y_label)
        }
        if (input$source_label != '') 
        {
          p <- p + labs(caption = input$source_label)
        }
        print ('successful print')
        p <- p + gao_theme
        p
      }
    }
  })
})