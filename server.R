library(shiny)
library(readr)
library(readxl)
library(ggplot2)

# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # plot specific options block based on dynamic ui example -------------------
  # http://shiny.rstudio.com/gallery/dynamic-ui.html
  output$plot_options <- renderUI({
    req(input$chart_type)

    switch(input$chart_type,
      "scatterplot" = 
      list(
        selectInput(inputId = "scatter_option_smooth", "add a smoother:", choices = c("smoother" = '', "loess", "linear")),
        sliderInput(inputId = "scatter_option_span", "wiggle", min = 0, max = 1, value = .7, step = .1, ticks = FALSE),
        checkboxInput(inputId = "scatter_option_se", "confidence interval?", value = TRUE)
        ),
      "bar" = radioButtons(inputId = "dynamic", "error bars:", choices = c('', names(graph_data()))),
      "pie" = a(p("no. pie charts are the worst."), href = "http://www.businessinsider.com/pie-charts-are-the-worst-2013-6")
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

    req(input$infile)

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
  which_error <- reactive({

    verbage1 <- paste(input$y, '+', input$dynamic) 
    verbage2 <- paste(input$y, '-', input$dynamic)
    limits <- aes_string(ymax=verbage1, ymin=verbage2)
    print ('made it to the error')
    geom_errorbar(limits, position='dodge')

  })

  which_smoother <- reactive({

    switch(input$scatter_option_smooth,
      'loess' = geom_smooth(method = 'loess', span = input$scatter_option_span, se = input$scatter_option_se),
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


  # added an extra which_geom function for z vars to override the default color
  # changed the these function from switch to if to handle some extra logic
  which_geom_z <- reactive({
    print (input$chart_type)

    req(graph_data())
    
    switch(input$chart_type,
      # bracket notation in switch is a bit awkward, but I think it's worth 
      # the trade in terms of compactness
      'histogram' = {
        if (class(graph_data()$input$x) %in% c("character", "factor")) {
          stat_count()
        } else { 
          geom_histogram()
        }
      },
      'density' = geom_density(),
      'step' = geom_step(),
      'scatterplot' = geom_point(),
      'bar' = geom_bar(position = 'dodge', stat = "identity", fun.y = "mean")
      )


    # if (input$chart_type == 'histogram')
    # {
    #   if (! is.null(graph_data()))
    #   {
    #     # if its a factor, just count
    #     if (is.factor(unlist(graph_data()[input$x])) | is.character (unlist(graph_data()[input$x])))
    #     {
    #       stat_count()
    #     }
    #     # otherwise do a regular histogram
    #     else
    #     {
    #       geom_histogram()
    #     }
    #   }
    # }
    # else if (input$chart_type == 'density')
    # {
    #   print ('DID I GET HERE')    
    #   geom_density()
    # }
    # else if (input$chart_type == 'step')
    # {
    #   geom_step()
    # }
    # else if (input$chart_type == 'line')
    # {
    #   geom_line()
    # }
    # else if (input$chart_type == 'scatterplot')
    # {
    #   geom_point()
    # }
    # else if (input$chart_type == 'bar')
    # {
    #   geom_bar(position = "dodge", stat = "identity", fun.y = "mean")
    # }

    # else if (input$chart_type == 'map')
    # {

    #   geom_map()
    # }

  })

  # changed from switch to if to handle extra logic
  which_geom <- reactive({

    print (input$chart_type)

    req(graph_data())
    
    switch(input$chart_type,
      'histogram' = {
        if (class(graph_data()$input$x) %in% c("character", "factor")) {
          stat_count(color = '#044F91', fill = '#044F91')
        } else { 
          geom_histogram(color = '#044F91', fill = '#044F91')
        }
      },
      'density' = geom_density(fill = '#044F91'),
      'step' = geom_step(fill = '#044F91'),
      'scatterplot' = geom_point(fill = '#044F91'),
      'bar' = geom_bar(position = 'dodge', stat = "identity", fill = '#044F91')
      )


    # if (input$chart_type == 'histogram')
    # {
    #   if (! is.null(graph_data()))
    #   {
    #     # if its a factor, just count
    #     if (is.factor(unlist(graph_data()[input$x])) | is.character (unlist(graph_data()[input$x])))
    #     {
    #       stat_count(color = '#044F91', fill = '#044F91')
    #     }
    #     # otherwise do a histogram
    #     else
    #     {
    #       geom_histogram(color = '#044F91', fill = '#044F91')
    #     }
    #   }
    # }
    # else if (input$chart_type == 'density')
    # {
    #   print ('DID I GET HERE')    
    #   geom_density(fill = '#044F91')
    # } 
    # else if (input$chart_type == 'line')
    # {
    #   geom_line(color = '#044F91')
    # }
    # else if (input$chart_type == 'step')
    # {
    #   geom_step(color = '#044F91')
    # }
    
    # else if (input$chart_type == 'scatterplot')
    # {
    #   geom_point(color = '#044F91')
    # }
    # else if (input$chart_type == 'bar')
    # {
    #   geom_bar(position = "dodge", stat = "identity", fill = '#044F91') 
    # }

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
      gao_pallete <- c('#99CCFF', '#3F9993', '#044F91', '#330033')

      if (input$x != '')
      {
        p <- ggplot(data = graph_data()) + which_aes()
        if (input$z == '')
        {
          p <- p + which_geom()
        }
        else if (input$z != '')
        {
          stop <- nrow(unique(graph_data()[input$z]))
          # gotta set out scatterplot to differentiate between fills and colors
          if (input$chart_type == 'scatterplot' | input$chart_type == 'step' | input$chart_type == 'line')
          {
            p <- p + scale_color_manual(values = gao_pallete[1:stop])        
          }
          else
          {
            p <- p + scale_fill_manual(values = gao_pallete[1:stop])    
          }
          p <- p + which_geom_z()
        }

        if (input$scatter_option_smooth == 'linear' | input$scatter_option_smooth == 'loess')
        {
          p <- p + which_smoother()
        }
        # print (input$dynamic)
        print (names(graph_data()))
        # if (input$dynamic %in% names(graph_data()))
        # {
        #   p <- p + which_error() 
        # }
        
        if (input$x_label != '')
        {
          p <- p + xlab(input$x_label)
        }
        if (input$y_label != '')
        {
          p <- p + ylab(input$y_label)
        }
        print (p + gao_theme)
        print ('successful print')
      }
    }
  })
})