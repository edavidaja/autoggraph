library(shiny)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)

# server ----------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # plot specific options block based on dynamic ui example -------------------
  # http://shiny.rstudio.com/gallery/dynamic-ui.html
  output$plot_options <- renderUI({
    req(input$chart_type)

    switch(input$chart_type,
      "scatterplot" = 
      list(
        selectInput(inputId = "scatter_smooth", "add a smoother:", 
          choices = c("smoother" = "", "loess", "linear", "quadratic")
          ),
        radioButtons("scatter_smooth_ci", "show confidence interval?", 
          choices = c("yes", "no")
          )
        ),
      "bar" = 
      radioButtons(inputId = "bar_orientation", "orientation:", 
        choices = c("horizontal", "vertical")
        ),
      "pie" = 
      a(p("no. pie charts are the worst."), 
        href = "http://www.businessinsider.com/pie-charts-are-the-worst-2013-6")
      )

  })


  inserted_scatter <- FALSE
  inserted_error <- FALSE
  id <- ''
  limit_id <- ''
  fil  <- ''

  ### code to add and remove smoother and error bars. im trying to do think of a way to refactor this code, as its going to get 
  ### more and more complex (i.e., remove CI on the smoother, other things i can't think of right now). would love to hear some
  ### other thoughts on how to do this
  observeEvent({
    input$chart_type
    input$infile
  },
  {
    print(input$infile$name)
    print(fil)
    if (fil != '' & input$infile$name != fil)
    {

     if (id  != '' | limit_id != '')
     {
       remove_id <- ifelse(id != '', id, limit_id)
       if (id != '')
       {
         updateSelectizeInput(session, inputId = "smoother", "", c('', 'loess', 'linear', 'quadratic'),
          options = list(
            placeholder = 'Select',
            onInitialize = I('function() { this.setValue(""); }')))

       }
       if (limit_id != '')
       {
         updateSelectizeInput(session, 'limit', choices = c(names(graph_data()), ''),
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')))
       }
       print (remove_id)
       removeUI(
         selector = paste0('#', remove_id)
         )}
     }
     fil <<- input$infile$name
   # if the graph switches .... reset!!

   # if they ask for a scatter, added the scatter  
     if (input$chart_type == 'scatterplot' & inserted_scatter == FALSE)
     {
                     # have to put some randomness in the id, otherwise shiny doesnt like it
       id <<- paste0('smoother', floor(runif(1, min=0, max=101)))
       insertUI(
         selector = '#placeholder',
         ui = tags$div(
           selectizeInput('smoother', "choose smoother", c('', 'loess', 'linear', 'quadratic'),
            options = list(
              placeholder = 'Select',
              onInitialize = I('function() { this.setValue(""); }'))), 
           id = id)
         )
       inserted_scatter <<- TRUE

     }
   # once they switch, clear and remove
     else if (inserted_scatter == TRUE & input$chart_type != 'scatterplot')
     {
      updateSelectizeInput(session, inputId = "smoother", "", c('', 'loess', 'linear', 'quadratic'),
       options = list(
         placeholder = 'Select',
         onInitialize = I('function() { this.setValue(""); }')))
      removeUI(
       selector = paste0('#', id)
       )
      inserted_scatter <<- FALSE
    }
    
    if (input$chart_type == 'bar'  & inserted_error == FALSE)
    {
                     # do the same for error bars
     limit_id <<- paste0('limit', floor(runif(1, min=0, max=101)))
     print ('I AM HERE')
     insertUI(
       selector = '#placehold',
       ui = tags$div(
         selectizeInput("limit",
          "",
          choices = c(names(graph_data()), ''),
          options = list(
            placeholder = 'Select',
            onInitialize = I('function() { this.setValue(""); }')
            )                     
          ),
         id = limit_id)
       )

     inserted_error <<- TRUE

   }
   else if (inserted_error == TRUE & input$chart_type != 'bar')
   {
     updateSelectizeInput(session, 'limit', choices = c(names(graph_data()), ''), 
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')))
     removeUI(
       selector = paste0('#', limit_id)
       )
     inserted_error <<- FALSE

   }
 })

  # so i can mess with the assignemt
  values <- reactiveValues()
  # Ingest file -----------------------------------------------------------------
  graph_data <- eventReactive(input$infile, {
    fil <- input$infile
    values <- reactiveValues(data = NULL)
    values$data <- read.csv(fil$datapath)
  })

  # Variable selectors ----------------------------------------------------------
  output$x_variable <- renderUI({

    selectizeInput("x",
     "select your x variable:",
     choices =  names(graph_data()),
     options = list(
       placeholder = 'Please select an option below',
       onInitialize = I('function() { this.setValue(""); }')
       )
     )
  })

  output$y_variable <- renderUI({

    selectizeInput("y",
     "add your y variable:",
     choices = c(names(graph_data()), ''),
     options = list(
       placeholder = 'Please select an option below',
       onInitialize = I('function() { this.setValue(""); }')
       )
     )
  })

  output$z_variable <- renderUI({

    selectizeInput("z",
     "add your z variable:",
     choices = c(names(graph_data()), ''),
     options = list(
       placeholder = 'Please select an option below',
       onInitialize = I('function() { this.setValue(""); }')
       )
     )
  })

  output$w_variable <- renderUI({

    selectizeInput("w",
     "add your w variable:",
     choices = c(names(graph_data()), ''),
     options = list(
       placeholder = 'Please select an option below',
       onInitialize = I('function() { this.setValue(""); }')
       )
     )
  })


  # Graphs ----------------------------------------------------------------
  which_error <- reactive({

    verbage1 <- paste(input$y, '+', input$limit) 
    verbage2 <- paste(input$y, '-', input$limit)
    limits <- aes_string(ymax=verbage1, ymin=verbage2)
    geom_errorbar(limits, position='dodge')

  })

  which_smoother <- reactive({

    if (input$smoother == 'loess')
    {
      geom_smooth(method='loess')
    }
    else if (input$smoother == 'linear')
    {
      geom_smooth(method='lm')
    }


  })


  which_aes <- reactive({

    if (input$x != '' & input$y == '' & input$z == '')
    {
      aes_string(x=input$x)
    }
    # x and z
    else if (input$x != '' & input$y == '' & input$z != '')
    {
      aes_string(x=input$x, fill = input$z)
    }    
    # x and y
    else if (input$x != '' & input$y != '' & input$z == '')
    {
      aes_string(x=input$x, y=input$y)
    }
    else if (input$x != '' & input$y != '' & input$z != '')
    {
      # gotta set out scatterplot to differentiate between fills and colors
      if (input$chart_type == 'scatterplot')
      {
        aes_string(x=input$x, y=input$y, colour=input$z)  
      }
      else
      {
        aes_string(x=input$x, y=input$y, fill=input$z)
      } 
    }
  })


  # added an extra which_geom function for z vars to override the default color
  # changed the these function from switch to if to handle some extra logic
  which_geom_z <- reactive({
    print (input$chart_type)
    
    if (input$chart_type == 'histogram')
    {
      if (! is.null(graph_data()))
      {
        # if its a factor, just count
        if (is.factor(unlist(graph_data()[input$x])) | is.character (unlist(graph_data()[input$x])))
        {
          stat_count()
        }
        # otherwise do a regular histogram
        else
        {
          geom_histogram()
        }
      }
    }
    else if (input$chart_type == 'density')
    {
      print ('DID I GET HERE')    
      geom_density()
    }
    else if (input$chart_type == 'step')
    {
      geom_step()
    }
    else if (input$chart_type == 'line')
    {
      geom_line()
    }
    else if (input$chart_type == 'scatterplot')
    {
      geom_point()
    }
    else if (input$chart_type == 'bar')
    {
      geom_bar(position = "dodge", stat = "identity", fun.y = "mean")
    }

    else if (input$chart_type == 'map')
    {

      geom_map()
    }

  })

  # changed from switch to if to handle extra logic
  which_geom <- reactive({

    if (input$chart_type == 'histogram')
    {
      if (! is.null(graph_data()))
      {
        # if its a factor, just count
        if (is.factor(unlist(graph_data()[input$x])) | is.character (unlist(graph_data()[input$x])))
        {
          stat_count(color = '#044F91', fill = '#044F91')
        }
        # otherwise do a histogram
        else
        {
          geom_histogram(color = '#044F91', fill = '#044F91')
        }
      }
    }
    else if (input$chart_type == 'density')
    {
      print ('DID I GET HERE')    
      geom_density(fill = '#044F91')
    } 
    else if (input$chart_type == 'line')
    {
      geom_line(color = '#044F91')
    }
    else if (input$chart_type == 'step')
    {
      geom_step(color = '#044F91')
    }
    
    else if (input$chart_type == 'scatterplot')
    {
      geom_point(color = '#044F91')
    }
    else if (input$chart_type == 'bar')
    {
      geom_bar(position = "dodge", stat = "identity", fill = '#044F91') 
    }

  })



  output$graph <- renderPlot({

    req(input$x)
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
        if (! is.null(input$smoother))
        {
          if (input$smoother != '')
          {

            p <- p + which_smoother()
          }          
        }
        if (! is.null(input$limit))
        {
          if (input$limit != '')
          {
            p <- p + which_error() 
          }
        }
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