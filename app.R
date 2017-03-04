list.of.packages <- c('shiny', 'readr', 'readxl', 'haven', 'ggplot2', 'dplyr', 'stringr','maps')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)

viz_vocab <- read_csv("data/visual_vocabulary.csv")

ui <- fluidPage(
  # Application title
  titlePanel("tablol", windowTitle = "uninvent the wheel."),
  includeCSS("www/simplex.css"),
  fluidRow(  
    # Sidebar with a slider input for number of bins
    column(3,
     wellPanel(
             # cascading menu system prompts relationship and then chart
       uiOutput("select_relationship"),
       textOutput("desc_text"),
       br(),
       uiOutput("select_chart"),

       fileInput("infile", label = "upload your data (in csv format)"),

       uiOutput("x_variable"),
       uiOutput("y_variable"),
       uiOutput("z_variable"),

       actionButton("go", "Go!", icon = icon("area-chart"))
       )
     ),
    column(6, 
           # Show a plot of the generated distribution
     mainPanel(
       plotOutput("graph")
       )
     ),
    column(3, 
     wellPanel(
      h4("plot labels"),
      textInput("x_label", "x-axis label"),
      textInput("y_label", "y-axis label"),
      textInput("source_label", "source label", placeholder = "Source: GAO analysis of XXX. | GAO-XX-XXX")
      ) 
     )
    ),
  
  hr(),
  fluidRow(
    column(12, 
     a("Inspired by the FT Visual Vocabulary.",
       href="https://github.com/ft-interactive/chart-doctor/raw/master/visual-vocabulary/Visual-vocabulary.pdf")
     )
    )
  )

server <- function(input, output, session) {
  # observe({
  #   if (input$chart_type != 'scatterplot')
  #   {
  #     updateCheckboxGroupInput(session=session, inputId="smoother", choices=c('loess', 'linear', 'quadratic'), label = NULL, selected=NULL)
  #   }
  # })
  # Relationship selectors ------------------------------------------------------
  output$select_relationship <- renderUI({
    selectInput("relationship_type", "what kind of relationship are you depicting?",
      choices = unique(viz_vocab$relationship_type))
  })
  
  output$desc_text <- renderText({
    req(input$relationship_type)
    
    relationship_description <- viz_vocab %>%
    filter(relationship_type == input$relationship_type) %>%
    select(description) %>% unique(.) %>% as.character(.)
    
    (relationship_description)
  })
  
  output$select_chart <- renderUI({

    req(input$relationship_type)
    
    available_charts <- viz_vocab %>%
    filter(relationship_type == input$relationship_type) %>%
    select(chart_type)
    
    selectInput("chart_type", "which chart would you like to use?",
      choices = available_charts)
  })
  
  inserted_scatter <- FALSE
  inserted_error <- FALSE
  id <- ''
  limit_id <- ''

  ### code to add and remove smoother and error bars. im trying to do think of a way to refactor this code, as its going to get 
  ### more and more complex (i.e., remove CI on the smoother, other things i can't think of right now). would love to hear some
  ### other thoughts on how to do this
  observeEvent({
    input$chart_type
    input$infile
  },
  {
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
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }'))), 
         id = id)
       )
     inserted_scatter <<- TRUE

   }
                   # once they switch, clear and remove
   else if (inserted_scatter == TRUE & input$chart_type != 'scatterplot')
   {
    updateSelectizeInput(session, inputId = "smoother", "choose smoother", c('', 'loess', 'linear', 'quadratic'),
     options = list(
       placeholder = 'Please select an option below',
       onInitialize = I('function() { this.setValue(""); }')))
    removeUI(
     selector = paste0('#', id)
     )
    inserted_scatter <<- FALSE
  }
  else if (input$chart_type == 'bar'  & inserted_error == FALSE)
  {
                     # do the same for error bars
   limit_id <<- paste0('limit', floor(runif(1, min=0, max=101)))
   insertUI(
     selector = '#placeholder',
     ui = tags$div(
       selectizeInput("limit",
        "limit",
        choices = c(names(graph_data()), ''),
        options = list(
          placeholder = 'Please select an option below',
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
      onInitialize = I('function() { this.setValue(""); }'),
      server = server))
   removeUI(
     selector = paste0('#', limit_id)
     )
   inserted_error <<- FALSE

 }


}
)

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
    if (! is.null(graph_data()))
    {
      # custom theme ... I think there's a different v of ggplot2 on VDI; plot.caption isn't implemented yet so
      # I commented out for now
      gao_theme <-  theme(
        #plot.caption = element_text(hjust = 0, size = 6),
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        axis.title.x = element_text(hjust = 0, size = 7, face = "bold"),
        axis.text = element_text(size = 7, face = "bold"),
        panel.grid = element_blank()
        )
      
      # gao custome pallette
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
          if (input$chart_type == 'scatterplot')
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
        print (p + gao_theme)
        print ('successful print')
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
