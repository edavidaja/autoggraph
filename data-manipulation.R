  source("logit.R", local = TRUE)

  # set up a counter for dynamic reshaping ----------------------------------

  counter <- reactiveValues(count = 0)

  # set up showModal flag ---------------------------------------------------

  observeEvent(input$reset, {
    counter$count <- 0
    stored_data$data <- stored_data$orig_data
    stored_data$plot_data <- stored_data$data
  })

  observeEvent(input$start, {
    counter$count <- 1
  })

output$reshape_btns <- renderUI({
    req(input$infile)

    list(
      h4("reshape data"),
      actionButton("start", "start reshaping"),
      actionButton("reset", "reset")
    )
  })

  output$reshape_me <- renderUI({
    req(input$infile)
    req(counter$count > 0)

    selectInput("reshape_variables", "",
      choices = c(
        `select a what you want to do` = "",
        "make my data longer",
        "make my data wider",
        "select columns",
        "drop columns",
        "rename columns",
        "summarise",
        "transform",
        "recode",
        "change variable type"
      )
    )
  })

  get_column_names <- function() {
    map(names(stored_data$data), function(x) textInput(x, x, x))
  }

get_data_types <- reactive({
    
    req(stored_data$data)
    x <- data_frame(
      varlist = names(stored_data$data), 
      type = map(names(stored_data$data), ~paste0(class(stored_data$data[[.x]]), collapse = ','))
    )
    x$type <- as.character(x$type)
    x %>%
      mutate(
        type_collapse = case_when(
          type == 'numeric' | type == 'integer' ~ 'Numeric',
          grepl('POSIXct|POSIXt', type) ~ 'Datetime',
          type == 'Date' ~ 'Date',
          type == 'character' | type == 'factor' ~ 'Character/Factor',
          type == 'logical' ~ 'Logical',
          TRUE ~ type
        )
      )
  })


# function to get var type ------------------------------------------------

  change_var_type <- reactive({
    
    req(input$select_variables)
    req(input$choose_type)
    
    error_return <- FALSE
    
    if (stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) == input$choose_type) {
      return(stored_data$data)
    }
    
    
    if (input$choose_type == 'Numeric'){
      
      mutation <- 'mutate_at(input$select_variables, as.numeric)'
    }
    else if (input$choose_type == 'Character/Factor'){
      mutation <- 'mutate_at(input$select_variables, as.character)'
    }
    else if (input$choose_type == 'Logical'){
      mutation <- 'mutate_at(input$select_variables, as.logical)'
    }
    
    else if (input$choose_type == 'DateTime'){
      if (stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) == 'Numeric'){
        mutation <- 'mutate_at(input$select_variables, as.POSIXct, origin = input$origin)'
      }else{
        # going to try do this here but save the mutation
        mutation <- "mutate_at(input$select_variables, lubridate::parse_date_time, orders = trimws(unlist(str_split(input$datetime, ';'))))"
        x <-  tryCatch(stored_data$data %>% mutate_at(input$select_variables, lubridate::parse_date_time, orders = trimws(unlist(str_split(input$datetime, ';')))),
                       error=function(e) e, 
                       warning=function(w) w)
        }
    }
    else if (input$choose_type == 'Date'){
      if (stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) == 'Numeric'){
        mutation <- 'mutate_at(input$select_variables, as.Date, origin = input$origin)'
      }else{
        # going to try do this here but save the mutation
        mutation <- "mutate_at(input$select_variables, lubridate::parse_date_time, orders = trimws(unlist(str_split(input$date, ';'))))"
        x <-  tryCatch(stored_data$data %>% mutate_at(input$select_variables, lubridate::parse_date_time, orders = trimws(unlist(str_split(input$date, ';')))),
                       error=function(e) e, 
                       warning=function(w) w)
      }
      
    }
    
    print (stored_data$data)
    
    if (exists("x")){
      if (is(x,"warning") != TRUE){
        full_mutation <- glue('stored_data$data <- stored_data$data %>% ', mutation)
        write_to_log(full_mutation)
        eval(full_mutation %>% parse_expr())
      }
    }else{
      full_mutation <- glue('stored_data$data <- stored_data$data %>% ', mutation)
      write_to_log(full_mutation)
      eval(full_mutation %>% parse_expr())
    }


    stored_data$types <- get_data_types()
    stored_data$data
  })
  
  rename_modal <- function() {
    reset("reshape_variables")

    modalDialog(
      title = "Rename Variables",
      get_column_names(),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  output$date_format <- renderUI({
    
    req(input$choose_type)
    req(input$choose_type %in% c('Date', 'DateTime'))
    req(! is.na(input$choose_type))
    

    if (input$choose_type == 'Date' & stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) != 'Numeric'){
      textInput('date', 'enter a date format. mutiple formats must be separated by a semicolon')
    }else if (input$choose_type == 'DateTime' & stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) != 'Numeric'){
      textInput('datetime', 'enter a datetime format. mutiple formats must be separated by a semicolon')
    }else if (input$choose_type %in% c('Date', 'DateTime') & stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) == 'Numeric'){
      textInput('origin', 'enter an origin in the format of Y-m-d (e.g., 2001-01-01)')
    }
  })
  
  # function to get what to display for changing the var type
  get_more <- reactive({
  
      list(
        selectizeInput(
          "select_variables",
          label = "select which variables you want to do it to",
          choices = c(names(stored_data$data)),
          selected = "",
          multiple = FALSE
        ),
        renderPrint(paste0('Your variable is currently ', stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse))),
        selectizeInput(
          "choose_type",
          label = "select which type you want it to be",
          choices = c('Numeric', 'Character/Factor', 'Date', 'DateTime'),
          selected = '',
          multiple = FALSE
        )
      )
  })

  output$reshape_options <- renderUI({
    
    req(input$reshape_variables)

    switch(input$reshape_variables,
      "make my data longer" = selectizeInput(
        "select_variables",
        label = "select variables you want stored in key:value pairs",
        choices = names(stored_data$data),
        selected = "",
        multiple = TRUE
      ),
      "make my data wider" = selectizeInput(
        "select_variables",
        label = "select the variable you want across columns and the variable you want in rows (MAX:2)",
        choices = names(stored_data$data),
        selected = "",
        multiple = TRUE
      ),
      "select columns" = selectizeInput(
        "select_variables",
        label = "select which variables you want to do it to",
        choices = names(stored_data$data),
        selected = "",
        multiple = TRUE
      ),
      "drop columns" = selectizeInput(
        "select_variables",
        label = "select which variables you want to do it to",
        choices = names(stored_data$data),
        selected = "",
        multiple = TRUE
      ),
      "rename columns" = showModal(rename_modal()),
      "summarise" = list(
        selectizeInput(
          "group_variables",
          label = "do you want to group by another variable",
          choices = names(stored_data$data),
          selected = "",
          multiple = TRUE
        ),
        selectizeInput(
          "select_variables",
          label = "select which variables you want to do it to",
          choices = names(stored_data$data),
          selected = "",
          multiple = TRUE
        ),
        selectizeInput(
          "choose_summary",
          label = "choose summary functions",
          choices = c("mean", "median", "min", "max", "sd"),
          selected = "",
          multiple = TRUE
        )
      ),
      "transform" = list(
        selectizeInput(
          "group_variables",
          label = "do you want to group by another variable",
          choices = names(stored_data$data),
          selected = "",
          multiple = TRUE
        ),
        selectizeInput(
          "select_variables",
          label = "select which variables you want to do it to",
          choices = names(stored_data$data),
          selected = "",
          multiple = TRUE
        ),
        selectizeInput("choose_transformation",
          label = "choose a transformation functions",
          choices = c("scale", "log", "sqrt"),
          selected = "",
          multiple = TRUE
        )
      ),
      "recode" = selectizeInput(
        "select_variables",
        label = "select which variables you want to do it to",
        choices = names(stored_data$data),
        selected = "",
        multiple = FALSE
      ),
      "change variable type" = get_more()
    )
  })

  output$recode <- renderRHandsontable({
    
    req(input$select_variables, input$reshape_variables == "recode")

    to_recode <- sym(input$select_variables)
    
    if (stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) %in% c("Character/Factor")) {
      rhandsontable(stored_data$data %>% distinct(!!to_recode), height = 250) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  output$table_btn <- renderUI({
    req(input$infile)
    actionButton("do_table", "Save Changes", icon = icon("save"))
  })


  rename_them <- reactive({
    # here is how i am going to create the new names
    create_new_names <- 'new_names <- unlist(map(names(stored_data$data), function(x) input[[x]]))'
    # write it to a log
    write_to_log(create_new_names)
    # new do it 
    eval(create_new_names %>% parse_expr())
    # now write the new names to the log to show that they are
    write_to_log(new_names, checkinputs = F)
    # now make the actual assignment
    mutation <- 'names(stored_data$data) <- new_names'
    # write what youre going to do to the log
    write_to_log(mutation, checkinputs = F)
    # now do it
    eval(mutation %>% parse_expr())
  })

  recode_them <- reactive({
    
    req(stored_data$types %>% filter(varlist == input$select_variables) %>% pull(type_collapse) %in% c("Character/Factor"))
    
    # variable to recode
    to_recode <- sym(input$select_variables)
    
    new_vals <- hot_to_r(input$recode) %>% pull(input$select_variables)
    old_vals <- stored_data$data %>% distinct(!!to_recode) %>% pull(input$select_variables)

    write_to_log('recoding. new values are', checkinputs = F)
    write_to_log(new_vals, checkinputs = F)
    write_to_log('recoding. old values are', checkinputs = F)
    write_to_log(old_vals, checkinputs = F)
    
    # old names to new names
    mutation <- 'old_to_new <- set_names(new_vals, old_vals)'
    write_to_log(mutation)
    eval(mutation %>% parse_expr())
    write_to_log('showing everything before assignment')
    write_to_log('old_to_new is', checkinputs = F)
    write_to_log(old_to_new, checkinputs = F)
    write_to_log(glue('variables to recode: ', to_recode), checkinputs = F)
    mutation <- 'stored_data$data <- stored_data$data %>%
      mutate(!!input$select_variables := recode(!!to_recode, !!!old_to_new))'
    
    write_to_log(mutation, specificinput = "select_variables")
    eval(mutation %>% parse_expr())
    

    stored_data$data
  })

  summary_function <- reactive({
    if (!is.null(input$group_variables)) {
      if (length(input$select_variables) > 1) {
        
        mutation <- 'stored_data$data %>%
          summarise_at(
            input$select_variables,
            input$choose_summary
          ) %>%
          gather(key, value, -one_of(input$group_variables)) %>%
          separate(key, into = c("measure", "stat"), sep = "_") %>%
          spread(stat, value) %>%
          ungroup())'
        write_to_log(mutation)
        return(eval(mutation %>% parse_expr()))
      } else {
        
        mutation <- 'stored_data$data %>% summarise_at(
          input$select_variables,
          input$choose_summary
        ) %>% gather(key, value, -one_of(input$group_variables))
        '
        write_to_log(mutation)
        return(eval(mutation  %>% parse_expr()))
      }
    } else {
      if (length(input$select_variables) > 1) {
        
        mutation <- 'stored_data$data %>%
          summarise_at(
            input$select_variables,
            input$choose_summary
          ) %>%
          gather(key, value) %>%
          separate(key, into = c("measure", "stat"), sep = "_") %>%
          spread(stat, value) %>%
          ungroup())'
        write_to_log(mutation)
        return(eval(mutation %>% parse_expr()))
      } else {
        
        mutation <- 'stored_data$data %>% summarise_at(
          input$select_variables,
          input$choose_summary
        ) %>% gather(key, value))
        '
        write_to_log(mutation)
        return(eval(mutation %>% parse_exrp()))
      }
    }
  })


  do_reshaping <- observeEvent(input$do_table, {
    req(input$infile)
    req(counter$count > 0)

    stored_data$data <- hot_to_r(input$table)

    group_it()

    stored_data$data <- stored_data$data %>%
      when(
        (input$reshape_variables == "select columns") ~
        stored_data$data %>% select(input$select_variables),
        (input$reshape_variables == "drop columns") ~
        stored_data$data %>% select(-one_of(input$select_variables)),
        (input$reshape_variables == "make my data longer") ~
        stored_data$data %>% gather("key", "value", !!input$select_variables),
        (input$reshape_variables == "make my data wider") ~
        stored_data$data %>% spread(
          !!input$select_variables[[1]],
          !!input$select_variables[[2]]
        ),
        (input$reshape_variables == "transform") ~
        stored_data$data %>% mutate_at(
          input$select_variables,
          input$choose_transformation
        ),
        (input$reshape_variables == "recode") ~ recode_them(),
        # adding this block in the for the modals
        (input$reshape_variables == "") ~ stored_data$data,
        (input$reshape_variables == "summarise") ~ summary_function(),
        (input$reshape_variables == "change variable type") ~ change_var_type()
        
      )

    # have to set orig data to stored_data!
    stored_data$plot_data <- stored_data$data

    reset("reshape_variables")
  })


  observeEvent(input$ok, {
    req(input$infile)

    rename_them()

    stored_data$plot_data <- stored_data$data

    reset("reshape_variables")

    removeModal()
  })

  group_it <- reactive({
    if (!is.null(input$group_variables)) {
        stored_data$data <- stored_data$data %>%
          group_by_at(vars(input$group_variables))
    }
    stored_data$data
  })

  table_it <- reactive({
    req(input$infile)
    req(stored_data$data)
    stored_data$data
  })

  output$table <- renderRHandsontable({
    rhandsontable(table_it(), height = 250) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
