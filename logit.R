
# get all inputs ----------------------------------------------------------
write_to_log <- function(mutation){
  # get the list of inputs
  inputs <- reactiveValuesToList(input)
  # only get those inputs that appear in oru mutation call
  inputs_to_grab <- map(names(inputs), function(x) grepl(x, mutation))
  inputs_to_grab <- inputs[unlist(inputs_to_grab)]

  names(inputs_to_grab) %>% purrr::walk(function(x){
    mutation <<- gsub(x, inputs_to_grab[x], mutation)
    mutation <<- gsub("input\\$", '', mutation)
  })
  print (mutation)
}

