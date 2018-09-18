library(dplyr)
library(rlang)
library(lme4)
library(formatR)
data(iris)

x <- parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
eval(x)
X <- tidy_eval(text = assignment)
X

P <- eval(assignment %>% parse_expr())
P
mtcars %>% parse_expr(assignment)


mtcars %>% parse_expr(!! 'dplyr::mutate(cyl_prime = cyl / sd(cyl))')

assignment <- "mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))"


to_grab <- list('cyl' = 'Cycle', 'mutate' = 'MUTATION')
to_grab

purrr::walk(names(to_grab), function(x) gsub(x, to_grab[x], assignment))
assignment

names(to_grab) %>% purrr::walk(function(x){
  assignment <<- gsub(x, to_grab[x], assignment)
})

assignment  
data(iris)

new_names <- c('pee', 'poo')
eval(new_names)
eval('names(iris) <- new_names' %>% parse_expr())
iris

iris <- iris %>% mutate(Sepal.Length = 4)


full_mutation <- "iris <- iris %>% mutate(Species = 'Poo')"

eval("iris <- iris %>% mutate(Sepal.Length = 4)" %>% parse_expr())
iris
