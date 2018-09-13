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
