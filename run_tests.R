library(testthat)
library(shinytest)

test_that("Application works", {
  # set compare images to false if testing on Mac or Windows
  # images were built in docker container running Linux
  expect_pass(testApp(".", compareImages = TRUE))
})
