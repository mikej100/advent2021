library(testthat)

source("./R/day_13.R")

test_that("Day14 task 1 polymer generation",{
  
  data_fname <- "day14_polymer.txt"
  # day13_data <-  read_data(data_fname) 
  test_day14_data <-  test_read_data(data_fname) 
  day14_data <-  read_data(data_fname) 
 
  expect_equal(get_polymer_metric(test_day14_data), 1588)
  expect_equal(get_polymer_metric1(test_day14_data), 1588)
  #expect_equal(get_polymer_metric(day14_data), 2194)
  expect_equal(get_polymer_metric1(test_day14_data, 40), 2188189693529)
})


