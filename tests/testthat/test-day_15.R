library(testthat)

source("./R/day_15.R")

test_that("Day15 task 1 chiton lowest risk route",{
  
  data_fname <- "day15_chitons.txt"
  test_day15_data <-  test_read_data(data_fname) 
  day15_data <-  read_data(data_fname) 
 
   expect_equal(get_chiton_least_risk(test_day15_data), 40)
})


