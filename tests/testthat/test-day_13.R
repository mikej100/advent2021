library(testthat)
setwd("../../")
  log_info("test_day_13 - Working directory: {getwd()}")
source("./R/day_13.R")

test_that("Read day23 test data",{
  
  data_fname <- "day13_dots.txt"
  # day13_data <-  read_data(data_fname) 
  test_day13_data <-  test_read_data(data_fname) 
  day13_data <-  read_data(data_fname) 

})
 