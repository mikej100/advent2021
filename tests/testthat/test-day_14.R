library(testthat)

source("./R/day_14.R")

test_that("Day14 task 1 polymer generation",{
  
  data_fname <- "day14_polymer.txt"
  test_day14_data <-  test_read_data(data_fname) 
  day14_data <-  read_data(data_fname) 
 
   expect_equal(get_polymer_metric(test_day14_data), 1588)
   expect_equal(get_polymer_metric1(test_day14_data), 1588)
   expect_equal(get_polymer_metric_fast(test_day14_data), 1588)
   expect_equal(get_polymer_metric_fast(test_day14_data, 40), 2188189693529)
   expect_equal(get_polymer_metric_fast(day14_data, 40), 2360298895777)
  # value found 20220217T0001 :2236085841534 - rejected by AoC.
  # expect_equal(get_polymer_metric(day14_data,2), 9)
  # expect_equal(get_polymer_metric_fast(day14_data, 2), 9)
  # expect_equal(get_polymer_metric(day14_data,3), 20)
  # expect_equal(get_polymer_metric_fast(day14_data, 3), 20)
  # expect_equal(get_polymer_metric(day14_data,), 2194)
  #expect_equal(get_polymer_metric_fast(day14_data), 2194)
})


