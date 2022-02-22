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
   
   expect_equal(get_polymer_metric(test_day14_data, 13), 13573)
   expect_equal(get_polymer_metric(test_day14_data, 14), 28261)
   expect_equal(get_polymer_metric(test_day14_data, 15), 56892)
   
   expect_equal(get_polymer_metric1(test_day14_data, 14), 28261)
   expect_equal(get_polymer_metric1(test_day14_data, 15), 56892)
   expect_equal(get_polymer_metric1(test_day14_data, 16), 117020)
   expect_equal(get_polymer_metric1(test_day14_data, 17), 235560)
   
   expect_equal(get_polymer_metric_fast(test_day14_data, 14), 28261)
   expect_equal(get_polymer_metric_fast(test_day14_data, 15), 56892)
   expect_equal(get_polymer_metric_fast(test_day14_data, 17), 235560)
   
   expect_equal(get_polymer_metric_fast(day14_data, 14), 35435)
   expect_equal(get_polymer_metric_fast(day14_data, 15), 71005)
   expect_equal(get_polymer_metric_fast(day14_data, 40), 2360298895777)
   expect_equal(get_polymer_metric_fast(day14_data, 41), 4720607101179)
   expect_equal(get_polymer_metric_fast(day14_data, 42), 9441243388298)
})


