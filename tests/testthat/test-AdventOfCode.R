library(testthat)
# print(paste("From test-AdventOfCode.R working directory is ", getwd()))
 setwd("../../")
# print(paste("After setwd working directory is ", getwd()))
source("./R/AdventOfCode.R")
testdata_folder <- file.path ("data","test_data")

# Set test level, 1 shallow, deeper, etc.
test_level <- 2
log_threshold(DEBUG, index = 2)
# Day 1 ------------------------------------------------------------------------
test_depth = c(199, 200, 208, 210, 200, 207, 240, 269, 260,263)
test_that("Depth calculations for 1 Dec task 1", {
  expect_equal(increases_count(test_depth), 7 )
})

test_that("Depth calculations for 1 Dec task 2", {
  expect_equal(increases3_count(test_depth), 5 )
})

# Day 2 ------------------------------------------------------------------------
test_course <- read_data("test_course.txt",  fpath = testdata_folder) 
test_that("course calculations for 2Dec task 1", {
  expect_equal(position_product1(test_course), 150)
})

test_that("course calculations for 2Dec task 2", {
  expect_equal(position_product2(test_course), 900)
})

# Day 3 ------------------------------------------------------------------------
test_diagnostic <-  read_data("test_data_day03.txt", fpath = testdata_folder) 
diagnostic <- read_data("day03_diagnostics.txt")

test_that("bool_to_decimal gets right answer", {
  expect_equal( bool_to_dec( c(TRUE, TRUE)), 3)
  expect_equal( bool_to_dec( c(TRUE, TRUE, FALSE, TRUE)), 13)
})

test_that("power consumption from diagnostic data", {
  expect_equal(power_from_diag(test_diagnostic), 198)  
  expect_equal(power_from_diag(diagnostic), 2035764)  
})

test_that("life support from diagnostic data", {
  expect_equal(get_life_support(test_diagnostic), 230)  
  expect_equal(get_life_support(diagnostic), 2817661)  
})


# Day 4 ------------------------------------------------------------------------
test_bingo <-  read_data("test-day04_bingo.txt", fpath = testdata_folder) 
bingo <- read_data("day04_bingo.txt")

test_that("finds first bingo board and calculates score", {
  expect_equal( get_first_bingo_score(test_bingo), 4512)
  skip_if (test_level < 2)
  expect_equal( get_first_bingo_score(bingo_data), 14093)
})

test_that("finds last bingo board and calculates score", {
  expect_equal( get_last_bingo_score(test_bingo), 1924)
  skip_if (test_level < 2)
  expect_equal( get_last_bingo_score(bingo_data), 17388)
})

mlist <- gen_matrix_list(1:36, dim=3)
m1 <- matrix(10:18, 3, 3, byrow=TRUE)
m2 <- matrix(11:19, 3, 3, byrow=TRUE)
test_that(" function tests existence of matrix m in mlist list of matricses", {
 expect_equal(matrix_in( mlist, m1), TRUE) 
 expect_equal(matrix_in(mlist, m2), FALSE) 
} )
test_that(" function gives position of matrix m in mlist list of matricses", {
 expect_equal(matrix_which( mlist, m1), 2) 
 expect_equal(matrix_which( mlist, m2), NA) 
} )

# ------------------------------------------------------------------------------
# Day 5
