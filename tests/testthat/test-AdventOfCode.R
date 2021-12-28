library(testthat)
# print(paste("From test-AdventOfCode.R working directory is ", getwd()))
# setwd("../../")
# print(paste("After setwd working directory is ", getwd()))
source("./R/AdventOfCode.R")
testdata_folder <- file.path ("data","test_data")
test_read_data <- function (fname) {
  read_data( paste0("test-",fname), fpath = testdata_folder)
}

# Set test level, 1 shallow, deeper, etc.
test_level <- 2
log_threshold(INFO, index = 2)
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
bingo_data <- read_data("day04_bingo.txt")

test_that("finds first bingo board and calculates score", {
  expect_equal( get_bingo_score_first(test_bingo), 4512)
  skip_if (test_level < 2)
  expect_equal( get_bingo_score_first(bingo_data), 14093)
})

test_that("finds last bingo board and calculates score", {
  expect_equal( get_bingo_score_last(test_bingo), 1924)
  skip_if (test_level < 2)
  expect_equal( get_bingo_score_last(bingo_data), 17388)
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
test_vents <-  read_data("test-day05_vents.txt", fpath = testdata_folder) 
vents_data <- read_data("day05_vents.txt")


test_that(" count points on more than one vertical or horizontal line", {
  expect_equal (get_busy_coord_count_vh( test_vents), 5)  
  expect_equal (get_busy_coord_count_vh( vents_data), 4873)  
} )

test_that(" count points on >1 vertical, horizontal or diagnoal line", {
  expect_equal (get_busy_coord_count_vhd( test_vents), 12)  
  expect_equal (get_busy_coord_count_vhd( vents_data), 19472)  
} )


# ------------------------------------------------------------------------------
# Day 6
test_fish_data <- "3,4,3,1,2"
fish_data <- read_data("day06_fish.txt")


test_that(" fish reproduction calculation", {
  expect_equal (total_fish_after_n_days( test_fish_data,  18), 26)  
  expect_equal (total_fish_after_n_days( test_fish_data,  80), 5934)  
  expect_equal (total_fish_after_n_days( fish_data,       80), 343441)  
# task 2 
  expect_equal (total_fish_after_n_days( test_fish_data, 256), 26984457539 ) 
  expect_equal (total_fish_after_n_days( fish_data,      256), 1569108373832 ) 
} )


# ------------------------------------------------------------------------------
# Day 7
test_positions_data <- "16,1,2,0,4,2,7,1,2,14"
positions_data <- read_data("day07_crabs.txt")


test_that("fuel for optimal alignment with l1 metric", {
  expect_equal (
    fuel_for_optimal_alignment( test_positions_data, cost_model ="li"), 37) 
  expect_equal (
    fuel_for_optimal_alignment( positions_data, cost_model ="li"), 336701) 
})

test_that("fuel for optimal alignment with increment sum metric", {
  expect_equal (
    fuel_for_optimal_alignment( test_positions_data, cost_model="incr"), 168 ) 
  expect_equal (
    fuel_for_optimal_alignment( positions_data, cost_model="incr"), 95167302) 
})

# ------------------------------------------------------------------------------
# Day 08
digits_data <-  read_data("day08_digits.txt") 
test_digits_data <-  read_data("test-day08_digits.txt", fpath = testdata_folder) 
example_digits_data <-  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"

test_that("count number of digits which have segment count 2, 3, 4 or 7", {
  expect_equal (count_digit_length_matches( test_digits_data), 26 ) 
  expect_equal (count_digit_length_matches( digits_data), 440 ) 
})



test_that("get number of shared letters",{
  expect_equal( shared("abc", "abd"), 2)
  expect_equal( shared("abc", "def"), 0)
  expect_equal( shared("abcgr", "bac"), 3)
})

test_that("Decode segment patterns of set of digits data", {
  expect_equal (get_readout_total( test_digits_data), 61229) 
  expect_equal (get_readout_total( digits_data), 1046281) 
})

# ------------------------------------------------------------------------------
# Day 09
data_fname <- "day09_cave.txt"
cave_data <-  read_data(data_fname) 
test_cave_data <-  test_read_data(data_fname) 

test_that("Day 09, test find low points and calculate risk level", {
  expect_equal (get_risk_level_total( test_cave_data), 15) 
  expect_equal (get_risk_level_total( cave_data), 526) 
#  expect_equal (get_readout_total( digits_data), 1046281) 
})

