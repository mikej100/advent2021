library(readr)
library(testthat)
library(logger)


# testdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file.path(testdir, "../../"))
setwd( "../../")

log_appender(appender_file("./log/log.txt"), index = 2)
# log_threshold(TRACE, index = 2)

log_info("Starting AdventOfCode.R")
log_info("Helper - Working directory: {getwd()}")

log_trace("Starting AdventOfCode.R trace level")

data_folder <- file.path("data")
read_data <- function ( fname, fpath = data_folder) 
   read_lines(file.path(fpath, fname) ) 


testdata_folder <- file.path ("data","test_data")
test_read_data <- function (fname) {
  read_data( paste0("test-",fname), fpath = testdata_folder)
}

# Set test level, 1 shallow, deeper, etc.
test_level <- 2
log_threshold(INFO, index = 2)
log_info("Completed run of test helper-utilities")
