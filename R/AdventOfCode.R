library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(usethis)
library(logger)

t <- tempfile()
log_appender(appender_file("./log/log.txt"), index = 2)
log_threshold(TRACE, index = 2)

log_info("Starting AdventOfCode.R")
log_trace("Starting AdventOfCode.R trace level")

data_folder <- file.path("data")
read_data <- function ( fname, fpath = data_folder) 
   read_lines(file.path(fpath, fname) ) 

#   ----------------------------------------------------------------------------
# Day 1
depth <- as.numeric(read_data("20211201_depths.txt"))
increases_count <- function(x) sum (diff(x) >0)

answer1_1 <- increases_count(depth)

# Day 01, task 2
window3 <- function (x) tail( x + lag(x) + lag(x, 2) , -2)
increases3_count <- function(x) sum(diff(window3(x)) >0)

answer1_2 <- increases3_count(depth)

#   ----------------------------------------------------------------------------
# Day 2
course_data <- (read_data("20211202_course.txt"))

# transform raw course data to a dataframe.
course_data_to_df <- function (x) {
  course <- x %>% 
    str_split(" ") %>%
    transpose() %>%
    simplify_all()
  
  course_df <- tibble("direction" = course[[1]], 
                      "amount"= as.numeric(course[[2]]))
  
}

# Calculate position using rules according to task 1.
position_product1 <- function (x) {
    course_df <- course_data_to_df(x)
    
    move <- course_df %>%
    group_by (direction) %>%
    summarise (sum(amount))
  
  z <- move[ move$direction == "up", ]$`sum(amount)` -
    move[ move$direction == "down", ]$`sum(amount)`
  x <- move[ move$direction == "forward", ]$`sum(amount)`
  pp <- x * -z
}
answer2_1 <- position_product1(course_data)


# Calculate position using rules according to task 2.
position_product2 <- function (x) {
    course_df <- course_data_to_df(x)
    
    df1 <- course_df %>%
      mutate (aim_change = {
        case_when (direction == "down" ~ - amount,
                   direction == "up" ~ amount)
      }) %>%
      mutate (aim = cumsum( replace_na( aim_change, 0))) 
      
    df2 <- df1 %>%
      filter (direction == "forward") %>%
      mutate (z_change =  amount * aim) 
    
  x <- sum(df2$amount)
    z <- sum(df2$z_change)
  pp <- x * -z
}
answer2_2 <- position_product2(course_data)

#   ----------------------------------------------------------------------------
# Day 03, task 1

diag_raw <- read_data("day03_diagnostics.txt")

# convert boolean vector representing binary number to decimal.
bool_to_dec <- function (x) {
  bits <- length(x)
  dec <- x %>%
    imap_dbl( ~ if_else ( .x,  2 ^ (bits - .y ), 0 ))  %>%
    sum()
}

# convert raw diagnostics data to columns of binary digits.
get_diag_cols <- function (diag_raw) {
  diag_raw %>%
  str_split("") %>%
  transpose() %>%
  simplify_all() %>%
  map(~ as.numeric(.))
}

# Calculate power from raw diagnostic data following task instrucitons.
power_from_diag <- function (diag_raw) {
  diag_cols <- get_diag_cols(diag_raw)
  # Behaviour not specified if number of 1s and 0s are the samany
  any_equal_count <- map(diag_cols, ~ sum(.) == length(.) / 2)
  
  gamma_bool <- diag_cols %>%
    map( ~ sum(.) > length(.) / 2) %>%
    unlist()
  
  gamma <- bool_to_dec(gamma_bool)
  epsilon <- bool_to_dec(!gamma_bool)
  power <- gamma * epsilon
}

answer3_1 <- power_from_diag(diag_raw)


# Calculate life support rating from raw diagnostic data following task instrucitons.
get_life_support <- function (diag_raw) {
  diag_cols = get_diag_cols(diag_raw)
  
  # get most common value in vector of binaries, rounding up
  get_mcv <- function (x) {
    if_else(sum(x) >= length(x) / 2, 1, 0)
  }
  
# filter diagnostics for oxygen rating.
  ox <- diag_cols
  for (i in 1:length(ox)) {
    if (length(ox[[i]]) > 1) {
      mask <- ox[[i]] == get_mcv(ox[[i]])
      ox <- map (ox, ~ .x[mask])
    }
  }
  
# filter diagnostics for co2 scrubbing rating.
  co <- diag_cols
  for (i in 1:length(co)) {
    if (length(co[[i]]) > 1) {
      mask <- co[[i]] == get_mcv(co[[i]])
      co <- map (co, ~ .x[!mask])
    }
  }
  
  # convert vector representing binary number to decimal.
  binary_to_decimal <- function (x) {
    bits <- length(x)
    dec <- x %>%
      imap_dbl(~  .x * 2 ^ (bits - .y))  %>%
      sum()
  }
  
  oxygen_generator_rating <- binary_to_decimal(ox)
  co2_scrubber_rating <- binary_to_decimal(co)
  life_support_rating <- oxygen_generator_rating * co2_scrubber_rating
}

answer3_2 <- get_life_support(diag_raw)
# ------------------------------------------------------------------------------
# Day 4
#
# check boards contain numbers in range 0:99
# for each numberdraw
#   scan cells of all matrices, add 1000 to any that match
#   check for house 
#     multipy by vertical and horizontal unit vectors 
#     see if any resuls are greater than 5000
#   break loop if house found
# calculate score
#    in the winning board, sum all elements less than 100
#    multiply by most recent numberdraw

# Accept bingo data line stream and boolean to indicate whether to find
# first orlast winning bingo board. 
# Returns the score of the first or last board.
#

# Generate list of square matrices from integer vector of values.
# Returns a list of matrices of dimension 'dim'
gen_matrix_list <- function (values, dim) {
  span <- dim^2
  matrices <- list()
  for (i in 0:(length(values)/span-1) ){
    index = i * span
    matrices[[i+1]] <- matrix( values[(1+ index):(span+index)], dim, dim, byrow=TRUE)
  }
  matrices
}

# Test whether matrix m is in mlist list of matrices. 
# Returns a logical.
matrix_in <- function (mlist, m) {
  any( map_lgl( mlist, ~ all(.x == m )))
}

# Find which matrix in mlist list of matrices matches given matrix m
# Returns position of match in list, or NA if not found
matrix_which <- function (mlist, m) {
  result <- which( map_lgl( mlist, ~ all(.x == m )))
  if (length(result) == 0) result <- NA
  result
}

get_bingo_score <- function (bingo_raw, first_not_last = TRUE) {
  draws <- as.numeric( str_split( bingo_raw[[1]], ",", simplify = TRUE) )
  
  board_raw <- tail(bingo_raw, -1)
  b1 <- board_raw %>%
    map( ~ str_replace( .x, "^ ", "") ) %>%
    map(~ str_split( .x, " {1,2}")) %>%
    unlist()
  
  b2 <- as.numeric( b1[ !b1 %in% ""] )
  
  dim <- 5
  boards <- gen_matrix_list(b2, dim=dim)
  
  # set the target, either first or last board with house
  if (first_not_last) {
    target <- length(boards) - 1
  }  else {
    target <- 0
  }
  
  # Set up identity vectors, vertical and horizontal.
  id_h = matrix (1, 1, 5)
  id_v <- matrix( 1, 5, 1)
  
  # Find first board which has 'house' of row or column of matching elements.
  flag <- 1000
  threshold<- flag * dim
  
  boardsm <- boards
  houses <- list()
  houses_seq <- list()
  last_draw <- NA
  for (draw in draws) {
    # flag element matching the draw
    log_trace("Draw number {which(draws == draw)}, value {draw}")
    boardsm <- boardsm %>%
      map(~ map_dbl(.x, ~ if_else (.x == draw, .x + flag, .x))) %>%
      map (~ matrix( .x, nrow = dim, ncol = dim, byrow = TRUE ))
    
    # identify boards with house (row or column of elements matched to a draw)
    th <- map(boardsm, ~ id_h %*% .x )
    tv <- map (boardsm, ~ .x %*% id_v )
    houses <- c(boardsm [map_lgl(th, ~ any(.x > threshold))],
                boardsm [map_lgl(tv, ~ any(.x > threshold))])
    #log_trace("houses found {length(houses)}") 
    # remove house board from boardsm
    for (house in houses) {
      log_trace('house found:
      {str_c( str_pad(house[1, ], 5), collapse = " ")} 
      {str_c( str_pad(house[2, ], 5), collapse = " ")} 
      {str_c( str_pad(house[3, ], 5), collapse = " ")} 
      {str_c( str_pad(house[4, ], 5), collapse = " ")} 
      {str_c( str_pad(house[5, ], 5), collapse = " ")} 
                ') 
      if ( matrix_in( boardsm, house) ) {
        boardsm <- boardsm[-matrix_which( boardsm, house)]
        houses_seq[ length(houses_seq) + 1 ] <- house
        log_trace (" house removed from boardsm")
      } else {
        log_trace (" board has two houses")
      }
    }
    log_trace("total houses found {length(houses_seq)}") 
    log_trace("boards left {length(boardsm)}") 
    
    
    if (length(boardsm) == target) {
      last_draw <- draw
      break
    }
  }
  
  # Calculate the 'score'.
  sum_unmarked <- map_dbl(
    houses[[1]], ~ map_dbl(.x, ~ if_else(.x < flag, .x, 0))) %>%
    sum(na.rm = TRUE)
  score <- last_draw * sum_unmarked
}

bingo_data <- read_data("day04_bingo.txt")

# answer4_1 <- get_bingo_score(bingo_data)
answer4_2 <- get_bingo_score(bingo_data, first_not_last = FALSE)
