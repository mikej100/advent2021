library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(usethis)
library(logger)

t <- tempfile()
log_appender(appender_file("./log/log.txt"), index = 2)
# log_threshold(TRACE, index = 2)

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

get_bingo_score_first <- function (bingo_raw) 
  get_bingo_score(bingo_raw, last = FALSE)

get_bingo_score_last <- function (bingo_raw) 
  get_bingo_score(bingo_raw, last = TRUE)

# Run bingo draws against bingo boards specified in bingo_raw data,
# for first or last board with home calculate the 'score' 
# using algorithm provided.
# Returns the score value.

get_bingo_score <- function (bingo_raw, last = FALSE) {
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
  if (last) {
    target <- 0
  }  else {
    target <- length(boards) - 1
  }
  
  # Set up identity vectors, vertical and horizontal.
  id_h = matrix (1, 1, 5)
  id_v <- matrix( 1, 5, 1)
  
  # Find first board which has 'house' of row or column of matching elements.
  flag <- 1000
  threshold<- flag * dim
  
  boardsm <- boards
  houses <- list()
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
    # remove house board from boardsm
    for (house in houses) {
      format_row <- function (matrix, row)
        str_c( str_pad( matrix[ row, ], 5), collapse = " ")
      
                                          
      log_trace('house found:
     {format_row(house, 1)} 
     {format_row(house, 2)} 
     {format_row(house, 3)} 
     {format_row(house, 4)} 
     {format_row(house, 5)} 
                ') 
      # Check that board is still in boardsm, it may have been removed
      # if it has house in both a row and column.
      if ( matrix_in( boardsm, house) ) {
        boardsm <- boardsm[-matrix_which( boardsm, house)]
        log_trace (" house removed from boardsm")
      } else {
        log_trace (" board has two houses")
      }
    }
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


# answer4_1 <- get_bingo_score(bingo_data)
# answer4_2 <- get_bingo_score(bingo_data, first_not_last = FALSE)

# ------------------------------------------------------------------------------
# Day 5

# Set up functions to extract x and y coordinates from raw data line.
get_match <- function (txt, pattern) {
  result <- function (txt) as.numeric(str_match(txt, pattern )[2])
}
fx1 <- get_match(txt, "^(\\d+),")
fy1 <- get_match(txt, "^\\d+,(\\d+)")
fx2 <- get_match(txt, "-> (\\d+)")
fy2 <- get_match(txt, "-> \\d+,(\\d+)")

# Build coordinates on a line
# mode is either 'vh' for vertical and horizontal lines only, 
# or 'vhd' which includes diagonals as well.
# Returns a dataframe of x, y coordinates
# 
get_coords <- function (v, mode) {
  log_debug("v: {v}")
  coords_v = data.frame()
  x1 <- fx1(v)
  x2 <- fx2(v)
  y1 <- fy1(v)
  y2 <- fy2(v)
  if (x1 == x2) {
    x <- map_dbl(y1:y2, ~ x1)
    y <- map_dbl(y1:y2, ~ .x)
    coords_v <- data.frame (x, y)
  }
  if (y1 == y2) {
    x <- map_dbl (x1:x2, ~ .x)
    y <- map_dbl(x1:x2, ~ y1)
    coords_v <- data.frame (x, y)
  }
  if (  abs(x1 - x2) == abs(y1 - y2) && mode == "vhd") {
    x <- map_dbl (x1:x2, ~ .x)
    y <- map_dbl (y1:y2, ~ .x)
    coords_v <- data.frame (x, y)
  }
  coords_v
} 

# Generate# list of co-ordinate pairs for each line
# returns list of (x,y) coordinates.
# get_coords <- function (v) {
#   log_debug("v: {v}")
#   for (x in fx1(v):fx2(v) ) {
#     for (y in fy1(v):fy2(v) ) {
#       log_debug("Coords: {x}, {y}")
#       coords[[length(coords) + 1]] <- c(x,y)
#       log_debug('Length of coords: {length(coords)}')
#     }}
#   coords
#   }

get_busy_coord_count_vh <- function (vents_raw) {
  get_busy_coord_count (vents_raw, mode="vh")
}

get_busy_coord_count_vhd <- function (vents_raw) {
  get_busy_coord_count (vents_raw, mode="vhd")
}

# Performance improvement options
#   create coordinate pair try check count, if already count > 1 then discard it
get_busy_coord_count <- function (vents_raw, mode = "vh" ) {
  coords <- data.frame()
  coords_v <- data.frame()
  for (v in vents_raw) {
    log_trace("Vent {v}, ends {str_c(
            fx1(v), fy1(v), fx2(v), fy2(v), collape=',')}")
    coords_v <- get_coords(v, mode)
    if ( length(coords_v) > 1) {
      coords <- rbind(coords, coords_v)
    }
  }
  
  busy_coords <- coords %>%
    group_by(x, y) %>%
    summarize(n()) %>%
    filter (`n()` > 1)
  
  busy_coord_count <- length( busy_coords[[1]])
}


# ------------------------------------------------------------------------------
# Day 6
# 
# Logic
# Operations on each day
#   new_count <- count fish at zero 
#   decrement every fish, moving 0 -> 6
#   create new_count new fish value 8
# Aggregate all the fish at the same point in the cycle
#   sum fish at each point in the cycle 0 to 7
#   operation for each day
#     new_count <- count fish in zero 
#     shift the counts down one
#     set 8 <- new_count 
