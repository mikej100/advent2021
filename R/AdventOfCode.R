library(tidyverse)
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
# Implementation approach:
#   Aggregate all the fish at the same point in the cycle
#   sum fish at each point in the cycle 0
#   operation for each day
#     new_count <- count fish in zero 
#     shift the counts down one
#     set 8 <- new_count 
# map this to 1-based vector array
#   Aggregate the count of fish into vector 1:9
#   f1 <- value of fish[1]
#   shift counts down one, adding value of old fish[1] to new fish[7]
#   set fish[9] to f1

# Prepare fish data by setting up
# vector fish holds the count of fish at each cycle. fish[1] is at cycle 0
prepare_fish <- function (fish_data_raw) {
  fish_data <- as.numeric( str_split( fish_data_raw, ",")[[1]])
  fish <- rep(0, 9)
  for ( i in seq_along(fish_data)) {
    fish[fish_data[i] ] <- fish[ fish_data[i] ] + 1
  }
  fish
}
# age the fish one day
age_one_day <- function (f) {
  f1 <- f[1]
  f <- lead(f)
  f[7] <- f[7] + f1
  f[9] <- f1
  f
}

age_n_days <- function (f, n) {
  for (i in 1:(n-1) ) {
    f <- age_one_day(f)
  }
  f
}

total_fish_after_n_days <- function(f_data, n) {
  fish <- prepare_fish(f_data)
  sum( age_n_days( fish, n) )
}

# ------------------------------------------------------------------------------
# Day 7
# 
# Logic
# let a be candidate alignment position
# cost of misalignment <- sum over points (abs(pi - a) 
# which is least absolute deviation regression, for which there is not
# computational solution. So use R optimise function.

# Task 2, cost model is cost <- 
  
# abs. difference cost model, a alignment position, p starting position vector
l1_cost <- function(a, v) {  sum(abs(v-a)) }

# incremental difference cost model, a alignment position, v starting position vector
# incr_sum is sum of digits leading up to given number.
incr_sum <- function(x) (x+1)*x/2
incr_cost <- function(a, v) { sum( incr_sum(abs(v-a)) )}

fuel_for_optimal_alignment <- function (position_data_raw, cost_model="l1") {
  # wrangle input data to vector.
  position <- as.numeric( str_split(position_data_raw, ",")[[1]] )
  
  if (cost_model == "incr") {
    cost_function <- incr_cost
  } else {
    cost_function <- l1_cost
  }
  
  optimum <- optimise( 
    cost_function, lower = 1, upper = mean(position), 4, position
    ) 
  
  # Round optimum and use to calculate cost.
  cost <- cost_function( round( optimum$minimum ), position)
}


# ------------------------------------------------------------------------------
# Day 8
# 
# Task 1
# numbers with unique number of segments
# 1 - 2 segments
# 4 - 4
# 7 - 3
# 8 - 7
# 
# task 1, find the number of  output digits which are in 1, 4, 7, 8
# map digits to number of segments lit
# count digits which have number of segments in 2, 4, 3 and 7
#
# Task two
# Deduce mappings for each display cell.
#
# a b c d e f g  N L Solve by
#     .     .    1 2 Unique length
#   . . .   .    4 4 Unique length
# .   .     .    7 3 Unique length    
# .   .     .    8 7 Unique length    
# .   . .   . .  3 5 Shares 2 segments with Digit 1
# . .   . . . .  6 6 Shares 1 segments with Digit 1
# . . . .   . .  9 6 Shares 5 segments with Digit 3
# . . .   . . .  0 6 Remaining sixer          
# .   . . .   .  2 5 Shares 4 segments with Digit 6
# . .   .   . .  5 5 Last remaining digit
#
# Task 1
  # wrangle the data
  # split unique pattersn from digit reads
  # sort letter order for all patterns
get_data1 <- function (digits_raw) {
  data1 <- digits_raw %>%
    str_split("\\|") %>%
    transpose()
  
  unique_patterns <- data1[[1]] %>%
    unlist() %>%
    str_split(" ") %>%
   map(~ map_chr(.x, ~ order_letters(.x)))
 
  readout_patterns <- data1[[2]] %>%
    unlist() %>%
    str_split(" ") %>%
    map( ~ map_chr(.x, ~ order_letters(.x))) %>%
    map( ~ keep(.x, ~ str_length(.x) > 1) )
  
    data1 <- list(unique_patterns = unique_patterns, 
                  readout_patterns = readout_patterns)
}    

order_letters <- function (word) {
  word%>%
    str_split("") %>%
    unlist() %>%
    sort() %>%
    reduce(~ paste0(.x, .y), .init="")
}
count_digit_length_matches <- function(digits_raw) {
  # wrangle the data
  data1 <- get_data1(digits_raw) 
  
  digits<- data1$readout_patterns
  
  
  # count number of string length matches
  digit_counts <- digits %>%
    map( ~ str_length(.)) %>%
    map( ~ . %in% c( 2, 4, 3, 7 )) %>%
    map( ~ sum(.) )
  
  total_counts <- sum(unlist(digit_counts))
}

# For task 2
#
# function shared_segments
#  pass two words as chr[1]
#  return number of shared letters
shared <- function (a, b) {
  aa <- unlist(str_split(a, ""))
  bb <- unlist(str_split(b, ""))
  aa %>%
    map_lgl(~ . %in% bb) %>%
    sum()
}

# Find the mapping of set of unique patterns to digits they represent.
# Return data frame of patterns and the digit represented.
get_mapped_patterns <- function (patterns) {
  patterns_df <- head( tibble(segs=patterns), -1)
  
  # Digits 1, 4, 7 and 9 assigned based on unique lengths.
  pp <- patterns_df %>%
    mutate( len = str_length(segs)) %>% 
    mutate( digit = case_when (
      len == 2 ~ 1
      ,len == 4 ~ 4
      ,len == 3 ~ 7
      ,len == 7 ~ 8
    )
    )
  
  # Digits 6, 9 and 0 are sixers, they have segs length 6.
  sixers <- pp %>%
    filter(len ==6)
  
  # Digits 2, 3 and 5 are fivers, they have segs length 5.
  fivers <- pp %>%
    filter(len == 5)
  
  # Digit 3 is only fiver which shares two segments with Digit 1 (whose segs has length 2)
  d1_segs <- pp[pp$len==2,'segs']
  d3_row <- fivers [map_lgl(fivers$segs,~ shared(., d1_segs) == 2 ),]
  pp[pp$segs == d3_row$segs, 'digit' ] <- 3 
  
  # Digit 6 is only sixer which shares 1 segment with Digit 1
  d6_row <- sixers [map_lgl(sixers$segs,~ shared(., d1_segs) == 1 ),]
  pp[pp$segs == d6_row$segs, 'digit' ] <- 6 
  
  # Digit 9 is only sixer which shares 4 segment with Digit 4
  d4_segs <- pp[pp$len==4,'segs']
  d6_row <- sixers [map_lgl(sixers$segs,~ shared(., d4_segs) == 4 ),]
  pp[pp$segs == d6_row$segs, 'digit' ] <- 9 
  
  # Get pattern row by a givn number of shared segments in the digit pattern
  # Returns the segment pattern(s) of the matching digit rows.
  # uses pp from environment.
  get_match_by_shared <- function (i_base, segs, n) {
    base <- pp[i_base]
    result <- base[ map_lgl( base$segs, ~ shared( .x, segs) == n), 'segs' ]
  }
  
  #
  set_digit_mapping <- function (data, segs, value) {
    
  }
  
  # Digit 0 is the remaining sixer.
  d0_row <- subset(pp, len ==6 & is.na(digit))
  pp[pp$segs == d0_row$segs, 'digit' ] <- 0
  
  # Digit 2 is the fiver which shares 4 segments with Digit 6.
  d6_segs <- d6_row$segs
  d2_row <- fivers [map_lgl(fivers$segs,~ shared(., d6_segs) == 4 ),]
  pp[pp$segs == d2_row$segs, 'digit' ] <- 2
  
  # Digit 5 is the last remaining digit
  d5_row <- subset(pp, is.na(digit))
  pp[pp$segs == d5_row$segs, 'digit' ] <- 5
  
  arrange(pp, digit)
  
}

# Dev area
get_readout_total <- function (digits_data) { 
  data1 <- get_data1(digits_data) 
  
  patterns <- data1$unique_patterns
  mappings <- map( patterns, ~ get_mapped_patterns(.x) )
  readouts <- data1$readout_patterns
  
  
  
  # get digit corresponding to segment pattern p in mapping table m.
  # Return numeric value of one digit.
  get_digit <- function( m, p) {
    as.numeric( m[ m$segs == p, 'digit'])
  }
  
  # get digit sequence corresponding to set of patterns in readout.
  # Return vector of integers for the digits.
  reading_digits <- function(m, reading) {
    map_dbl(reading, ~ get_digit(m, .x))
  }
  
  # get numeric value of digit sequence of patterns in readout for mapping m.
  # Return double representing numeric value of the digits combined as number.
  reading_value <- function (m, reading) {
    reduce(reading_digits( m, reading), ~ 10 *.x + .y)
  }
  
  # Convert the readouts to values using mappings from unique values and sum them.
  # Returns integer representing the sum of readings.
  readings_total <- map2(mappings, readouts, ~ reading_value(.x, .y)) %>%
    unlist() %>%
    sum()
  
}
# Here are some lines of code used as tools to develop the solution.
# True Segment assignments for deducing the rules, digits 1-9 and 0.
ts <-  c( "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf",
           "abcdef", "abcdfg", "abcefg" )

i_uniques <- c(1, 4, 7, 8)
i_fivers <- c(2, 3, 5)
i_sixers <- c(6, 9, 10)

# View matrix of shared seg lengths between two sets of digits.
# pass the indices in ts of the segment pattern to cross
view_shared <- function (i_row, i_col) {
  m <- matrix(nrow=length(i_row), ncol=length(i_col))
  colnames(m) <- i_col
  rownames(m) <- i_row
  for (i in seq_along(i_row) ) {
    m[i, ] <- map_int(ts[i_col], ~ shared(.x, ts[i_row[i]]) )
  }
  m
}
# Look for ways to identify members of fivers and sixers by comparing to uniques.
view_shared(i_fivers, i_uniques)
view_shared(i_sixers, i_uniques)
view_shared( c(2,5), c(3, 6, 9, 10))
