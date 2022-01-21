library(tidyverse)
library(usethis)
library(logger)

log_appender(appender_file("./log/log.txt"), index = 2)
# log_threshold(TRACE, index = 2)

log_info("Starting AdventOfCode.R")
log_trace("Starting AdventOfCode.R trace level")

data_folder <- file.path("data")
read_data <- function ( fname, fpath = data_folder) 
   read_lines(file.path(fpath, fname) ) 

# Day 01-----------------------------------------------------------------------
#
depth <- as.numeric(read_data("20211201_depths.txt"))
increases_count <- function(x) sum (diff(x) >0)

# Day 01, task 2
window3 <- function (x) tail( x + lag(x) + lag(x, 2) , -2)
increases3_count <- function(x) sum(diff(window3(x)) >0)

# Day 02---------------------------------------------------------------------
# 
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
    
  x <-sum(df2$amount)
    z <- sum(df2$z_change)
  pp <- x * -z
}

# Day 03-----------------------------------------------------------------------

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

# Day 04-----------------------------------------------------------------------
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

# Day 05-----------------------------------------------------------------------

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


# Day 06----------------------------------------------------------------------
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

# Day 07------------------------------------------------------------------------
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


# Day 08------------------------------------------------------------------------
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
 
  # Incomplete code to make functions for the matching and setting of values. 
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
  # end of imncomplete code.
   
  arrange(pp, digit)
  
}

# 
# Main function for task 2
get_readout_total <- function (digits_data) { 
  data1 <- get_data1(digits_data) 
  
  patterns <- data1$unique_patterns
  mappings <- map( patterns, ~ get_mapped_patterns(.x) )
  readouts <- data1$readout_patterns
  
  # Get digit corresponding to segment pattern p in mapping table m.
  # Return numeric value of one digit.
  get_digit <- function( m, p) {
    as.numeric( m[ m$segs == p, 'digit'])
  }
  
  # Get digit sequence corresponding to set of patterns in readout.
  # Return vector of integers for the digits.
  reading_digits <- function(m, reading) {
    map_dbl(reading, ~ get_digit(m, .x))
  }
  
  # Get numeric value of digit sequence of patterns in readout for mapping m.
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

# ------------------------------------------------------------------------------
# Day 09
# 
# Task 1

# Load height data into matrix .
wrangle_cave_data <- function (cave_data_raw) {
  
  ncol <- str_length(cave_data_raw[1])
  nrow <- length(cave_data_raw)
  
  heights <- cave_data_raw %>%
    map( ~ str_split(.x, "")) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = ncol,
           nrow = nrow,
           byrow = TRUE)
}

# get values of matrix m elements up, down, left and right; or 10 if on border
# focus is (row, column) pair.
up <- function(m, focus) {
ifelse (focus[1] == 1,   10,   m[focus[1] - 1, focus[2]])
}

down <- function (m, focus){
ifelse (focus[1] == nrow(m),   10,   m[focus[1] + 1, focus[2]])
}

left <- function (m, focus) {
ifelse (focus[2] == 1,   10,   m[focus[1], focus[2] - 1])
}

right <- function (m, focus) {
ifelse (focus[2] == ncol(m),   10,   m[focus[1], focus[2] + 1])
}

# Get values of matrix m elements up, down, right and left of location irow,icol.
# Returns double vector  length 4.
neighbours <- function (m, focus) {
  invoke_map_dbl(list(up, down, left, right), 
               list(list(m, focus)))
}

# Main section for task 1. Find dips where all elements avove and below are 
# greater than the value of the given cell. Add one to that value and sum for
# all dips.
get_risk_level_total <- function (cave_data_raw) {
  m <- wrangle_cave_data(cave_data_raw)
  
  risk_level_total <- 0
  for (irow in seq_along(m[, 1])) {
    for (icol in seq_along(m[1,])) {
      if (every(neighbours(m, c(irow, icol)), ~ .x > m[irow, icol])) {
        log_trace (" found dip at m[{irow},{icol}]")
        risk_level_total <- risk_level_total + m[irow, icol] + 1
      }
      if (every(neighbours(m, c(irow, icol)), ~ .x >= m[irow, icol]) &
          !every(neighbours(m, c(irow, icol)), ~ .x > m[irow, icol])) {
        log_trace (" found gulley at m[{irow},{icol}]")
      }
    }
  }

  # Alternative using imap for loops is, I think, more cumbersome.i
  #iwalk(heights[, 1], ~  {
  #  irow <- .y
  #  iwalk(heights[1, ], ~ {
  #    icol <- .y
  #    cat("\n element", heights[irow, icol])
  #  })
  #})
  risk_level_total
}
    
# Find a basin which contains element given by element in position (i,j).
# returns basin index or integer(0) if not found
which_basin <- function (basins, i, j) {
  result <- detect_index(basins, ~ list( as.integer( c(i,j)) ) %in% .x)
}

val <- function(m, i, j) {
  result <- m[i,j]
}

# Add location (i,j) to basins[[b1]]
# Returns modified basins
add_to_basin <- function(basins, b1, i, j) {
  if (length(basins) < b1 ) {
    # Start new basin.
   basins <- append( basins, list(list( c(i,j)) ) ) 
  } else {
    # Append to existing basin.
    basins[[b1]] <- append( basins[[b1]], list(c(i,j) ) )
  }
  basins
}
    

# Merge basin basin[[b2]] into basins[[b1]], but only if they are different.
# Returns updated basins.
merge_basins <- function (basins, b1, b2) {
  if (b1 == b2) {
    # skip if they are the same.
  } else {
    basins[[b1]] <- append(basins[[b1]], basins[[b2]])
    basins[[b2]] <- NULL
  }
  basins
}

# Main function for task 09.2
# Find all basins in cave data
# Return the product of the size of the three largest basins.
get_basins <- function (cave_data_raw) {
  m <- wrangle_cave_data(cave_data_raw)
  
  basins <- list()
  for (irow in 1:nrow(m)) {
    log_trace ("Processing task 9.2 row {irow}")
    for (icol in 1:ncol(m)) {
      if (val(m, irow, icol) < 9) {
        # check if adding to basin of cell to left.
        i_basin <- which_basin(basins,  irow, icol - 1)
        if (i_basin == 0) {
          i_basin <- length(basins) + 1
        }
        basins <- add_to_basin(basins, i_basin, irow, icol)
        
        # check if add current basin to basin of cell up
        up_basin <- which_basin(basins, irow - 1, icol)
        if (up_basin > 0)
          basins <- merge_basins(basins, up_basin, i_basin)
      }
    }
  } #end of row
  
  prod_top_three_basins <- basins %>%
    map( ~ length(.x)) %>%
    unlist() %>%
    sort () %>%
    tail (3) %>%
    reduce( ~ .x * .y)
  
}


# Day 10------------------------------------------------------------------------
# 
# Task 1

# Test character to see if is an opening delimiter, return T/F.
is_opener <- function(c) {
 str_detect(c, "[\\[\\{\\(<]")
}

# Test character to see if is a closing delimiter, return T/F.
is_closer <- function(c) {
 str_detect(c, "[\\]\\}\\)>]")
}

# Test if delimiters are matching pair, return T/F.
is_balanced <- function (opener, closer) {
  paste0(opener,closer) %in% list("[]", "{}", "()", "<>") 
}

generate_closers <- function(stack) {
  openers <- c("[","{", "(", "<")
  closers <- c("]", "}", ")", ">")
  matched_closer <- function (delimiter) {
    result <- closers[ detect_index( openers, ~ .== delimiter)]
  } 
  map_chr(rev(stack), ~ matched_closer(.x) )
}

# Run through string to see if delimiters open and close in legal sequence.
# Returns list of two values: bad closer, and vector of good closers.
# If a bad closer is found, that closing delimeter is returned as bad_closer, or
# empty string.
# For all cases, generates the correct set fo closing vectors to balance 
# unclosed openers as good_closers.


get_closers <- function (line) {
  bad_closer <- ""
  good_closers <- ""
  stack <- vector()
  chars <- unlist(str_split(line, ""))
  
  for (c in chars) {
    if (is_opener(c))  {
      stack <- append(stack, c)
    }
    
    if (is_closer(c)) {
      if (length(stack) < 1) {
        bad_closer <- c
      } else {
        last <- tail(stack, 1)
        stack <- head(stack,-1)
        if (!is_balanced (last, c)) {
          bad_closer <- c
        }
      }
    }
    if ( str_length(bad_closer) > 0) {
      break
    } # end of if closer
  }  # end for
  
  # No bad closers if get to this code, complete with required closes.
  if (length(stack) > 0 ) {
    good_closers <- generate_closers(stack)
  }
    # reverse complementary closers
    # devise way to signal good or bad closers
  result <- list(bad_closer=bad_closer, good_closers=good_closers)
}

# Return penalty score a particular bad closing delimiter.
# Closer is vector of closing delimiters.
# Returns total of closing delimiter scores.
closer_score <- function(closer) {
  switch ( (str_locate( ")]}>", fixed(closer))[[1]] ), 
           3, 57, 1197, 25137)
}
# Return total penalty score for collection of bad closing delimiters
closers_score <- function(closers) {
  map_dbl(closers, ~ closer_score(.x)) %>%
    sum()
}

# Process raw navchunk data to find good and bad closers.
get_navchunk_results <- function (navchunk_data) {
  map(navchunk_data, ~ get_closers(.x) )
}


# Main code for day 10 task 1.
# Calculate total syntax error score for errors in navigation chunks in navchunk
# Find the bad closer delimiters in each line of navchunk data, 
# lookup score for that delimiter and sum them.
# Returns dbl of  the sum value.
get_bad_closer_score <- function (navchunk_results) {
  map_chr(navchunk_results, ~ .x$bad_closer) %>%
   keep(~ str_length(.x) > 0) %>%
    map_dbl(~ closer_score(.x)) %>%
    sum()
}

# Day 10 task 2 ----------------------------------------------------------------
#
# Automatic correction task has its own scoring values for closers.
auto_closer_value <- function(closer) {
  switch ( (str_locate( ")]}>", fixed(closer))[[1]] ), 
           1, 2, 3, 4)
}
# Return total penalty score for list of closers. 
auto_closers_score <- function(closers) {
  reduce(closers, ~ 5*.x + auto_closer_value(.y), .init=0) 
}

# Main code fro Day 10 task 2.
# From the processed results remove lines with bad closers, find scores for
# remaining good closures and return value of the middle-sized score.
get_autocomplete_score <- function (navchunk_results) {
  closers <- 
    keep(navchunk_results, ~ str_length(.x$bad_closer) ==0) %>%
    map( ~ .x$good_closers)
  scores <- map_dbl (closers, ~ auto_closers_score(.x))
  ordered_scores <- scores[order(scores)]
  mid_score <- ordered_scores[[length(ordered_scores) %/% 2 + 1]]
  
}


# Using regexes to find matching delimiters in string.-------------------------
# Code below is not used to solve the task.
# This code was developed an exercise with using regexes for balanced
# delimeters. It workds, but does not solve the task at hand. It is retained
# for as a learning exercise for future use.
# see https://www.regular-expressions.info/recurse.html#balanced
# TODO  change this to allow recursion without alternation.

# Set up regex patterns to find balanced pairs, and singles.
bal_patt <- function (beginning, middle, end) {
  result <- paste0(beginning,"(?:", middle, "|(?R))*", end)
#  result <- paste0(beginning, "(?R)*",  end, "|", middle)
}
bracket_bal_p <- bal_patt("\\[", "[^\\[\\]]", "]")
bracket_p <- "[\\[\\]]"
brace_bal_p <- bal_patt("\\{", "[^\\{\\}]", "\\}")

brace_p <- "[\\{}]"
parenth_bal_p <- bal_patt("\\(", "[^\\(\\)]", "\\)")
parenth_p <- "[\\(\\)]"
gtlt_bal_p <- bal_patt("<", "[^<>]", ">")
gtlt_p <- "[<>]"

# Helper for dev and test, show the matches
re <- function(p, x) {
  unlist( regmatches(x, gregexpr(p, x, perl=TRUE)) )
}  

# helper substition used to strip paired delimiters from string
strip_bal <- function (p, x) {
  gsub(p, "", x, perl=TRUE)
}


bracket_is_bal <- function (x) {
  stripped <- strip_bal(bracket_bal_p, x)
  result <- ! grepl(bracket_p, stripped, perl=T)
}
brace_is_bal <- function (x) {
  stripped <- strip_bal(brace_bal_p, x)
  result <- ! grepl(brace_p, stripped, perl=T)
}
parenth_is_bal <- function (x) {
  stripped <- strip_bal(parenth_bal_p, x)
  result <- ! grepl(parenth_p, stripped, perl=T)
}
gtlt_is_bal <- function (x) {
  stripped <- strip_bal(gtlt_bal_p, x)
  result <- ! grepl(gtlt_p, stripped, perl=T)
}

delimited_text <- "put test data here"
are_bal <- list(bracket_is_bal, brace_is_bal, parenth_is_bal, gtlt_is_bal )
# Main function to test whether all strings in d have complete set of
# matching pairs.
# Unfortunately, this is not the required analysis, as chunks can be incomplete.
aa <-  map(delimited_text, ~ ( invoke_map_lgl(are_bal, .x)) )


# Day 11 -----------------------------------------------------------------------
#
wrangle_octopus_data <- function (octopus_data_raw) {
  
  ncol <- str_length(octopus_data_raw[1])
  nrow <- length(octopus_data_raw)
  
  octopodes <- octopus_data_raw %>%
    map( ~ str_split(.x, "")) %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = ncol,
           nrow = nrow,
           byrow = TRUE)
}

# Helper functions for task 1.

# if i, j are in the octopus matrix, add one.
# return modified matrix.
incr_oct <- function (os, i, j) {
  if ( i > 0 & i <= nrow(os)  & j > 0 & j <= ncol(os) ) {
    os[i, j] <-  os[i,j] + 1
  }
  os
}

# Increment all neighbours of octopus o in matrix of octopodes os
# Return new os matrix.
flash_neighbs <- function (os, irow, jcol) {
  offsets <- cross2(-1:1, -1:1 )[-5]
  os <- reduce(offsets, .init=os,
         ~ incr_oct (.x, irow + .y[[1]], jcol + .y[[2]] )
  )
  # Set octopus just flashed to high value to mark not to flash again this step.
  os[irow,jcol] <-  99
  return(os)
}

flash_neighbs_orig <- function (os, irow, jcol) {
  offsets <- cross2(-1:1, -1:1 )[-5]
  for ( i in seq_along(offsets) )  {
    os <- incr_oct (os, irow + offsets[[i]][[1]], jcol + offsets[[i]][[2]])
  }
  # Set octopus just flashed to high value to mark not to flash again this step.
  os[irow,jcol] <-  99
  return(os)
}

# An octopus is due to flash if its value is greater than nine and also
# less then the high value which is used to mark octopus which have flashed 
# in this step.
# Return boolean.
is_flash_value <- function (val) {
  result <-  val > 9 && val < 99 
}
 
# Flash all octopus in os matrix which have the appropriate value.
# return new os matrix.
flash_os <- function (os) {
  row_indices <- unlist(map(1:nrow(os), ~ rep(., ncol(os))))
  col_indices <- rep(1:ncol(os), nrow(os))
  result <-
    reduce2(row_indices, col_indices, .init = os, function(os, i, j) {
      if (is_flash_value(os[i, j])) {
        os <- flash_neighbs (os, i, j)
      } else {
        os
      }
    }
    )
  if ( none( result, ~ is_flash_value(.) ) ) {
    result <- done(result)
  }
  return(result)
  }

# Original version of flash_os, with for loops. Really, this is clearer than
# reduce version.
flash_os_orig <- function (os) {
  for (i in 1:nrow(os)) {
    for (j in 1:ncol(os)) {
      if (is_flash_value( os[i, j] ) )  {
        os <- flash_neighbs (os, i, j)
      }
    }
  }
  os
}

# Set elements in matrix os which are greater than or equal to 99 to value 0.
set_ge99_to_0 <- function (os, x) {
  map_dbl( os, ~ if_else (. >= 99, 0, .)) %>%
    matrix( ncol= ncol(os), nrow= nrow(os) )
}

# Do one  bioluminesence step.
# do flash for all elegible octopus, and keep going whilst new elegible flashers
# are generated. Then set flashed octopus to value zero.
one_step <- function (os) {
  os <- os + 1
  os <- reduce(1:1000, .init=os, ~ flash_os(.x))
  os <- set_ge99_to_0 (os)
}

one_step_orig <- function (os) {
  os <- os + 1
   while ( some( os, ~ is_flash_value(.) ) ) {
    os <- flash_os(os)
  }
  os <- set_ge99_to_0 (os)
}

# Main function for Day 11 task 1
# generate all the intermediate octopus energy level matrices
# and sum the number of flashes.
# Return number of flashes.
get_flash_count <- function (raw_octopus_data) {
  octopodes <- wrangle_octopus_data(raw_octopus_data)
  steps <- accumulate(1:100, ~ one_step(.x), .init = octopodes)
  flash_count <- reduce(steps, ~ .x + length(.y[.y == 0]), .init = 0)
}

# Helper functions for task 2.

# Test whether all elements of matrix are value zero.
# Return boolean.
is_all_sync <- function (os) {
  every(os, ~ . == 0)
}

# Do one octopus flashing step and flag to caller if all flashed in sync.
step_to_sync <- function (os) {
  os <- one_step(os)
  if (is_all_sync(os)) {
    result <- done(os)
  } else{
    result <- os
  }
  return( result )
}

# Day 11 task 2 main function.
# Find number of steps to reach syncrhony.
# Return count of steps, or zero if not reached within loop limit set in code.
# Terminate the accumulate loop when all in sync by wrapping result in done().
get_steps_to_sync <- function (raw_octopus_data) {
  octopodes <- wrangle_octopus_data(raw_octopus_data)
  
  steps <- accumulate(1:10000, .init=octopodes, ~ step_to_sync(.x) )

  if (is_all_sync( tail(steps) ) ) {
    result <- length(steps) - 1
  } else {
    log_error("get_steps_to_sync error, failed to reach sync within step limit.")
    result <- integer(0)
  }
  return( result)
}

  


# Day 12 -----------------------------------------------------------------------
#
#
# Assumptions of the data. These are not checked.
# No adjacent large caves.think this would cause an infinte loop.
# No mixed case cave names. This code would not reject mixed case.
# Caves are either upper or lower case.
#
# Optimisation which could be done.
# Process next links at start.  node_links() consumes 95% of the processing 
# time as it is called in every recursion. The links can be extracted once and 
# passed around in a fast to access form.
wrangle_links <- function(raw_cavelink_data) {
  raw_cavelink_data|>
  str_split("-") |>
  transpose() |>
  simplify_all()|>
  as_tibble(.name_repair= \(x) c("from", "to"))
}

# Return list of links from this node
# TODO change implemenation to do this once up front, rather than repeated
# inside the recursion.

node_links <- function(links, node) {
  append(
    filter(links, from == node) |> select(to) |> unlist(),
    filter(links, to == node) |> select(from) |> unlist()
  ) 
}

path_allowed_1 <- function (next_node, path) {
   ! (str_detect(next_node, "[a-z]+") && next_node %in% path )
}

path_allowed_2 <- function (node, path) {
  is_small <- function(node) str_detect(node, "[a-z]+")
  
  no_small_twice_in_path <- function (p) {
     ! length(p[is_small(p)]) - length(unique(p[is_small(p)])) == 1
  }
  
  not_in_path <- function(node, path) {
    ! node %in% path
  }
  
  is_start_or_end <- function(node)  str_detect(node, "(start|end)")
  
  # Here is the logic
  if ( ! is_small(node) ) {
    result <- TRUE
  } else if (is_start_or_end(node) ) {
    result <- FALSE
  } else if (is_small(node) &&   not_in_path(node, path)) {
    result <- TRUE
  } else if (is_small(node) &&  no_small_twice_in_path(path) ){
    result <- TRUE
  } else {
    FALSE
  }
}


# p1 <- list("start", "a", "A", "b", "a", "A")
# p2 <- list("start", "a", "A", "b", "c", "A")
# is_small <- function(node) str_detect(node, "[a-z]+")
# p[is_small(p )]|>  unique()|>length()
# (length(p[is_small(p)]) - (p[is_small(p )]|>  unique()|>length()))>1
       

 

path_allowed <- function (rules, node, path) {
  if ( str_detect(rules, fixed("rules_2")) ) {
    f <- path_allowed_2
} else {
    f <- path_allowed_1
}
  f(node, path)
} 

try_link <- function( links, rules, next_node, path, good_paths) {
  log_path <- reduce(path, paste, sep="-")
  log_debug("try_link, next_node: {next_node}, path: {log_path}")
  if  (str_detect(next_node, "^end$")  ) {
    path <- append(path, next_node)
    
    log_path <- reduce(path, paste, sep="-")
    log_debug("good path: {log_path}")
    good_paths <- append(good_paths, list(path))
  } else if ( path_allowed( rules, next_node, path)) {
    good_paths <-  do_node(links, rules, next_node, path, good_paths)
  } else {
    #do nothing.
  }
  return(good_paths)
}


  
do_node <- function (links, rules, node, path, good_paths) {
  path <- append(path, node)
  node_links <- node_links(links, node)
  log_path <- reduce(path, paste, sep="-")
  log_links <- reduce(node_links, paste, sep=",")
  log_debug("do_node, node: {node}, path: {log_path}, node_links: {log_links} ")
  for (i in seq_along(node_links)) {
    log_debug("node: {node} trying link: {node_links[i]}")
    good_paths <-  try_link(links, rules, node_links[i], path, good_paths)
  }
  return(good_paths) 
}

get_paths <- function(raw_cavelink_data, rules) {
  path <- list()
  good_paths <- list()
  paths <- do_node(wrangle_links(raw_cavelink_data), rules, "start", path, good_paths)
  return(paths)
}

get_cave_paths_count <- function(raw_cavelink_data, rules = "rules_1") {
  get_paths(raw_cavelink_data, rules) |> 
    length()
}
