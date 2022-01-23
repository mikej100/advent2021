# Day 14
library(purrr)
library(dplyr)
library(usethis)

wrangle_polymer_data <- function(raw_data) {
  template <- head_while(raw_data, ~  str_length(.x) > 0) |>
    str_split("") |>
    unlist()
    
  pairs <- tail_while(raw_data, ~ str_length(.x) > 0) |>
    map(~ ( unlist(str_split( .x, fixed( " -> ") ) ) ) ) |>
    transpose()
  
  pair_rules <- pairs[[2]]
  names(pair_rules) <- pairs[[1]]
  
  return (list(template=template, pair_rules = pair_rules))
}

raw_data <- test_day14_data
polymer_data <- wrangle_polymer_data(raw_data)
# lookup table for pair rules to insert value.
lk <- polymer_data$pair_rules
# polymer starting template.
tp <- polymer_data$template

# calculate letters to insert into template tp for lookup table lk
insert <- function(tp, lk) {
  map2(tp, lead(tp), ~ paste0( .x, .y ) ) |>
  map( ~ lk[[.x]]) |>
  unlist()
}

# zip template tp with insert letters as vector.
zip <- function (tp, insert) {
  reduce2 ( tail(tp, -1), insert, .init=tp[1], \(c, t, i) c(c, i ,t))
}

# run the template n steps through the insert generation and zipping.
make_polymer <- function (tp, lk, n) {
  reduce(1:n, .init=tp, \(x, i) zip(x, insert(x, lk) ) )
}

make_polymer( tp, lk, 2 )
