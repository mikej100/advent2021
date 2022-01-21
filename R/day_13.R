library(tidyverse)
library(logger)

wrangle_dots <- function (raw) {
  coords <- head_while(raw, ~  str_length(.x) > 0) |>
    map( ~ as.numeric(unlist(str_split(.x, ",")))) |>
    transpose() |>
    simplify_all() |>
    as_tibble(.name_repair = \(x) c("x", "y"))
  
  folds <- tail_while(raw, ~ str_length(.x) > 0)
  
  return (list(coords = coords, folds = folds))
}

# Make the matrix to fold matrics in half along axos of row centre or 
# column centre.
# dim is dimension in the axis perpendicualr to the fold
# axis is the axis of the fole taking value "row" or "col".
# returns squere matrix, left multipy for row fold and right multiply for 
# column fold
make_folder <- function (dim, axis='row') {
  make_entries <- function (dim) {
    map(0:(dim/2-1), ~ 
          c(rep(0, .x), 1, rep(0, 2*(dim/2 - .x - 1)), 1, rep(0, .x)) ) |>
      unlist()
  }
  if (axis == 'col') {
    matrix(make_entries(dim), ncol = dim/2, byrow = FALSE)
  } else {
    matrix(make_entries(dim), nrow = dim/2, byrow = TRUE)
  }
}

fold_by_row <- function(m) {
  make_folder(nrow(m), 'row') %*% m
}

fold_by_col <- function(m) {
  m %*% make_folder(ncol(m), 'col') 
}

count_dots <- function (m) {
  reduce(m, .init=0,  ~ ifelse(.y > 0, .x + 1, .x))
  
}

dot_data <- wrangle_dots( day13_data )

m0 <- matrix(0, nrow=max(dot_data$coords$y)+1, ncol = max(dot_data$coords$x)+1 )

m1 <- reduce2(dot_data$coords$y, dot_data$coords$x, .init = m0, 
              .f= function (m, y, x) {
                m[y+1,x+1] <- 1
                m
              }
             )

m2<- fold_by_col(m1)

count_dots(m2)

m3<- fold_by_col(m2)

