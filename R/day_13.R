library(tidyverse)

# Make the matrix to fold matrics in half along axos of row centre or 
# column centre.
# dim is dimension in the axis perpendicualr to the fold
# axis is the axis of the fole taking value "row" or "col".
# returns squere matrix, left multipy for row fold and right multiply for 
# column fold
make_folder <- function (dim, axis='row') {
  make_entries <- function (cols, rows) {
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

m1 <- matrix(c(0,1,1,0,0,0, 0,1,0,0,1,0, 0,0,0,0,0,1,
               0,0,0,0,0,1, 0,0,0,0,0,0, 0,1,0,0,0,0),
             ncol=6, byrow=TRUE)
m2 <- matrix(c(0,1,1,0,0,0, 0,1,0,0,1,0, 
               0,0,0,0,0,1, 0,0,1,1,0,1),
             ncol=4, byrow=TRUE)
t1 <- matrix(c(1,0,0,0,0,1, 0,1,0,0,1,0, 0,0,1,1,0,0,
               0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0),
             ncol=6, byrow=TRUE)

t2 <- matrix(c(1,0,0,0,0,1, 0,1,0,0,1,0, 0,0,1,1,0,0),
             ncol=6, byrow = TRUE)




t2%*%m2

t3<-matrix(c(1,0,0,1, 0,1,1,0),ncol=2, byrow=FALSE)


map(0:3, ~ c(rep(0,.x), 1, rep(0, 2*(len-.x-1)), 1, rep(0,.x)) )
map(0:3, ~ c(rep(0,.x), 1)) 
