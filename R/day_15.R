 # Advent of Code day 15
library(igraph)
library(stringr)
library(purrr)

wrangle_chiton_data <- function (chiton_data) {
  ncol <- str_length( chiton_data[1] )
  chiton_data |>
    map( ~ str_split(.x, "")) |>
    unlist() |>
    as.numeric() |>
    matrix(ncol = ncol, byrow = TRUE)
}
# Assumes square matrix.
row_m <- function (m, r) {
  dim <- dim(m)
  zp <- map2(m[r,2:dim[2]], m[r+1,1:dim[2]-1], ~ list(.x, .y)) |>
    unlist()
  row <- append (zp, m[r+1,dim[2]] )
  return(row)
}

# Set up sequence of edge weights based on matrix m.
# View E(m) to see order required.
# Returns vector of weights in correct sequence to assign to lattice edges.
edge_weights <- function(m) {
  dim <- dim(m)
  map(1:(dim[1]-1), ~ row_m(m, .x)) |>
    append ( m[dim[1],2:dim[2]] ) |>
    unlist()
}

find_shortest_path <- function (chiton_data) {
  cost_matrix <- wrangle_chiton_data(chiton_data)
  
  # Set up the graph to solve, this is a lattice of dimensions of input data.
  # Tested for square input data only.
  ncol <- str_length(chiton_data[1])
  chiton_graph  <- make_lattice(length = ncol,
                                directed = TRUE,
                                dim = 2)
  
  # As# sign the weights,
  E(chiton_graph)$weight <- edge_weights(cost_matrix)
  
  # Vi# ew graph so can check.
  # tkid <- tkplot(gr,
  #                edge.label=E(gr)$weight,
  #                vertex.color="light green",
  #                layout=layout_as_tree(gr, root=ncol)
  #                )
  # tk_rotate(tkid, degree=30 )
  
  # Call iGraph function ot find shortest path (uses Dijkstra's).
  path <-
    shortest_paths(
      chiton_graph,
      from = 1,
      to = ncol ^ 2,
      output = "vpath",
      mode = "all"
    )
  # Extract vertices of path from shortest_paths result, remove starting position
  # sum the corresponding weightings.
  
  
  path_sum <- sum(t(cost_matrix) [path$vpath[[1]][-1]])
  
  
  vp <- path$vpath[[1]]
  sum1 <- sum(t(cost_matrix)[vp[2:(length(vp))]])
 
  # Make a mark-up version to view best path
  # path is shown by two digit numbers (10 added)
  markup <- t(cost_matrix) |>
    imap( ~ ifelse(.y %in% vp, .x + 10, .x)) |>
    matrix(ncol = ncol, byrow = TRUE)
  
  cc <- 1
}

aa <- find_shortest_path(test_day15_data)
bb <- find_shortest_path(day15_data)
