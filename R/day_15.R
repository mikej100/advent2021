 # Advent of Code day 15
library(igraph)
library(stringr)
library(purrr)

g3 <- make_lattice(length=3, dim=2)
plot(g3)
tkplot(g3, edge.label=E(g3)$id)
plot(g3)
E(g3)

wel3 <- c(1,2,1,2,3,6,4,5,3,5,6,8,7,8,1,8,9,3,1,4,1,2,5,3,3,6,8,4,7,2,5,8,1,6,9,3)
wm3<-matrix( wel3, ncol=3,  byrow=TRUE)
wgm3 <- graph_from_edgelist(wm3[,1:2], directed=FALSE)
plot(wgm3)
E(wgm3)$weight <- wm3[,3]
plot(wgm3, edge.width=(1/(E(wgm3)$weight)), edge.label=wm3[,3])

shortest_paths(wgm3, from=1, to=9, output="both")

el3 <-c(1,2,2,3,4,5,5,6,7,8,8,9,1,4,2,5,3,6,4,7,5,8,6,9)
w3<-c(   1,  6,  3,  8,  1,  3,  1,  3,  8,  2,  1,  3)
m3 <- matrix(el3, ncol=2, byrow = TRUE)
gm3<-graph_from_edgelist(m3, directed = FALSE)
E(gm3)$weight<-w3
plot(gm3,  edge.label=E(gm3)$weight)
shortest_paths(gm3, from=1, to=9, output="both")

l3 <- make_lattice(length=3, dim=2)
plot(l3)
E(l3)

g10 <- make_lattice(length=10, dim=2 )
plot(g10)
E(g10)

raw_day15_data <-test_day15_data

ncol <- str_length(raw_day15_data[1])
wt_mat <-  raw_day15_data|>
  map(~ str_split(.x, "")) |>
  unlist() |>
  as.numeric() |>
  matrix(ncol = ncol, byrow=TRUE)
wt_mat

# Assumes square matrix.
row_m <- function (m, r) {
  dim <- dim(m)
  zp <- map2(m[r,2:dim[2]], m[r+1,1:dim[2]-1], ~ list(.x, .y)) |>
    unlist()
  row <- append (zp, m[r+1,dim[2]] )
  return(row)
}

edge_wt <- function(m) {
  dim <- dim(m)
  wts <- map(1:(dim[1]-1), ~ row_m(m, .x)) |>
    append ( m[dim[1],2:dim[2]] ) |>
    unlist()
  return(wts)
}


 E(g10)$weight <- edge_wt(wt_mat)

tkid <- tkplot(g10,
               edge.label=E(g10)$weight,
               vertex.color="light green",
               layout=layout_as_tree(g10, root=10)
               )
tk_rotate(tkid, degree=30 )



#get.edge.attribute(g10)
shortest_paths(g10, from=1, to=100, output="both")
