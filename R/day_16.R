library (stringr)
library(purrr)
# transmission = packet;
# packet = header, data |{packet}
# packet = literal packet | operator packet
# header= version, typeID
# version::=3 * bit
# typeID:== 3 * bit
# 
# literal type id = b100
# literal packet::=version, literal type id, 
#    literal number
# literal number = cont, 4bit number, {cont, 4bit number}
# 
# cont = b1(continued) | b0(last)
# 
# operator_packet = version, operator type id,
# length type id, 15bit sub-packet length | 11bit sub packet count, sub-packets
# length type id = b0 (15bit sub-packet length) | b1 (11bit sub-packet count) 
# length_type_15 = 0
# length_type_11 = 1
t_transmission <- function (s, p, t) {
  t_packet(s, p, t)
}

t_packet <- function (s, p, t){
  if (p == str_length(s))
    return(list(source=s, pointer=p , tokens=t))
  else
    return (t_version(s, p, t) )
}
t_version <- function (s, p, t){
  span <- 3
  bite <- get_bite(s, p, span)
  t <- push_t(t, "version", bite)
  p <- p + span
  return (t_type(s, p, t))
}

t_type <- function(s, p, t){
  span <- 3
  bite <- get_bite(s, p , span)
  t <- push_t(t, "type_id", bite)
  p <- p + span
  if (str_detect(bite, "100")) {
    return (t_literal (s, p ,t))
  }else{
    return (t_operator(s, p,t))
  }
}

t_literal <- function (s, p, t){
  span <- 5
  bite <- get_bite(s, p, span)
  if ( str_detect(bite, "1.{4}") ) {
    return (t_number_continued (s, p, t) )
  } else {
    return (t_number_terminal (s, p, t) )
  }
}
t_number_continued <- function(s, p, t) {
  span <- 5
  bite <- get_bite(s, p, span)
  t <- push_t(t, "number_continued", bite)
  p <- p + span
  if (str_detect(bite, "1.{4}")) {
    return( t_literal(s, p,t ))
  } else {
    return( t_number_terminal(s, p , t))
  }
}
get_token <- function(s, p, t, span, token_name) {
  t <- push_t(t, token_name, substring(s, p, p + span -1 ))
}
t_number_terminal <- function(s, p, t) {
  span <- 5
  bite <- get_bite(s, p, span)
  t <- push_t(t, "number_terminal", bite)
  p <- p + span
  return (t_any_more(s, p, t))
}

t_operator <- function(s, p , t) {
  span <- 1
  bite <- get_bite(s, p, span)
  t <- push_t(t, "length_type_id", bite)
  p <- p + span
  if (str_detect(bite, "0")) {
    return(t_length_type_15(s, p, t))
  } else{
    return(t_length_type_11(s, p, t))
  }
}
t_lengthtype_15 <- function (s,p,t) {
  span <- 15
}
# Check for trailing zeroes.
# If rest of transmission is zeroes then throw away
# else try for another packet
t_any_more <- function(s, p ,t){
  rest_of_transmission <- substring(s,p, str_length(s))
  if ( str_detect(rest_of_transmission, "0*" )) {
    t <- push_t(t, "terminal zeroes", rest_of_transmission )
    return(list(source=s, pointer=p, tokens=t))
  } else {
    t_packet(s, p, t)
  }
}

get_bite <- function (s, p, span){
  substring(s, p, p + span -1 )
}

push_t <- function(t, token, value) {
  t <- rbind(t , list(token=token, value=value))
}

hex_to_binstring <- function(hex) {
  ints <- str_split(hex, "") |>
    unlist() |>
    map( ~strtoi(.x, 16)) |>
    map(intToBits) |>
    map( ~ head(.x, 4)) |>
    map(rev) |>
    map(as.integer)|>
    map(as.character)|>
    unlist() |>
    reduce( ~ paste0( .x, .y))
    #reduce(~paste0(.x, .y))
   return(ints) 
}

#t <- list(token=NULL, value=NULL)
ex_1_b <-"110100101111111000101000"
t <- list()
ex_1 <- "D2FE28"
ex_2 <- "38006F45291200"
p <- 1
s <- 


res1 <- t_transmission(s, p , t) 
