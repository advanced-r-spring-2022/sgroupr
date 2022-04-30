#'Klein 8 Group
#'
#'
#'
#'
#'
#'
library(vctrs)

# This is an overarching group sgrp
new_sgrp <- function(data = integer(), group = character()) {
  vec_assert(data,integer())
  vec_assert(group, character(), size = 1)
  if(setequal(group,"Klein_8")) {
    data[data > 7L | data < 0L] <- NA_integer_
  } 
  new_vctr(data, group = group , class = "sgrp")
}

new_Klein_8 <- function(x = integer()) {
  # check whether the x is integer vector 
  vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  x[x > 7L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Klein_8")
}

#Helper function
sgrp <- function(x = integer(), group = NA_character_) {
  # make sure the input data is integral. 
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  group <- vec_cast(group, character())
  new_sgrp(x, group = group)
}
Klein_8 <- function(x = integer()) {
  # make sure the input data is integral. 
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_Klein_8(x)
}

#Other helper function
is_sgrp <- function(x) {
  inherits(x, "sgrp")
}
is_Klein_8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "Klein_8") {
    return(!result)
  } else return(result)
}

#Coercion 
vec_cast.sgrp.sgrp <- function(x,to, ...) {
  new_sgrp(vec_data(x), group = group(to))
}
vec_cast.sgrp.integer <- function(x, to, ...) {
  new_sgrp(x, group = group(to))
}
vec_cast.integer.sgrp <- function(x, to, ...)  vec_data(x)

vec_arith.sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgrp", y)
}
vec_arith.sgrp.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


#Extract the attributes
group <- function(x) {
  attr(x,"group")
}


#Format
format.sgrp <- function(x, ...) {
  out <- formatC(signif(vec_data(x),3))
  # Makes nicer to print NA instead of "NA"
  out[is.na(x)] <- NA
  cat(attr(x,"group"), "\n")
  out
}


#Arithmetic
Klein8_cayley <- matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                        1L, 0L, 3L, 2L, 5L, 4L, 7L, 6L,
                        2L, 3L, 0L, 4L, 6L, 7L, 4L, 5L,
                        3L, 2L, 4L, 0L, 1L, 2L, 3L, 4L,
                        4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L,
                        5L, 4L, 7L, 6L, 1L, 0L, 3L, 2L,
                        6L, 7L, 4L, 5L, 2L, 3L, 0L, 1L),
                       nrow = 8, ncol = 8)
rownames(Klein8_cayley) <- 0:7
colnames(Klein8_cayley) <- 0:7


vec_arith.sgrp.sgrp <- function(op, x, y, ...) {
  # check whether the group is the via pre-defined group function
  # This group function extract the group attribute of sgrp class. 
  stopifnot(group(x) == group(y))
  # create claytable
  table <- Klein8_cayley
  return_index <- as.integer(1L + vec_cast(y,integer()) %% 8L)
  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_Klein_8(x),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.numeric.sgrp <- function(op, x, y, ...) {
  # if the x is float, we will discard the decimal, and convert it into the integer.
  x <- as.integer(x)
  # For each group we have special rule of adding a number. 
  table <- Klein8_cayley
  # initialize the return index as 0 and  we return original if we do nothing
  # the cycle is 4
  return_index <- 1L + as.integer(x) %% 8L
  y <- table[cbind(as.integer(y) + 1L, return_index)]
  switch(
    op,
    "+" = new_Klein_8(y),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.sgrp.numeric <- function(op, x, y, ...) {
  y <- as.integer(y)
  # For each group we have special rule of adding a number. 
  table <- Klein8_cayley
  
  return_index <- 1L + y %% 8L
  
  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_Klein8(x),
    stop_incompatible_op(op, x, y)
  )
}

#Example 
y <- Klein_8(2)
x <- Klein_8(0:7)
x + y
2 + x
x + 0
x + 1
x + 2
x + 3


