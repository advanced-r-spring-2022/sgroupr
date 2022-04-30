#' @export
new_cyclic_16 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 15L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Cyclic_16")
}

#' @export
Cyclic_16 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_cyclic_16(x)
}

#' @export
vec_arith.sgrp.sgrp <- function(op, x, y, ...) {
  # check whether the group is the via pre-defined group function
  # This group function extract the group attribute of sgrp class.
  stopifnot(group(x) == group(y))
  # create claytable
  table <- cayley_c16_table
  return_index <- as.integer(1L + vec_cast(y,integer()) %% 16L)
  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_cyclic_16(x),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.numeric.sgrp <- function(op, x, y, ...) {
  # if the x is float, we will discard the decimal, and convert it into the integer.
  x <- as.integer(x)
  # For each group we have special rule of adding a number.
  table <- cayley_c16_table
  # initialize the return index as 0 and  we return original if we do nothing
  # the cycle is 4
  return_index <- 1L + as.integer(x) %% 16L
  y <- table[cbind(as.integer(y) + 1L, return_index)]
  switch(
    op,
    "+" = new_cyclic_16(y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.sgrp.numeric <- function(op, x, y, ...) {
  y <- as.integer(y)
  # For each group we have special rule of adding a number.
  table <- cayley_c16_table

  return_index <- 1L + y %% 16L

  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_cyclic_16(x),
    stop_incompatible_op(op, x, y)
  )
}



