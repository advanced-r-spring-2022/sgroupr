
#' @export
new_sgrp <- function(data = integer(), group = character()) {
  vctrs::vec_assert(data,integer())
  vctrs::vec_assert(group, character(), size = 1)
  if(setequal(group,"Cyclic_3")) {
    data[data > 2L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_6")) {
    data[data > 5L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_8")) {
    data[data > 7L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_16")) {
    data[data > 15L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Dih_8")) {
    data[data > 8L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Group_C4_C3")) {
    data[data > 11L | data < 0L] <- NA_integer_
  }
  vctrs::new_vctr(data, group = group , class = "sgroupr_sgrp")
}

#' @export
new_cyclic_3 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 2L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Cyclic_3")
}

#' @export
sgrp <- function(x = integer(), group = NA_character_) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vctrs::vec_cast(x, integer())
  group <- vctrs::vec_cast(group, character())
  new_sgrp(x, group = group)
}

#' @export
Cyclic_3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vctrs::vec_cast(x, integer())
  new_cyclic_3(x)
}

#' @export
group <- function(x) {
  attr(x,"group")
}

#' @export
vec_cast.sgroupr_sgrp.sgroupr_sgrp <- function(x,to, ...) {
  new_sgrp(vctrs::vec_data(x), group = group(to))
}
#' @export
vec_cast.sgroupr_sgrp.integer <- function(x, to, ...) {
  new_sgrp(x, group = group(to))
}
#' @export
vec_cast.integer.sgroupr_sgrp <- function(x, to, ...)  vctrs::vec_data(x)


#' @export
vec_arith.sgroupr_sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgroupr_sgrp", y)
}
#' @export
vec_arith.sgroupr_sgrp.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.sgroupr_sgrp.sgroupr_sgrp <- function(op, x, y, ...) {
  # check whether the group is the via pre-defined group function
  # This group function extract the group attribute of sgrp class.
  stopifnot(group(x) == group(y))
  # create claytable
  table <- cayley_c3_table
  return_index <- as.integer(1L + vctrs::vec_cast(y,integer()) %% 3L)
  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_cyclic_3(x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.numeric.sgroupr_sgrp <- function(op, x, y, ...) {
  # if the x is float, we will discard the decimal, and convert it into the integer.
  x <- as.integer(x)
  # For each group we have special rule of adding a number.
  table <- cayley_c3_table
  # initialize the return index as 0 and  we return original if we do nothing
  # the cycle is 4
  return_index <- 1L + as.integer(x) %% 3L
  y <- table[cbind(as.integer(y) + 1L, return_index)]
  switch(
    op,
    "+" = new_cyclic_3(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.sgroupr_sgrp.numeric <- function(op, x, y, ...) {
  y <- as.integer(y)
  # For each group we have special rule of adding a number.
  table <- cayley_c3_table

  return_index <- 1L + y %% 3L

  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_cyclic_3(x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

































