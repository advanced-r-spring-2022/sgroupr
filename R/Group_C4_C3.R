#' @export
new_group_C4_C3 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 15L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Group_C4_C3")
}

#' @export
Group_C4_C3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_group_C4_C3(x)
}










