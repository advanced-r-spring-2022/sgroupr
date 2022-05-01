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
Cyclic_3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vctrs::vec_cast(x, integer())
  new_cyclic_3(x)
}
