new_cyclic_16 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 15L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Cyclic_16")
}

#' @title
#' Create a Cyclic 16 sgroupr_sgrp object
#'
#' @description The length of Cyclic 16 should be 0 to 15
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic 16
#' @export
#' @example
#' Cyclic_16(0:15)
Cyclic_16 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_cyclic_16(x)
}




