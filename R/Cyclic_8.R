new_cyclic_8 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 8L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Cyclic_8")
}

#' @title
#' Create a Cyclic 8 sgroupr_sgrp object
#'
#' @description The length of Cyclic 8 should be 0 to 7
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic 8
#' @export
#' @examples
#' Cyclic_8(0:7)
Cyclic_8 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_cyclic_8(x)
}



