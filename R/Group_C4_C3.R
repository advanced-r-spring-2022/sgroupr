new_group_C4_C3 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 11L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Group_C4_C3")
}

#' @title
#' Create a Group C4*C3 sgroupr_sgrp object
#'
#' @description The length of Group C4*C3 should be 0 to 11
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic C4*C3
#' @export
#' @examples
#' Group_C4_C3(0:11)
Group_C4_C3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_group_C4_C3(x)
}










