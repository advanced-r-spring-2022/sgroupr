new_C4_C3 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "C4_C3")
}

#' @title
#' Create a Group C4_C3 sgroupr_sgrp object
#' @description C4_C3 belongs to the multiplication Group
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group C4_C3
#' @export
#' @examples
#' C4_C3(0:11)
C4_C3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_C4_C3(x)
}

#' @title
#' Check if a sgroupr_sgpr is C4_C3
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples 
#' is_C4_C3(C4_C3(0:11))
is_C4_C3 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "C4_C3") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C4_C3
#' @param x A vector
#' @return An Sgroupr_sgrp group that belongs to C4_C3
#' @export
#' @examples
#' x <- 0:11
#' as_C4_C3(x)
as_C4_C3 <- function(x) {
  as_sgrp(x, group = "C4_C3")
}
