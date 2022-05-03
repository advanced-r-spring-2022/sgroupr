new_C4_C3 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  # create it!
  new_sgrp(x, group = "C4_C3")
}

#' @title
#' Create a Group C4*C3 sgroupr_sgrp object
#' @description The length of Group C4*C3 should be 0 to 11
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic C4*C3
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
#' Check if a sgroupr_sgpr is Group_C4_C3
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_C4_C3(C4_C3(0:11))
is_C4_C3 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "C4_C3") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Group_C4_C3
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Group_C4_C3
#' @export
#' @examples
#' x <- 0:11
#' as_C4_C3(x)
as_C4_C3 <- function(x) {
  as_sgrp(x, group = "C4_C3")
}










