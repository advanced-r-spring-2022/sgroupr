new_cyclic_6 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 5L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Cyclic_6")
}

#' @title
#' Create a Cyclic 6 sgroupr_sgrp object
#'
#' @description The length of Cyclic 6 should be 0 to 5
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic 6
#' @export
#' @examples
#' Cyclic_6(0:5)
Cyclic_6 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_cyclic_6(x)
}

#' @title
#' Check if a sgroupr_sgpr is Cyclic 6
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_Cyclic_6(Cyclic_6(0:5))
is_Cyclic_6 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "Cyclic_6") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Cyclic 6 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Cyclic 6
#' @export
#' @examples
#' x <- 0:5
#' as_Cyclic_6(x)
as_Cyclic_6 <- function(x) {
  as_sgrp(x, group = "Cyclic_6")
}
