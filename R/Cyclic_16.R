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
#' @examples
#' Cyclic_16(0:15)
Cyclic_16 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_cyclic_16(x)
}

#' @title
#' Check if a sgroupr_sgpr is Cyclic 16
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_Cyclic_16(Cyclic_16(0:15))
is_Cyclic_16 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "Cyclic_16") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Cyclic 16 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Cyclic 16
#' @export
#' @examples
#' x <- 0:15
#' as_Cyclic_16(x)
as_Cyclic_16 <- function(x) {
  as_sgrp(x, group = "Cyclic_16")
}


